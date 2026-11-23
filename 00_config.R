# =====================================================================
# TCC - Recursos represados no SUS (MG, 2016-2022)
# Script 00 - Configuração: pacotes, pastas e funções auxiliares
#
# Este script NÃO precisa ser rodado sozinho: os demais scripts
# chamam este arquivo na primeira linha (source("scripts/00_config.R")).
# Rode-o sozinho apenas na primeira vez, para instalar os pacotes.
#
# Todos os caminhos são relativos à pasta do projeto (a que contém
# TCC_R.Rproj, "scripts" e "dados"). Abra o TCC_R.Rproj no RStudio, ou
# use setwd() apontando para essa pasta, antes de rodar qualquer script.
# =====================================================================

# ---------------------------------------------------------------------
# 1. Pacotes (instala apenas os que faltarem)
# ---------------------------------------------------------------------
pacotes <- c("readxl", "dplyr", "tidyr", "stringr", "stringi", "ggplot2",
             "writexl", "sandwich", "lmtest", "readr")
faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltando) > 0) install.packages(faltando)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr); library(stringi)
  library(ggplot2); library(writexl); library(sandwich); library(lmtest); library(readr)
})
options(scipen = 999, dplyr.summarise.inform = FALSE)

# ---------------------------------------------------------------------
# 2. Pastas (caminhos relativos à pasta do projeto TCC_R)
# ---------------------------------------------------------------------
DADOS   <- "dados"
SAIDA   <- "saida"
TAB     <- file.path(SAIDA, "tabelas")
FIG     <- file.path(SAIDA, "figuras")
BASES   <- file.path(SAIDA, "bases")
if (!dir.exists(DADOS)) {
  stop("Pasta 'dados' não encontrada em ", getwd(), ". Abra o projeto TCC_R.Rproj no RStudio ",
       "(ou use setwd() para apontar para a pasta do projeto) e coloque nela os arquivos de ",
       "entrada listados no README.")
}
# cria as pastas de saída só depois de confirmar que se está na pasta certa
for (p in c(SAIDA, TAB, FIG, BASES)) dir.create(p, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 3. Constantes
# ---------------------------------------------------------------------
FAIXAS <- c("Até 5 mil", "5 a 10 mil", "10 a 20 mil", "20 a 50 mil",
            "50 a 100 mil", "Acima de 100 mil")
CORTES <- c(0, 5000, 10000, 20000, 50000, 100000, Inf)
ANO_INI <- 2016; ANO_FIM <- 2022

# ---------------------------------------------------------------------
# 4. Funções auxiliares
# ---------------------------------------------------------------------
# texto sem acento, em maiúsculas e sem espaços duplicados
norm_txt <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  str_squish(toupper(x))
}

# converte para número (aceita texto vazio)
num <- function(x) suppressWarnings(as.numeric(x))

# coeficiente de Gini
gini <- function(x) {
  x <- sort(x[!is.na(x)]); n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

# participação dos 10% maiores no total (%)
top10 <- function(x) {
  x <- sort(x[!is.na(x)], decreasing = TRUE)
  sum(head(x, round(length(x) * 0.10))) / sum(x) * 100
}

# estrelas de significância
estrela <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))

# V de Cramér a partir de uma tabela de contagens
cramer_v <- function(tab) {
  ch <- suppressWarnings(chisq.test(tab, correct = FALSE))
  list(qui2 = unname(ch$statistic), gl = unname(ch$parameter), p = ch$p.value,
       v = sqrt(unname(ch$statistic) / (sum(tab) * (min(dim(tab)) - 1))), n = sum(tab))
}

# Modelo de resposta fracionária (Papke e Wooldridge, 1996):
# GLM quase-binomial com ligação logit e erros-padrão robustos (HC1).
# Se 'cluster' for informado (nome de uma ou duas colunas), os erros são agrupados por elas.
frac_logit <- function(formula, dados, cluster = NULL) {
  dados <- as.data.frame(dados)
  vars <- unique(c(all.vars(formula), cluster))
  dados <- dados[complete.cases(dados[, vars]), , drop = FALSE]
  m <- glm(formula, data = dados, family = quasibinomial(link = "logit"))
  # um nome de coluna = agrupamento simples; dois nomes (ex.: c("cod6", "instr_id")) = agrupamento duplo
  # (CAMERON; GELBACH; MILLER, 2011)
  V <- if (is.null(cluster)) vcovHC(m, type = "HC1") else
    if (length(cluster) == 1) vcovCL(m, cluster = dados[[cluster]], type = "HC1") else
      vcovCL(m, cluster = dados[, cluster], type = "HC1", multi0 = FALSE)
  ct <- coeftest(m, vcov. = V)
  p <- fitted(m)
  ame <- mean(p * (1 - p)) * coef(m) * 100       # efeito marginal médio em pontos percentuais
  # pseudo-R² de McFadden calculado com a verossimilhança binomial
  ll <- function(mu) sum(m$y * log(pmax(mu, 1e-12)) + (1 - m$y) * log(pmax(1 - mu, 1e-12)))
  pr2 <- 1 - ll(p) / ll(rep(mean(m$y), length(m$y)))
  list(modelo = m, coef = ct, ame = ame, p = ct[, 4], n = nobs(m), pseudo_r2 = pr2)
}

# MQO com erros-padrão robustos (HC1)
mqo <- function(formula, dados) {
  m <- lm(formula, data = dados)
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1"))
  list(modelo = m, coef = ct, p = ct[, 4], n = nobs(m), r2 = summary(m)$r.squared)
}

# Monta uma coluna "coef*** (p=0,000)" para tabelas de modelos
fmt_coef <- function(valor, p, dig = 2) {
  ifelse(is.na(valor), "", paste0(formatC(valor, format = "f", digits = dig, decimal.mark = ","), estrela(p),
                                  " (p=", formatC(p, format = "f", digits = 3, decimal.mark = ","), ")"))
}

# Rótulos das variáveis dos modelos
ROTULOS <- c(
  "(Intercept)" = "Constante",
  x_serv10 = "Servidores por mil hab. (+10)", x_estat10 = "% estatutários (+10 p.p.)",
  x_comiss10 = "% comissionados (+10 p.p.)", x_prec10 = "% estagiários e sem vínculo (+10 p.p.)",
  gestor_saude_superior = "Gestor da saúde com nível superior", fundo_gerido_pela_sms = "Fundo gerido pela SMS",
  colegiado_regional = "Participa de colegiado regional", x_auton = "IFGF Autonomia (+0,1)",
  x_pessoal = "IFGF Gastos com Pessoal (+0,1)", x_invest = "IFGF Investimentos (+0,1)",
  x_rec_propria = "% receita própria em ASPS (+10 p.p.)", x_dep_transf = "% transf. SUS na despesa (+10 p.p.)",
  x_desp_hab_log = "log despesa em saúde por hab.", c_instr_mil = "log(instrumentos por mil hab.)",
  c_recente = "Parcela do recebido em 2021-2022", x_repasse_hab_log = "log repasse estadual por hab./ano (R$ dez./2023)",
  x_pib_pc_log = "log PIB per capita (média 2016-2021)",
  `faixaAté 5 mil` = "Porte: até 5 mil", `faixa5 a 10 mil` = "Porte: 5 a 10 mil",
  `faixa10 a 20 mil` = "Porte: 10 a 20 mil", `faixa50 a 100 mil` = "Porte: 50 a 100 mil",
  `faixaAcima de 100 mil` = "Porte: acima de 100 mil")
rotulo <- function(v) ifelse(v %in% names(ROTULOS), ROTULOS[v], v)

# Tabela comparativa de modelos. 'lista' = lista nomeada de modelos (saídas de mqo ou frac_logit).
# Nos modelos fracionários mostra o efeito marginal médio (p.p.); no MQO, o coeficiente.
tab_modelos <- function(lista, dig = 2) {
  vars <- unique(unlist(lapply(lista, function(m) rownames(m$coef))))
  out <- tibble(`Variável` = rotulo(vars))
  for (nm in names(lista)) {
    m <- lista[[nm]]
    est <- if (!is.null(m$ame)) m$ame else m$coef[, 1]
    out[[nm]] <- fmt_coef(unname(est[vars]), unname(m$coef[, 4][vars]), dig = dig)
  }
  out <- out |> filter(`Variável` != "Constante")
  extra <- tibble(`Variável` = c("N", "R² (MQO) ou pseudo-R² (fracionário)"))
  for (nm in names(lista)) {
    m <- lista[[nm]]
    extra[[nm]] <- c(as.character(m$n), formatC(if (!is.null(m$ame)) m$pseudo_r2 else m$r2,
                                               format = "f", digits = 3, decimal.mark = ","))
  }
  bind_rows(out, extra)
}

# Grava várias tabelas em um arquivo Excel (uma aba por tabela)
grava_excel <- function(lista, arquivo) {
  names(lista) <- substr(names(lista), 1, 31)
  write_xlsx(lapply(lista, as.data.frame), file.path(TAB, arquivo))
  message("Tabelas gravadas em: ", file.path(TAB, arquivo))
}

# Tema dos gráficos (estático, para impressão)
AZUL <- "#2a78d6"; LARANJA <- "#eb6834"; VERDE <- "#1baf7a"; CINZA <- "#52514e"
tema_tcc <- theme_minimal(base_size = 10, base_family = "serif") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#e4e3df", linewidth = 0.4),
        axis.text = element_text(color = CINZA), legend.position = "bottom",
        legend.title = element_blank())
salva_fig <- function(g, nome, larg = 17, alt = 9) {
  ggsave(file.path(FIG, nome), g, width = larg, height = alt, units = "cm", dpi = 300, bg = "white")
  message("Figura gravada: ", file.path(FIG, nome))
}

message("Configuração carregada.")

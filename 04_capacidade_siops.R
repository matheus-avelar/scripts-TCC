# =====================================================================
# Script 04 - Capacidade estatal (MUNIC e IFGF) e indicadores fiscais (SIOPS)
# Entradas : dados/Base_MUNIC_2017.xls, dados/Base_MUNIC_2021.xlsx
#            dados/IFGF.xlsx (Firjan, evolução por indicador)
#            dados/siops/*.txt (7 exportações do TabNet do SIOPS, 2016-2022)
#            saida/bases/base_municipal_pop.rds (script 03)
# Saídas   : saida/tabelas/parte4_capacidade.xlsx (inclui a Tabela 2 do texto, aba T2_recursos_proprios_ano)
#            saida/bases/base_analitica.rds  (uma linha por município, todas as covariáveis)
#            saida/bases/siops_transf_estadual_ano.rds
#
# Indicadores (MUNIC, administração direta; média de 2017 e 2021):
#   servidores por mil hab.; % estatutários; % somente comissionados; % estagiários e sem vínculo.
# MUNIC 2021 (bloco Saúde): gestor da saúde com nível superior; fundo gerido pela SMS;
#   participação em colegiado de gestão regional.
# IFGF: média 2016-2022 de Autonomia, Gastos com Pessoal e Investimentos
#   (Liquidez fica fora dos modelos: saldo parado em conta aumenta a liquidez - causalidade reversa).
# SIOPS: média 2016-2022 de % recursos próprios em ASPS (3.2), % transferências SUS na despesa (3.1),
#   despesa total em saúde por habitante (2.1), % pessoal (2.2), % investimento (2.5);
#   transferências estaduais estimadas = R.Transf.SUS x (1 - %Transf. União/100).
# =====================================================================
source("scripts/00_config.R")
aba <- function(arq, padrao) { s <- excel_sheets(arq); s[grep(padrao, norm_txt(s))[1]] }

# ---------------------------------------------------------------------
# 1. MUNIC - recursos humanos (2017 e 2021)
# ---------------------------------------------------------------------
le_rh <- function(arq) {
  x <- read_excel(arq, sheet = aba(arq, "^RECURSOS HUMANOS"), col_types = "text")
  names(x) <- toupper(names(x))
  x |> filter(str_starts(CODMUN, "31")) |>
    transmute(cod6 = as.integer(substr(CODMUN, 1, 6)),
              estat = num(MREH0111), comiss = num(MREH0113), estag = num(MREH0114),
              semvinc = num(MREH0115), total = num(MREH0116))
}
r17 <- le_rh(file.path(DADOS, "Base_MUNIC_2017.xls"))
r21 <- le_rh(file.path(DADOS, "Base_MUNIC_2021.xlsx"))

# ---------------------------------------------------------------------
# 2. MUNIC 2021 - bloco Saúde
# ---------------------------------------------------------------------
arq21 <- file.path(DADOS, "Base_MUNIC_2021.xlsx")
s <- read_excel(arq21, sheet = aba(arq21, "^SAUDE$"), col_types = "text")
names(s) <- toupper(names(s))
SUP <- c("ENSINO SUPERIOR COMPLETO", "ESPECIALIZACAO", "MESTRADO", "DOUTORADO")
saude <- s |> filter(str_starts(CODMUN, "31")) |>
  transmute(cod6 = as.integer(substr(CODMUN, 1, 6)),
            gestor_saude_superior = as.integer(norm_txt(MSAU06) %in% SUP),
            fundo_gerido_pela_sms = as.integer(norm_txt(MSAU171) == "SECRETARIA MUNICIPAL DE SAUDE"),
            colegiado_regional = as.integer(norm_txt(MSAU19) == "SIM"))

# ---------------------------------------------------------------------
# 3. IFGF (média 2016-2022)
# ---------------------------------------------------------------------
arqI <- file.path(DADOS, "IFGF.xlsx")
le_ifgf <- function(nome_aba, var) {
  x <- read_excel(arqI, sheet = nome_aba)
  names(x)[1] <- "codigo"
  cols <- names(x)[str_detect(names(x), paste(2016:2022, collapse = "|"))]
  x |> filter(UF == "MG") |> transmute(cod6 = as.integer(substr(as.character(codigo), 1, 6)), !!var := rowMeans(across(all_of(cols), num), na.rm = TRUE))
}
ifgf <- le_ifgf("IFGF Geral", "ifgf_geral") |>
  left_join(le_ifgf("IFGF Autonomia", "ifgf_autonomia"), by = "cod6") |>
  left_join(le_ifgf("IFGF Gastos com Pessoal", "ifgf_pessoal"), by = "cod6") |>
  left_join(le_ifgf("IFGF Liquidez", "ifgf_liquidez"), by = "cod6") |>
  left_join(le_ifgf("IFGF Investimentos", "ifgf_investimentos"), by = "cod6")

# ---------------------------------------------------------------------
# 4. SIOPS (exportações do TabNet; o indicador é reconhecido pela 1ª linha do arquivo)
# ---------------------------------------------------------------------
ANOS <- as.character(2016:2022)
le_siops <- function(arq) {
  l <- readLines(arq, warn = FALSE, encoding = "UTF-8")
  l <- str_remove(l, "^\ufeff")                       # remove o BOM, se o Bloco de Notas gravou
  reg <- l[str_detect(l, '^"?\\d{6} ')]               # linhas de município, com ou sem aspas
  p <- str_split(str_remove_all(reg, '"'), ";", simplify = TRUE)
  v <- apply(p[, 2:8, drop = FALSE], 2, function(x) num(str_replace(str_remove_all(x, "\\."), ",", ".")))
  out <- as_tibble(v, .name_repair = "minimal"); names(out) <- ANOS
  list(titulo = norm_txt(str_remove_all(l[nchar(str_trim(l)) > 0][1], '"')), dados = bind_cols(tibble(cod6 = as.integer(substr(p[, 1], 1, 6))), out))
}
arqs <- list.files(file.path(DADOS, "siops"), pattern = "\\.(txt|csv)$", full.names = TRUE)
sio <- lapply(arqs, le_siops)
pega <- function(prefixo) {
  i <- which(str_starts(str_remove_all(sapply(sio, `[[`, "titulo"), " "), fixed(str_remove_all(prefixo, " "))))
  if (length(i) == 0) stop("Indicador do SIOPS não encontrado na pasta dados/siops: ", prefixo)
  sio[[i[1]]]$dados
}
media <- function(x) tibble(cod6 = x$cod6, m = rowMeans(as.matrix(x[, ANOS]), na.rm = TRUE))
transf_sus <- pega("R.TRANSF.SUS"); pct_uniao <- pega("1.4")
stopifnot(all(transf_sus$cod6 == pct_uniao$cod6))
est_ano <- as.matrix(transf_sus[, ANOS]) * (1 - as.matrix(pct_uniao[, ANOS]) / 100)
siops <- tibble(cod6 = transf_sus$cod6,
                siops_transf_estadual_2016_2022 = rowSums(est_ano),
                siops_transf_estadual_2020_2022 = rowSums(est_ano[, c("2020", "2021", "2022")])) |>
  left_join(media(pega("3.2")) |> rename(siops_rec_propria_asps = m), by = "cod6") |>
  left_join(media(pega("3.1")) |> rename(siops_dep_transf = m), by = "cod6") |>
  left_join(media(pega("2.1")) |> rename(siops_desp_saude_hab = m), by = "cod6") |>
  left_join(media(pega("2.2")) |> rename(siops_pct_pessoal = m), by = "cod6") |>
  left_join(media(pega("2.5")) |> rename(siops_pct_invest = m), by = "cod6")
siops_ano <- tibble(Ano = as.integer(ANOS), `Transferências estaduais estimadas - SIOPS (R$ mi)` = colSums(est_ano, na.rm = TRUE) / 1e6)
saveRDS(siops_ano, file.path(BASES, "siops_transf_estadual_ano.rds"))

# Tabela 2 do texto (subseção 5.4): % de recursos próprios aplicados em ASPS, por ano,
# entre os municípios de MG (indicador 3.2 do SIOPS); hiato = média - piso de 15%
rp <- pega("3.2")
t_rp <- tibble(Ano = as.integer(ANOS),
               `Municípios com dado` = sapply(ANOS, function(a) sum(!is.na(rp[[a]]))),
               `Média (%)` = sapply(ANOS, function(a) mean(rp[[a]], na.rm = TRUE)),
               `Mediana (%)` = sapply(ANOS, function(a) median(rp[[a]], na.rm = TRUE)),
               `Desvio-padrão` = sapply(ANOS, function(a) sd(rp[[a]], na.rm = TRUE)),
               `Mínimo` = sapply(ANOS, function(a) min(rp[[a]], na.rm = TRUE)),
               `Máximo` = sapply(ANOS, function(a) max(rp[[a]], na.rm = TRUE))) |>
  mutate(`Hiato do piso (p.p.)` = `Média (%)` - 15)

# ---------------------------------------------------------------------
# 4b. PIB per capita (IBGE, tabela 5938) e macrorregião de saúde (PDR-SUS/MG 2023)
#     As duas planilhas trazem só o nome do município: a ligação é feita pelo nome
#     normalizado, como no Censo 2022 (script 03).
#     PIB per capita = PIB a preços correntes (mil reais) x 1.000 / população estimada do
#     mesmo ano; usa-se a média de 2016 a 2021 (metodologia, Quadro 2).
# ---------------------------------------------------------------------
chave <- function(x) str_remove_all(tolower(stri_trans_general(x, "Latin-ASCII")), "[^a-z]")
pop_ano <- readRDS(file.path(BASES, "populacao.rds"))
pib <- read_excel(file.path(DADOS, "tabela5938.xlsx"), col_names = FALSE, col_types = "text", skip = 6,
                  .name_repair = "unique_quiet")[, 1:7]
names(pib) <- c("nome", paste0("pib", 2016:2021))
pib <- pib |> filter(str_ends(nome, "\\(MG\\)")) |>
  mutate(k = chave(str_remove(nome, " \\(MG\\)$")), across(starts_with("pib"), num))
pibpc <- pop_ano |> mutate(k = chave(str_remove(nome, " \\(MG\\)$"))) |> select(cod6, k, starts_with("p20")) |>
  inner_join(select(pib, -nome), by = "k") |>
  mutate(pib_pc = rowMeans(cbind(pib2016 / p2016, pib2017 / p2017, pib2018 / p2018, pib2019 / p2019,
                                 pib2020 / p2020, pib2021 / p2021) * 1000, na.rm = TRUE)) |>
  select(cod6, pib_pc)

mac <- read_excel(file.path(DADOS, "macrorregioes_pdr2023.xlsx"), sheet = 1, skip = 3, col_types = "text")
names(mac)[1:4] <- c("n", "macro", "micro", "municipio")
mac <- mac |> filter(!is.na(num(n)), !is.na(municipio)) |>
  mutate(k = chave(municipio),
         k = if_else(k == "baraodemontealto", "baraodomontealto", k))   # grafia do IBGE: Barão do Monte Alto
macro <- pop_ano |> mutate(k = chave(str_remove(nome, " \\(MG\\)$"))) |> select(cod6, k) |>
  left_join(select(mac, k, macro, micro), by = "k") |> select(-k)
message("PIB per capita sem dado: ", 853 - nrow(pibpc), " | macrorregião sem dado: ", sum(is.na(macro$macro)))

# ---------------------------------------------------------------------
# 5. Base analítica (853 municípios)
# ---------------------------------------------------------------------
bp <- readRDS(file.path(BASES, "base_municipal_pop.rds"))
cap <- bp |> select(cod6, pop_media) |>
  left_join(r17, by = "cod6") |> left_join(r21, by = "cod6", suffix = c("_17", "_21")) |>
  # média dos dois anos; se um ano estiver sem informação, usa o outro
  mutate(serv_mil = rowMeans(cbind(total_17, total_21), na.rm = TRUE) / pop_media * 1000,
         estat_pct = rowMeans(cbind(estat_17 / total_17, estat_21 / total_21), na.rm = TRUE) * 100,
         comiss_pct = rowMeans(cbind(comiss_17 / total_17, comiss_21 / total_21), na.rm = TRUE) * 100,
         precario_pct = rowMeans(cbind((estag_17 + semvinc_17) / total_17, (estag_21 + semvinc_21) / total_21), na.rm = TRUE) * 100) |>
  mutate(across(c(serv_mil, estat_pct, comiss_pct, precario_pct), ~ if_else(is.nan(.x), NA_real_, .x))) |>
  select(cod6, serv_mil, estat_pct, comiss_pct, precario_pct)
ba <- bp |> left_join(cap, by = "cod6") |> left_join(saude, by = "cod6") |>
  left_join(ifgf, by = "cod6") |> left_join(siops, by = "cod6") |>
  left_join(pibpc, by = "cod6") |> left_join(macro, by = "cod6") |>
  mutate(faixa = factor(faixa, levels = FAIXAS))
saveRDS(ba, file.path(BASES, "base_analitica.rds"))

faltas <- tibble(Fonte = c("MUNIC RH", "MUNIC Saúde", "IFGF", "SIOPS"),
                 `Municípios sem dado` = c(sum(is.na(ba$serv_mil)), sum(is.na(ba$gestor_saude_superior)),
                                           sum(is.na(ba$ifgf_autonomia)), sum(is.na(ba$siops_transf_estadual_2016_2022))))
print(faltas)

# ---------------------------------------------------------------------
# 6. Tabela 11 - capacidade por faixa (municípios com saldo declarado)
# ---------------------------------------------------------------------
d <- ba |> filter(declarou)
t11 <- d |> group_by(Faixa = faixa) |>
  summarise(`Municípios` = n(),
            `Servidores por mil hab. - mediana` = median(serv_mil, na.rm = TRUE),
            `% estatutários - mediana` = median(estat_pct, na.rm = TRUE),
            `% comissionados - mediana` = median(comiss_pct, na.rm = TRUE),
            `% estagiários e sem vínculo - mediana` = median(precario_pct, na.rm = TRUE),
            `Gestor da saúde com nível superior (%)` = mean(gestor_saude_superior, na.rm = TRUE) * 100,
            `Fundo gerido pela SMS (%)` = mean(fundo_gerido_pela_sms, na.rm = TRUE) * 100,
            `Participa de colegiado regional (%)` = mean(colegiado_regional, na.rm = TRUE) * 100,
            `IFGF Autonomia - mediana` = median(ifgf_autonomia, na.rm = TRUE),
            `IFGF Gastos com Pessoal - mediana` = median(ifgf_pessoal, na.rm = TRUE),
            `IFGF Investimentos - mediana` = median(ifgf_investimentos, na.rm = TRUE),
            `% recursos próprios em ASPS - mediana` = median(siops_rec_propria_asps, na.rm = TRUE),
            `Despesa em saúde por hab. - mediana (R$)` = median(siops_desp_saude_hab, na.rm = TRUE))

# ---------------------------------------------------------------------
# 7. Modelos provisórios sem repasses (mantidos para comparação com a versão anterior)
#    M1: log(1 + saldo fin./hab.) - MQO; M2: % impossibilidade - resposta fracionária
# ---------------------------------------------------------------------
d <- d |> mutate(y_fin_pc = log1p(saldo_fin / pop_media),
                 y_imposs = pmin(pmax(coalesce(pct_impossibilidade, 0) / 100, 0), 1),
                 x_serv10 = serv_mil / 10, x_estat10 = estat_pct / 10, x_comiss10 = comiss_pct / 10,
                 x_prec10 = precario_pct / 10, x_auton = ifgf_autonomia * 10, x_pessoal = ifgf_pessoal * 10,
                 x_invest = ifgf_investimentos * 10, c_instr_mil = log(n_instrumentos / pop_media * 1000),
                 c_recente = if_else(saldo_fin > 0, saldo_fin_2021_2022 / saldo_fin, 0),
                 faixa = relevel(factor(faixa, levels = FAIXAS), ref = "20 a 50 mil"))
CAP <- "x_serv10 + x_estat10 + x_comiss10 + x_prec10 + gestor_saude_superior + fundo_gerido_pela_sms + colegiado_regional + x_auton + x_pessoal + x_invest"
dd <- d |> filter(if_all(c(x_serv10, x_estat10, x_comiss10, x_prec10, gestor_saude_superior, fundo_gerido_pela_sms,
                           colegiado_regional, x_auton, x_pessoal, x_invest, c_instr_mil), ~ !is.na(.x)))
m1a <- mqo(as.formula(paste("y_fin_pc ~", CAP, "+ faixa")), dd)
m1b <- mqo(as.formula(paste("y_fin_pc ~", CAP, "+ c_instr_mil + c_recente + faixa")), dd)
m2a <- frac_logit(as.formula(paste("y_imposs ~", CAP, "+ faixa")), dd)
m2b <- frac_logit(as.formula(paste("y_imposs ~", CAP, "+ c_instr_mil + c_recente + faixa")), dd)

t12 <- tab_modelos(list(`M1a - log saldo fin./hab.` = m1a, `M1b - com controles` = m1b,
                        `M2a - % impossibilidade (EMM, p.p.)` = m2a, `M2b - com controles` = m2b), dig = 3)

grava_excel(list(T11_capacidade_por_faixa = t11, T12_modelos_provisorios = t12, Dados_faltantes = faltas,
                 SIOPS_transf_estadual_ano = siops_ano, T2_recursos_proprios_ano = t_rp), "parte4_capacidade.xlsx")
print(t_rp); print(t11); print(t12, n = 40)
message("Script 04 concluído.")

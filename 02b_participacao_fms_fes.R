# =============================================================================
# Participacao dos repasses aos Fundos Municipais de Saude (FMS) na despesa
# empenhada do Fundo Estadual de Saude (FES) - Minas Gerais, 2016-2022
#
# Script 02b - roda sozinho (não depende das bases dos demais scripts)
# Fonte: SIAFI-MG, extracoes "Exercicio_2016_2021.xlsx" e "Exercicio_2022_2026.xlsx"
# Saida: saida/tabelas/repasses_FMS_FES_2016_2022.xlsx
#
# Atenção: a identificação dos FMS aqui (razão social que CONTÉM "FUNDO MUNICIPAL
# DE SAUDE") é mais simples que a do script 02 (começa com "FUNDO MUNICIPAL DE
# SAUDE" ou com "PM "), porque o objetivo é dimensionar os repasses na despesa do FES.
# =============================================================================
source("scripts/00_config.R")

# ---- 0. Pacotes --------------------------------------------------------------
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

# ---- 1. Parametros -----------------------------------------------------------
# Pastas definidas no 00_config.R (relativas à raiz do projeto)
dir_dados <- DADOS
dir_saida <- TAB

arquivos <- c("Exercicio_2016_2021.xlsx", "Exercicio_2022_2026.xlsx")
ano_ini  <- 2016
ano_fim  <- 2022

# Raiz do CNPJ (8 primeiros digitos) das fundacoes vinculadas a SES-MG.
# A raiz pega tambem as unidades (filiais), como os hospitais da Fhemig.
raiz_fundacoes <- c(
  "19843929" = "Fhemig",
  "17503475" = "Funed",
  "26388330" = "Hemominas"
)

# ---- 2. Leitura --------------------------------------------------------------
# As planilhas tem 3 linhas de titulo; o cabecalho esta na 4a linha.
ler_siafi <- function(arquivo) {
  f <- file.path(dir_dados, arquivo)
  if (!file.exists(f)) stop("Arquivo não encontrado: ", f)
  read_excel(f, skip = 3) |>
    select(-starts_with("..."))          # remove a coluna A, vazia
}

base <- bind_rows(lapply(arquivos, ler_siafi))

# Nomes curtos para as colunas usadas. As colunas sao localizadas pelo inicio
# do nome, sem acentos, para evitar problemas de codificacao no Windows.
coluna <- function(padrao) {
  achou <- grep(padrao, names(base), value = TRUE)
  if (length(achou) != 1) stop("Coluna nao encontrada (ou repetida): ", padrao)
  achou
}
base <- base |>
  rename(
    ano          = all_of(coluna("^Ano de Exerc")),
    cnpj_cpf     = all_of(coluna("^CNPJ_CPF Credor")),
    razao_social = all_of(coluna("^Raz.o Social Credor")),
    empenhado    = all_of(coluna("^Valor Despesa Empenhada")),
    liquidado    = all_of(coluna("^Valor Despesa Liquidada")),
    pago_orc     = all_of(coluna("^Valor Pago Or.ament"))
  ) |>
  filter(ano >= ano_ini, ano <= ano_fim) |>
  mutate(
    across(c(empenhado, liquidado, pago_orc), ~ coalesce(as.numeric(.x), 0)),
    # sem acentos, como no script 02 (as expressões abaixo são escritas sem acento)
    razao  = norm_txt(coalesce(razao_social, "")),
    # CNPJ/CPF vem como numero: converte para texto com 14 digitos
    doc    = str_pad(sprintf("%.0f", coalesce(as.numeric(cnpj_cpf), 0)),
                     width = 14, pad = "0"),
    doc_n  = nchar(sprintf("%.0f", coalesce(as.numeric(cnpj_cpf), 0))),
    raiz   = substr(doc, 1, 8)
  )

# ---- 3. Identificacao dos FMS e das fundacoes estaduais ----------------------
base <- base |>
  mutate(
    eh_fms      = str_detect(razao, fixed("FUNDO MUNICIPAL DE SAUDE")),
    eh_fundacao = raiz %in% names(raiz_fundacoes)
  )

# ---- 4. Tabela anual ---------------------------------------------------------
anual <- base |>
  group_by(ano) |>
  summarise(
    fes_empenhado       = sum(empenhado),
    fundacoes_empenhado = sum(empenhado[eh_fundacao]),
    fms_empenhado       = sum(empenhado[eh_fms]),
    fms_pago_exercicio  = sum(pago_orc[eh_fms]),
    .groups = "drop"
  ) |>
  mutate(
    pct_fms_fes           = 100 * fms_empenhado / fes_empenhado,
    pct_fms_sem_fundacoes = 100 * fms_empenhado / (fes_empenhado - fundacoes_empenhado),
    pct_fms_pago          = 100 * fms_pago_exercicio / fms_empenhado
  )

# Totais por periodo (2016-2022, 2016-2019 e 2020-2022)
resumir_periodo <- function(dados, rotulo) {
  dados |>
    summarise(
      fes_empenhado       = sum(fes_empenhado),
      fundacoes_empenhado = sum(fundacoes_empenhado),
      fms_empenhado       = sum(fms_empenhado),
      fms_pago_exercicio  = sum(fms_pago_exercicio)
    ) |>
    mutate(
      periodo               = rotulo,
      pct_fms_fes           = 100 * fms_empenhado / fes_empenhado,
      pct_fms_sem_fundacoes = 100 * fms_empenhado / (fes_empenhado - fundacoes_empenhado),
      pct_fms_pago          = 100 * fms_pago_exercicio / fms_empenhado,
      .before = 1
    )
}

periodo <- bind_rows(
  resumir_periodo(anual, paste0(ano_ini, "-", ano_fim)),
  resumir_periodo(filter(anual, ano <= 2019), "2016-2019"),
  resumir_periodo(filter(anual, ano >= 2020), "2020-2022")
)

# ---- 5. Destino da despesa por tipo de credor --------------------------------
# A base nao traz a modalidade de aplicacao. O destino e aproximado pelo
# credor (razao social e CNPJ/CPF). A aba "Credores_classificados" permite
# conferir cada caso.
re_empresa <- paste0("LTDA|\\bS[/.]?A\\.?$|\\bS[/.]A\\b|\\bSA\\b|EIRELI|\\bME\\b|",
                     "\\bEPP\\b|\\bS\\.A\\.|COMERCIO|INDUSTRIA|DISTRIBUIDORA|",
                     "FARMACEUTIC|LABORATORIO|TECNOLOGIA|ENGENHARIA|CONSTRUTORA|SERVICOS")
re_sem_fins <- paste0("SANTA CASA|IRMANDADE|CASA DE CARIDADE|ASSOCIACAO|SOCIEDADE BENEFICENTE|",
                      "BENEFICENCIA|FUNDACAO|HOSPITAL|OBRAS SOCIAIS|CONGREGACAO|INSTITUTO|",
                      "APAE|CRUZ VERMELHA|CARIDADE|MISERICORDIA|BENEFICENTE|PIO XII|",
                      "FILANTROP|CENTRO DE|COOPERATIVA")

classificar <- function(razao, raiz, doc_n) {
  case_when(
    str_detect(razao, fixed("FUNDO MUNICIPAL DE SAUDE")) ~ "Fundos municipais de saúde",
    str_detect(razao, "FUNDO MUNICIPAL|MUNICIPIO DE|PREFEITURA") ~ "Outros entes municipais",
    str_detect(razao, "CONSORC|^CIS\\b|\\bCIS[ -/]|CISNORJE") &
      str_detect(razao, "INTERM|PUBLICO|SAUDE|URG|SAMU|CIS") ~ "Consórcios públicos de saúde",
    raiz %in% names(raiz_fundacoes) ~ "Fhemig, Funed e Hemominas",
    str_detect(razao, paste0("ESCOLA DE SAUDE PUBLICA DO ESTADO|UNIVERSIDADE ESTADUAL DE MONTES CLAROS|",
                             "SECRETARIA DE ESTADO|POLICIA MILITAR|CORPO DE BOMBEIROS|",
                             "UNIVERSIDADE DO ESTADO DE MINAS|ADVOCACIA GERAL DO ESTADO|",
                             "DEPARTAMENTO DE EDIFICACOES|GABINETE MILITAR")) ~ "Outros órgãos e entidades estaduais",
    str_detect(razao, paste0("FOLHA DE PAGAMENTO|INSTITUTO DE PREVIDENCIA DOS SERVIDORES DO ESTADO|",
                             "FUNFIP|FUNDO FINANCEIRO DE PREVIDENCIA|INSTITUTO NACIONAL DO SEGURO SOCIAL|",
                             "RECEITA FEDERAL|CAIXA ECONOMICA FEDERAL|FGTS")) ~ "Pessoal e encargos",
    str_detect(razao, "DIARIAS DE VIAGENS") ~ "Pessoas físicas e diárias",
    str_detect(razao, paste0("BLOQUEIOS JUDICIAIS|TRIBUNAL D[AE] JUSTICA|CONTRATOS ADMINISTRATIVOS|",
                             "DEPOSITOS? JUDICIA")) ~ "Despesas judiciais e lançamentos agregados",
    str_detect(razao, "FUNDO NACIONAL DE SAUDE|FUNDO ESTADUAL DE SAUDE|^MINISTERIO|POLICIA RODOVIARIA FEDERAL") ~
      "Outros (União e lançamentos do próprio FES)",
    str_detect(razao, re_sem_fins) & !str_detect(razao, re_empresa) ~
      "Hospitais e entidades privadas sem fins lucrativos",
    str_detect(razao, re_empresa) | doc_n >= 12 ~ "Empresas fornecedoras e prestadoras",
    doc_n <= 11 ~ "Pessoas físicas e diárias",
    TRUE ~ "Não classificado"
  )
}

# Agrupamento usado na discussao "repassador x executor"
grupo_de <- function(categoria) {
  case_when(
    categoria %in% c("Fundos municipais de saúde", "Outros entes municipais",
                     "Consórcios públicos de saúde") ~ "A. Transferências a municípios e consórcios",
    categoria %in% c("Fhemig, Funed e Hemominas",
                     "Outros órgãos e entidades estaduais") ~ "B. Fundações e órgãos estaduais",
    categoria == "Hospitais e entidades privadas sem fins lucrativos" ~ "C. Hospitais e entidades sem fins lucrativos",
    categoria %in% c("Pessoal e encargos", "Empresas fornecedoras e prestadoras",
                     "Pessoas físicas e diárias",
                     "Despesas judiciais e lançamentos agregados") ~ "D. Execução direta pela SES-MG",
    TRUE ~ "E. Outros"
  )
}

credores <- base |>
  group_by(razao, doc, raiz, doc_n) |>
  summarise(empenhado = sum(empenhado), .groups = "drop") |>
  mutate(categoria = classificar(razao, raiz, doc_n),
         grupo     = grupo_de(categoria)) |>
  arrange(categoria, desc(empenhado))

base <- base |>
  left_join(select(credores, razao, doc, categoria, grupo), by = c("razao", "doc"))

destino_periodo <- base |>
  group_by(grupo, categoria) |>
  summarise(empenhado = sum(empenhado), .groups = "drop") |>
  mutate(pct_fes = 100 * empenhado / sum(empenhado)) |>
  arrange(grupo, desc(empenhado))

destino_grupo <- base |>
  group_by(grupo) |>
  summarise(empenhado = sum(empenhado), .groups = "drop") |>
  mutate(pct_fes = 100 * empenhado / sum(empenhado))

destino_anual <- base |>
  group_by(ano, grupo) |>
  summarise(empenhado = sum(empenhado), .groups = "drop") |>
  group_by(ano) |>
  mutate(pct_fes = 100 * empenhado / sum(empenhado)) |>
  ungroup() |>
  select(-empenhado) |>
  pivot_wider(names_from = grupo, values_from = pct_fes)

# ---- 6. Conferencia com os numeros do TCC (Tabela 1 da versão final) ---------
p <- filter(periodo, periodo == paste0(ano_ini, "-", ano_fim))
tab1_tcc <- tibble(
  ano            = 2016:2022,
  tcc_empenhado  = c(2.12, 2.33, 1.69, 4.30, 3.00, 4.31, 4.54),
  tcc_pago       = c(0.81, 0.53, 0.56, 0.71, 1.73, 3.86, 4.13),
  tcc_pct_pago   = c(38.3, 22.8, 33.4, 16.5, 57.8, 89.5, 91.1)
) |>
  left_join(anual |> transmute(ano,
                               calc_empenhado = fms_empenhado / 1e9,
                               calc_pago      = fms_pago_exercicio / 1e9,
                               calc_pct_pago  = pct_fms_pago), by = "ano")

conferencia <- tibble(
  indicador = c("Repasses aos FMS / despesa empenhada do FES (2016-2022)",
                "Idem, excluídas Fhemig, Funed e Hemominas",
                "% pago no exercício, 2016-2019",
                "% pago no exercício, 2020-2022"),
  valor_no_tcc = c(39.3, 55.1, 25.1, 82.1),
  valor_calculado = c(p$pct_fms_fes,
                      p$pct_fms_sem_fundacoes,
                      filter(periodo, periodo == "2016-2019")$pct_fms_pago,
                      filter(periodo, periodo == "2020-2022")$pct_fms_pago)
)

# ---- 7. Exportacao para Excel ------------------------------------------------
bi <- function(x) round(x / 1e9, 2)

aba_anual <- anual |>
  transmute(
    Ano = ano,
    `Despesa empenhada do FES (R$ bi)`              = bi(fes_empenhado),
    `Empenhado aos FMS (R$ bi)`                     = bi(fms_empenhado),
    `% dos FMS na despesa do FES`                   = round(pct_fms_fes, 1),
    `Empenhado a Fhemig, Funed e Hemominas (R$ bi)` = bi(fundacoes_empenhado),
    `% dos FMS sem as fundações`                    = round(pct_fms_sem_fundacoes, 1),
    `Pago aos FMS no mesmo exercício (R$ bi)`       = bi(fms_pago_exercicio),
    `% pago no mesmo exercício`                     = round(pct_fms_pago, 1)
  )

aba_periodo <- periodo |>
  transmute(
    `Período` = periodo,
    `Despesa empenhada do FES (R$ bi)`              = bi(fes_empenhado),
    `Empenhado aos FMS (R$ bi)`                     = bi(fms_empenhado),
    `% dos FMS na despesa do FES`                   = round(pct_fms_fes, 1),
    `Empenhado a Fhemig, Funed e Hemominas (R$ bi)` = bi(fundacoes_empenhado),
    `% dos FMS sem as fundações`                    = round(pct_fms_sem_fundacoes, 1),
    `Pago aos FMS no mesmo exercício (R$ bi)`       = bi(fms_pago_exercicio),
    `% pago no mesmo exercício`                     = round(pct_fms_pago, 1)
  )

leia_me <- tibble(Nota = c(
  "Fonte: SIAFI-MG, despesa do Fundo Estadual de Saúde (FES), exercícios 2016 a 2022.",
  "Valores nominais, em R$ bilhões. Base: despesa empenhada no exercício.",
  "FMS: credores cuja razão social contém 'FUNDO MUNICIPAL DE SAUDE'.",
  "Fundações: credores com raiz de CNPJ da Fhemig (19843929), Funed (17503475) e Hemominas (26388330), incluídas as filiais.",
  "Pago no mesmo exercício: 'Valor Pago Orçamentário' dos empenhos do próprio ano (não inclui restos a pagar).",
  "Destino por tipo de credor: aproximação feita pela razão social e pelo CNPJ/CPF, porque a base não traz a modalidade de aplicação.",
  "A planilha de restos a pagar (RP_2016_2025.xlsx) não é usada neste cálculo."
))

wb <- createWorkbook()
estilo_cab <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                          wrapText = TRUE, border = "TopBottom", fgFill = "#D9D9D9")
adicionar <- function(nome, dados) {
  addWorksheet(wb, nome)
  writeData(wb, nome, dados, headerStyle = estilo_cab)
  setColWidths(wb, nome, cols = seq_len(ncol(dados)), widths = "auto")
}
adicionar("Leia-me", leia_me)
adicionar("Tabela_anual", aba_anual)
adicionar("Periodo", aba_periodo)
adicionar("Destino_grupos", destino_grupo |>
            transmute(Grupo = grupo, `Empenhado (R$ bi)` = bi(empenhado),
                      `% da despesa do FES` = round(pct_fes, 1)))
adicionar("Destino_categorias", destino_periodo |>
            transmute(Grupo = grupo, Categoria = categoria,
                      `Empenhado (R$ bi)` = bi(empenhado),
                      `% da despesa do FES` = round(pct_fes, 1)))
adicionar("Destino_anual_pct", destino_anual |> mutate(across(-ano, ~ round(.x, 1))) |>
            rename(Ano = ano))
adicionar("Credores_classificados", credores |>
            transmute(`Razão social` = razao, `CNPJ/CPF` = doc, Categoria = categoria,
                      Grupo = grupo, `Empenhado 2016-2022 (R$)` = round(empenhado, 2)))
adicionar("Conferencia_TCC", conferencia |>
            mutate(diferenca = round(valor_calculado - valor_no_tcc, 1),
                   valor_calculado = round(valor_calculado, 1)) |>
            rename(Indicador = indicador, `Valor no TCC` = valor_no_tcc,
                   `Valor calculado` = valor_calculado, `Diferença` = diferenca))
adicionar("Conferencia_Tabela1", tab1_tcc |> mutate(across(starts_with("calc"), ~ round(.x, 2))) |>
            mutate(calc_pct_pago = round(calc_pct_pago, 1)))

arquivo_saida <- file.path(dir_saida, "repasses_FMS_FES_2016_2022.xlsx")
saveWorkbook(wb, arquivo_saida, overwrite = TRUE)

# ---- 8. Resumo no console ----------------------------------------------------
cat("\nPeríodo", ano_ini, "-", ano_fim, "\n")
cat(sprintf("Despesa empenhada do FES: R$ %.2f bi\n", p$fes_empenhado / 1e9))
cat(sprintf("Empenhado aos FMS:        R$ %.2f bi (%.1f%% do FES)\n",
            p$fms_empenhado / 1e9, p$pct_fms_fes))
cat(sprintf("Sem Fhemig, Funed e Hemominas: %.1f%%\n", p$pct_fms_sem_fundacoes))
cat("\nArquivo gerado:", arquivo_saida, "\n")

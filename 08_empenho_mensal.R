# =====================================================================
# Script 08 - H3: calendário dos empenhos e pagamento conforme o mês do empenho
# Entradas : dados/mensal/exercicio_mensal_2016_2021.csv, exercicio_mensal_2022_2026.csv,
#            rp_mensal_2016_2026.csv (gerados por scripts/08a_converter_mensal_csv.py
#            a partir das planilhas mensais do SIAFI-MG, que cobrem toda a despesa da SES-MG)
#            saida/bases/base_trf.rds (script 05: municípios que declararam saldos)
# Saídas   : saida/tabelas/parte8_empenho_mensal.xlsx
#            saida/figuras/fig14_empenho_nov_dez.png
#
# Pergunta: o não pagamento dos repasses de 2016-2019 no próprio exercício decorre de falta de
#   caixa (até os empenhos feitos cedo ficaram sem pagamento) ou de empenhos concentrados no fim
#   do exercício (os empenhos feitos cedo foram pagos e o problema está nos de fim de ano)?
# Universo dos repasses: mesmos filtros do script 02 (credor Fundo Municipal de Saúde ou PM,
#   número de instrumento válido), instrumentos celebrados no próprio exercício (ano do
#   instrumento = ano do empenho), municípios que declararam saldos - o mesmo da Tabela 9.
# Valores: empenhado = Valor Despesa Empenhada (positivo = empenho; negativo = anulação no mês);
#   pago = Valor Pago Orçamentário (o mesmo valor das bases anuais).
# =====================================================================
source("scripts/00_config.R")
# o pacote readr lê os CSV grandes com rapidez; instala se faltar (independe da versão do 00_config.R)
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
library(readr)

MENSAL <- file.path(DADOS, "mensal")
le_csv <- function(arq) {
  f <- file.path(MENSAL, arq)
  if (!file.exists(f)) stop("Arquivo não encontrado: ", f, ". Rode antes: python scripts/08a_converter_mensal_csv.py")
  x <- read_delim(f, delim = ";", col_types = cols(.default = "c"), progress = FALSE)
  names(x) <- norm_txt(names(x)) |> str_replace_all("[^A-Z0-9]+", "_") |> str_remove("_$")
  x
}
filtro_fms <- function(x) x |>
  filter(str_starts(credor, "FUNDO MUNICIPAL DE SAUDE") | str_detect(credor, "^PM "),
         instrumento != "", !str_detect(instrumento, "\\*"), str_detect(instrumento, "^0*\\d+/\\d{4}$")) |>
  mutate(inum = as.integer(str_match(instrumento, "^0*(\\d+)/(\\d{4})$")[, 2]),
         iano = as.integer(str_match(instrumento, "^0*(\\d+)/(\\d{4})$")[, 3]),
         instr_id = paste0(inum, "/", iano))

# ---------------------------------------------------------------------
# 1. Leitura
# ---------------------------------------------------------------------
E <- bind_rows(le_csv("exercicio_mensal_2016_2021.csv"), le_csv("exercicio_mensal_2022_2026.csv")) |>
  transmute(ano = as.integer(ANO_DE_EXERCICIO), mes = as.integer(MES_NUMERICO),
            credor = norm_txt(RAZAO_SOCIAL_CREDOR),
            instrumento = str_trim(coalesce(NUM_REF_CONTRATOCONVENIO_SAIDA, "")),
            cod6 = as.integer(num(MUNICIPIO_CREDOR_CODIGO)),
            emp = coalesce(num(VALOR_DESPESA_EMPENHADA), 0), pago = coalesce(num(VALOR_PAGO_ORCAMENTARIO), 0))
R <- le_csv("rp_mensal_2016_2026.csv") |>
  transmute(ano = as.integer(ANO_DE_EXERCICIO), origem = as.integer(num(ANO_ORIGEM_RESTOS_A_PAGAR)),
            credor = norm_txt(RAZAO_SOCIAL_CREDOR),
            instrumento = str_trim(coalesce(NUM_REF_CONTRATOCONVENIO_SAIDA, "")),
            cod6 = as.integer(num(MUNICIPIO_CREDOR_CODIGO)),
            inscrito = coalesce(num(VALOR_INSCRITO_PROCESSADO), 0) + coalesce(num(VALOR_INSCRITO_NAO_PROCESSADO), 0),
            cancelado = coalesce(num(VALOR_CANCELADO_PROCESSADO), 0) + coalesce(num(VALOR_CANCELADO_NAO_PROCESSADO), 0),
            pago = coalesce(num(VALOR_PAGO_PROCESSADO), 0) + coalesce(num(VALOR_PAGO_NAO_PROCESSADO), 0))
mun_decl <- readRDS(file.path(BASES, "base_trf.rds")) |> filter(!is.na(trf)) |> pull(cod6)

# conferência com a base anual usada nos scripts 02 a 07 (os totais devem coincidir)
Pa <- readRDS(file.path(BASES, "pagamentos.rds"))
Ef <- filtro_fms(E)
conf <- Ef |> group_by(Ano = ano) |> summarise(`Empenhado - base mensal (R$ mi)` = sum(emp) / 1e6,
                                               `Pago no exercício - base mensal (R$ mi)` = sum(pago) / 1e6) |>
  left_join(Pa |> filter(fonte == "Exercício") |> group_by(Ano = ano_pag) |>
              summarise(`Empenhado - base anual (R$ mi)` = sum(empenhado) / 1e6,
                        `Pago no exercício - base anual (R$ mi)` = sum(pago) / 1e6), by = "Ano") |>
  filter(Ano <= 2025)

# ---------------------------------------------------------------------
# 2. Calendário dos empenhos: repasses aos fundos municipais x demais despesas da SES-MG
#    (empenho bruto = soma dos valores positivos; anulações = soma dos negativos)
# ---------------------------------------------------------------------
E <- E |> mutate(grupo = if_else(str_starts(credor, "FUNDO MUNICIPAL DE SAUDE") | str_detect(credor, "^PM "),
                                 "Repasses a fundos municipais", "Demais despesas da SES-MG"))
cal <- E |> filter(ano >= ANO_INI, ano <= ANO_FIM) |> group_by(Ano = ano, Grupo = grupo) |>
  summarise(`Empenho bruto (R$ mi)` = sum(pmax(emp, 0)) / 1e6,
            `Anulações (R$ mi)` = -sum(pmin(emp, 0)) / 1e6,
            `% do empenho bruto em nov.-dez.` = sum(pmax(emp, 0)[mes >= 11]) / sum(pmax(emp, 0)) * 100,
            `% do empenho bruto em dez.` = sum(pmax(emp, 0)[mes == 12]) / sum(pmax(emp, 0)) * 100,
            `% do empenhado líquido pago no exercício` = sum(pago) / sum(emp) * 100) |> ungroup()

# ---------------------------------------------------------------------
# 3. Pares município-instrumento do próprio exercício: mês do empenho e pagamento no exercício
# ---------------------------------------------------------------------
pares <- Ef |> filter(ano >= ANO_INI, ano <= ANO_FIM, iano == ano, cod6 %in% mun_decl) |>
  group_by(ano, cod6, instr_id) |>
  summarise(# mês do empenho = mês com o maior valor empenhado no par
            mes_emp = if (any(emp > 0)) mes[which.max(emp)] else NA_integer_,
            emp_bruto = sum(pmax(emp, 0)), pago = sum(pago), emp = sum(emp), .groups = "drop") |>
  filter(emp > 0, !is.na(mes_emp)) |>
  mutate(momento = factor(case_when(mes_emp <= 9 ~ "Janeiro a setembro", mes_emp == 10 ~ "Outubro",
                                    mes_emp == 11 ~ "Novembro", TRUE ~ "Dezembro"),
                          levels = c("Janeiro a setembro", "Outubro", "Novembro", "Dezembro")),
         tardio = as.integer(mes_emp >= 11),
         periodo = if_else(ano <= 2019, "2016-2019", "2020-2022"),
         sh_pago = pmin(pmax(pago / emp, 0), 1),
         nao_pago = pmax(emp - pago, 0))

# 3a. Parcela do empenhado paga no exercício, por ano e momento do empenho (ponderada pelo valor)
t3 <- pares |> group_by(Ano = ano, `Mês do empenho` = momento) |>
  summarise(Pares = n(), `Empenhado (R$ mi)` = sum(emp) / 1e6,
            `% pago no exercício` = sum(pmin(pago, emp)) / sum(emp) * 100, .groups = "drop") |>
  group_by(Ano) |> mutate(`% do empenhado do ano` = `Empenhado (R$ mi)` / sum(`Empenhado (R$ mi)`) * 100) |> ungroup()
t3_wide <- t3 |> select(Ano, `Mês do empenho`, `% pago no exercício`) |>
  pivot_wider(names_from = `Mês do empenho`, values_from = `% pago no exercício`) |>
  left_join(t3 |> group_by(Ano) |> summarise(`% do empenhado em nov.-dez.` = sum(`% do empenhado do ano`[`Mês do empenho` %in% c("Novembro", "Dezembro")])), by = "Ano") |>
  left_join(pares |> group_by(Ano = ano) |> summarise(`Total: % pago no exercício` = sum(pmin(pago, emp)) / sum(emp) * 100), by = "Ano")

# 3b. Decomposição do valor não pago no exercício, por momento do empenho
t4 <- pares |> group_by(Ano = ano) |>
  summarise(`Não pago no exercício (R$ mi)` = sum(nao_pago) / 1e6,
            `... de empenhos de janeiro a setembro (%)` = sum(nao_pago[mes_emp <= 9]) / sum(nao_pago) * 100,
            `... de empenhos de outubro (%)` = sum(nao_pago[mes_emp == 10]) / sum(nao_pago) * 100,
            `... de empenhos de novembro e dezembro (%)` = sum(nao_pago[mes_emp >= 11]) / sum(nao_pago) * 100)

# 3c. Modelo: o efeito de empenhar no fim do ano sobre a parcela paga difere entre os períodos?
#     (resposta fracionária, ponderado pelo valor empenhado; erros agrupados por município e instrumento)
pares <- pares |> mutate(w = emp / mean(emp))
m_tard <- glm(sh_pago ~ tardio * periodo + factor(ano), data = pares, family = quasibinomial(link = "logit"), weights = w)
V <- vcovCL(m_tard, cluster = pares[, c("cod6", "instr_id")], type = "HC1", multi0 = FALSE)
ct <- coeftest(m_tard, vcov. = V)
efeito <- function(per) {      # efeito médio (p.p.) de empenhar em nov.-dez., dentro de um período
  d <- pares[pares$periodo == per, ]; d1 <- d; d1$tardio <- 1L; d0 <- d; d0$tardio <- 0L
  sum(d$w * (predict(m_tard, d1, type = "response") - predict(m_tard, d0, type = "response"))) / sum(d$w) * 100
}
t5 <- tibble(Indicador = c("Efeito de empenhar em nov.-dez. sobre a parcela paga no exercício, 2016-2019 (p.p.)",
                           "Efeito de empenhar em nov.-dez. sobre a parcela paga no exercício, 2020-2022 (p.p.)",
                           "Interação tardio x período (escala logit) - p-valor",
                           "Parcela paga dos empenhos de jan.-set., 2016-2019 (%)",
                           "Parcela paga dos empenhos de jan.-set., 2020-2022 (%)",
                           "Pares no modelo"),
             Valor = c(efeito("2016-2019"), efeito("2020-2022"), ct["tardio:periodo2020-2022", 4],
                       with(filter(pares, periodo == "2016-2019", mes_emp <= 9), sum(pmin(pago, emp)) / sum(emp) * 100),
                       with(filter(pares, periodo == "2020-2022", mes_emp <= 9), sum(pmin(pago, emp)) / sum(emp) * 100),
                       nrow(pares)))

# ---------------------------------------------------------------------
# 4. Restos a pagar dos repasses: inscrição, cancelamento e pagamento por ano de origem
#    (o valor inscrito aparece em janeiro de cada exercício, como estoque reinscrito: a inscrição
#     original é a do exercício seguinte ao de origem)
# ---------------------------------------------------------------------
Rf <- filtro_fms(R)
t6 <- Rf |> filter(origem >= ANO_INI, origem <= ANO_FIM) |> group_by(`Ano de origem` = origem) |>
  summarise(`Inscrito em restos a pagar (R$ mi)` = sum(inscrito[ano == origem + 1]) / 1e6,
            `Pago até 2026 (R$ mi)` = sum(pago) / 1e6,
            `Cancelado até 2026 (R$ mi)` = sum(cancelado) / 1e6) |>
  mutate(`% cancelado` = `Cancelado até 2026 (R$ mi)` / `Inscrito em restos a pagar (R$ mi)` * 100,
         `% pago` = `Pago até 2026 (R$ mi)` / `Inscrito em restos a pagar (R$ mi)` * 100)
t6b <- E |> filter(grupo == "Repasses a fundos municipais", ano >= ANO_INI, ano <= ANO_FIM) |>
  group_by(Ano = ano) |> summarise(`Anulações de empenho no exercício (R$ mi)` = -sum(pmin(emp, 0)) / 1e6,
                                   `... em dezembro (R$ mi)` = -sum(pmin(emp, 0)[mes == 12]) / 1e6)

# ---------------------------------------------------------------------
# 5. Distribuição mensal do empenho bruto dos repasses (para a figura)
# ---------------------------------------------------------------------
mensal <- Ef |> filter(ano >= ANO_INI, ano <= ANO_FIM) |> group_by(ano, mes) |>
  summarise(v = sum(pmax(emp, 0)), .groups = "drop") |> group_by(ano) |>
  mutate(pct = v / sum(v) * 100, acum = cumsum(pct)) |> ungroup()
t7 <- mensal |> select(Ano = ano, mes, pct) |> pivot_wider(names_from = mes, values_from = pct, names_prefix = "Mês ")

grava_excel(list(T0_conferencia_base_anual = conf, T1_calendario_fms_x_ses = cal,
                 T2_pago_por_mes_empenho = t3, T2b_pago_por_mes_empenho_largo = t3_wide,
                 T3_decomposicao_nao_pago = t4, T4_modelo_tardio = t5,
                 T5_restos_a_pagar_origem = t6, T5b_anulacoes_exercicio = t6b,
                 T6_distribuicao_mensal = t7), "parte8_empenho_mensal.xlsx")

f14 <- cal |> select(Ano, Grupo, `% do empenho bruto em nov.-dez.`) |>
  mutate(Grupo = factor(Grupo, levels = c("Repasses a fundos municipais", "Demais despesas da SES-MG")))
salva_fig(ggplot(f14, aes(factor(Ano), `% do empenho bruto em nov.-dez.`, fill = Grupo)) +
            geom_col(position = position_dodge(0.8), width = 0.75) +
            geom_text(aes(label = formatC(`% do empenho bruto em nov.-dez.`, format = "f", digits = 0)),
                      position = position_dodge(0.8), vjust = -0.4, size = 2.8, family = "serif") +
            scale_fill_manual(values = c(AZUL, "#b9b8b2")) + scale_y_continuous(limits = c(0, 70)) +
            labs(x = NULL, y = "% do empenho bruto do ano") + tema_tcc,
          "fig14_empenho_nov_dez.png", alt = 8)

print(conf); print(cal, n = 20, width = Inf); print(t3_wide, width = Inf); print(t4, width = Inf); print(t5); print(t6, width = Inf); print(t6b)
message("Script 08 concluído.")

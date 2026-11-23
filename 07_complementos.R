# =====================================================================
# Script 07 - Complementos para a redação dos resultados
# Entradas : saida/bases (scripts 01 a 06)
# Saídas   : saida/tabelas/parte7_complementos.xlsx
# Conteúdo : H3 - ritmo dos pagamentos por ano do instrumento, taxa de não repasse
#            por ano, concentração do saldo constante, pagamentos posteriores a 2023
#            dos instrumentos com saldo constante e teste pareado da TNR por período.
# =====================================================================
source("scripts/00_config.R")

A  <- readRDS(file.path(BASES, "formA_limpo.rds"))
P  <- readRDS(file.path(BASES, "pagamentos.rds"))
pr <- readRDS(file.path(BASES, "pares_mun_instr.rds"))
ins <- readRDS(file.path(BASES, "instrumentos.rds"))
v  <- readRDS(file.path(BASES, "base_trf.rds"))

sa <- A |> mutate(instr_id = paste0(as.integer(str_match(INSTRUMENTO_COD, "^0*(\\d+)/")[, 2]), "/", ANO)) |>
  group_by(cod6, instr_id, iano = ANO) |>
  summarise(saldo_fin = sum(SALDO_FIN), saldo_const = sum(SALDO_CONST)) |> ungroup() |>
  filter(cod6 %in% v$cod6) |>
  left_join(pr |> select(cod6, instr_id, pago_ate_2023), by = c("cod6", "instr_id")) |>
  mutate(com_pag = coalesce(pago_ate_2023, 0) > 0) |> select(-pago_ate_2023)

# ---------------------------------------------------------------------
# 1. Por ano do instrumento: empenhado, pago até 2023, saldo constante, TNR e TRF
# ---------------------------------------------------------------------
Pi <- P |> filter(iano >= ANO_INI, iano <= ANO_FIM, cod6 %in% v$cod6)
t1 <- Pi |> group_by(`Ano do instrumento` = iano) |>
  summarise(`Empenhado no exercício de origem (R$ mi)` = sum(empenhado[fonte == "Exercício" & ano_pag == iano]) / 1e6,
            `Pago no próprio ano (R$ mi)` = sum(pago[ano_pag == iano]) / 1e6,
            `Pago em restos a pagar até 2023 (R$ mi)` = sum(pago[fonte == "Restos a pagar" & ano_pag <= 2023]) / 1e6,
            `Pago até 2023 (R$ mi)` = sum(pago[ano_pag <= 2023]) / 1e6,
            `Pago de 2024 em diante (R$ mi)` = sum(pago[ano_pag >= 2024]) / 1e6) |>
  left_join(sa |> group_by(`Ano do instrumento` = iano) |>
              summarise(`Saldo constante (R$ mi)` = sum(saldo_const) / 1e6, `Saldo financeiro (R$ mi)` = sum(saldo_fin) / 1e6,
                        # numerador da TRF: só pares com pagamento até 2023 (mesma regra do script 05)
                        sf_com_pag = sum(saldo_fin[com_pag]) / 1e6),
            by = "Ano do instrumento") |>
  # Indicador da metodologia (Quadro 1 e 7.4): parcela do EMPENHADO paga no próprio exercício
  mutate(`% do empenhado pago no próprio exercício` = `Pago no próprio ano (R$ mi)` / `Empenhado no exercício de origem (R$ mi)` * 100,
         `% do pago até 2023 em restos a pagar` = `Pago em restos a pagar até 2023 (R$ mi)` / `Pago até 2023 (R$ mi)` * 100,
         # medida usada na versão anterior (parcela do total pago), mantida só para comparação
         `% do total pago que foi pago no próprio ano (versão anterior)` = `Pago no próprio ano (R$ mi)` / (`Pago até 2023 (R$ mi)` + `Pago de 2024 em diante (R$ mi)`) * 100,
         `TNR (%)` = `Saldo constante (R$ mi)` / (`Pago até 2023 (R$ mi)` + `Saldo constante (R$ mi)`) * 100,
         `TRF (%)` = sf_com_pag / `Pago até 2023 (R$ mi)` * 100) |> select(-sf_com_pag)

# ---------------------------------------------------------------------
# 2. Concentração do saldo constante por instrumento
# ---------------------------------------------------------------------
ic <- sa |> group_by(instr_id) |> summarise(const = sum(saldo_const), mun = sum(saldo_const > 0)) |>
  filter(const > 0) |> arrange(desc(const)) |> mutate(acum = cumsum(const) / sum(const) * 100) |>
  left_join(ins |> select(instr_id, politica, modalidade, acao, unidade, primeiro_pagamento), by = "instr_id")
t2 <- tibble(Indicador = c("Instrumentos com saldo constante", "Instrumentos que somam 50% do saldo constante",
                           "Participação dos 5 maiores (%)", "Participação dos 10 maiores (%)",
                           "Participação da atenção primária no saldo constante (%)"),
             Valor = c(nrow(ic), sum(ic$acum < 50) + 1, ic$acum[5], ic$acum[10],
                       sum(ic$const[ic$politica %in% "Atenção primária"]) / sum(ic$const) * 100))
t2b <- ic |> head(15) |> mutate(`Saldo constante (R$ mi)` = const / 1e6) |>
  select(instr_id, `Saldo constante (R$ mi)`, `Municípios` = mun, `% acumulado` = acum, politica, acao, unidade, primeiro_pagamento)

# ---------------------------------------------------------------------
# 3. Pagamentos depois de 2023 nos pares com saldo constante (o Estado quitando a dívida?)
# ---------------------------------------------------------------------
pos <- Pi |> filter(ano_pag >= 2024) |> group_by(cod6, instr_id) |> summarise(pago_pos = sum(pago)) |> ungroup()
pc <- sa |> filter(saldo_const > 0) |> left_join(pos, by = c("cod6", "instr_id")) |> mutate(pago_pos = coalesce(pago_pos, 0))
t3 <- tibble(Indicador = c("Pares com saldo constante", "Saldo constante declarado (R$ mi)",
                           "Pares com algum pagamento de 2024 em diante (%)",
                           "Pago de 2024 em diante nesses pares (R$ mi)",
                           "Pago de 2024 em diante / saldo constante (%)"),
             Valor = c(nrow(pc), sum(pc$saldo_const) / 1e6, mean(pc$pago_pos > 0) * 100,
                       sum(pc$pago_pos) / 1e6, sum(pmin(pc$pago_pos, pc$saldo_const)) / sum(pc$saldo_const) * 100))
max_ano <- max(P$ano_pag)

# ---------------------------------------------------------------------
# 4. TNR por período, município a município (Wilcoxon pareado)
# ---------------------------------------------------------------------
per <- pr |> filter(cod6 %in% v$cod6) |> mutate(periodo = if_else(iano <= 2019, "a", "b")) |>
  group_by(cod6, periodo) |> summarise(rec = sum(pago_ate_2023)) |>
  left_join(sa |> mutate(periodo = if_else(iano <= 2019, "a", "b")) |> group_by(cod6, periodo) |>
              summarise(sc = sum(saldo_const), sf = sum(saldo_fin[com_pag])), by = c("cod6", "periodo")) |>
  mutate(across(c(sc, sf), ~ coalesce(.x, 0)), tnr = sc / (rec + sc), trf = pmin(sf / rec, 1)) |>
  select(cod6, periodo, tnr, trf) |> pivot_wider(names_from = periodo, values_from = c(tnr, trf)) |>
  filter(!is.na(tnr_a), !is.na(tnr_b))
w_tnr <- wilcox.test(per$tnr_a, per$tnr_b, paired = TRUE, exact = FALSE)
w_trf <- wilcox.test(per$trf_a, per$trf_b, paired = TRUE, exact = FALSE)
t4 <- tibble(Indicador = c("Municípios com instrumentos nos dois períodos",
                           "TNR 2016-2019 - mediana (%)", "TNR 2020-2022 - mediana (%)",
                           "Municípios com TNR maior em 2016-2019 (%)", "Wilcoxon pareado TNR - p-valor",
                           "TRF 2016-2019 - mediana (%)", "TRF 2020-2022 - mediana (%)",
                           "Municípios com TRF maior em 2020-2022 (%)", "Wilcoxon pareado TRF - p-valor"),
             Valor = c(nrow(per), median(per$tnr_a) * 100, median(per$tnr_b) * 100, mean(per$tnr_a > per$tnr_b) * 100,
                       w_tnr$p.value, median(per$trf_a, na.rm = TRUE) * 100, median(per$trf_b, na.rm = TRUE) * 100,
                       mean(per$trf_b > per$trf_a, na.rm = TRUE) * 100, w_trf$p.value))

# parcela do empenhado paga no próprio exercício, por município e período (Wilcoxon pareado)
emp_mun <- Pi |> mutate(periodo = if_else(iano <= 2019, "a", "b")) |> group_by(cod6, periodo) |>
  summarise(emp = sum(empenhado[fonte == "Exercício" & ano_pag == iano]), pag = sum(pago[ano_pag == iano])) |>
  ungroup() |> filter(emp > 0) |> mutate(sh = pmin(pag / emp, 1)) |> select(cod6, periodo, sh) |>
  pivot_wider(names_from = periodo, values_from = sh) |> filter(!is.na(a), !is.na(b))
w_emp <- wilcox.test(emp_mun$a, emp_mun$b, paired = TRUE, exact = FALSE)
t4 <- bind_rows(t4, tibble(Indicador = c("Municípios com empenho nos dois períodos",
                                         "% do empenhado pago no exercício 2016-2019 - mediana municipal",
                                         "% do empenhado pago no exercício 2020-2022 - mediana municipal",
                                         "Wilcoxon pareado (% do empenhado pago no exercício) - p-valor"),
                           Valor = c(nrow(emp_mun), median(emp_mun$a) * 100, median(emp_mun$b) * 100, w_emp$p.value)))

grava_excel(list(T1_por_ano_instrumento = t1, T2_concentracao_constante = t2, T2b_top15_constante = t2b,
                 T3_pagamentos_pos_2023 = t3, T4_periodos_pareado = t4), "parte7_complementos.xlsx")
message("Último ano de pagamento na base: ", max_ano)
print(t1, width = Inf); print(t2); print(t2b, width = Inf); print(t3); print(t4)
message("Script 07 concluído.")

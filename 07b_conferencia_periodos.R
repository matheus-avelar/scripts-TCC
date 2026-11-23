# =====================================================================
# Script 07b - Conferência dos números da comparação entre períodos
#              (2016-2019 x 2020-2022) citados no texto
# Entradas : saida/bases (scripts 01, 02 e 05)
# Saídas   : apenas no console
#   N1 / X : municípios com empenho nos dois períodos e % deles em que a parcela
#            do empenhado paga no próprio exercício aumentou em 2020-2022
#   N2 / Y : municípios com instrumentos nos dois períodos e % deles em que a
#            taxa de não repasse (TNR) caiu em 2020-2022
# Os mesmos cálculos estão na aba T4_periodos_pareado do script 07.
# =====================================================================
source("scripts/00_config.R")
A  <- readRDS(file.path(BASES, "formA_limpo.rds"))
P  <- readRDS(file.path(BASES, "pagamentos.rds"))
pr <- readRDS(file.path(BASES, "pares_mun_instr.rds"))
v  <- readRDS(file.path(BASES, "base_trf.rds"))

# Saldo constante por município e instrumento
sa <- A |>
  mutate(instr_id = paste0(as.integer(str_match(INSTRUMENTO_COD, "^0*(\\d+)/")[, 2]), "/", ANO)) |>
  group_by(cod6, instr_id, iano = ANO) |>
  summarise(saldo_const = sum(SALDO_CONST), .groups = "drop") |>
  filter(cod6 %in% v$cod6)

# TNR de cada município nos dois períodos (a = 2016-2019; b = 2020-2022)
per <- pr |> filter(cod6 %in% v$cod6) |>
  mutate(periodo = if_else(iano <= 2019, "a", "b")) |>
  group_by(cod6, periodo) |> summarise(rec = sum(pago_ate_2023), .groups = "drop") |>
  left_join(sa |> mutate(periodo = if_else(iano <= 2019, "a", "b")) |>
              group_by(cod6, periodo) |> summarise(sc = sum(saldo_const), .groups = "drop"),
            by = c("cod6", "periodo")) |>
  mutate(sc = coalesce(sc, 0), tnr = sc / (rec + sc)) |>
  select(cod6, periodo, tnr) |>
  pivot_wider(names_from = periodo, values_from = tnr) |>
  filter(!is.na(a), !is.na(b))

# Parcela do empenhado paga no próprio exercício, nos dois períodos
emp_mun <- P |> filter(iano >= ANO_INI, iano <= ANO_FIM, cod6 %in% v$cod6) |>
  mutate(periodo = if_else(iano <= 2019, "a", "b")) |>
  group_by(cod6, periodo) |>
  summarise(emp = sum(empenhado[fonte == "Exercício" & ano_pag == iano]),
            pag = sum(pago[ano_pag == iano]), .groups = "drop") |>
  filter(emp > 0) |> mutate(sh = pmin(pag / emp, 1)) |>
  select(cod6, periodo, sh) |>
  pivot_wider(names_from = periodo, values_from = sh) |>
  filter(!is.na(a), !is.na(b))

cat("N1 =", nrow(emp_mun), "\n")
cat("X  =", round(mean(emp_mun$b > emp_mun$a) * 100, 1), "%\n")
cat("Conferência - mediana da parcela paga:", round(median(emp_mun$a) * 100, 1), "->",
    round(median(emp_mun$b) * 100, 1), "\n")
cat("N2 =", nrow(per), "\n")
cat("Y  =", round(mean(per$b < per$a) * 100, 1), "%\n")
cat("Conferência - mediana da TNR:", round(median(per$a) * 100, 1), "->",
    round(median(per$b) * 100, 1), "\n")
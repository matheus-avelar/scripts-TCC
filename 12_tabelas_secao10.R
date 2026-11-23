# =====================================================================
# Script 12 - Tabelas 10, 11 e 12 da seção 10 (análise documental)
# Entradas : dados/mensal/exercicio_mensal_2016_2021.csv, exercicio_mensal_2022_2026.csv,
#            rp_mensal_2016_2026.csv (os mesmos do script 08; gerados pelo 08a)
#            saida/bases/instrumentos.rds (script 02: área de política de cada resolução)
#            dados/resolucoes/anexo_I_res_7153_2020.xlsx   (Anexo I da Res. SES/MG nº 7.153/2020:
#                 código, município, população IBGE/TCU 2019, valor por eixo e valor global)
#            dados/resolucoes/anexo_I_res_7799_2021.xlsx   (Anexo I da Res. SES/MG nº 7.799/2021:
#                 código, município, população 2019 e valor do incentivo)
#            As planilhas foram transcritas dos anexos publicados das resoluções.
#            Colunas usadas: 7.153/2020 - cod6, pop_2019, valor_por_eixo;
#                            7.799/2021 - cod6, pop_2019, valor_anexo_I.
#            (A conferência da Res. 6.907/2019, no fim, usa só pares_trf.rds.)
# Saídas   : saida/tabelas/parte12_tabelas_secao9.xlsx (abas Tab10, Tab11, Tab12)
#
# =====================================================================
source("scripts/00_config.R")

# ---------------------------------------------------------------------
# 1. Tabela 10 - parcela do valor pago em um único mês, por área de política
#    Resoluções de 2020 a 2022; pagamentos no exercício e em restos a pagar até 2023.
#    Para cada par município x resolução, conta-se o número de meses com pagamento.
# ---------------------------------------------------------------------
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
E <- bind_rows(le_csv("exercicio_mensal_2016_2021.csv"), le_csv("exercicio_mensal_2022_2026.csv")) |>
  transmute(ano = as.integer(ANO_DE_EXERCICIO), mes = as.integer(MES_NUMERICO),
            credor = norm_txt(RAZAO_SOCIAL_CREDOR), instrumento = str_trim(coalesce(NUM_REF_CONTRATOCONVENIO_SAIDA, "")),
            cod6 = as.integer(num(MUNICIPIO_CREDOR_CODIGO)), pago = coalesce(num(VALOR_PAGO_ORCAMENTARIO), 0))
R <- le_csv("rp_mensal_2016_2026.csv") |>
  transmute(ano = as.integer(ANO_DE_EXERCICIO), mes = as.integer(MES_NUMERICO),
            credor = norm_txt(RAZAO_SOCIAL_CREDOR), instrumento = str_trim(coalesce(NUM_REF_CONTRATOCONVENIO_SAIDA, "")),
            cod6 = as.integer(num(MUNICIPIO_CREDOR_CODIGO)),
            pago = coalesce(num(VALOR_PAGO_PROCESSADO), 0) + coalesce(num(VALOR_PAGO_NAO_PROCESSADO), 0))
ins <- readRDS(file.path(BASES, "instrumentos.rds")) |> select(instr_id, politica)

pares <- bind_rows(filtro_fms(E), filtro_fms(R)) |>
  filter(iano >= 2020, iano <= 2022, ano <= 2023, pago > 0, !is.na(cod6)) |>
  group_by(instr_id, cod6, ano, mes) |> summarise(pago = sum(pago), .groups = "drop") |>
  group_by(instr_id, cod6) |> summarise(meses = n(), tot = sum(pago), .groups = "drop") |>
  left_join(ins, by = "instr_id")
rm(E, R); gc()

resumo_forma <- function(d, rotulo) d |>
  summarise(`Área de política` = rotulo, `Resoluções` = n_distinct(instr_id),
            `Valor pago até 2023 (R$ bilhões)` = sum(tot) / 1e9,
            `Pago em um único mês (%)` = sum(tot[meses == 1]) / sum(tot) * 100,
            `Pago em 2 ou 3 meses (%)` = sum(tot[meses %in% 2:3]) / sum(tot) * 100,
            `Pago em 4 meses ou mais (%)` = sum(tot[meses >= 4]) / sum(tot) * 100)
tab10 <- bind_rows(
  resumo_forma(filter(pares, politica == "Vigilância em saúde"), "Vigilância em saúde"),
  resumo_forma(filter(pares, politica == "Vigilância em saúde", instr_id != "7153/2020"), "Vigilância em saúde, exceto Res. 7.153/2020"),
  resumo_forma(filter(pares, politica == "Atenção primária"), "Atenção primária"),
  resumo_forma(filter(pares, politica == "Assistência farmacêutica"), "Assistência farmacêutica"),
  resumo_forma(filter(pares, politica == "Média e alta complexidade"), "Média e alta complexidade"))

# ---------------------------------------------------------------------
# 2. Tabela 11 - valores das resoluções de vigilância por faixa de porte (medianas municipais)
#    Porte pela população IBGE/TCU de 2019, a usada pelas duas resoluções.
# ---------------------------------------------------------------------
RES <- file.path(DADOS, "resolucoes")
a7153 <- read_excel(file.path(RES, "anexo_I_res_7153_2020.xlsx")) |>
  mutate(faixa = cut(pop_2019, CORTES, labels = FAIXAS, right = TRUE))
a7799 <- read_excel(file.path(RES, "anexo_I_res_7799_2021.xlsx")) |>
  left_join(select(a7153, cod6, faixa), by = "cod6")
stopifnot(nrow(a7153) == 853, nrow(a7799) == 853, sum(is.na(a7799$faixa)) == 0)

tab11 <- a7153 |> group_by(`Faixa de porte` = faixa) |>
  summarise(`Municípios` = n(),
            `Res. 7.153/2020: valor por eixo (R$)` = median(valor_por_eixo),
            `Res. 7.153/2020: valor por eixo por mês de prazo (R$)` = median(valor_por_eixo / 60)) |>
  left_join(a7799 |> group_by(`Faixa de porte` = faixa) |>
              summarise(`Res. 7.799/2021: valor por habitante (R$)` = median(valor_anexo_I / pop_2019),
                        `Res. 7.799/2021: valor por município (R$)` = median(valor_anexo_I)), by = "Faixa de porte") |>
  relocate(`Res. 7.799/2021: valor por habitante (R$)`, `Res. 7.799/2021: valor por município (R$)`, .after = `Municípios`)

# ---------------------------------------------------------------------
# 3. Tabela 12 - TRF dos instrumentos de investimento, por modalidade e faixa de porte
#    Mesma base de pares município x resolução do script 05/06 (pares_trf.rds).
#    Valores esperados: acima de 100 mil - emenda 33,6% (R$ 151,8 mi), regular 3,6% (R$ 50,5 mi);
#    total - emenda 18,7% (R$ 788,2 mi), regular 12,9% (R$ 181,1 mi).
# ---------------------------------------------------------------------
pares <- readRDS(file.path(BASES, "pares_trf.rds"))
inv <- pares |> filter(as.integer(investimento) == 1)
resumo_inv <- function(d) d |>
  summarise(`Emenda parlamentar: valor recebido (R$ milhões)` = sum(pago_ate_2023[modalidade == "Emenda parlamentar"]) / 1e6,
            `Emenda parlamentar: TRF (%)` = sum(saldo_fin[modalidade == "Emenda parlamentar"]) /
              sum(pago_ate_2023[modalidade == "Emenda parlamentar"]) * 100,
            `Repasse regular: valor recebido (R$ milhões)` = sum(pago_ate_2023[modalidade == "Regular"]) / 1e6,
            `Repasse regular: TRF (%)` = sum(saldo_fin[modalidade == "Regular"]) /
              sum(pago_ate_2023[modalidade == "Regular"]) * 100)
tab12 <- bind_rows(
  inv |> group_by(`Faixa de porte` = factor(faixa, levels = FAIXAS)) |> resumo_inv() |>
    mutate(`Faixa de porte` = as.character(`Faixa de porte`)),
  inv |> resumo_inv() |> mutate(`Faixa de porte` = "Total", .before = 1))
# Conferência: participação da Res. 6.907/2019 no investimento recebido acima de 100 mil habitantes
print(inv |> filter(faixa == last(FAIXAS)) |>
        summarise(res6907 = sum(pago_ate_2023[instr_id == "6907/2019"]) / 1e6, total = sum(pago_ate_2023) / 1e6))

grava_excel(list(Tab10 = tab10, Tab11 = tab11, Tab12 = tab12), "parte12_tabelas_secao9.xlsx")
print(tab10, width = Inf); print(tab11, width = Inf); print(tab12, width = Inf)
message("Script 12 concluído.")

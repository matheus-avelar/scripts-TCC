# =====================================================================
# Script 13 - Tabela por resolução (seleção das resoluções da análise documental)
# Entradas : saida/bases/pares_trf.rds    (script 06: pares com pagamento até 2023)
#            saida/bases/formA_limpo.rds  (script 01)
#            saida/bases/instrumentos.rds (script 02)
#            saida/bases/base_trf.rds     (script 05: municípios que declararam saldos)
# Saída    : saida/tabelas/parte13_resolucoes.xlsx
#            - Resolucoes: uma linha por resolução de 2016-2022
#            - Resumo_por_area: conferência com a Tabela A.9 e distribuição da TRF entre resoluções
#            - Corpus: as resoluções da análise documental e os atos lidos junto
#
# Mesmas regras da seção 9:
#   TRF = saldo financeiro dos pares COM pagamento até 2023 / valor recebido até 2023
#   TNR = saldo constante (todos os pares) / (recebido até 2023 + saldo constante)
#   Pares sem pagamento registrado entram só no saldo constante e na TNR.
# Este script não altera nenhuma base: só lê e resume.
# =====================================================================
source("scripts/00_config.R")

A     <- readRDS(file.path(BASES, "formA_limpo.rds"))
pares <- readRDS(file.path(BASES, "pares_trf.rds"))
ins   <- readRDS(file.path(BASES, "instrumentos.rds"))
v     <- readRDS(file.path(BASES, "base_trf.rds"))

MOT <- c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação")

# Saldos do Formulário A por par (mesma construção do script 06)
sa <- A |> mutate(instr_id = paste0(as.integer(str_match(INSTRUMENTO_COD, "^0*(\\d+)/")[, 2]), "/", ANO)) |>
  group_by(cod6, instr_id) |>
  summarise(saldo_fin = sum(SALDO_FIN), saldo_const = sum(SALDO_CONST),
            v_obj = sum(TOTAL[MOTIVO == MOT[1]]), v_imp = sum(TOTAL[MOTIVO == MOT[2]]),
            v_des = sum(TOTAL[MOTIVO == MOT[3]])) |> ungroup()
sem_pag <- sa |> filter(cod6 %in% v$cod6) |> anti_join(pares, by = c("cod6", "instr_id"))

# ---------------------------------------------------------------------
# 1. Uma linha por resolução
# ---------------------------------------------------------------------
com <- pares |> group_by(instr_id) |>
  summarise(mun_receberam = n_distinct(cod6),
            # contado antes de somar o saldo (depois da soma, saldo_fin vira um único valor)
            mun_com_saldo_fin = sum(saldo_fin > 0),
            recebido = sum(pago_ate_2023),
            saldo_fin = sum(saldo_fin),
            const_a = sum(saldo_const),
            obj_a = sum(v_obj), imp_a = sum(v_imp), des_a = sum(v_des))
sem <- sem_pag |> group_by(instr_id) |>
  summarise(mun_sem_pagamento = n_distinct(cod6),
            saldo_fin_sem_pag = sum(saldo_fin),
            const_b = sum(saldo_const),
            obj_b = sum(v_obj), imp_b = sum(v_imp), des_b = sum(v_des))

res <- full_join(com, sem, by = "instr_id") |>
  mutate(across(c(mun_receberam, recebido, saldo_fin, const_a, mun_com_saldo_fin, obj_a, imp_a, des_a,
                  mun_sem_pagamento, saldo_fin_sem_pag, const_b, obj_b, imp_b, des_b), ~ coalesce(.x, 0))) |>
  left_join(ins |> select(instr_id, politica, subfuncao, modalidade, investimento, acao, unidade), by = "instr_id") |>
  mutate(numero = as.integer(str_match(instr_id, "^(\\d+)/")[, 2]),
         ano = as.integer(str_sub(instr_id, -4)),
         politica = coalesce(politica, "Sem pagamento registrado"),
         saldo_const = const_a + const_b,
         decl = obj_a + obj_b + imp_a + imp_b + des_a + des_b,
         `TRF (%)` = if_else(recebido > 0, saldo_fin / recebido * 100, NA_real_),
         `TNR (%)` = if_else(recebido + saldo_const > 0, saldo_const / (recebido + saldo_const) * 100, NA_real_),
         `Municípios com saldo financeiro (% dos que receberam)` = if_else(mun_receberam > 0, mun_com_saldo_fin / mun_receberam * 100, NA_real_),
         `Desnecessidade (% do declarado)` = if_else(decl > 0, (des_a + des_b) / decl * 100, NA_real_),
         `Objeto cumprido (% do declarado)` = if_else(decl > 0, (obj_a + obj_b) / decl * 100, NA_real_),
         `Impossibilidade (% do declarado)` = if_else(decl > 0, (imp_a + imp_b) / decl * 100, NA_real_)) |>
  filter(ano >= ANO_INI, ano <= ANO_FIM)

# Posição de cada resolução na sua área, pela TRF.
# Só entram no ranking resoluções com valor relevante (parâmetros abaixo), para que
# resoluções minúsculas, com TRF instável, não apareçam como "extremas".
MIN_RECEBIDO <- 1e6     # R$ 1 milhão recebido até 2023
MIN_MUN      <- 20      # pelo menos 20 municípios
res <- res |>
  mutate(no_ranking = recebido >= MIN_RECEBIDO & mun_receberam >= MIN_MUN) |>
  group_by(politica) |>
  mutate(`Posição da TRF na área (1 = maior)` = if_else(no_ranking, rank(-replace(`TRF (%)`, !no_ranking, NA), na.last = "keep", ties.method = "min"), NA_integer_),
         `Resoluções da área no ranking` = sum(no_ranking),
         `Posição do saldo financeiro na área (1 = maior)` = rank(-saldo_fin, ties.method = "min")) |>
  ungroup()

tab_res <- res |>
  transmute(`Resolução` = instr_id, `Número` = numero, Ano = ano, `Área de política` = politica,
            `Subfunção` = subfuncao, Modalidade = modalidade, Investimento = investimento,
            `Municípios que receberam` = mun_receberam,
            `Recebido até 2023 (R$ mi)` = recebido / 1e6,
            `Saldo financeiro (R$ mi)` = saldo_fin / 1e6,
            `Saldo financeiro sem pagamento registrado (R$ mi)` = saldo_fin_sem_pag / 1e6,
            `Saldo constante (R$ mi)` = saldo_const / 1e6,
            `TRF (%)`, `TNR (%)`,
            `Municípios com saldo financeiro (% dos que receberam)`,
            `Objeto cumprido (% do declarado)`, `Impossibilidade (% do declarado)`, `Desnecessidade (% do declarado)`,
            `Posição da TRF na área (1 = maior)`, `Resoluções da área no ranking`,
            `Posição do saldo financeiro na área (1 = maior)`,
            `Ação` = acao, `Unidade de gasto` = unidade) |>
  arrange(`Área de política`, desc(`TRF (%)`))

# ---------------------------------------------------------------------
# 2. Resumo por área e por subfunção (conferência com a Tabela A.9)
# ---------------------------------------------------------------------
resumo <- function(d, chave) d |> filter(recebido > 0) |> group_by(Grupo = .data[[chave]]) |>
  summarise(`Resoluções com pagamento` = n(),
            `Recebido (R$ mi)` = sum(recebido) / 1e6,
            `Saldo financeiro (R$ mi)` = sum(saldo_fin) / 1e6,
            `TRF agregada (%)` = sum(saldo_fin) / sum(recebido) * 100,
            `Resoluções no ranking` = sum(no_ranking),
            `TRF entre resoluções do ranking - 1º quartil (%)` = quantile(`TRF (%)`[no_ranking], .25, na.rm = TRUE),
            `TRF entre resoluções do ranking - mediana (%)` = median(`TRF (%)`[no_ranking], na.rm = TRUE),
            `TRF entre resoluções do ranking - 3º quartil (%)` = quantile(`TRF (%)`[no_ranking], .75, na.rm = TRUE),
            `TRF entre resoluções do ranking - máxima (%)` = suppressWarnings(max(`TRF (%)`[no_ranking], na.rm = TRUE))) |>
  arrange(desc(`TRF agregada (%)`))
t_area <- resumo(res, "politica")
t_sub  <- resumo(res, "subfuncao")
# Esperado: vigilância sanitária 7 e vigilância epidemiológica 90 resoluções (Tabela A.9),
# TRF agregada de 54,9% e 36,6%; média e alta complexidade 621 resoluções e 6,2%.

# ---------------------------------------------------------------------
# 3. Resoluções do corpus e atos lidos junto
# ---------------------------------------------------------------------
CORPUS <- c("7799/2021", "7153/2020", "8007/2022", "7857/2021", "8124/2022",
            "6932/2019", "6907/2019", "6712/2019",
            "6906/2019", "8387/2022", "6895/2019", "5816/2017")
t_corpus <- tab_res |> filter(`Resolução` %in% CORPUS) |>
  arrange(match(`Resolução`, CORPUS))
# 5816/2017 só aparece se tiver pagamento ou saldo no período; 7224/2020 não tem pagamento próprio
# (o Valora Minas de 2022 é pago pela 8007/2022).

grava_excel(list(Resolucoes = tab_res, Resumo_por_area = t_area, Resumo_por_subfuncao = t_sub,
                 Corpus = t_corpus,
                 Parametros = tibble(Parametro = c("Recebido mínimo para o ranking (R$)", "Municípios mínimos para o ranking"),
                                     Valor = c(MIN_RECEBIDO, MIN_MUN))),
            "parte13_resolucoes.xlsx")
print(t_area, width = Inf); print(t_sub, width = Inf); print(t_corpus, width = Inf)
message("Script 13 concluído.")

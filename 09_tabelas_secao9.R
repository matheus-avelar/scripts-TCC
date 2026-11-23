# =====================================================================
# Script 09 - Tabelas da seção de resultados quantitativos, como aparecem no texto
# Entradas : saídas dos scripts 01 a 08 (saida/tabelas e saida/bases)
# Saída    : saida/tabelas/tabelas_secao8.xlsx (nome mantido da versão anterior do texto)
#            - aba "Indice": de onde vem cada tabela (script e aba de origem)
#            - abas "Tab06" a "Tab22": cada tabela (numeração de uma versão anterior do texto;
#              a aba "Indice" diz o conteúdo de cada uma)
#            - aba "Numeros_do_texto": números citados no texto que não estão em tabela
# Rode depois dos scripts 01 a 08. Este script não faz cálculo novo: reúne o que os
# scripts anteriores calcularam e refaz apenas alguns totais e estatísticas simples.
# =====================================================================
source("scripts/00_config.R")
aba <- function(arq, nome) as.data.frame(read_excel(file.path(TAB, arq), sheet = nome))
r1 <- function(x) round(x, 1)

v   <- readRDS(file.path(BASES, "base_trf.rds")) |> filter(!is.na(trf))
pr  <- readRDS(file.path(BASES, "pares_trf.rds"))
ins <- readRDS(file.path(BASES, "instrumentos.rds"))
bmp <- readRDS(file.path(BASES, "base_municipal_pop.rds")) |> filter(declarou)

TABS <- list(); I <- list()
reg <- function(id, titulo, origem, x) { TABS[[id]] <<- x; I[[id]] <<- tibble(Aba = id, Tabela = titulo, Origem = origem) }

# ---------------------------------------------------------------- 8.1
reg("Tab06", "Tabela 6 - Ligação entre saldos do Formulário A e pagamentos", "05_trf.R, parte5_trf.xlsx, aba T1_ligacao_formA_pagtos",
    aba("parte5_trf.xlsx", "T1_ligacao_formA_pagtos"))
reg("Tab07", "Tabela 7 - Repasses SES-MG x SIOPS", "05_trf.R, parte5_trf.xlsx, aba T2b_repasses_x_siops_ano",
    aba("parte5_trf.xlsx", "T2b_repasses_x_siops_ano"))
reg("Tab08", "Tabela 8 - Represamento por macrorregião", "05_trf.R, parte5_trf.xlsx, aba T6_macrorregiao (testes: T6b e T6d)",
    aba("parte5_trf.xlsx", "T6_macrorregiao"))
# ---------------------------------------------------------------- 8.2
t9 <- aba("parte7_complementos.xlsx", "T1_por_ano_instrumento")
reg("Tab09", "Tabela 9 - Empenho, pagamento e saldos por ano de celebração", "07_complementos.R, parte7_complementos.xlsx, aba T1_por_ano_instrumento",
    t9 |> select(1, 2, `% do empenhado pago no próprio exercício`, `Pago até 2023 (R$ mi)`, `% do pago até 2023 em restos a pagar`,
                 `Saldo constante (R$ mi)`, `TNR (%)`, `Pago de 2024 em diante (R$ mi)`))
reg("Tab10", "Tabela 10 - Instrumentos com maior saldo constante", "07_complementos.R, parte7_complementos.xlsx, aba T2b_top15_constante (5 primeiras linhas)",
    head(aba("parte7_complementos.xlsx", "T2b_top15_constante"), 5))
if (file.exists(file.path(TAB, "parte8_empenho_mensal.xlsx"))) {
  reg("Tab11", "Tabela 11 - Calendário e pagamento: repasses x demais despesas da SES-MG", "08_empenho_mensal.R, parte8_empenho_mensal.xlsx, aba T1_calendario_fms_x_ses",
      aba("parte8_empenho_mensal.xlsx", "T1_calendario_fms_x_ses"))
  reg("Tab12", "Tabela 12 - Parcela paga conforme o mês do empenho e origem do não pago", "08_empenho_mensal.R, parte8_empenho_mensal.xlsx, abas T2b_pago_por_mes_empenho_largo e T3_decomposicao_nao_pago",
      aba("parte8_empenho_mensal.xlsx", "T2b_pago_por_mes_empenho_largo") |>
        left_join(aba("parte8_empenho_mensal.xlsx", "T3_decomposicao_nao_pago"), by = "Ano"))
} else {
  warning("Tabelas 11 e 12 não geradas: falta saida/tabelas/parte8_empenho_mensal.xlsx. ",
          "Coloque as planilhas mensais em dados/mensal, rode 'python scripts/08a_converter_mensal_csv.py' ",
          "e depois o script 08; em seguida rode este script de novo.")
}
# ---------------------------------------------------------------- 8.3
t13 <- aba("parte5_trf.xlsx", "T3_TRF_por_faixa") |>
  left_join(aba("parte3_porte.xlsx", "T8_perfil_por_faixa") |>
              select(Faixa, `% da população`, `Instrumentos por mil hab. - mediana`, `Saldo fin. por hab. - mediana (R$)`), by = "Faixa")
reg("Tab13", "Tabela 13 - Perfil das faixas de porte", "05_trf.R (T3_TRF_por_faixa) + 03_porte.R (T8_perfil_por_faixa)", t13)
reg("Tab14", "Tabela 14 - Efeito do porte nos modelos (linhas de porte de Tab16)", "05_trf.R, parte5_trf.xlsx, aba T4_modelos_TRF (linhas 'Porte')",
    aba("parte5_trf.xlsx", "T4_modelos_TRF") |> filter(str_starts(`Variável`, "Porte") | str_detect(`Variável`, "R²|^N$")))
# ---------------------------------------------------------------- 8.4
reg("Tab15", "Tabela 15 - Capacidade e situação fiscal por faixa", "04_capacidade_siops.R, parte4_capacidade.xlsx, aba T11_capacidade_por_faixa (+ % estatutários etc.)",
    aba("parte4_capacidade.xlsx", "T11_capacidade_por_faixa"))
reg("Tab16", "Tabela 16 - Modelos de capacidade (colunas M3b a M3e)", "05_trf.R, parte5_trf.xlsx, aba T4_modelos_TRF; robustez em T5_robustez",
    aba("parte5_trf.xlsx", "T4_modelos_TRF"))
# ---------------------------------------------------------------- 8.5
t17 <- pr |> left_join(select(ins, instr_id, subfuncao), by = "instr_id") |> group_by(`Subfunção` = subfuncao) |>
  summarise(`Resoluções` = n_distinct(instr_id), `Recebido (R$ mi)` = sum(pago_ate_2023) / 1e6,
            `Saldo financeiro (R$ mi)` = sum(saldo_fin) / 1e6, `TRF agregada (%)` = sum(saldo_fin) / sum(pago_ate_2023) * 100,
            `Pares com saldo (%)` = mean(com_saldo) * 100) |>
  filter(`Recebido (R$ mi)` >= 110) |> arrange(desc(`TRF agregada (%)`))
reg("Tab17", "Tabela 17 - TRF por subfunção (última coluna do texto: SES/MG, 2025, digitada)", "calculada aqui a partir de pares_trf.rds (script 06) e instrumentos.rds (script 02)", t17)
reg("Tab18", "Tabela 18 - TRF por área de política e porte", "06_politicas.R, parte6_politicas.xlsx, aba T5_TRF_politica_x_porte",
    aba("parte6_politicas.xlsx", "T5_TRF_politica_x_porte"))
t19 <- bind_rows(aba("parte6_politicas.xlsx", "T2_por_modalidade") |> mutate(Recorte = "Modalidade"),
                 aba("parte6_politicas.xlsx", "T3_por_objeto") |> mutate(Recorte = "Objeto")) |>
  filter(Grupo != "Sem pagamento registrado") |>
  left_join(bind_rows(aba("parte6_politicas.xlsx", "T5b_modalidade_x_porte") |> rename(Grupo = Modalidade),
                      aba("parte6_politicas.xlsx", "T5c_objeto_x_porte") |> rename(Grupo = Objeto)) |>
              select(Grupo, `TRF até 5 mil (%)` = `Até 5 mil`, `TRF acima de 100 mil (%)` = `Acima de 100 mil`), by = "Grupo") |>
  relocate(Recorte)
reg("Tab19", "Tabela 19 - Modalidade e objeto", "06_politicas.R, parte6_politicas.xlsx, abas T2_por_modalidade, T3_por_objeto, T5b e T5c", t19)
reg("Tab20", "Tabela 20 - Modelo de pares (agrupamento duplo; última coluna: agrupamento só por município)", "06_politicas.R, parte6_politicas.xlsx, abas T6_modelos_par e T6b_agrupamento_simples_x_duplo; poder explicativo em T7",
    aba("parte6_politicas.xlsx", "T6_modelos_par"))
# ---------------------------------------------------------------- 8.6
fa <- readRDS(file.path(BASES, "formA_limpo.rds")) |> filter(MOTIVO %in% c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação"))
tot <- fa |> summarise(`Objeto cumprido` = sum(TOTAL[MOTIVO == "Objeto cumprido"]), `Impossibilidade material` = sum(TOTAL[MOTIVO == "Impossibilidade material"]),
                       `Desnecessidade da ação` = sum(TOTAL[MOTIVO == "Desnecessidade da ação"])) |>
  mutate(t = `Objeto cumprido` + `Impossibilidade material` + `Desnecessidade da ação`) |>
  transmute(Recorte = "Total", across(c(`Objeto cumprido`, `Impossibilidade material`, `Desnecessidade da ação`), ~ .x / t * 100), `Valor (R$ mi)` = t / 1e6)
porte_val <- bmp |> group_by(Recorte = as.character(faixa)) |> summarise(`Valor (R$ mi)` = sum(saldo_total) / 1e6)
t21 <- bind_rows(tot,
                 aba("parte1_formularios.xlsx", "T4_motivos") |> filter(Recorte %in% c("Financeiro", "Constante", "2016-2019", "2020-2022")),
                 aba("parte6_politicas.xlsx", "T4_motivo_por_politica") |>
                   transmute(Recorte = `Política`, `Objeto cumprido` = `Objeto cumprido (%)`, `Impossibilidade material` = `Impossibilidade material (%)`,
                             `Desnecessidade da ação` = `Desnecessidade da ação (%)`, `Valor (R$ mi)` = `Valor declarado (R$ mi)`) |>
                   filter(!Recorte %in% c("Outras", "Sem pagamento registrado")),
                 aba("parte3_porte.xlsx", "T9_motivo_por_faixa") |> rename(Recorte = Faixa) |> left_join(porte_val, by = "Recorte"))
reg("Tab21", "Tabela 21 - Motivo declarado (% do valor)", "Total: calculado aqui; tipo e período: parte1 T4_motivos; política: parte6 T4_motivo_por_politica; porte: parte3 T9_motivo_por_faixa", t21)
reg("Tab22", "Tabela 22 - Capital na origem e no destino, por porte", "03_porte.R, parte3_porte.xlsx, aba T10_destino_por_faixa (total: parte1 T7_origem_destino)",
    aba("parte3_porte.xlsx", "T10_destino_por_faixa"))

# ---------------------------------------------------------------- números citados no texto
o_abs <- v |> arrange(desc(saldo_fin_a)); o_trf <- v |> arrange(desc(trf)); n10 <- round(nrow(v) * 0.10)
q <- quantile(v$trf, c(.25, .5, .75, .9)) * 100
sp_rp <- cor.test(v$trf, v$siops_rec_propria_asps, method = "spearman", exact = FALSE)
sp_pib <- cor.test(v$pib_pc, v$ifgf_autonomia, method = "spearman", exact = FALSE)
sp_pop <- cor.test(v$pop_media, v$trf, method = "spearman", exact = FALSE)
txt <- tibble(
  `Seção` = c(rep("8.1.3", 10), "8.3", "8.4", "8.4"),
  Indicador = c("TRF - 1º quartil (%)", "TRF - mediana (%)", "TRF - 3º quartil (%)", "TRF - percentil 90 (%)",
                "Municípios com TRF > 25% (%)", "Municípios com TRF > 50% (%)", "Municípios com TRF < 5% (%)",
                "10% maiores saldos absolutos: parcela do saldo (%)", "10% maiores TRF: parcela do saldo (%)",
                "10% maiores TRF: parcela do recebido (%)",
                "Spearman população x TRF", "Spearman % recursos próprios x TRF", "Spearman PIB per capita x IFGF Autonomia"),
  Valor = c(q[1], q[2], q[3], q[4], mean(v$trf > .25) * 100, mean(v$trf > .5) * 100, mean(v$trf < .05) * 100,
            sum(o_abs$saldo_fin_a[1:n10]) / sum(v$saldo_fin_a) * 100, sum(o_trf$saldo_fin_a[1:n10]) / sum(v$saldo_fin_a) * 100,
            sum(o_trf$recebido[1:n10]) / sum(v$recebido) * 100,
            unname(sp_pop$estimate), unname(sp_rp$estimate), unname(sp_pib$estimate)),
  `Origem` = c(rep("calculado aqui a partir de base_trf.rds (script 05)", 13)))
txt <- bind_rows(txt, tibble(`Seção` = "8.1.3", Indicador = "Belo Horizonte: saldo nos pares com pagamento (R$ mi) / TRF (%)",
                             Valor = NA, Origem = paste0(r1(v$saldo_fin_a[v$cod6 == 310620] / 1e6), " / ", r1(v$trf[v$cod6 == 310620] * 100))))
TABS[["Numeros_do_texto"]] <- txt

grava_excel(c(list(Indice = bind_rows(I)), TABS), "tabelas_secao8.xlsx")
message("Script 09 concluído.")

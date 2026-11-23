# =====================================================================
# Script 02 - Repasses da SES-MG aos municípios (execução orçamentária)
# Entradas : dados/Exercicio_2016_2021.xlsx  (pagamentos do exercício, 2016-2021)
#            dados/Exercicio_2022_2026.xlsx  (pagamentos do exercício, 2022-2026)
#            dados/RP_2016_2025.xlsx          (pagamentos de restos a pagar, 2017-2026)
# Saídas   : saida/tabelas/parte2_repasses.xlsx
#            saida/tabelas/dicionario_instrumentos.xlsx
#            saida/figuras/fig9_repasses_por_ano.png
#            saida/bases/pagamentos.rds, pares_mun_instr.rds, instrumentos.rds
#
# Regras (orientação do NES):
#   Filtro 1 - Razão Social do Credor começa com "FUNDO MUNICIPAL DE SAUDE" ou "PM "
#              (texto normalizado: maiúsculas, sem acento, sem espaços duplos).
#   Filtro 2 - exclui linhas com o número do instrumento em branco ou com asteriscos.
#   Passo técnico - exclui as poucas linhas cujo "instrumento" não segue o padrão
#              número/ano (ex.: textos colados por engano na coluna).
# Valor de referência: "Valor Pago Orçamentário" (no exercício e nos restos a pagar;
#   nos restos a pagar ele é a soma do processado e do não processado).
# =====================================================================
source("scripts/00_config.R")

# ---------------------------------------------------------------------
# 1. Leitura das três planilhas (cabeçalho na 3ª linha) e conferência das colunas
# ---------------------------------------------------------------------
le_rep <- function(arquivo, fonte) {
  x <- read_excel(file.path(DADOS, arquivo), sheet = 1, skip = 2, col_types = "text")
  names(x) <- norm_txt(names(x)) |> str_replace_all("[^A-Z0-9]+", "_") |> str_remove("_$")
  x$fonte <- fonte
  x
}
ex1 <- le_rep("Exercicio_2016_2021.xlsx", "Exercício")
ex2 <- le_rep("Exercicio_2022_2026.xlsx", "Exercício")
rp  <- le_rep("RP_2016_2025.xlsx", "Restos a pagar")
# a coluna do instrumento tem grafias diferentes ("Num Ref" e "Num. Ref"): após a
# normalização, ambas viram NUM_REF_CONTRATOCONVENIO_SAIDA

colunas <- tibble(Coluna = union(union(names(ex1), names(ex2)), names(rp))) |>
  mutate(`Exercício 2016-2021` = Coluna %in% names(ex1), `Exercício 2022-2026` = Coluna %in% names(ex2),
         `RP 2016-2025` = Coluna %in% names(rp))
print(colunas |> filter(!(`Exercício 2016-2021` & `Exercício 2022-2026` & `RP 2016-2025`)))
# Esperado: só VALOR_PAGO_PROCESSADO e VALOR_PAGO_NAO_PROCESSADO aparecem apenas no arquivo de RP.

P0 <- bind_rows(ex1, ex2, rp) |>
  transmute(fonte,
            ano_pag    = as.integer(num(ANO_DE_EXERCICIO)),
            subfuncao_cod = as.integer(num(SUBFUNCAO_CODIGO)), subfuncao = SUBFUNCAO_DESCRICAO,
            programa_cod  = as.integer(num(PROGRAMA_CODIGO)),  programa  = PROGRAMA_DESCRICAO,
            acao_cod   = as.integer(num(PROJETO_ATIVIDADE_CODIGO)), acao = PROJETO_ATIVIDADE_DESCRICAO,
            unid_cod   = as.integer(num(UNID_PROGRAMACAO_GASTO_CODIGO)), unid = UNID_PROGRAMACAO_GASTO_DESCRICAO,
            credor     = RAZAO_SOCIAL_CREDOR, credor_norm = norm_txt(RAZAO_SOCIAL_CREDOR),
            instrumento = str_trim(coalesce(NUM_REF_CONTRATOCONVENIO_SAIDA, "")),
            cod6       = as.integer(num(MUNICIPIO_CREDOR_CODIGO)), municipio = MUNICIPIO_CREDOR_DESCRICAO,
            empenhado  = coalesce(num(VALOR_DESPESA_EMPENHADA), 0),
            liquidado  = coalesce(num(VALOR_DESPESA_LIQUIDADA), 0),
            pago       = coalesce(num(VALOR_PAGO_ORCAMENTARIO), 0),
            pago_proc  = coalesce(num(VALOR_PAGO_PROCESSADO), 0),
            pago_nproc = coalesce(num(VALOR_PAGO_NAO_PROCESSADO), 0))
rm(ex1, ex2, rp); gc()

# conferência: nos RP, pago orçamentário = processado + não processado
dif_rp <- P0 |> filter(fonte == "Restos a pagar") |> summarise(dif = sum(abs(pago - pago_proc - pago_nproc)))
message("Diferença RP (orçamentário - processado - não processado), R$: ", round(dif_rp$dif, 2))

# ---------------------------------------------------------------------
# 2. Filtros (Tabela de tratamento)
# ---------------------------------------------------------------------
etapa <- function(x, nome) tibble(Etapa = nome, Linhas = nrow(x),
                                  `Pago - exercício (R$ mi)` = sum(x$pago[x$fonte == "Exercício"]) / 1e6,
                                  `Pago - restos a pagar (R$ mi)` = sum(x$pago[x$fonte == "Restos a pagar"]) / 1e6,
                                  Municipios = n_distinct(x$cod6))
f1 <- P0 |> filter(str_starts(credor_norm, "FUNDO MUNICIPAL DE SAUDE") | str_detect(credor_norm, "^PM "))
f2 <- f1 |> filter(instrumento != "", !str_detect(instrumento, "\\*"))
f3 <- f2 |> mutate(inum = as.integer(str_match(instrumento, "^0*(\\d+)/(\\d{4})$")[, 2]),
                   iano = as.integer(str_match(instrumento, "^0*(\\d+)/(\\d{4})$")[, 3])) |>
  filter(!is.na(inum))
t1 <- bind_rows(etapa(P0, "0. Base bruta (3 planilhas)"),
                etapa(f1, "1. Credor = Fundo Municipal de Saúde ou PM"),
                etapa(f2, "2. Instrumento preenchido (sem branco ou asterisco)"),
                etapa(f3, "3. Instrumento no padrão número/ano"))

# auditoria dos filtros: maiores credores excluídos e grafias aceitas
excluidos <- P0 |> filter(!(str_starts(credor_norm, "FUNDO MUNICIPAL DE SAUDE") | str_detect(credor_norm, "^PM "))) |>
  group_by(Credor = credor_norm) |> summarise(Linhas = n(), `Pago (R$ mi)` = sum(pago) / 1e6) |>
  arrange(desc(`Pago (R$ mi)`)) |> head(40)
aceitos <- f1 |> mutate(Tipo = if_else(str_detect(credor_norm, "^PM "), "PM", "Fundo Municipal de Saúde")) |>
  group_by(Tipo) |> summarise(Credores = n_distinct(credor_norm), Linhas = n(), `Pago (R$ mi)` = sum(pago) / 1e6)
fora_padrao <- f2 |> filter(!str_detect(instrumento, "^0*\\d+/\\d{4}$")) |> count(instrumento, name = "Linhas")

P <- f3 |> mutate(
  instr_id = paste0(inum, "/", iano),
  politica = case_when(subfuncao_cod == 301 ~ "Atenção primária",
                       subfuncao_cod == 302 ~ "Média e alta complexidade",
                       subfuncao_cod == 303 ~ "Assistência farmacêutica",
                       subfuncao_cod %in% c(304, 305) ~ "Vigilância em saúde",
                       TRUE ~ "Outras"),
  emenda = coalesce(str_detect(norm_txt(unid), "EMENDA"), FALSE),
  # objeto de investimento (despesa de capital provável), identificado pela descrição da unidade de gasto
  investimento = coalesce(str_detect(norm_txt(unid), "INVESTIMENTO|CONSTRUCAO|EQUIPAMENTO|MATERIAL PERMANENTE|MATERIAIS PERMANENTES|OBRA|REFORMA"), FALSE),
  covid  = coalesce(str_detect(norm_txt(paste(unid, acao)), "CORONAVIRUS|COVID"), FALSE))
saveRDS(P, file.path(BASES, "pagamentos.rds"))

# ---------------------------------------------------------------------
# 3. Pagamentos por ano (exercício + restos a pagar)
# ---------------------------------------------------------------------
t2 <- P |> group_by(`Ano de pagamento` = ano_pag) |>
  summarise(`Exercício (R$ mi)` = sum(pago[fonte == "Exercício"]) / 1e6,
            `Restos a pagar (R$ mi)` = sum(pago[fonte == "Restos a pagar"]) / 1e6) |>
  mutate(`Total (R$ mi)` = `Exercício (R$ mi)` + `Restos a pagar (R$ mi)`,
         `% restos a pagar` = `Restos a pagar (R$ mi)` / `Total (R$ mi)` * 100)

# ---------------------------------------------------------------------
# 4. Matriz ano do instrumento x ano de pagamento (instrumentos de 2016-2022) - H3
# ---------------------------------------------------------------------
Pi <- P |> filter(iano >= ANO_INI, iano <= ANO_FIM)
t3 <- Pi |> group_by(`Ano do instrumento` = iano, ano_pag) |> summarise(v = sum(pago) / 1e6) |>
  pivot_wider(names_from = ano_pag, values_from = v, values_fill = 0, names_sort = TRUE)
t3b <- Pi |> group_by(`Ano do instrumento` = iano) |>
  summarise(`Pago total até 2026 (R$ mi)` = sum(pago) / 1e6,
            `% pago no próprio ano` = sum(pago[ano_pag == iano]) / sum(pago) * 100,
            `% pago até o ano seguinte` = sum(pago[ano_pag <= iano + 1]) / sum(pago) * 100,
            `% pago depois de 2022` = sum(pago[ano_pag > 2022]) / sum(pago) * 100,
            `% pago em restos a pagar` = sum(pago[fonte == "Restos a pagar"]) / sum(pago) * 100,
            `Empenhado no exercício de origem (R$ mi)` = sum(empenhado[fonte == "Exercício" & ano_pag == iano]) / 1e6)

# ---------------------------------------------------------------------
# 5. Dicionário de instrumentos (uma linha por resolução, todos os anos)
#    Política, programa, ação e unidade = os de maior valor pago no instrumento
# ---------------------------------------------------------------------
moda_valor <- function(rotulo, valor) { s <- tapply(valor, rotulo, sum); names(s)[which.max(s)] }
instr <- P |> group_by(instr_id, inum, iano) |>
  summarise(politica = moda_valor(politica, pmax(pago, empenhado)),
            subfuncao = moda_valor(paste(subfuncao_cod, subfuncao), pmax(pago, empenhado)),
            programa = moda_valor(paste(programa_cod, programa), pmax(pago, empenhado)),
            acao = moda_valor(paste(acao_cod, acao), pmax(pago, empenhado)),
            unidade = moda_valor(paste(unid_cod, unid), pmax(pago, empenhado)),
            subfuncoes_distintas = n_distinct(subfuncao_cod),
            emenda = any(emenda), covid = any(covid),
            investimento = sum(pmax(pago, empenhado)[investimento]) > sum(pmax(pago, empenhado)[!investimento]),
            municipios_pagos = n_distinct(cod6[pago > 0]),
            pago_total = sum(pago),
            primeiro_pagamento = if (any(pago > 0)) min(ano_pag[pago > 0]) else NA_integer_,
            ultimo_pagamento = if (any(pago > 0)) max(ano_pag[pago > 0]) else NA_integer_) |> ungroup() |>
  mutate(modalidade = case_when(emenda ~ "Emenda parlamentar", covid ~ "Enfrentamento à covid-19", TRUE ~ "Regular"))
saveRDS(instr, file.path(BASES, "instrumentos.rds"))

t4 <- instr |> filter(iano >= ANO_INI, iano <= ANO_FIM) |> group_by(`Política (subfunção predominante)` = politica) |>
  summarise(Instrumentos = n(), `Pago até 2026 (R$ mi)` = sum(pago_total) / 1e6) |>
  mutate(`% do valor` = `Pago até 2026 (R$ mi)` / sum(`Pago até 2026 (R$ mi)`) * 100) |> arrange(desc(`% do valor`))
t4c <- instr |> filter(iano >= ANO_INI, iano <= ANO_FIM) |>
  group_by(Objeto = if_else(investimento, "Investimento (obras e equipamentos)", "Custeio e outros")) |>
  summarise(Instrumentos = n(), `Pago até 2026 (R$ mi)` = sum(pago_total) / 1e6) |>
  mutate(`% do valor` = `Pago até 2026 (R$ mi)` / sum(`Pago até 2026 (R$ mi)`) * 100)
t4b <- instr |> filter(iano >= ANO_INI, iano <= ANO_FIM) |> group_by(Modalidade = modalidade) |>
  summarise(Instrumentos = n(), `Pago até 2026 (R$ mi)` = sum(pago_total) / 1e6) |>
  mutate(`% do valor` = `Pago até 2026 (R$ mi)` / sum(`Pago até 2026 (R$ mi)`) * 100)

# ---------------------------------------------------------------------
# 6. Base de pares município x instrumento (instrumentos de 2016-2022)
#    pago_ate_2023 = denominador principal da TRF (os saldos foram declarados em 2023)
# ---------------------------------------------------------------------
pares <- Pi |> group_by(cod6, instr_id, inum, iano) |>
  summarise(pago_ate_2022 = sum(pago[ano_pag <= 2022]), pago_ate_2023 = sum(pago[ano_pag <= 2023]),
            pago_total = sum(pago), empenhado_origem = sum(empenhado[fonte == "Exercício" & ano_pag == iano])) |>
  ungroup()
saveRDS(pares, file.path(BASES, "pares_mun_instr.rds"))

grava_excel(list(T1_filtros = t1, T1b_credores_aceitos = aceitos, T1c_maiores_excluidos = excluidos,
                 T1d_instr_fora_padrao = fora_padrao, T1e_colunas = colunas,
                 T2_pagos_por_ano = t2, T3_matriz_instr_x_pagto = t3, T3b_tempo_de_pagamento = t3b,
                 T4_politica = t4, T4b_modalidade = t4b, T4c_investimento = t4c), "parte2_repasses.xlsx")
write_xlsx(instr, file.path(TAB, "dicionario_instrumentos.xlsx"))

f9 <- t2 |> filter(`Ano de pagamento` <= 2025) |>
  pivot_longer(c(`Exercício (R$ mi)`, `Restos a pagar (R$ mi)`), names_to = "Fonte", values_to = "v") |>
  mutate(Fonte = str_remove(Fonte, " \\(R\\$ mi\\)"))
salva_fig(ggplot(f9, aes(factor(`Ano de pagamento`), v / 1e3, fill = Fonte)) + geom_col(width = 0.7) +
            scale_fill_manual(values = c(AZUL, LARANJA)) + labs(x = NULL, y = "R$ bilhões") + tema_tcc,
          "fig9_repasses_por_ano.png")

print(t1); print(t2, n = 20); print(t3b); print(t4); print(t4b); print(t4c)
message("Script 02 concluído.")

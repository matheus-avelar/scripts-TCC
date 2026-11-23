# =====================================================================
# Script 01 - Formulários A e B da LC nº 171/2023
# Entradas : dados/Formulario_A.xlsx, dados/Formulario_B.xlsx
# Saídas   : saida/tabelas/parte1_formularios.xlsx
#            saida/figuras/fig1 a fig4
#            saida/bases/formA_limpo.rds, formB_limpo.rds, base_municipal.rds
# Hipóteses: H3 (saldos por ano de origem), H2 (categoria e motivos),
#            desajuste alocativo (origem x destino)
# Segurança: as colunas de senha não são lidas nem gravadas, e a aba
#            auxiliar do Formulário B nunca é aberta (lê-se só a 1ª aba).
# =====================================================================
source("scripts/00_config.R")

# ---------------------------------------------------------------------
# 1. Leitura (somente a 1ª aba; coluna de senha descartada na leitura)
# ---------------------------------------------------------------------
A <- read_excel(file.path(DADOS, "Formulario_A.xlsx"), sheet = 1, col_types = "text") |>
  select(-any_of("ENTE_SENHA_REGISTRADA"))
B <- read_excel(file.path(DADOS, "Formulario_B.xlsx"), sheet = 1, col_types = "text") |>
  select(-any_of("ENTE_SENHA_REGISTRADA"))

# motivo declarado -> três categorias legais
motivo <- function(x) {
  t <- str_trim(as.character(x))
  case_when(str_starts(t, "1") | str_starts(t, "O objeto") ~ "Objeto cumprido",
            str_starts(t, "2") | str_starts(t, "Impossib") ~ "Impossibilidade material",
            str_starts(t, "3") | str_starts(t, "Desnecess") ~ "Desnecessidade da ação",
            TRUE ~ "Não informado")
}

# ---------------------------------------------------------------------
# 2. Limpeza do Formulário A (Tabela 1)
# ---------------------------------------------------------------------
A <- A |> mutate(SALDO_FIN = num(SALDO_FIN), SALDO_CONST = num(SALDO_CONST),
                 SALDO_3 = num(SALDO_3), SALDO_4 = num(SALDO_4), ENTE_COD = num(ENTE_COD),
                 ANO = num(INSTRUMENTO_ANO))
etapa <- function(d, nome) tibble(
  Etapa = nome, Registros = nrow(d), Entes = n_distinct(d$ENTE_COD),
  Instrumentos = n_distinct(d$INSTRUMENTO_COD),
  `Saldo financeiro (R$ mi)` = sum(d$SALDO_FIN, na.rm = TRUE) / 1e6,
  `Saldo constante (R$ mi)`  = sum(d$SALDO_CONST, na.rm = TRUE) / 1e6)

d0 <- A
d1 <- d0 |> filter(ENTE_REGISTRO_VALIDO == "Sim")
d2 <- d1 |> filter(ENTE_COD > 3100000)
d3 <- d2 |> filter(INSTRUMENTO_ELEGIVEL == "Sim")
d4 <- d3 |> filter(ANO >= ANO_INI, ANO <= ANO_FIM)
d5 <- d4 |> distinct(ENTE_COD, INSTRUMENTO_COD, SALDO_FIN, SALDO_CONST, `MOTIVO TRANSF/TRANSP`, .keep_all = TRUE)
t1 <- bind_rows(etapa(d0, "0. Base bruta"), etapa(d1, "1. Credencial validada"),
                etapa(d2, "2. Apenas municípios"), etapa(d3, "3. Instrumentos elegíveis"),
                etapa(d4, "4. Ano de origem 2016-2022"), etapa(d5, "5. Exclusão de duplicatas exatas"))

d <- d5 |>
  mutate(across(c(SALDO_FIN, SALDO_CONST, SALDO_3, SALDO_4), ~ coalesce(.x, 0)),
         MOTIVO = motivo(`MOTIVO TRANSF/TRANSP`),
         TOTAL = SALDO_FIN + SALDO_CONST,
         TIPO = case_when(SALDO_FIN > 0 & SALDO_CONST == 0 ~ "Financeiro",
                          SALDO_CONST > 0 & SALDO_FIN == 0 ~ "Constante", TRUE ~ "Misto"),
         CATEGORIA = if_else(SALDO_4 > SALDO_3, "Capital", "Corrente"),
         s34 = SALDO_3 + SALDO_4,
         sh_cap = if_else(s34 > 0, SALDO_4 / s34, if_else(CATEGORIA == "Capital", 1, 0)),
         FIN_CAP = SALDO_FIN * sh_cap, FIN_COR = SALDO_FIN - FIN_CAP,
         CONST_CAP = SALDO_CONST * sh_cap, CONST_COR = SALDO_CONST - CONST_CAP,
         PERIODO = if_else(ANO <= 2019, "2016-2019", "2020-2022"),
         cod6 = ENTE_COD %/% 10)
inconsist <- sum(abs(d$TOTAL - d$s34) > 1)

# ---------------------------------------------------------------------
# 3. Tabela 2 - saldos por ano de origem (H3)
# ---------------------------------------------------------------------
t2 <- d |> group_by(Ano = as.character(ANO)) |>
  summarise(`Saldo financeiro (R$ mi)` = sum(SALDO_FIN) / 1e6, `Saldo constante (R$ mi)` = sum(SALDO_CONST) / 1e6)
t2 <- bind_rows(t2, tibble(Ano = "Total", `Saldo financeiro (R$ mi)` = sum(t2[[2]]), `Saldo constante (R$ mi)` = sum(t2[[3]]))) |>
  mutate(`Total (R$ mi)` = `Saldo financeiro (R$ mi)` + `Saldo constante (R$ mi)`,
         `% financeiro` = `Saldo financeiro (R$ mi)` / `Total (R$ mi)` * 100)

# ---------------------------------------------------------------------
# 4. Tabela 3 - categoria econômica (SALDO_3 = corrente; SALDO_4 = capital)
# ---------------------------------------------------------------------
t3 <- tibble(Categoria = c("Corrente", "Capital"),
             `Saldo financeiro (R$ mi)` = c(sum(d$FIN_COR), sum(d$FIN_CAP)) / 1e6,
             `Saldo constante (R$ mi)` = c(sum(d$CONST_COR), sum(d$CONST_CAP)) / 1e6) |>
  mutate(`% do saldo financeiro` = `Saldo financeiro (R$ mi)` / sum(`Saldo financeiro (R$ mi)`) * 100,
         `% do saldo constante` = `Saldo constante (R$ mi)` / sum(`Saldo constante (R$ mi)`) * 100)

# ---------------------------------------------------------------------
# 5. Tabela 4 - motivo por categoria, período e tipo de saldo (+ testes)
# ---------------------------------------------------------------------
MOT <- c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação")
perfil <- function(dd, chave) {
  dd |> filter(MOTIVO %in% MOT) |> group_by(Recorte = .data[[chave]], MOTIVO) |>
    summarise(v = sum(TOTAL)) |> mutate(pct = v / sum(v) * 100, total = sum(v) / 1e6) |>
    select(-v) |> pivot_wider(names_from = MOTIVO, values_from = pct, values_fill = 0) |>
    select(Recorte, any_of(MOT), `Valor (R$ mi)` = total)
}
teste <- function(dd, chave) {
  tab <- table(dd[[chave]], dd$MOTIVO)
  r <- cramer_v(tab)
  tibble(Recorte = chave, `Qui-quadrado` = r$qui2, gl = r$gl, `p-valor` = r$p, `V de Cramér` = r$v, `n (registros)` = r$n)
}
t4 <- bind_rows(perfil(d, "CATEGORIA"), perfil(filter(d, TIPO != "Misto"), "TIPO"), perfil(d, "PERIODO"))
t4_testes <- bind_rows(teste(d, "CATEGORIA"), teste(d, "PERIODO"))

# ---------------------------------------------------------------------
# 6. Tabela 5 - concentração
# ---------------------------------------------------------------------
inst <- d |> group_by(INSTRUMENTO_COD) |> summarise(total = sum(TOTAL)) |> arrange(desc(total)) |>
  mutate(acum = cumsum(total) / sum(total))
mun <- d |> group_by(ENTE_COD) |> summarise(total = sum(TOTAL), fin = sum(SALDO_FIN), n_instr = n_distinct(INSTRUMENTO_COD))
t5 <- tibble(Indicador = c("Instrumentos com saldo declarado", "Instrumentos que somam 50% do valor",
                           "Instrumentos que somam 80% do valor", "Instrumentos que somam 90% do valor",
                           "Municípios com saldo declarado", "Participação dos 10% maiores municípios no saldo total (%)",
                           "Gini do saldo total entre municípios", "Participação dos 10% maiores no saldo financeiro (%)",
                           "Gini do saldo financeiro entre municípios", "Instrumentos por município - mediana",
                           "Instrumentos por município - máximo"),
             Valor = c(nrow(inst), sum(inst$acum < .5) + 1, sum(inst$acum < .8) + 1, sum(inst$acum < .9) + 1,
                       nrow(mun), top10(mun$total), gini(mun$total), top10(mun$fin), gini(mun$fin),
                       median(mun$n_instr), max(mun$n_instr)))

# ---------------------------------------------------------------------
# 7. Formulário B - destino pretendido (Tabela 6)
# ---------------------------------------------------------------------
INCISOS <- c(I = "Vigilância em saúde", II = "Atenção integral (assistência)", III = "Capacitação de pessoal",
             IV = "Desenvolvimento científico e tecnológico", V = "Insumos (medicamentos, equipamentos etc.)",
             VI = "Saneamento de domicílios e pequenas comunidades", VII = "Saneamento em DSEI e quilombos",
             VIII = "Manejo ambiental (controle de vetores)", IX = "Investimento na rede física",
             X = "Remuneração de pessoal ativo", XI = "Apoio administrativo", XII = "Gestão do sistema e operação de unidades")
b <- B |> mutate(ENTE_COD = num(ENTE_COD)) |>
  filter(ENTE_REGISTRO_VALIDO == "Sim", ENTE_COD > 3100000) |> distinct() |>
  mutate(across(starts_with("VALOR_"), ~ coalesce(num(.x), 0)),
         VALOR = VALOR_TRANSPOSICAO + VALOR_TRANSFERENCIA,
         COR = VALOR_TRANSPOSICAO_CORR + VALOR_TRANSFERENCIA_CORR,
         CAP = VALOR_TRANSPOSICAO_CAP + VALOR_TRANSFERENCIA_CAP,
         INCISO = str_trim(str_split_i(DESTINO_ACAO_DE_SAUDE, " - ", 1)))
t6 <- b |> group_by(INCISO) |> summarise(Registros = n(), `Valor (R$ mi)` = sum(VALOR) / 1e6, Municipios = n_distinct(ENTE_COD)) |>
  mutate(`Ação (art. 3º da LC 141/2012)` = INCISOS[INCISO], `% do valor` = `Valor (R$ mi)` / sum(`Valor (R$ mi)`) * 100) |>
  arrange(desc(`% do valor`)) |> relocate(`Ação (art. 3º da LC 141/2012)`, .after = INCISO)
t6b <- b |> group_by(Tipo = DESTINO_TIPO) |> summarise(`Valor (R$ mi)` = sum(VALOR) / 1e6) |>
  mutate(`% do valor` = `Valor (R$ mi)` / sum(`Valor (R$ mi)`) * 100)

# ---------------------------------------------------------------------
# 8. Tabela 7 - capital na origem x no destino, por município (Wilcoxon pareado)
#    (usa todos os anos de origem, porque o Formulário B não identifica o instrumento de origem)
# ---------------------------------------------------------------------
a_all <- d3 |> distinct(ENTE_COD, INSTRUMENTO_COD, SALDO_FIN, SALDO_CONST, `MOTIVO TRANSF/TRANSP`, .keep_all = TRUE)
orig <- a_all |> group_by(ENTE_COD) |> summarise(o_cor = sum(SALDO_3, na.rm = TRUE), o_cap = sum(SALDO_4, na.rm = TRUE))
dest <- b |> group_by(ENTE_COD) |> summarise(d_cor = sum(COR), d_cap = sum(CAP), d_pessoal = sum(VALOR[INCISO == "X"]))
od <- inner_join(orig, dest, by = "ENTE_COD") |> filter(o_cor + o_cap > 0, d_cor + d_cap > 0) |>
  mutate(cap_o = o_cap / (o_cor + o_cap) * 100, cap_d = d_cap / (d_cor + d_cap) * 100, var = cap_d - cap_o)
w <- wilcox.test(od$cap_o, od$cap_d, paired = TRUE, exact = FALSE)
t7 <- tibble(Indicador = c("Municípios comparados", "Capital na origem - total estadual (%)",
                           "Capital no destino - total estadual (%)", "Capital na origem - mediana municipal (%)",
                           "Capital no destino - mediana municipal (%)", "Municípios que reduziram a parcela de capital",
                           "Municípios que aumentaram a parcela de capital", "Municípios sem variação",
                           "Wilcoxon pareado - estatística V", "Wilcoxon pareado - p-valor"),
             Valor = c(nrow(od), sum(od$o_cap) / sum(od$o_cor + od$o_cap) * 100, sum(od$d_cap) / sum(od$d_cor + od$d_cap) * 100,
                       median(od$cap_o), median(od$cap_d), sum(od$var < -0.5), sum(od$var > 0.5),
                       sum(abs(od$var) <= 0.5), unname(w$statistic), w$p.value))

# ---------------------------------------------------------------------
# 9. Base municipal (uma linha por município) para os scripts seguintes
# ---------------------------------------------------------------------
pct_mot <- d |> filter(MOTIVO %in% MOT) |> group_by(ENTE_COD, MOTIVO) |> summarise(v = sum(TOTAL)) |>
  mutate(p = v / sum(v) * 100) |> select(-v) |>
  pivot_wider(names_from = MOTIVO, values_from = p, values_fill = 0) |>
  rename(pct_objeto = `Objeto cumprido`, pct_impossibilidade = `Impossibilidade material`,
         pct_desnecessidade = `Desnecessidade da ação`)
base_mun <- d |> group_by(ENTE_COD, cod6) |>
  summarise(municipio = first(ENTE_NOME), saldo_fin = sum(SALDO_FIN), saldo_const = sum(SALDO_CONST),
            saldo_fin_corrente = sum(FIN_COR), saldo_fin_capital = sum(FIN_CAP),
            saldo_fin_2020_2022 = sum(SALDO_FIN[ANO >= 2020]), saldo_fin_2021_2022 = sum(SALDO_FIN[ANO >= 2021]),
            n_instrumentos = n_distinct(INSTRUMENTO_COD)) |> ungroup() |>
  left_join(pct_mot, by = "ENTE_COD") |>
  left_join(orig |> rename(origem_corrente_todos_anos = o_cor, origem_capital_todos_anos = o_cap), by = "ENTE_COD") |>
  left_join(dest |> rename(destino_corrente = d_cor, destino_capital = d_cap, destino_pessoal_X = d_pessoal), by = "ENTE_COD")

saveRDS(d, file.path(BASES, "formA_limpo.rds"))
saveRDS(b, file.path(BASES, "formB_limpo.rds"))
saveRDS(base_mun, file.path(BASES, "base_municipal.rds"))

grava_excel(list(T1_tratamento = t1, T2_ano_origem = t2, T3_categoria = t3, T4_motivos = t4, T4_testes = t4_testes,
                 T5_concentracao = t5, T5b_top30_instrumentos = head(inst, 30), T6_destino_inciso = t6,
                 T6b_transp_transf = t6b, T7_origem_destino = t7, Base_municipal = base_mun),
            "parte1_formularios.xlsx")

# ---------------------------------------------------------------------
# 10. Figuras
# ---------------------------------------------------------------------
f1 <- t2 |> filter(Ano != "Total") |>
  pivot_longer(c(`Saldo financeiro (R$ mi)`, `Saldo constante (R$ mi)`), names_to = "Tipo", values_to = "v") |>
  mutate(Tipo = factor(str_remove(Tipo, " \\(R\\$ mi\\)"), levels = c("Saldo financeiro", "Saldo constante")))
salva_fig(ggplot(f1, aes(Ano, v, fill = Tipo)) + geom_col(position = position_dodge(0.8), width = 0.75) +
            scale_fill_manual(values = c(AZUL, LARANJA)) + labs(x = NULL, y = "R$ milhões") + tema_tcc,
          "fig1_saldos_por_ano.png")

f2 <- t4 |> filter(Recorte %in% c("Corrente", "Capital")) |> pivot_longer(all_of(MOT), names_to = "Motivo", values_to = "pct") |>
  mutate(Motivo = factor(Motivo, levels = rev(MOT)))
salva_fig(ggplot(f2, aes(pct, Recorte, fill = Motivo)) + geom_col(width = 0.6, color = "white") +
            geom_text(aes(label = ifelse(pct > 7, paste0(round(pct), "%"), "")), position = position_stack(vjust = 0.5),
                      color = "white", size = 3, family = "serif") +
            scale_fill_manual(values = c(VERDE, LARANJA, AZUL), breaks = MOT) +
            labs(x = "% do valor declarado", y = NULL) + tema_tcc, "fig2_motivo_por_categoria.png", alt = 6)

f3 <- t6 |> filter(`% do valor` >= 0.5)
salva_fig(ggplot(f3, aes(`% do valor`, reorder(`Ação (art. 3º da LC 141/2012)`, `% do valor`))) +
            geom_col(fill = AZUL, width = 0.6) +
            geom_text(aes(label = paste0(formatC(`% do valor`, format = "f", digits = 1, decimal.mark = ","), "%")),
                      hjust = -0.15, size = 3, family = "serif") +
            scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
            labs(x = "% do valor replanejado (Formulário B)", y = NULL) + tema_tcc, "fig3_destino_por_acao.png")

salva_fig(ggplot(inst |> mutate(ordem = row_number()), aes(ordem, acum * 100)) + geom_line(color = AZUL, linewidth = 1) +
            geom_hline(yintercept = c(50, 80), linetype = "dashed", color = "#c3c2b7") +
            labs(x = "Instrumentos ordenados pelo saldo declarado", y = "% acumulado do saldo") + tema_tcc,
          "fig4_concentracao_instrumentos.png")

message("Registros com SALDO_3 + SALDO_4 diferente do total: ", inconsist)
print(t1); print(t2)
message("Script 01 concluído.")

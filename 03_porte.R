# =====================================================================
# Script 03 - Porte populacional
# Entradas : dados/tabela6579.xlsx (IBGE, estimativas 2016-2021, com código)
#            dados/tabela4709.xlsx (IBGE, Censo 2022, só com o nome)
#            saida/bases/base_municipal.rds (script 01)
# Saídas   : saida/tabelas/parte3_porte.xlsx
#            saida/figuras/fig5 a fig7
#            saida/bases/populacao.rds, base_municipal_pop.rds
# População de referência = média 2016-2022; faixas definidas por ela.
# =====================================================================
source("scripts/00_config.R")

# ---------------------------------------------------------------------
# 1. População (6579 com código; 4709 ligada pelo nome normalizado)
# ---------------------------------------------------------------------
chave <- function(x) str_remove_all(tolower(stri_trans_general(x, "Latin-ASCII")), "[^a-z]")

est <- read_excel(file.path(DADOS, "tabela6579.xlsx"), col_names = FALSE, skip = 4, col_types = "text", .name_repair = "unique_quiet")[, 1:8]
names(est) <- c("cod7", "nome", paste0("p", 2016:2021))
est <- est |> filter(!is.na(num(cod7))) |> mutate(cod7 = as.integer(cod7), across(starts_with("p"), num))

cen <- read_excel(file.path(DADOS, "tabela4709.xlsx"), col_names = FALSE, skip = 5, col_types = "text", .name_repair = "unique_quiet")[, 1:2]
names(cen) <- c("nome", "p2022")
cen <- cen |> filter(str_ends(nome, "\\(MG\\)")) |> mutate(p2022 = num(p2022), k = chave(str_remove(nome, " \\(MG\\)$")))

pop <- est |> mutate(k = chave(str_remove(nome, " \\(MG\\)$"))) |> left_join(select(cen, k, p2022), by = "k") |>
  mutate(pop_media = rowMeans(across(c(p2016, p2017, p2018, p2019, p2020, p2021, p2022)), na.rm = TRUE),
         faixa = cut(pop_media, CORTES, labels = FAIXAS, right = TRUE),
         cod6 = cod7 %/% 10)
sem_censo <- pop |> filter(is.na(p2022)) |> select(cod7, nome)
message("Municípios: ", nrow(pop), " | sem correspondência no Censo 2022: ", nrow(sem_censo))
saveRDS(pop, file.path(BASES, "populacao.rds"))

# ---------------------------------------------------------------------
# 2. Base municipal com população
# ---------------------------------------------------------------------
bm <- readRDS(file.path(BASES, "base_municipal.rds"))
d <- pop |> select(cod7, cod6, nome, pop_media, faixa) |> left_join(select(bm, -cod6), by = c("cod7" = "ENTE_COD")) |>
  mutate(declarou = !is.na(saldo_fin),
         across(c(saldo_fin, saldo_const, n_instrumentos, destino_corrente, destino_capital, destino_pessoal_X,
                  origem_corrente_todos_anos, origem_capital_todos_anos), ~ coalesce(.x, 0)),
         saldo_total = saldo_fin + saldo_const,
         fin_pc = saldo_fin / pop_media, const_pc = saldo_const / pop_media,
         instr_mil = n_instrumentos / pop_media * 1000)
saveRDS(d, file.path(BASES, "base_municipal_pop.rds"))
dm <- d |> filter(declarou)

# ---------------------------------------------------------------------
# 3. Perfil por faixa (Tabela 8) e testes
# ---------------------------------------------------------------------
t8 <- dm |> group_by(Faixa = faixa) |>
  summarise(`Municípios com saldo` = n(), pop = sum(pop_media), fin = sum(saldo_fin),
            `Saldo fin. por hab. - mediana (R$)` = median(fin_pc),
            `Saldo const. por hab. - mediana (R$)` = median(const_pc),
            `Instrumentos por município - mediana` = median(n_instrumentos),
            `Instrumentos por mil hab. - mediana` = median(instr_mil)) |>
  mutate(`% da população` = pop / sum(pop) * 100, `% do saldo financeiro` = fin / sum(fin) * 100) |>
  select(-pop, -fin) |> relocate(`% da população`, `% do saldo financeiro`, .after = `Municípios com saldo`)

kw <- function(y, g) { k <- kruskal.test(y ~ g); c(k$statistic, k$p.value) }
pares_mw <- function(dd, var) {
  out <- list()
  for (i in 1:5) for (j in (i + 1):6) {
    a <- dd[[var]][dd$faixa == FAIXAS[i]]; b <- dd[[var]][dd$faixa == FAIXAS[j]]
    p <- wilcox.test(a, b, exact = FALSE)$p.value
    out[[length(out) + 1]] <- tibble(`Faixa A` = FAIXAS[i], `Faixa B` = FAIXAS[j], `Mediana A` = median(a),
                                     `Mediana B` = median(b), `p bruto` = p, `p Bonferroni` = min(1, p * 15))
  }
  bind_rows(out)
}
t8b <- pares_mw(dm, "fin_pc")

# ---------------------------------------------------------------------
# 4. Motivo por faixa (Tabela 9) e destino por faixa (Tabela 10)
# ---------------------------------------------------------------------
t9 <- dm |> mutate(v_obj = coalesce(pct_objeto, 0) / 100 * saldo_total,
                   v_imp = coalesce(pct_impossibilidade, 0) / 100 * saldo_total,
                   v_des = coalesce(pct_desnecessidade, 0) / 100 * saldo_total) |>
  group_by(Faixa = faixa) |> summarise(o = sum(v_obj), i = sum(v_imp), s = sum(v_des)) |>
  mutate(t = o + i + s, `Objeto cumprido` = o / t * 100, `Impossibilidade material` = i / t * 100,
         `Desnecessidade da ação` = s / t * 100) |> select(Faixa, `Objeto cumprido`:`Desnecessidade da ação`)

ok <- dm |> filter(destino_capital + destino_corrente > 0) |>
  mutate(pessoal_destino = destino_pessoal_X / (destino_capital + destino_corrente))
t10 <- ok |> group_by(Faixa = faixa) |>
  summarise(`Municípios` = n(),
            `Capital na origem (%)` = sum(origem_capital_todos_anos) / sum(origem_capital_todos_anos + origem_corrente_todos_anos) * 100,
            `Capital no destino (%)` = sum(destino_capital) / sum(destino_capital + destino_corrente) * 100,
            `Pessoal (inciso X) no destino (%)` = sum(destino_pessoal_X) / sum(destino_capital + destino_corrente) * 100,
            `Municípios que destinam algo a pessoal (%)` = mean(destino_pessoal_X > 0) * 100)

# Qui-quadrado e V de Cramér (metodologia, 7.5 a): distribuição dos motivos (Formulário A)
# e dos destinos por inciso do art. 3º da LC 141/2012 (Formulário B) entre as seis faixas.
# Unidade: registro. Os percentuais por valor estão nas tabelas T9 e T10.
fa <- readRDS(file.path(BASES, "formA_limpo.rds")) |> filter(MOTIVO %in% c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação")) |>
  left_join(pop |> select(cod7, faixa), by = c("ENTE_COD" = "cod7"))
fb <- readRDS(file.path(BASES, "formB_limpo.rds")) |> left_join(pop |> select(cod7, faixa), by = c("ENTE_COD" = "cod7")) |>
  filter(!is.na(INCISO), INCISO != "")
cv_mot <- cramer_v(table(fa$faixa, fa$MOTIVO))
cv_des <- cramer_v(table(fb$faixa, fb$INCISO))
cv_des_cap <- cramer_v(table(fb$faixa, fb$CAP > fb$COR))
t9b <- tibble(Teste = c("Qui-quadrado: motivo declarado x faixa de porte (registros do Formulário A)",
                        "Qui-quadrado: inciso de destino x faixa de porte (registros do Formulário B)",
                        "Qui-quadrado: destino predominantemente de capital x faixa de porte (Formulário B)"),
              `Qui-quadrado` = c(cv_mot$qui2, cv_des$qui2, cv_des_cap$qui2), gl = c(cv_mot$gl, cv_des$gl, cv_des_cap$gl),
              `p-valor` = c(cv_mot$p, cv_des$p, cv_des_cap$p), `V de Cramér` = c(cv_mot$v, cv_des$v, cv_des_cap$v),
              `n (registros)` = c(cv_mot$n, cv_des$n, cv_des_cap$n))
t9c <- fa |> count(Faixa = faixa, MOTIVO) |> group_by(Faixa) |> mutate(pct = n / sum(n) * 100) |>
  select(-n) |> pivot_wider(names_from = MOTIVO, values_from = pct)

# ---------------------------------------------------------------------
# 5. Testes e relação contínua
# ---------------------------------------------------------------------
dm <- dm |> mutate(lpop = log(pop_media), y = log1p(fin_pc))
ols <- mqo(y ~ lpop + I(lpop^2), dm)
sp <- cor.test(dm$pop_media, dm$fin_pc, method = "spearman", exact = FALSE)
testes <- bind_rows(
  tibble(Teste = "Kruskal-Wallis: saldo fin. por hab. entre faixas", `Estatística` = kw(dm$fin_pc, dm$faixa)[1], `p-valor` = kw(dm$fin_pc, dm$faixa)[2]),
  tibble(Teste = "Kruskal-Wallis: saldo const. por hab. entre faixas", `Estatística` = kw(dm$const_pc, dm$faixa)[1], `p-valor` = kw(dm$const_pc, dm$faixa)[2]),
  tibble(Teste = "Kruskal-Wallis: % impossibilidade entre faixas", `Estatística` = kw(coalesce(dm$pct_impossibilidade, 0), dm$faixa)[1],
         `p-valor` = kw(coalesce(dm$pct_impossibilidade, 0), dm$faixa)[2]),
  tibble(Teste = "Kruskal-Wallis: % do destino em pessoal entre faixas", `Estatística` = kw(ok$pessoal_destino, ok$faixa)[1],
         `p-valor` = kw(ok$pessoal_destino, ok$faixa)[2]),
  tibble(Teste = "Spearman: população x saldo fin. por hab.", `Estatística` = unname(sp$estimate), `p-valor` = sp$p.value),
  tibble(Teste = "MQO log(1+saldo fin./hab.): coef. log(pop)", `Estatística` = ols$coef[2, 1], `p-valor` = ols$coef[2, 4]),
  tibble(Teste = "MQO log(1+saldo fin./hab.): coef. log(pop)^2", `Estatística` = ols$coef[3, 1], `p-valor` = ols$coef[3, 4]))

grava_excel(list(T8_perfil_por_faixa = t8, T8b_pares_saldo_fin_hab = t8b, T9_motivo_por_faixa = t9,
                 T9b_quiquadrado_por_faixa = t9b, T9c_motivo_por_faixa_registros = t9c,
                 T10_destino_por_faixa = t10, Testes = testes, Sem_censo = sem_censo), "parte3_porte.xlsx")

# ---------------------------------------------------------------------
# 6. Figuras
# ---------------------------------------------------------------------
salva_fig(ggplot(dm, aes(faixa, pmax(fin_pc, 1))) + geom_boxplot(fill = "#cfe0f6", color = AZUL, outlier.shape = NA, width = 0.55) +
            scale_y_log10() + labs(x = NULL, y = "R$ por habitante (escala log)") + tema_tcc,
          "fig5_saldo_fin_hab_por_faixa.png")

MOT <- c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação")
f6 <- t9 |> pivot_longer(-Faixa, names_to = "Motivo", values_to = "pct") |>
  mutate(Motivo = factor(Motivo, levels = rev(MOT)), Faixa = factor(Faixa, levels = rev(FAIXAS)))
salva_fig(ggplot(f6, aes(pct, Faixa, fill = Motivo)) + geom_col(width = 0.65, color = "white") +
            geom_text(aes(label = ifelse(pct > 7, paste0(round(pct), "%"), "")), position = position_stack(vjust = 0.5),
                      color = "white", size = 2.8, family = "serif") +
            scale_fill_manual(values = c(VERDE, LARANJA, AZUL), breaks = MOT) +
            labs(x = "% do valor declarado", y = NULL) + tema_tcc, "fig6_motivo_por_faixa.png")

f7 <- t10 |> select(Faixa, `Capital na origem (%)`, `Capital no destino (%)`) |>
  pivot_longer(-Faixa, names_to = "Tipo", values_to = "v") |> mutate(Tipo = str_remove(Tipo, " \\(%\\)"))
salva_fig(ggplot(f7, aes(Faixa, v, fill = Tipo)) + geom_col(position = position_dodge(0.8), width = 0.75) +
            scale_fill_manual(values = c(AZUL, LARANJA)) + labs(x = NULL, y = "% do valor") + tema_tcc,
          "fig7_capital_origem_destino_por_faixa.png")

print(t8); print(testes)
message("Script 03 concluído.")

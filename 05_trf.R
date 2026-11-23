# =====================================================================
# Script 05 - Taxa de represamento financeiro (TRF) com os repasses da SES-MG
# Entradas : saida/bases/formA_limpo.rds, pares_mun_instr.rds, pagamentos.rds,
#            base_analitica.rds, siops_transf_estadual_ano.rds (scripts 01 a 04)
# Saídas   : saida/tabelas/parte5_trf.xlsx
#            saida/figuras/fig8_trf_por_faixa.png, fig10_repasses_x_siops.png
#            saida/bases/base_trf.rds, pares_trf.rds
#
# Definições
#   Recebido (denominador) = soma do Valor Pago Orçamentário (exercício + restos a pagar)
#     dos instrumentos de 2016-2022, pago até 2023 (os saldos foram declarados em 2023).
#   TRF = saldo financeiro declarado (instrumentos de 2016-2022) / recebido.
#   TRF até 2022 = mesma razão com pagamentos só até 2022 (robustez).
#   TRF recente = só instrumentos de 2020-2022.
#   Taxa de não repasse (TNR) = saldo constante / (recebido + saldo constante).
#   Razões acima de 1 são limitadas a 1 e sinalizadas (podem refletir rendimentos de
#   aplicação ou saldos de instrumentos pagos com outro número).
# Universo: municípios que declararam saldos (Formulário A).
# =====================================================================
source("scripts/00_config.R")

A  <- readRDS(file.path(BASES, "formA_limpo.rds"))
pr <- readRDS(file.path(BASES, "pares_mun_instr.rds"))
P  <- readRDS(file.path(BASES, "pagamentos.rds"))
ba <- readRDS(file.path(BASES, "base_analitica.rds"))
siops_ano <- readRDS(file.path(BASES, "siops_transf_estadual_ano.rds"))

# ---------------------------------------------------------------------
# 1. Saldos do Formulário A por par município x instrumento e ligação com os pagamentos
# ---------------------------------------------------------------------
sa <- A |> mutate(inum = as.integer(str_match(INSTRUMENTO_COD, "^0*(\\d+)/(\\d{4})$")[, 2]),
                  iano = as.integer(str_match(INSTRUMENTO_COD, "^0*(\\d+)/(\\d{4})$")[, 3]),
                  instr_id = paste0(inum, "/", iano)) |>
  group_by(cod6, instr_id) |>
  summarise(saldo_fin = sum(SALDO_FIN), saldo_const = sum(SALDO_CONST),
            fin_cap = sum(FIN_CAP), fin_cor = sum(FIN_COR)) |> ungroup()
mun_decl <- unique(sa$cod6)

lig <- sa |> left_join(pr |> select(cod6, instr_id, pago_ate_2023), by = c("cod6", "instr_id")) |>
  mutate(situacao = case_when(is.na(pago_ate_2023) ~ "Par sem nenhum pagamento registrado",
                              pago_ate_2023 <= 0 ~ "Par sem nenhum pagamento registrado",
                              TRUE ~ "Par com pagamento até 2023"),
         instr_existe = instr_id %in% unique(P$instr_id))
t1 <- lig |> group_by(`Situação` = situacao) |>
  summarise(Pares = n(), `Saldo financeiro (R$ mi)` = sum(saldo_fin) / 1e6, `Saldo constante (R$ mi)` = sum(saldo_const) / 1e6) |>
  mutate(`% do saldo financeiro` = `Saldo financeiro (R$ mi)` / sum(`Saldo financeiro (R$ mi)`) * 100,
         `% do saldo constante` = `Saldo constante (R$ mi)` / sum(`Saldo constante (R$ mi)`) * 100)
t1b <- tibble(Indicador = c("Instrumentos no Formulário A (2016-2022)", "... encontrados na base de pagamentos",
                            "Saldo financeiro de instrumentos não encontrados (R$ mi)",
                            "Pares com pagamento em que saldo financeiro > recebido",
                            "... dos quais com saldo até 20% acima do recebido (compatível com rendimentos)"),
              Valor = c(n_distinct(lig$instr_id), n_distinct(lig$instr_id[lig$instr_existe]),
                        sum(lig$saldo_fin[!lig$instr_existe]) / 1e6,
                        sum(lig$pago_ate_2023 > 0 & lig$saldo_fin > lig$pago_ate_2023, na.rm = TRUE),
                        sum(lig$pago_ate_2023 > 0 & lig$saldo_fin > lig$pago_ate_2023 & lig$saldo_fin <= 1.2 * lig$pago_ate_2023, na.rm = TRUE)))

# ---------------------------------------------------------------------
# 2. TRF municipal
# ---------------------------------------------------------------------
rec <- pr |> filter(cod6 %in% mun_decl) |> group_by(cod6) |>
  summarise(recebido = sum(pago_ate_2023), recebido_ate_2022 = sum(pago_ate_2022),
            recebido_2020_2022 = sum(pago_ate_2023[iano >= 2020]),
            recebido_2021_2022 = sum(pago_ate_2023[iano >= 2021]),
            instr_recebidos = n_distinct(instr_id[pago_ate_2023 > 0]))
# Atualização monetária pelo IPCA (robustez e repasse por habitante)
# A planilha do SIDRA (tabela 1737) traz a variação acumulada no ano (dez./dez.). Os pagamentos
# de cada ano são levados a dezembro de 2023 supondo que, em média, ocorreram no meio do ano:
# fator(t) = (1 + ipca_t)^0,5 x produto de (1 + ipca_s) para s = t+1, ..., 2023.
ipca <- read_excel(file.path(DADOS, "ipca_tabela_1737.xlsx"), col_names = FALSE, col_types = "text",
                   .name_repair = "unique_quiet")[, c(1, 3)]
names(ipca) <- c("per", "var")
ipca <- ipca |> mutate(ano = suppressWarnings(as.integer(str_sub(per, 1, 4))), var = num(str_replace(var, ",", ".")) / 100) |>
  filter(!is.na(ano), !is.na(var), ano >= 2016, ano <= 2023) |> arrange(ano)
stopifnot(nrow(ipca) == 8)
ipca$fator <- sapply(ipca$ano, function(t) (1 + ipca$var[ipca$ano == t])^0.5 * prod(1 + ipca$var[ipca$ano > t]))
rec_ipca <- P |> filter(iano >= ANO_INI, iano <= ANO_FIM, ano_pag <= 2023, cod6 %in% mun_decl) |>
  left_join(select(ipca, ano_pag = ano, fator), by = "ano_pag") |>
  group_by(cod6) |> summarise(recebido_ipca = sum(pago * fator))
rec <- rec |> left_join(rec_ipca, by = "cod6")

# Numerador da TRF: só os saldos financeiros de pares COM pagamento até 2023, para que
# numerador e denominador se refiram aos mesmos instrumentos (mesma regra do modelo de pares).
# Os saldos de pares sem pagamento registrado entram apenas na versão de sensibilidade
# (trf_com_sem_pag), que reproduz o cálculo da versão anterior.
# O saldo constante (TNR) usa todos os pares: ele é, por definição, recurso não pago.
num_trf <- lig |> mutate(iano = as.integer(str_sub(instr_id, -4)),
                         com_pag = situacao == "Par com pagamento até 2023") |> group_by(cod6) |>
  summarise(saldo_fin_a = sum(saldo_fin[com_pag]), saldo_fin_todos = sum(saldo_fin),
            saldo_const_a = sum(saldo_const),
            saldo_fin_2020_2022_a = sum(saldo_fin[com_pag & iano >= 2020]))
v <- ba |> filter(declarou) |> left_join(rec, by = "cod6") |> left_join(num_trf, by = "cod6") |>
  mutate(trf_bruta = saldo_fin_a / recebido,
         trf_acima_1 = trf_bruta > 1,
         trf = pmin(trf_bruta, 1),
         trf_com_sem_pag = pmin(saldo_fin_todos / recebido, 1),
         trf_ipca = pmin(saldo_fin_a / recebido_ipca, 1),
         trf_2022 = pmin(saldo_fin_a / recebido_ate_2022, 1),
         trf_recente = pmin(saldo_fin_2020_2022_a / recebido_2020_2022, 1),
         tnr = saldo_const_a / (recebido + saldo_const_a),
         repasse_hab_ano_nominal = recebido / pop_media / 7,
         repasse_hab_ano = recebido_ipca / pop_media / 7,     # em R$ de dez./2023 (controle dos modelos)
         trf_siops = pmin(saldo_fin_a / siops_transf_estadual_2016_2022, 1)) |>
  mutate(across(c(trf, trf_com_sem_pag, trf_ipca, trf_2022, trf_recente, trf_siops, tnr), ~ if_else(is.finite(.x), .x, NA_real_)))
saveRDS(v, file.path(BASES, "base_trf.rds"))

# ---------------------------------------------------------------------
# 3. Validação: repasses x SIOPS e TRF agregada
# ---------------------------------------------------------------------
anual <- P |> filter(ano_pag <= 2022) |> group_by(Ano = ano_pag) |>
  summarise(`Repasses SES - todos os instrumentos (R$ mi)` = sum(pago) / 1e6) |>
  left_join(siops_ano, by = "Ano") |>
  mutate(`Razão repasses / SIOPS` = `Repasses SES - todos os instrumentos (R$ mi)` / `Transferências estaduais estimadas - SIOPS (R$ mi)`)
rec_mun_todos <- P |> filter(ano_pag <= 2022) |> group_by(cod6) |> summarise(rep = sum(pago))
cmp <- ba |> select(cod6, siops_transf_estadual_2016_2022) |> inner_join(rec_mun_todos, by = "cod6")
sp_val <- cor.test(cmp$rep, cmp$siops_transf_estadual_2016_2022, method = "spearman", exact = FALSE)
sp_trf <- cor.test(v$trf, v$trf_siops, method = "spearman", exact = FALSE)
w <- v |> filter(!is.na(trf))
t2 <- tibble(Indicador = c("Municípios com TRF calculada", "Municípios com TRF acima de 100% (limitada a 100%)",
                           "Recebido 2016-2022 (pago até 2023) - soma (R$ bi)", "Saldo financeiro declarado (R$ bi)",
                           "TRF agregada (%)", "TRF agregada incluindo saldos de pares sem pagamento (%) - sensibilidade",
                           "TRF agregada com repasses atualizados pelo IPCA até dez./2023 (%) - robustez",
                           "TRF agregada com pagamentos até 2022 (%)", "TRF agregada 2020-2022 (%)",
                           "TRF - mediana municipal (%)", "TRF incluindo pares sem pagamento - mediana municipal (%)", "TRF com IPCA - mediana municipal (%)", "TRF até 2022 - mediana municipal (%)", "TRF 2020-2022 - mediana municipal (%)",
                           "Taxa de não repasse agregada (%)", "Taxa de não repasse - mediana municipal (%)",
                           "Spearman: repasses 2016-2022 x SIOPS (municípios)", "Spearman: TRF (repasses) x TRF (SIOPS)",
                           "Referência SES/MG (2025): saldo / repasses 2020-2023 (%)"),
             Valor = c(nrow(w), sum(w$trf_acima_1), sum(w$recebido) / 1e9, sum(w$saldo_fin_a) / 1e9,
                       sum(w$saldo_fin_a) / sum(w$recebido) * 100, sum(w$saldo_fin_todos) / sum(w$recebido) * 100,
                       sum(w$saldo_fin_a) / sum(w$recebido_ipca) * 100,
                       sum(w$saldo_fin_a) / sum(w$recebido_ate_2022) * 100,
                       sum(w$saldo_fin_2020_2022_a) / sum(w$recebido_2020_2022) * 100,
                       median(w$trf) * 100, median(w$trf_com_sem_pag, na.rm = TRUE) * 100, median(w$trf_ipca, na.rm = TRUE) * 100, median(w$trf_2022, na.rm = TRUE) * 100, median(w$trf_recente, na.rm = TRUE) * 100,
                       sum(w$saldo_const_a) / sum(w$recebido + w$saldo_const_a) * 100, median(w$tnr) * 100,
                       unname(sp_val$estimate), unname(sp_trf$estimate), 13.87))

t2 <- bind_rows(t2, tibble(Indicador = c("Spearman: TRF nominal x TRF com IPCA", paste("Fator IPCA", ipca$ano)),
                           Valor = c(unname(cor.test(w$trf, w$trf_ipca, method = "spearman", exact = FALSE)$estimate), ipca$fator)))

# ---------------------------------------------------------------------
# 4. TRF por faixa de porte (H1b)
# ---------------------------------------------------------------------
t3 <- w |> group_by(Faixa = faixa) |>
  summarise(`Municípios` = n(),
            `TRF - mediana (%)` = median(trf) * 100,
            `TRF - agregada (%)` = sum(saldo_fin_a) / sum(recebido) * 100,
            `TRF 2020-2022 - mediana (%)` = median(trf_recente, na.rm = TRUE) * 100,
            `TNR - mediana (%)` = median(tnr) * 100,
            `Repasse estadual por hab./ano - mediana (R$)` = median(repasse_hab_ano),
            `Instrumentos recebidos - mediana` = median(instr_recebidos))
kw_trf <- kruskal.test(trf ~ faixa, data = w)
kw_tnr <- kruskal.test(tnr ~ faixa, data = w)
pares_mw <- list()
for (i in 1:5) for (j in (i + 1):6) {
  a <- w$trf[w$faixa == FAIXAS[i]]; b <- w$trf[w$faixa == FAIXAS[j]]
  p <- wilcox.test(a, b, exact = FALSE)$p.value
  pares_mw[[length(pares_mw) + 1]] <- tibble(`Faixa A` = FAIXAS[i], `Faixa B` = FAIXAS[j],
                                             `Mediana A (%)` = median(a) * 100, `Mediana B (%)` = median(b) * 100,
                                             `p Bonferroni` = min(1, p * 15))
}
t3b <- bind_rows(pares_mw)
t3c <- tibble(Teste = c("Kruskal-Wallis: TRF entre faixas", "Kruskal-Wallis: TNR entre faixas"),
              H = c(unname(kw_trf$statistic), unname(kw_tnr$statistic)), `p-valor` = c(kw_trf$p.value, kw_tnr$p.value))

# Contraste direto da H1b: faixas intermediárias contra as duas pontas.
# Definição principal (LIMA, 2008, p. 249): intermediários = 20 mil a 100 mil habitantes.
# Definição alternativa: 10 mil a 100 mil. Teste unilateral: H1b prevê TRF MAIOR nas intermediárias.
contraste <- function(inter, nome) {
  g <- case_when(w$faixa %in% inter ~ "inter",
                 match(w$faixa, FAIXAS) < min(match(inter, FAIXAS)) ~ "menores",
                 TRUE ~ "maiores")
  out <- list()
  for (ponta in c("menores", "maiores")) {
    a <- w$trf[g == "inter"]; b <- w$trf[g == ponta]
    out[[ponta]] <- tibble(`Definição de intermediárias` = nome,
                           `Comparação` = paste("Intermediárias x", ponta),
                           `Municípios (inter / ponta)` = paste(length(a), "/", length(b)),
                           `Mediana intermediárias (%)` = median(a) * 100, `Mediana ponta (%)` = median(b) * 100,
                           `p (unilateral: inter > ponta)` = wilcox.test(a, b, alternative = "greater", exact = FALSE)$p.value)
  }
  bind_rows(out)
}
t3d <- bind_rows(contraste(c("20 a 50 mil", "50 a 100 mil"), "20 mil a 100 mil (principal)"),
                 contraste(c("10 a 20 mil", "20 a 50 mil", "50 a 100 mil"), "10 mil a 100 mil (alternativa)"))

# ---------------------------------------------------------------------
# 5. Modelos de resposta fracionária para a TRF (H1a, H1b e substituição)
# ---------------------------------------------------------------------
w <- w |> mutate(x_serv10 = serv_mil / 10, x_estat10 = estat_pct / 10, x_comiss10 = comiss_pct / 10,
                 x_prec10 = precario_pct / 10, x_auton = ifgf_autonomia * 10, x_pessoal = ifgf_pessoal * 10,
                 x_invest = ifgf_investimentos * 10, x_rec_propria = siops_rec_propria_asps / 10,
                 x_dep_transf = siops_dep_transf / 10, x_desp_hab_log = log(siops_desp_saude_hab), x_pib_pc_log = log(pib_pc),
                 x_repasse_hab_log = log(repasse_hab_ano), c_recente = recebido_2021_2022 / recebido,
                 faixa = relevel(factor(faixa, levels = FAIXAS), ref = "20 a 50 mil"))
CAP <- "x_serv10 + x_estat10 + x_comiss10 + x_prec10 + gestor_saude_superior + fundo_gerido_pela_sms + colegiado_regional + x_auton + x_pessoal + x_invest"
FIS <- "x_rec_propria + x_dep_transf + x_desp_hab_log"
CTR <- "x_repasse_hab_log + c_recente"
wm <- w |> filter(if_all(c(x_serv10, x_estat10, x_comiss10, x_prec10, gestor_saude_superior, fundo_gerido_pela_sms,
                           colegiado_regional, x_auton, x_pessoal, x_invest, x_rec_propria, x_dep_transf,
                           x_desp_hab_log, x_repasse_hab_log, x_pib_pc_log), ~ !is.na(.x) & is.finite(.x)))
f <- function(rhs, y = "trf") as.formula(paste(y, "~", rhs))
m <- list(
  `M3a - só porte` = frac_logit(f("faixa"), wm),
  `M3b - + capacidade (MUNIC e IFGF)` = frac_logit(f(paste(CAP, "+ faixa")), wm),
  `M3c - + perfil fiscal da saúde (SIOPS)` = frac_logit(f(paste(CAP, "+", FIS, "+ faixa")), wm),
  `M3d - + volume e maturação` = frac_logit(f(paste(CAP, "+", FIS, "+", CTR, "+ faixa")), wm),
  # modelo adicional (metodologia, Quadro 2): o efeito do porte e da capacidade fiscal se mantém
  # quando se considera a riqueza do território?
  `M3e - + PIB per capita` = frac_logit(f(paste(CAP, "+", FIS, "+", CTR, "+ x_pib_pc_log + faixa")), wm))
t4 <- tab_modelos(m)

# robustez (especificação completa)
rhs <- paste(CAP, "+", FIS, "+", CTR, "+ faixa")
rob <- list(
  `MQO (TRF em p.p.)` = mqo(as.formula(paste("I(trf * 100) ~", rhs)), wm),
  `Fracionário sem BH` = frac_logit(f(rhs), filter(wm, cod6 != 310620)),
  `Fracionário - pagamentos até 2022` = frac_logit(f(rhs, "trf_2022"), filter(wm, !is.na(trf_2022))),
  `Fracionário - TRF 2020-2022` = frac_logit(f(rhs, "trf_recente"), filter(wm, !is.na(trf_recente))),
  `Fracionário - repasses atualizados pelo IPCA` = frac_logit(f(rhs, "trf_ipca"), filter(wm, !is.na(trf_ipca))),
  `Fracionário - incluindo saldos de pares sem pagamento` = frac_logit(f(rhs, "trf_com_sem_pag"), filter(wm, !is.na(trf_com_sem_pag))),
  `Fracionário - TNR (não repasse)` = frac_logit(f(rhs, "tnr"), wm))
t5 <- tab_modelos(rob)
# guarda os resultados dos modelos (coeficientes, erros e efeitos marginais) para as figuras do script 10
leve <- function(x) x[intersect(c("coef", "ame", "n", "pseudo_r2", "r2"), names(x))]
saveRDS(lapply(c(m, rob), leve), file.path(BASES, "modelos_municipais.rds"))

# ---------------------------------------------------------------------
# 5b. TRF e saldo per capita por macrorregião de saúde (PDR-SUS/MG 2023) - metodologia 7.5 (a)
# ---------------------------------------------------------------------
t6 <- w |> group_by(`Macrorregião` = macro) |>
  summarise(`Municípios` = n(), `TRF - mediana (%)` = median(trf) * 100,
            `TRF - agregada (%)` = sum(saldo_fin_a) / sum(recebido) * 100,
            `Saldo fin. por hab. - mediana (R$)` = median(saldo_fin / pop_media),
            `TNR - mediana (%)` = median(tnr) * 100) |> arrange(desc(`TRF - mediana (%)`))
kw_mac <- kruskal.test(trf ~ factor(macro), data = w)
kw_mac_pc <- kruskal.test(I(saldo_fin / pop_media) ~ factor(macro), data = w)
kw_mac_tnr <- kruskal.test(tnr ~ factor(macro), data = w)
t6b <- tibble(Teste = c("Kruskal-Wallis: TRF entre macrorregiões", "Kruskal-Wallis: saldo fin. por hab. entre macrorregiões",
                        "Kruskal-Wallis: TNR entre macrorregiões"),
              H = unname(c(kw_mac$statistic, kw_mac_pc$statistic, kw_mac_tnr$statistic)),
              gl = unname(c(kw_mac$parameter, kw_mac_pc$parameter, kw_mac_tnr$parameter)),
              `p-valor` = c(kw_mac$p.value, kw_mac_pc$p.value, kw_mac_tnr$p.value))
# a macrorregião muda os resultados do modelo? (controle por macrorregião na especificação completa)
m_mac <- frac_logit(f(paste(CAP, "+", FIS, "+", CTR, "+ faixa + factor(macro)")), wm)
t6c <- tab_modelos(list(`M3d - sem macrorregião` = m[[4]], `M3d - com macrorregião` = m_mac)) |>
  filter(!str_starts(`Variável`, "factor\\(macro"))
idx <- grep("factor\\(macro", names(coef(m_mac$modelo)))
Vm <- vcovHC(m_mac$modelo, type = "HC1")[idx, idx]; bm <- coef(m_mac$modelo)[idx]
t6d <- tibble(Teste = "Teste de Wald conjunto das macrorregiões (modelo completo)", gl = length(idx),
              `p-valor` = pchisq(as.numeric(t(bm) %*% solve(Vm) %*% bm), df = length(idx), lower.tail = FALSE),
              `Pseudo-R² sem macro` = m[[4]]$pseudo_r2, `Pseudo-R² com macro` = m_mac$pseudo_r2)

grava_excel(list(T1_ligacao_formA_pagtos = t1, T1b_ligacao_indicadores = t1b, T2_validacao_TRF = t2,
                 T2b_repasses_x_siops_ano = anual, T3_TRF_por_faixa = t3, T3b_pares = t3b, T3c_testes = t3c, T3d_contraste_H1b = t3d,
                 T4_modelos_TRF = t4, T5_robustez = t5, T6_macrorregiao = t6, T6b_testes_macro = t6b,
                 T6c_modelo_com_macro = t6c, T6d_wald_macro = t6d,
                 Base_TRF = v |> select(cod6, nome, faixa, pop_media, recebido, recebido_ate_2022, recebido_2020_2022,
                                        recebido_ipca, saldo_fin_a, saldo_fin_todos, saldo_const_a, saldo_fin_2020_2022_a, trf_bruta, trf, trf_2022,
                                        trf_com_sem_pag, trf_ipca, trf_recente, tnr, trf_siops,
                                        repasse_hab_ano_nominal, repasse_hab_ano, pib_pc, macro)), "parte5_trf.xlsx")

# ---------------------------------------------------------------------
# 6. Figuras
# ---------------------------------------------------------------------
salva_fig(ggplot(w |> mutate(faixa = factor(faixa, levels = FAIXAS)), aes(faixa, trf * 100)) +
            geom_boxplot(fill = "#cfe0f6", color = AZUL, outlier.shape = NA, width = 0.55) +
            coord_cartesian(ylim = c(0, 100)) + labs(x = NULL, y = "TRF (%)") + tema_tcc, "fig8_trf_por_faixa.png")
f10 <- anual |> select(Ano, `Repasses SES-MG` = `Repasses SES - todos os instrumentos (R$ mi)`,
                       `SIOPS (estimativa)` = `Transferências estaduais estimadas - SIOPS (R$ mi)`) |>
  pivot_longer(-Ano, names_to = "Fonte", values_to = "v")
salva_fig(ggplot(f10, aes(factor(Ano), v / 1e3, fill = Fonte)) + geom_col(position = position_dodge(0.8), width = 0.75) +
            scale_fill_manual(values = c(AZUL, LARANJA)) + labs(x = NULL, y = "R$ bilhões") + tema_tcc,
          "fig10_repasses_x_siops.png")

print(t1); print(t1b); print(t2, n = 20); print(anual); print(t3); print(t3c); print(t4, n = 40); print(t5, n = 40)
message("Script 05 concluído.")

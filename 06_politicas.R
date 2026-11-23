# =====================================================================
# Script 06 - Represamento por política de saúde e desenho do instrumento (H2)
#             e sua variação por porte (segunda parte da pergunta de pesquisa)
# Entradas : saida/bases/formA_limpo.rds, pares_mun_instr.rds, instrumentos.rds, base_trf.rds
# Saídas   : saida/tabelas/parte6_politicas.xlsx
#            saida/figuras/fig11_trf_por_politica.png, fig12_trf_politica_x_porte.png
#
# Unidade de análise: par município x instrumento (instrumentos de 2016-2022 com pagamento
#   até 2023, nos municípios que declararam saldos). Pares que não aparecem no Formulário A
#   têm saldo financeiro zero (o município executou tudo ou não tinha sobra a declarar).
# Política = subfunção de maior valor no instrumento (301 Atenção primária; 302 Média e alta
#   complexidade; 303 Assistência farmacêutica; 304/305 Vigilância em saúde; demais = Outras).
# Modalidade = emenda parlamentar / enfrentamento à covid-19 / regular (pela unidade de gasto).
# =====================================================================
source("scripts/00_config.R")

A   <- readRDS(file.path(BASES, "formA_limpo.rds"))
pr  <- readRDS(file.path(BASES, "pares_mun_instr.rds"))
ins <- readRDS(file.path(BASES, "instrumentos.rds"))
v   <- readRDS(file.path(BASES, "base_trf.rds"))

MOT <- c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação")
sa <- A |> mutate(instr_id = paste0(as.integer(str_match(INSTRUMENTO_COD, "^0*(\\d+)/")[, 2]), "/", ANO)) |>
  group_by(cod6, instr_id) |>
  summarise(saldo_fin = sum(SALDO_FIN), saldo_const = sum(SALDO_CONST), fin_cap = sum(FIN_CAP),
            v_obj = sum(TOTAL[MOTIVO == MOT[1]]), v_imp = sum(TOTAL[MOTIVO == MOT[2]]), v_des = sum(TOTAL[MOTIVO == MOT[3]])) |>
  ungroup()

# ---------------------------------------------------------------------
# 1. Base de pares
# ---------------------------------------------------------------------
pares <- pr |> filter(cod6 %in% v$cod6, pago_ate_2023 > 0) |>
  left_join(sa, by = c("cod6", "instr_id")) |>
  mutate(across(c(saldo_fin, saldo_const, fin_cap, v_obj, v_imp, v_des), ~ coalesce(.x, 0))) |>
  left_join(ins |> select(instr_id, politica, modalidade, investimento), by = "instr_id") |>
  left_join(v |> select(cod6, faixa, serv_mil, estat_pct, comiss_pct, precario_pct, gestor_saude_superior,
                        fundo_gerido_pela_sms, colegiado_regional, ifgf_autonomia, ifgf_pessoal, ifgf_investimentos,
                        siops_rec_propria_asps, siops_dep_transf, siops_desp_saude_hab), by = "cod6") |>
  mutate(trf_par = pmin(saldo_fin / pago_ate_2023, 1),
         com_saldo = saldo_fin > 0,
         objeto = if_else(investimento, "Investimento", "Custeio e outros"),
         periodo = if_else(iano <= 2019, "2016-2019", "2020-2022"))
saveRDS(pares, file.path(BASES, "pares_trf.rds"))

# Pares do Formulário A sem pagamento (saldo constante puro) entram só na taxa de não repasse
sem_pag <- sa |> filter(cod6 %in% v$cod6) |> anti_join(pares, by = c("cod6", "instr_id")) |>
  left_join(ins |> select(instr_id, politica, modalidade, investimento), by = "instr_id") |>
  mutate(politica = coalesce(politica, "Sem pagamento registrado"), modalidade = coalesce(modalidade, "Sem pagamento registrado"),
         objeto = case_when(is.na(investimento) ~ "Sem pagamento registrado", investimento ~ "Investimento", TRUE ~ "Custeio e outros"),
         periodo = if_else(as.integer(str_sub(instr_id, -4)) <= 2019, "2016-2019", "2020-2022"))

resumo <- function(chave) {
  a <- pares |> group_by(Grupo = .data[[chave]]) |>
    summarise(Instrumentos = n_distinct(instr_id), Pares = n(),
              `Recebido (R$ mi)` = sum(pago_ate_2023) / 1e6, `Saldo financeiro (R$ mi)` = sum(saldo_fin) / 1e6,
              `TRF agregada (%)` = sum(saldo_fin) / sum(pago_ate_2023) * 100,
              `TRF por par - média (%)` = mean(trf_par) * 100,
              `Pares com saldo (%)` = mean(com_saldo) * 100,
              `Capital no saldo financeiro (%)` = sum(fin_cap) / sum(saldo_fin) * 100,
              const_pago = sum(saldo_const))
  b <- sem_pag |> group_by(Grupo = .data[[chave]]) |> summarise(const_sem = sum(saldo_const))
  a |> full_join(b, by = "Grupo") |>
    mutate(across(c(const_pago, const_sem), ~ coalesce(.x, 0)),
           `Saldo constante (R$ mi)` = (const_pago + const_sem) / 1e6,
           `Taxa de não repasse (%)` = (const_pago + const_sem) / (coalesce(`Recebido (R$ mi)`, 0) * 1e6 + const_pago + const_sem) * 100) |>
    select(-const_pago, -const_sem) |> arrange(desc(`Recebido (R$ mi)`))
}
t1 <- resumo("politica"); t2 <- resumo("modalidade"); t3 <- resumo("objeto"); t3b <- resumo("periodo")

# motivo declarado por política (% do valor declarado)
t4 <- pares |> bind_rows(sem_pag) |> group_by(`Política` = politica) |>
  summarise(o = sum(v_obj), i = sum(v_imp), s = sum(v_des)) |>
  mutate(t = o + i + s, `Objeto cumprido (%)` = o / t * 100, `Impossibilidade material (%)` = i / t * 100,
         `Desnecessidade da ação (%)` = s / t * 100, `Valor declarado (R$ mi)` = t / 1e6) |>
  select(`Política`, ends_with("(%)"), `Valor declarado (R$ mi)`) |> filter(`Valor declarado (R$ mi)` > 0)
tab_mot <- pares |> filter(com_saldo) |> mutate(mot = case_when(v_imp >= pmax(v_obj, v_des) ~ "imp", v_des >= v_obj ~ "des", TRUE ~ "obj"))
cv_mot <- cramer_v(table(tab_mot$politica, tab_mot$mot))

# ---------------------------------------------------------------------
# 2. Política x porte: TRF agregada (%) - a variação conjunta pedida na pergunta de pesquisa
# ---------------------------------------------------------------------
t5 <- pares |> group_by(`Política` = politica, faixa) |> summarise(trf = sum(saldo_fin) / sum(pago_ate_2023) * 100) |>
  pivot_wider(names_from = faixa, values_from = trf) |> ungroup() |> select(`Política`, any_of(FAIXAS))
t5b <- pares |> group_by(Modalidade = modalidade, faixa) |> summarise(trf = sum(saldo_fin) / sum(pago_ate_2023) * 100) |>
  pivot_wider(names_from = faixa, values_from = trf) |> ungroup() |> select(Modalidade, any_of(FAIXAS))
t5c <- pares |> group_by(Objeto = objeto, faixa) |> summarise(trf = sum(saldo_fin) / sum(pago_ate_2023) * 100) |>
  pivot_wider(names_from = faixa, values_from = trf) |> ungroup() |> select(Objeto, any_of(FAIXAS))
kw_pol <- kruskal.test(trf_par ~ factor(politica), data = pares)

# ---------------------------------------------------------------------
# 3. Modelo fracionário no nível do par (erros agrupados por município)
#    TRF do par ~ política + modalidade + objeto + ano do instrumento + porte + capacidade + fiscal
# ---------------------------------------------------------------------
pm <- pares |> mutate(politica = relevel(factor(politica), ref = "Média e alta complexidade"),
                      modalidade = relevel(factor(modalidade), ref = "Regular"),
                      investimento = as.integer(investimento),
                      ano_instr = factor(iano),
                      faixa = relevel(factor(faixa, levels = FAIXAS), ref = "20 a 50 mil"),
                      x_serv10 = serv_mil / 10, x_estat10 = estat_pct / 10, x_comiss10 = comiss_pct / 10,
                      x_prec10 = precario_pct / 10, x_auton = ifgf_autonomia * 10, x_pessoal = ifgf_pessoal * 10,
                      x_invest = ifgf_investimentos * 10, x_rec_propria = siops_rec_propria_asps / 10,
                      x_dep_transf = siops_dep_transf / 10, x_desp_hab_log = log(siops_desp_saude_hab),
                      x_valor_log = log(pago_ate_2023)) |>
  filter(if_all(c(x_serv10, x_estat10, x_comiss10, x_prec10, x_auton, x_pessoal, x_invest, x_rec_propria,
                  x_dep_transf, x_desp_hab_log), ~ !is.na(.x) & is.finite(.x)))
DES <- "politica + modalidade + investimento + x_valor_log + ano_instr"
CAP <- "x_serv10 + x_estat10 + x_comiss10 + x_prec10 + gestor_saude_superior + fundo_gerido_pela_sms + colegiado_regional + x_auton + x_pessoal + x_invest"
FIS <- "x_rec_propria + x_dep_transf + x_desp_hab_log"
# Erros-padrão com agrupamento DUPLO (município e resolução): as variáveis de desenho são
# iguais para todos os municípios que receberam a mesma resolução, e as resoluções recebidas
# por um mesmo município compartilham a mesma gestão (CAMERON; GELBACH; MILLER, 2011).
CL2 <- c("cod6", "instr_id")
mods <- list(
  `M4a - desenho do instrumento` = frac_logit(as.formula(paste("trf_par ~", DES)), pm, cluster = CL2),
  `M4b - desenho + porte` = frac_logit(as.formula(paste("trf_par ~", DES, "+ faixa")), pm, cluster = CL2),
  `M4c - completo` = frac_logit(as.formula(paste("trf_par ~", DES, "+ faixa +", CAP, "+", FIS)), pm, cluster = CL2))

# ---------------------------------------------------------------------
# 3b. Proposição de aversão ao risco: o efeito do objeto de investimento depende da
#     capacidade fiscal do município? (interação investimento x IFGF Autonomia)
#     Leitura: se municípios com menor autonomia represam MAIS nos investimentos, o resultado
#     é compatível com a ideia de que gestores com menos folga evitam assumir despesas de capital.
# ---------------------------------------------------------------------
m_int <- frac_logit(as.formula(paste("trf_par ~", DES, "+ investimento:x_auton + faixa +", CAP, "+", FIS)), pm, cluster = CL2)
efeito_inv <- function(sub) {
  # efeito marginal médio do objeto de investimento (p.p.) dentro de um subgrupo de municípios
  d1 <- sub; d1$investimento <- 1L; d0 <- sub; d0$investimento <- 0L
  mean(predict(m_int$modelo, newdata = d1, type = "response") - predict(m_int$modelo, newdata = d0, type = "response")) * 100
}
pm_int <- pm[complete.cases(pm[, c(all.vars(m_int$modelo$formula), CL2)]), ]
q <- quantile(pm_int$x_auton, c(.25, .75))
t9 <- tibble(Indicador = c("Coeficiente da interação investimento x IFGF Autonomia (escala logit)",
                           "p-valor da interação (agrupamento duplo)",
                           "Efeito do investimento (p.p.) - municípios no 1º quartil de autonomia",
                           "Efeito do investimento (p.p.) - municípios no 4º quartil de autonomia",
                           "Efeito do investimento (p.p.) - todos"),
             Valor = c(m_int$coef["investimento:x_auton", 1], m_int$coef["investimento:x_auton", 4],
                       efeito_inv(pm_int[pm_int$x_auton <= q[1], ]), efeito_inv(pm_int[pm_int$x_auton >= q[2], ]),
                       efeito_inv(pm_int)))
pares_cap <- tibble(Indicador = c("Pares com saldo financeiro maior que o recebido (TRF limitada a 100%)", "Pares no modelo"),
                    Valor = c(sum(pares$saldo_fin > pares$pago_ate_2023), nrow(pm)))
ROT_EXTRA <- c(`politicaAtenção primária` = "Política: atenção primária", `politicaAssistência farmacêutica` = "Política: assistência farmacêutica",
               `politicaVigilância em saúde` = "Política: vigilância em saúde", politicaOutras = "Política: outras",
               `modalidadeEmenda parlamentar` = "Modalidade: emenda parlamentar",
               `modalidadeEnfrentamento à covid-19` = "Modalidade: covid-19", investimento = "Objeto de investimento",
               x_valor_log = "log do valor recebido no instrumento")
ROTULOS <- c(ROTULOS, ROT_EXTRA)
t6 <- tab_modelos(mods)
t6 <- t6 |> filter(!str_starts(`Variável`, "ano_instr"))   # efeitos de ano ficam no modelo, mas fora da tabela
# comparação: modelo completo com agrupamento só por município (versão anterior)
mods_cl1 <- list(`M4c - agrupado por município` = frac_logit(as.formula(paste("trf_par ~", DES, "+ faixa +", CAP, "+", FIS)), pm, cluster = "cod6"),
                 `M4c - agrupado por município e resolução` = mods[[3]])
t6b <- tab_modelos(mods_cl1) |> filter(!str_starts(`Variável`, "ano_instr"))
leve <- function(x) x[intersect(c("coef", "ame", "n", "pseudo_r2"), names(x))]
saveRDS(lapply(c(mods, mods_cl1), leve), file.path(BASES, "modelos_pares.rds"))

# Pseudo-R² comparado: quanto o desenho do instrumento explica em relação aos atributos municipais
m_mun <- frac_logit(as.formula(paste("trf_par ~ faixa +", CAP, "+", FIS)), pm, cluster = CL2)
t7 <- tibble(`Especificação` = c("Só atributos municipais (porte, capacidade, fiscal)", "Só desenho do instrumento",
                                 "Desenho + atributos municipais"),
             `Pseudo-R²` = c(m_mun$pseudo_r2, mods[[1]]$pseudo_r2, mods[[3]]$pseudo_r2),
             N = c(m_mun$n, mods[[1]]$n, mods[[3]]$n))

testes <- tibble(Teste = c("Kruskal-Wallis: TRF do par entre políticas", "Qui-quadrado: motivo predominante x política"),
                 `Estatística` = c(unname(kw_pol$statistic), cv_mot$qui2), `p-valor` = c(kw_pol$p.value, cv_mot$p),
                 `V de Cramér` = c(NA, cv_mot$v))

# instrumentos com maior saldo financeiro (para escolher as resoluções da análise documental)
t8 <- pares |> bind_rows(sem_pag) |> group_by(instr_id) |>
  summarise(`Saldo financeiro (R$ mi)` = sum(saldo_fin) / 1e6, `Saldo constante (R$ mi)` = sum(saldo_const) / 1e6,
            `Recebido (R$ mi)` = sum(pago_ate_2023, na.rm = TRUE) / 1e6, `Municípios com saldo` = sum(saldo_fin + saldo_const > 0)) |>
  mutate(`TRF agregada (%)` = if_else(`Recebido (R$ mi)` > 0, `Saldo financeiro (R$ mi)` / `Recebido (R$ mi)` * 100, NA_real_)) |>
  left_join(ins |> select(instr_id, politica, modalidade, investimento, acao, unidade), by = "instr_id") |>
  arrange(desc(`Saldo financeiro (R$ mi)` + `Saldo constante (R$ mi)`)) |> head(40)

grava_excel(list(T1_por_politica = t1, T2_por_modalidade = t2, T3_por_objeto = t3, T3b_por_periodo = t3b,
                 T4_motivo_por_politica = t4, T5_TRF_politica_x_porte = t5, T5b_modalidade_x_porte = t5b,
                 T5c_objeto_x_porte = t5c, T6_modelos_par = t6, T6b_agrupamento_simples_x_duplo = t6b,
                 T7_poder_explicativo = t7, T9_aversao_risco_interacao = t9, T10_pares_limitados = pares_cap, Testes = testes,
                 T8_top40_instrumentos = t8), "parte6_politicas.xlsx")

# ---------------------------------------------------------------------
# 4. Figuras
# ---------------------------------------------------------------------
f11 <- t1 |> filter(!is.na(`TRF agregada (%)`))
salva_fig(ggplot(f11, aes(`TRF agregada (%)`, reorder(Grupo, `TRF agregada (%)`))) + geom_col(fill = AZUL, width = 0.6) +
            geom_text(aes(label = paste0(formatC(`TRF agregada (%)`, format = "f", digits = 1, decimal.mark = ","), "%")),
                      hjust = -0.15, size = 3, family = "serif") +
            scale_x_continuous(expand = expansion(mult = c(0, 0.15))) + labs(x = "TRF agregada (%)", y = NULL) + tema_tcc,
          "fig11_trf_por_politica.png", alt = 7)
f12 <- t5 |> filter(`Política` != "Outras") |> pivot_longer(-`Política`, names_to = "Faixa", values_to = "trf") |>
  mutate(Faixa = factor(Faixa, levels = FAIXAS))
salva_fig(ggplot(f12, aes(Faixa, trf, color = `Política`, group = `Política`)) + geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
            scale_color_manual(values = c(AZUL, LARANJA, VERDE, "#8a5cc2")) + labs(x = NULL, y = "TRF agregada (%)") +
            tema_tcc + guides(color = guide_legend(nrow = 2)), "fig12_trf_politica_x_porte.png", alt = 10)

print(t1); print(t2); print(t3); print(t3b); print(t4); print(t5); print(t5b); print(t5c); print(t6, n = 50); print(t7); print(testes)
message("Script 06 concluído.")

# =====================================================================
# Script 10 - Figuras da seção de resultados quantitativos
# Entradas : saídas dos scripts 01 a 08 e dados/mapa/mg_municipios.json
#            (malha municipal de MG em GeoJSON, repositório tbrugz/geodata-br, a partir da malha do IBGE)
# Saídas   : saida/figuras/fig_s8_*.png (16 cm de largura, 600 dpi, gravadas com o pacote ragg)
#            e, opcionalmente, a mesma figura em PDF vetorial (GRAVA_PDF = TRUE)
#            (o prefixo fig_s8 foi mantido da versão anterior do texto)
# Requer   : sf, ggforce, ggrepel, ragg, systemfonts, forcats, scales (instala se faltar)
# A figura do mês do empenho (item 4) só é gerada se o script 08 já tiver sido rodado.
# =====================================================================
source("scripts/00_config.R")
for (p in c("sf", "ggforce", "ggrepel", "ragg", "systemfonts", "forcats", "tibble", "scales")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages(library(sf))

v   <- readRDS(file.path(BASES, "base_trf.rds")) |> filter(!is.na(trf))
ba  <- readRDS(file.path(BASES, "base_analitica.rds"))
pr  <- readRDS(file.path(BASES, "pares_trf.rds"))
ins <- readRDS(file.path(BASES, "instrumentos.rds"))
mm  <- readRDS(file.path(BASES, "modelos_municipais.rds"))
mp  <- readRDS(file.path(BASES, "modelos_pares.rds"))

# ---------------------------------------------------------------------
# 0. Padrão visual
# ---------------------------------------------------------------------
# Família da fonte. "serif" = Times New Roman (igual ao texto); "sans" = Arial.
# Fontes sem serifa têm traço de espessura uniforme e continuam nítidas quando a imagem é reduzida
# (a NBR 14724 não exige que o texto das figuras use a mesma fonte do corpo do trabalho).
FAMILIA <- Sys.getenv("FAMILIA_FIG", "serif")
fontes <- unique(systemfonts::system_fonts()$family)
opcoes <- if (FAMILIA == "sans") c("Arial", "Liberation Sans", "sans") else c("Times New Roman", "Liberation Serif", "serif")
FONTE  <- opcoes[opcoes %in% c(fontes, "serif", "sans")][1]

# Tintas (texto): todas com contraste alto sobre branco
TINTA  <- "#1f1f1d"   # texto principal e rótulos de valor
TINTA2 <- "#3a3935"   # títulos de eixo e textos secundários (contraste 11:1)
GRADE  <- "#dcdbd6"   # linhas de grade
EIXO   <- "#6f6e69"   # linha de base
REF    <- "#6b6a64"   # marcas de referência (cinza escuro, legível)
FUNDO_DESTAQUE <- "#f1f0ec"
# Paleta categórica (validada para daltonismo, na ordem de uso)
AZUL    <- "#1f5fa8"; AZUL_ESC <- "#123f75"; AZUL_CLARO <- "#a9c6ea"; AZUL_FUNDO <- "#e6eef8"
LARANJA <- "#e07b29"; LARANJA_TXT <- "#a4510f"   # laranja escuro para texto
VERDE   <- "#26a69a"; VINHO <- "#a23b72"

DPI <- 600
GRAVA_PDF <- FALSE    # TRUE grava também um PDF vetorial de cada figura (nitidez máxima no Word/LaTeX)

virg <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
pct  <- function(x, d = 0) paste0(virg(x, d), "%")
pp_sinal <- function(x, d = 1) paste0(ifelse(x > 0, "+", ifelse(x < 0, "\u2212", "")), virg(abs(x), d))

TXT  <- 3.5     # rótulos de valor (cerca de 10 pt)
TXTP <- 3.15    # rótulos secundários (cerca de 9 pt)

# Corpo mínimo de 9 pt em todo o texto; rótulos de eixo em tinta quase preta
tema <- theme_minimal(base_size = 11, base_family = FONTE) +
  theme(text = element_text(color = TINTA),
        axis.title = element_text(size = 10, color = TINTA2),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.text = element_text(size = 10, color = TINTA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = GRADE, linewidth = 0.4),
        axis.line.x = element_line(color = EIXO, linewidth = 0.5),
        legend.position = "top", legend.justification = "left",
        legend.title = element_blank(), legend.text = element_text(size = 10, color = TINTA),
        legend.key.size = unit(0.4, "cm"), legend.margin = margin(0, 0, 2, 0),
        strip.text = element_text(size = 10.5, face = "bold", color = TINTA, hjust = 0),
        plot.margin = margin(6, 12, 6, 6))
tema_h <- tema + theme(panel.grid.major.y = element_blank(),
                       panel.grid.major.x = element_line(color = GRADE, linewidth = 0.4),
                       axis.line.x = element_blank(),
                       axis.text.y = element_text(size = 10, color = TINTA))

salva <- function(g, nome, alt) {
  ggsave(file.path(FIG, nome), g, device = ragg::agg_png, width = 16, height = alt, units = "cm",
         dpi = DPI, bg = "white")
  if (GRAVA_PDF) ggsave(file.path(FIG, sub("\\.png$", ".pdf", nome)), g, device = cairo_pdf,
                        width = 16, height = alt, units = "cm", bg = "white")
}

# ---------------------------------------------------------------------
# 1. Distribuição da TRF municipal
# ---------------------------------------------------------------------
q <- quantile(v$trf, c(.25, .5, .75)) * 100
g <- ggplot(v, aes(trf * 100)) +
  annotate("rect", xmin = q[1], xmax = q[3], ymin = -Inf, ymax = Inf, fill = AZUL_FUNDO) +
  geom_histogram(binwidth = 2.5, boundary = 0, fill = AZUL, color = "white", linewidth = 0.35) +
  geom_vline(xintercept = q[2], color = TINTA, linewidth = 0.6) +
  geom_vline(xintercept = q[c(1, 3)], color = AZUL_ESC, linewidth = 0.35, linetype = "22") +
  annotate("text", x = q[2] + 0.7, y = 86, hjust = 0, vjust = 1, label = paste0("Mediana:\n", pct(q[2], 1)),
           family = FONTE, size = TXT, fontface = "bold", color = TINTA, lineheight = 0.9) +
  annotate("text", x = q[3] + 1.2, y = 66, hjust = 0, size = TXTP + 0.1, family = FONTE, color = TINTA2, lineheight = 0.95,
           label = paste0("Faixa sombreada: metade central\ndos municípios (", pct(q[1], 1), " a ", pct(q[3], 1), ")")) +
  scale_x_continuous(breaks = seq(0, 90, 5), expand = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0, 80, 10), expand = expansion(mult = c(0, 0)), limits = c(0, 88)) +
  coord_cartesian(xlim = c(0, 90), clip = "off") +
  labs(x = "Taxa de represamento financeiro - TRF (%)", y = "Número de municípios") + tema
salva(g, "fig_s8_histograma_trf.png", 8)

# ---------------------------------------------------------------------
# 2. Mapas: TRF e TNR medianas por macrorregião de saúde
# ---------------------------------------------------------------------
sf_use_s2(FALSE)
mun <- st_read(file.path(DADOS, "mapa", "mg_municipios.json"), quiet = TRUE) |>
  mutate(cod6 = as.integer(id) %/% 10) |> left_join(ba |> select(cod6, macro), by = "cod6")
stopifnot(sum(is.na(mun$macro)) == 0)
macro_geo <- suppressMessages(mun |> group_by(macro) |> summarise(geometry = st_union(geometry), .groups = "drop"))
contorno  <- suppressMessages(st_union(macro_geo))
ind <- v |> group_by(macro) |> summarise(TRF = median(trf) * 100, TNR = median(tnr) * 100)
PAINEL <- c(TRF = "A) Taxa de represamento financeiro (TRF)", TNR = "B) Taxa de não repasse (TNR)")
CORTES <- c(0, 5, 10, 15, 20, 25, 30)
CORES_MAPA <- c("#eef3fa", "#c9dbf1", "#93b7e2", "#5b8fcc", "#2c64a8", "#163f73")
mapa <- macro_geo |> left_join(ind, by = "macro") |>
  tidyr::pivot_longer(c(TRF, TNR), names_to = "ind", values_to = "valor") |>
  mutate(painel = factor(PAINEL[ind], levels = PAINEL),
         classe = cut(valor, CORTES, include.lowest = TRUE, right = FALSE,
                      labels = paste0(head(CORTES, -1), " a ", CORTES[-1], "%")))

pontos  <- suppressWarnings(st_point_on_surface(macro_geo))
coords  <- st_coordinates(pontos)
centros <- data.frame(macro = pontos$macro, x = coords[, 1], y = coords[, 2])
# Posição dos rótulos (graus). Áreas pequenas ou estreitas recebem rótulo fora do mapa, com linha-guia.
EXT <- tibble::tribble(~macro, ~lx, ~ly,
               "Leste", -39.75, -18.55, "Vale do Aço", -39.75, -19.45, "Leste do Sul", -39.75, -20.35,
               "Sudeste", -40.75, -21.75, "Extremo Sul", -48.55, -22.35)
AJ <- tibble::tribble(~macro, ~dx, ~dy,
              "Jequitinhonha", 0.05, 0.25, "Centro Sul", 0.00, 0.05, "Sul", 0.00, 0.05,
              "Triângulo do Sul", 0.10, 0.05, "Triângulo do Norte", -0.10, 0.20, "Oeste", -0.05, 0.05,
              "Nordeste", 0.10, 0.00, "Centro", 0.00, 0.10)
rot <- mapa |> st_drop_geometry() |> left_join(centros, by = "macro") |> left_join(AJ, by = "macro") |>
  left_join(EXT, by = "macro") |>
  mutate(x = x + coalesce(dx, 0), y = y + coalesce(dy, 0), externo = !is.na(lx),
         tx = coalesce(lx, x), ty = coalesce(ly, y), hj = if_else(externo, if_else(lx < x, 1, 0), 0.5),
         nome = macro)
halo <- function(...) ggrepel::geom_text_repel(..., family = FONTE, color = TINTA, bg.colour = "white", bg.r = 0.18,
                                               force = 0, force_pull = 0, max.iter = 0, max.overlaps = Inf,
                                               min.segment.length = Inf, seed = 1)

g <- ggplot(mapa) +
  geom_sf(aes(fill = classe), color = "white", linewidth = 0.45) +
  geom_sf(data = contorno, fill = NA, color = TINTA2, linewidth = 0.4, inherit.aes = FALSE) +
  geom_segment(data = filter(rot, externo), aes(x = x, y = y, xend = tx - if_else(hj == 0, 0.08, -0.08), yend = ty),
               color = TINTA2, linewidth = 0.3) +
  geom_point(data = filter(rot, externo), aes(x, y), size = 0.9, color = TINTA) +
  halo(data = rot, aes(tx, ty + 0.07, label = nome, hjust = hj), size = 2.85, vjust = 0) +
  halo(data = rot, aes(tx, ty - 0.05, label = pct(valor, 1), hjust = hj), size = 3.3, fontface = "bold", vjust = 1) +
  scale_fill_manual(values = setNames(CORES_MAPA, levels(mapa$classe)), drop = FALSE,
                    name = "Mediana municipal", guide = guide_legend(nrow = 1, label.position = "bottom",
                                                                     keywidth = unit(1.55, "cm"), keyheight = unit(0.3, "cm"))) +
  facet_wrap(~ painel, ncol = 1) +
  coord_sf(datum = NA, xlim = c(-51.1, -38.2), clip = "off") +
  theme_void(base_size = 10.5, base_family = FONTE) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10, color = TINTA, vjust = 0.9),
        legend.text = element_text(size = 9.5, color = TINTA),
        legend.spacing.x = unit(0, "cm"),
        strip.text = element_text(size = 11, face = "bold", color = TINTA, hjust = 0, margin = margin(t = 6, b = 4)),
        panel.spacing = unit(0.5, "cm"), plot.margin = margin(4, 4, 4, 4))
salva(g, "fig_s8_mapa_macrorregioes.png", 20)

# 2b. Alternativa ao mapa: as duas taxas lado a lado, por macrorregião (todas rotuladas)
ind2 <- ind |> mutate(macro = forcats::fct_reorder(macro, TRF))
indl <- ind2 |> tidyr::pivot_longer(c(TRF, TNR), names_to = "taxa", values_to = "valor") |>
  mutate(taxa = factor(taxa, levels = c("TRF", "TNR"),
                       labels = c("TRF: parado no município", "TNR: não repassado pelo Estado")))
g <- ggplot(indl, aes(valor, macro)) +
  geom_col(aes(fill = taxa), width = 0.68) +
  geom_text(aes(label = pct(valor, 1)), hjust = -0.15, size = TXTP + 0.1, family = FONTE, color = TINTA) +
  facet_wrap(~ taxa, nrow = 1) +
  scale_fill_manual(values = c(AZUL, LARANJA), guide = "none") +
  scale_x_continuous(limits = c(0, 33), breaks = seq(0, 30, 10), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Mediana municipal", y = NULL) + tema_h +
  theme(panel.spacing.x = unit(0.8, "cm"), strip.text = element_text(margin = margin(b = 4)))
salva(g, "fig_s8_macro_barras.png", 11)

# ---------------------------------------------------------------------
# 3. Pagamentos da SES-MG aos fundos municipais: composição dos pagamentos de cada ano (2017-2022)
#    O que a figura mede: de tudo o que foi pago no ano, quanto veio de empenhos do próprio ano e
#    quanto veio de restos a pagar de anos anteriores. NÃO é a parcela do empenhado paga no ano
#    (essa medida está na Tabela 6 e na figura do mês do empenho).
#    Por que começa em 2017: o recorte considera os empenhos feitos a partir de 2016, início do
#    período analisado. Os restos a pagar desses empenhos só começam a ser pagos em 2017
#    (RP_2016_2025.xlsx = restos a pagar inscritos de 2016 a 2025, pagos de 2017 a 2026).
#    Os restos a pagar pagos em 2016 viriam de empenhos de 2015 ou antes, fora do recorte,
#    e não constam da base; com eles ausentes, 2016 apareceria com 100% pago no próprio ano.
#    Barras de 100%: a parte paga como restos a pagar fica junto à base, para facilitar a
#    comparação entre os anos. Acima de cada barra, o total pago no ano (valores nominais).
# ---------------------------------------------------------------------
ANO_INI_PG <- ANO_INI + 1   # 2017: primeiro ano com pagamento de restos a pagar dos empenhos do recorte
pg <- read_excel(file.path(TAB, "parte2_repasses.xlsx"), sheet = "T2_pagos_por_ano") |>
  filter(`Ano de pagamento` >= ANO_INI_PG, `Ano de pagamento` <= ANO_FIM) |>
  transmute(ano = `Ano de pagamento`, total = `Total (R$ mi)` / 1e3,
            `Restos a pagar` = `% restos a pagar`, `No próprio ano` = 100 - `% restos a pagar`)
pgl <- pg |> tidyr::pivot_longer(c(`No próprio ano`, `Restos a pagar`), names_to = "forma", values_to = "p") |>
  mutate(forma = factor(forma, levels = c("No próprio ano", "Restos a pagar")))
g <- ggplot(pgl, aes(ano, p, fill = forma)) +
  annotate("rect", xmin = 2016.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = FUNDO_DESTAQUE) +
  annotate("text", x = 2018, y = 117, label = "Crise fiscal", size = TXTP + 0.1, family = FONTE,
           color = TINTA2, fontface = "italic") +
  geom_col(width = 0.66, color = "white", linewidth = 0.5) +
  geom_text(data = filter(pgl, p >= 5), aes(label = pct(p, 0), color = forma),
            position = position_stack(vjust = 0.5), size = TXT, family = FONTE, fontface = "bold", show.legend = FALSE) +
  geom_text(data = pg, aes(ano, 100, label = paste0("R$ ", virg(total, 1), " bi")), inherit.aes = FALSE,
            vjust = -0.6, size = TXTP, family = FONTE, color = TINTA2) +
  scale_fill_manual(values = c(`No próprio ano` = AZUL, `Restos a pagar` = LARANJA),
                    labels = c("Empenhos do próprio ano", "Restos a pagar de anos anteriores")) +
  scale_color_manual(values = c(`No próprio ano` = "white", `Restos a pagar` = TINTA)) +
  scale_x_continuous(breaks = ANO_INI_PG:ANO_FIM, expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(0, 121), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Ano do pagamento", y = "Composição do valor pago no ano") + tema +
  guides(fill = guide_legend(reverse = FALSE))
salva(g, "fig_s8_pagamentos_ano.png", 8.5)

# ---------------------------------------------------------------------
# 4. H3: parcela paga no próprio ano conforme o mês do empenho (rótulos diretos)
# ---------------------------------------------------------------------
arq8 <- file.path(TAB, "parte8_empenho_mensal.xlsx")
if (file.exists(arq8)) {
  t2  <- read_excel(arq8, sheet = "T2_pago_por_mes_empenho")
  cal <- read_excel(arq8, sheet = "T1_calendario_fms_x_ses")
  SER <- c("Demais despesas\nda SES-MG", "Repasses empenhados\nde janeiro a setembro", "Repasses empenhados\nem novembro e dezembro")
  h3 <- bind_rows(
    cal |> filter(Grupo == "Demais despesas da SES-MG") |> transmute(Ano, Serie = SER[1], v = `% do empenhado líquido pago no exercício`),
    t2 |> filter(`Mês do empenho` == "Janeiro a setembro") |> transmute(Ano, Serie = SER[2], v = `% pago no exercício`),
    t2 |> filter(`Mês do empenho` %in% c("Novembro", "Dezembro")) |> group_by(Ano) |>
      summarise(v = sum(`% pago no exercício` * `Empenhado (R$ mi)`) / sum(`Empenhado (R$ mi)`)) |> mutate(Serie = SER[3])) |>
    mutate(Serie = factor(Serie, levels = SER))
  fim <- h3 |> filter(Ano == ANO_FIM) |> mutate(v = case_when(Serie == SER[1] ~ 86, Serie == SER[2] ~ 101, TRUE ~ 72))
  CORES_H3 <- c(REF, AZUL, LARANJA); CORES_TXT <- c(TINTA2, AZUL, LARANJA_TXT)
  g <- ggplot(h3, aes(Ano, v, color = Serie)) +
    annotate("rect", xmin = 2015.6, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = FUNDO_DESTAQUE) +
    annotate("text", x = 2017.55, y = 106, label = "Crise fiscal", size = TXTP + 0.1, family = FONTE,
             color = TINTA2, fontface = "italic") +
    geom_line(linewidth = 1) + geom_point(size = 2.3, stroke = 0.9, fill = "white", shape = 21) +
    # rótulos das séries em tom mais escuro que o das linhas, para o texto ter bom contraste
    geom_text(data = fim, aes(x = ANO_FIM + 0.15, y = v, label = Serie), inherit.aes = FALSE, hjust = 0, size = TXT,
              family = FONTE, lineheight = 0.9, fontface = "bold", color = CORES_TXT[as.integer(fim$Serie)]) +
    scale_color_manual(values = setNames(CORES_H3, SER), guide = "none") +
    scale_x_continuous(breaks = ANO_INI:ANO_FIM, expand = expansion(add = c(0.3, 0.2))) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(limits = c(0, 109), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "Parcela do empenhado paga no próprio ano") + tema +
    theme(plot.margin = margin(6, 135, 6, 6))
  salva(g, "fig_s8_h3_mes_empenho.png", 8.5)
}

# ---------------------------------------------------------------------
# 5. TRF por faixa de porte (caixas com mediana rotulada)
# ---------------------------------------------------------------------
FX <- setNames(c("Até\n5 mil", "5 a\n10 mil", "10 a\n20 mil", "20 a\n50 mil", "50 a\n100 mil", "Acima de\n100 mil"), FAIXAS)
vf <- v |> mutate(faixa = factor(FX[as.character(faixa)], levels = FX))
medf <- vf |> group_by(faixa) |> summarise(m = median(trf) * 100, n = n())
g <- ggplot(vf, aes(faixa, trf * 100)) +
  geom_boxplot(fill = "#dbe7f6", color = AZUL_ESC, width = 0.55, outlier.shape = NA, linewidth = 0.45) +
  geom_segment(data = medf, aes(x = as.integer(faixa) - 0.275, xend = as.integer(faixa) + 0.275, y = m, yend = m),
               inherit.aes = FALSE, color = AZUL_ESC, linewidth = 1.1) +
  geom_text(data = medf, aes(faixa, 63, label = pct(m, 1)), size = TXT, family = FONTE, color = TINTA, fontface = "bold") +
  geom_text(data = medf, aes(faixa, -4, label = paste0(n, " municípios")), size = TXTP - 0.1, family = FONTE, color = TINTA2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 60, 10)) +
  coord_cartesian(ylim = c(-5, 65), clip = "off") +
  labs(x = "Faixa de porte populacional (habitantes)", y = "Taxa de represamento financeiro") + tema
salva(g, "fig_s8_trf_faixa.png", 8)

# ---------------------------------------------------------------------
# 6. Gráficos de coeficientes (efeitos marginais médios com intervalo de 95%)
#    Ponto cheio = diferente de zero (p < 0,05); ponto vazado = não se distingue de zero.
#    Todos os efeitos rotulados com o valor em p.p.
# ---------------------------------------------------------------------
ef <- function(mod) {
  ct <- mod$coef; nm <- rownames(ct)
  tibble(var = nm, coef = ct[, 1], se = ct[, 2], p = ct[, 4], ame = unname(mod$ame[nm])) |>
    filter(var != "(Intercept)", !str_starts(var, "ano_instr")) |>
    mutate(esc = if_else(coef != 0, ame / coef, 0), se_ame = abs(esc) * se,
           dobro = str_detect(var, "_log$"),
           ame = if_else(dobro, ame * log(2), ame), se_ame = if_else(dobro, se_ame * log(2), se_ame),
           lo = ame - 1.96 * se_ame, hi = ame + 1.96 * se_ame)
}
coefplot <- function(d, alt, nome, xlim, dodge = 0) {
  # d precisa de: rot, grupo, ame, lo, hi, sig (TRUE/FALSE), serie (fator; a última série é a principal e recebe rótulos)
  multi <- nlevels(d$serie) > 1
  principal <- last(levels(d$serie))
  cores <- if (multi) setNames(c(REF, AZUL), levels(d$serie)) else setNames(AZUL, levels(d$serie))
  pos <- position_dodge(width = dodge)
  d <- d |> mutate(rotulo = ifelse(serie == principal, pp_sinal(ame), ""),
                   preench = ifelse(sig, as.character(serie), "vazio"))
  g <- ggplot(d, aes(ame, rot, color = serie, group = serie)) +
    geom_vline(xintercept = 0, color = EIXO, linewidth = 0.5) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.8, position = pos, show.legend = FALSE) +
    geom_point(aes(fill = preench), shape = 21, size = 2.5, stroke = 0.9, position = pos) +
    ggrepel::geom_text_repel(aes(x = hi + diff(xlim) * 0.012, label = rotulo), position = pos, hjust = 0, size = TXTP,
                             family = FONTE, color = TINTA, bg.colour = "white", bg.r = 0.15, force = 0, force_pull = 0,
                             max.iter = 0, max.overlaps = Inf, min.segment.length = Inf, point.size = NA,
                             show.legend = FALSE, seed = 1) +
    ggforce::facet_col(vars(grupo), scales = "free_y", space = "free", strip.position = "top") +
    scale_color_manual(values = cores, breaks = rev(levels(d$serie)), name = NULL) +
    # legenda do preenchimento: ponto cheio x vazado, sempre em azul
    scale_fill_manual(values = c(cores, vazio = "white"), breaks = c(principal, "vazio"),
                      labels = c("Diferente de zero (p < 0,05)", "Não se distingue de zero"), name = NULL) +
    scale_x_continuous(labels = function(x) pp_sinal(x, 0), limits = xlim, breaks = scales::breaks_pretty(8)) +
    labs(x = "Variação da TRF esperada (pontos percentuais)", y = NULL) + tema_h +
    theme(strip.text = element_text(hjust = 0, size = 10, face = "bold", color = TINTA, margin = margin(t = 5, b = 2)),
          legend.justification = "right", legend.box = "vertical", legend.box.just = "right",
          legend.spacing.y = unit(0, "cm"), panel.spacing.y = unit(0.15, "lines")) +
    guides(fill = guide_legend(order = 2, nrow = 1, override.aes = list(color = AZUL, shape = 21, size = 2.5, stroke = 0.9)),
           color = if (multi) guide_legend(order = 1, nrow = 1, override.aes = list(fill = rev(unname(cores)), shape = 21, size = 2.5)) else "none")
  salva(g, nome, alt)
}
ROT <- c(`faixaAté 5 mil` = "Até 5 mil hab.", `faixa5 a 10 mil` = "5 a 10 mil hab.", `faixa10 a 20 mil` = "10 a 20 mil hab.",
         `faixa50 a 100 mil` = "50 a 100 mil hab.", `faixaAcima de 100 mil` = "Acima de 100 mil hab.",
         x_serv10 = "Servidores por mil hab. (+10)", x_estat10 = "% estatutários (+10 p.p.)",
         x_comiss10 = "% só comissionados (+10 p.p.)", x_prec10 = "% estagiários e sem vínculo (+10 p.p.)",
         gestor_saude_superior = "Gestor da saúde com nível superior", fundo_gerido_pela_sms = "Fundo gerido pela secretaria",
         colegiado_regional = "Participa do colegiado regional", x_auton = "IFGF Autonomia (+0,1)",
         x_pessoal = "IFGF Gastos com Pessoal (+0,1)", x_invest = "IFGF Investimentos (+0,1)",
         x_rec_propria = "% recursos próprios em saúde (+10 p.p.)", x_dep_transf = "% transferências do SUS na despesa (+10 p.p.)",
         x_desp_hab_log = "Despesa em saúde por hab. (dobro)", x_repasse_hab_log = "Repasse estadual por hab. (dobro)")
GRP <- c(rep("Porte (ref.: 20 a 50 mil)", 5), rep("Capacidade técnico-administrativa", 4), rep("Capacidade político-relacional", 3),
         rep("Capacidade fiscal (IFGF)", 3), rep("Perfil fiscal da saúde (SIOPS)", 3), "Controle")
names(GRP) <- names(ROT)
# a parcela recebida em 2021-2022 (controle de maturação) fica no modelo, mas não no gráfico:
# seu intervalo muito largo comprimiria a escala das demais variáveis
d <- ef(mm[["M3d - + volume e maturação"]]) |> filter(var %in% names(ROT)) |>
  mutate(rot = factor(ROT[var], levels = rev(ROT)), grupo = factor(GRP[var], levels = unique(GRP)),
         sig = p < 0.05, serie = factor("Modelo"))
coefplot(d, 16, "fig_s8_coef_municipal.png", xlim = c(-9, 13))

ROTP <- c(`politicaVigilância em saúde` = "Vigilância em saúde", `politicaOutras` = "Outras áreas",
          `politicaAtenção primária` = "Atenção primária", `politicaAssistência farmacêutica` = "Assistência farmacêutica",
          `modalidadeEnfrentamento à covid-19` = "Enfrentamento à covid-19", `modalidadeEmenda parlamentar` = "Emenda parlamentar",
          investimento = "Investimento")
GRPP <- c(rep("Área de política (ref.: média e alta complexidade)", 4), rep("Modalidade (ref.: repasse regular)", 2),
          "Objeto (ref.: custeio)")
names(GRPP) <- names(ROTP)
SERIES <- c("Erros agrupados só por município", "Erros agrupados por município e resolução (adotado)")
dp <- bind_rows(ef(mp[["M4c - agrupado por município"]]) |> mutate(serie = SERIES[1]),
                ef(mp[["M4c - agrupado por município e resolução"]]) |> mutate(serie = SERIES[2])) |>
  filter(var %in% names(ROTP)) |>
  mutate(rot = factor(ROTP[var], levels = rev(ROTP)), grupo = factor(GRPP[var], levels = unique(GRPP)),
         serie = factor(serie, levels = SERIES), sig = p < 0.05)
coefplot(dp, 11, "fig_s8_coef_pares.png", xlim = c(-17, 27), dodge = 0.8)

# ---------------------------------------------------------------------
# 7. TRF por subfunção, comparada à proporção calculada pela SES-MG (2020-2023)
# ---------------------------------------------------------------------
ses <- c(`304 VIGILANCIA SANITARIA` = 53.25, `305 VIGILANCIA EPIDEMIOLOGICA` = 37.17,
         `242 ASSISTENCIA AO PORTADOR DE DEFICIENCIA` = 25.22, `301 ATENCAO BASICA` = 12.98,
         `303 SUPORTE PROFILATICO E TERAPEUTICO` = 7.19, `302 ASSISTENCIA HOSPITALAR E AMBULATORIAL` = 7.31)
nomes <- c(`304 VIGILANCIA SANITARIA` = "Vigilância sanitária", `305 VIGILANCIA EPIDEMIOLOGICA` = "Vigilância epidemiológica",
           `242 ASSISTENCIA AO PORTADOR DE DEFICIENCIA` = "Pessoa com deficiência", `301 ATENCAO BASICA` = "Atenção básica",
           `303 SUPORTE PROFILATICO E TERAPEUTICO` = "Assistência farmacêutica", `302 ASSISTENCIA HOSPITALAR E AMBULATORIAL` = "Média e alta complexidade")
sub <- pr |> left_join(select(ins, instr_id, subfuncao), by = "instr_id") |> filter(subfuncao %in% names(ses)) |>
  group_by(subfuncao) |> summarise(trf = sum(saldo_fin) / sum(pago_ate_2023) * 100) |>
  mutate(ses = ses[subfuncao], nome = factor(nomes[subfuncao], levels = rev(nomes)))
g <- ggplot(sub, aes(y = nome)) +
  geom_col(aes(x = trf, fill = "Esta pesquisa (2016-2022)"), width = 0.64) +
  geom_point(aes(x = ses, shape = "Cálculo da SES-MG (2020-2023)"), size = 2.6, color = TINTA, fill = "white", stroke = 0.9) +
  geom_text(aes(x = pmax(trf, ses) + 1.6, label = pct(trf, 1)), hjust = 0, size = TXT, fontface = "bold",
            family = FONTE, color = TINTA) +
  scale_fill_manual(values = AZUL, name = NULL) + scale_shape_manual(values = 21, name = NULL) +
  scale_x_continuous(limits = c(0, 62), breaks = seq(0, 60, 10), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Saldo financeiro / valor recebido", y = NULL) + tema_h +
  theme(legend.box = "horizontal", legend.spacing.x = unit(0.15, "cm"), legend.box.spacing = unit(0, "cm"))
salva(g, "fig_s8_trf_subfuncao.png", 7.5)

# ---------------------------------------------------------------------
# 8. TRF por área de política e faixa de porte
#    Mapa de calor: linhas = políticas, colunas = faixas de porte, cor = TRF, com o valor em cada célula.
#    Lê-se nas duas direções: a linha da vigilância se destaca de todas as outras (a política pesa mais)
#    e, dentro de cada linha, a cor clareia dos menores para os maiores municípios (o porte pesa menos).
# ---------------------------------------------------------------------
POL <- c("Vigilância em saúde", "Atenção primária", "Assistência farmacêutica", "Média e alta complexidade")
FXC <- setNames(c("Até\n5 mil", "5 a\n10 mil", "10 a\n20 mil", "20 a\n50 mil", "50 a\n100 mil", "Acima de\n100 mil"), FAIXAS)
pp <- read_excel(file.path(TAB, "parte6_politicas.xlsx"), sheet = "T5_TRF_politica_x_porte") |>
  filter(`Política` %in% POL) |> tidyr::pivot_longer(-`Política`, names_to = "faixa", values_to = "trf") |>
  mutate(faixa = factor(FXC[faixa], levels = FXC), `Política` = factor(`Política`, levels = rev(POL)))
ESCALA_CALOR <- c("#eef3fa", "#c9dbf1", "#93b7e2", "#5b8fcc", "#2c64a8", "#163f73")
g <- ggplot(pp, aes(faixa, `Política`, fill = trf)) +
  geom_tile(color = "white", linewidth = 1.6) +
  geom_text(aes(label = pct(trf, 1), color = trf > 27), size = TXT + 0.1, family = FONTE, fontface = "bold") +
  scale_fill_gradientn(colors = ESCALA_CALOR, limits = c(0, 52), breaks = seq(0, 50, 10),
                       guide = "none") +
  scale_color_manual(values = c(`FALSE` = TINTA, `TRUE` = "white"), guide = "none") +
  scale_x_discrete(position = "top", expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Faixa de porte populacional (habitantes)", y = NULL) + tema +
  theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank(),
        axis.text.y = element_text(size = 10, color = TINTA), axis.text.x.top = element_text(size = 10, color = TINTA, lineheight = 0.9),
        axis.title.x.top = element_text(margin = margin(b = 6)),
        legend.position = "bottom", legend.justification = "right", legend.title = element_text(size = 9, color = TINTA2),
        legend.key.width = unit(1.6, "cm"), legend.key.height = unit(0.25, "cm"))
salva(g, "fig_s8_politica_porte.png", 8)

# 8b. Alternativa: barras agrupadas por política, uma barra por faixa (do menor ao maior porte, do mais escuro ao mais claro)
ppb <- pp |> mutate(`Política` = factor(`Política`, levels = rev(POL)), faixa = factor(faixa, levels = rev(FXC)))
TONS <- rev(c("#163f73", "#2c64a8", "#4f86c6", "#7fa8d8", "#adc8e8", "#d5e3f4"))
g <- ggplot(ppb, aes(trf, `Política`, fill = faixa)) +
  geom_col(position = position_dodge(width = 0.86), width = 0.82, color = "white", linewidth = 0.25) +
  geom_text(aes(label = pct(trf, 1)), position = position_dodge(width = 0.86), hjust = -0.15, size = TXTP - 0.15,
            family = FONTE, color = TINTA) +
  scale_fill_manual(values = setNames(TONS, levels(ppb$faixa)), breaks = FXC,
                    labels = str_replace(str_replace(FXC, "\n", " "), "Acima de ", "> "), name = NULL) +
  scale_x_continuous(limits = c(0, 56), breaks = seq(0, 50, 10), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Taxa de represamento financeiro", y = NULL) + tema_h +
  theme(legend.key.size = unit(0.3, "cm"), legend.spacing.x = unit(0.1, "cm")) +
  theme(axis.text.y = element_text(size = 10, color = TINTA), legend.text = element_text(size = 9.5))
salva(g, "fig_s8_politica_porte_barras.png", 12)

# ---------------------------------------------------------------------
# 9. Motivos declarados por tipo de saldo e por período
# ---------------------------------------------------------------------
MOT <- c("Objeto cumprido", "Impossibilidade material", "Desnecessidade da ação")
mot <- read_excel(file.path(TAB, "parte1_formularios.xlsx"), sheet = "T4_motivos") |>
  filter(Recorte %in% c("Constante", "Financeiro", "2016-2019", "2020-2022")) |>
  mutate(bloco = if_else(Recorte %in% c("Constante", "Financeiro"), "Por tipo de saldo", "Por ano do instrumento"),
         Recorte = recode(Recorte, Constante = "Saldo constante", Financeiro = "Saldo financeiro",
                          `2016-2019` = "2016 a 2019", `2020-2022` = "2020 a 2022")) |>
  select(bloco, Recorte, all_of(MOT)) |>
  tidyr::pivot_longer(all_of(MOT), names_to = "Motivo", values_to = "pct") |>
  mutate(Motivo = factor(Motivo, levels = rev(MOT)),
         bloco = factor(bloco, levels = c("Por tipo de saldo", "Por ano do instrumento")),
         Recorte = factor(Recorte, levels = rev(c("Saldo constante", "Saldo financeiro", "2016 a 2019", "2020 a 2022"))))
g <- ggplot(mot, aes(pct, Recorte, fill = Motivo)) +
  geom_col(width = 0.66, color = "white", linewidth = 0.7) +
  geom_text(aes(label = pct(pct, 0), color = Motivo), position = position_stack(vjust = 0.5),
            size = TXT, family = FONTE, fontface = "bold", show.legend = FALSE) +
  ggforce::facet_col(vars(bloco), scales = "free_y", space = "free") +
  scale_fill_manual(values = c(`Objeto cumprido` = AZUL_CLARO, `Impossibilidade material` = LARANJA,
                               `Desnecessidade da ação` = AZUL), breaks = MOT) +
  scale_color_manual(values = c(`Objeto cumprido` = TINTA, `Impossibilidade material` = TINTA, `Desnecessidade da ação` = "white")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Parcela do valor declarado", y = NULL) + tema_h +
  theme(plot.margin = margin(6, 14, 6, 6), panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 10, margin = margin(t = 4, b = 2)), panel.spacing.y = unit(0.2, "lines"))
salva(g, "fig_s8_motivos.png", 7.5)

# ---------------------------------------------------------------------
# 10. Destino pretendido dos saldos (Formulário B)
# ---------------------------------------------------------------------
de <- read_excel(file.path(TAB, "parte1_formularios.xlsx"), sheet = "T6_destino_inciso") |>
  filter(`% do valor` >= 0.5) |>
  mutate(acao = paste0(`Ação (art. 3º da LC 141/2012)`, " (", INCISO, ")"), acao = factor(acao, levels = rev(acao)),
         foco = case_when(INCISO == "X" ~ "a", INCISO == "I" ~ "c", TRUE ~ "b"))
g <- ggplot(de, aes(`% do valor`, acao, fill = foco)) +
  geom_col(width = 0.66) +
  geom_text(aes(label = pct(`% do valor`, 1)), hjust = -0.15, size = TXT, family = FONTE, fontface = "bold", color = TINTA) +
  scale_fill_manual(values = c(a = AZUL, b = AZUL_CLARO, c = LARANJA), guide = "none") +
  scale_x_continuous(limits = c(0, 42), breaks = seq(0, 40, 10), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Parcela do valor replanejado", y = NULL) + tema_h
salva(g, "fig_s8_destino.png", 7)

message("Script 10 concluído. Fonte usada: ", FONTE)

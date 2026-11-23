## =============================================================================
# TCC - Recursos represados no SUS: federalismo, capacidade estatal e a baixa
#       execução financeira em Minas Gerais
#
# Figura 1 - Distribuição dos municípios mineiros por faixa populacional, com
#            os limites das macrorregiões de saúde definidas pelo PDR 2023
#
# Entradas (pasta "dados"):
#   - macrorregioes_pdr2023.xlsx : município -> macrorregião (PDR-SUS/MG 2023,
#                                  Deliberação CIB-SUS/MG nº 4.394/2023)
#   - tabela4709.xlsx            : população residente, Censo Demográfico 2022
#                                  (IBGE/SIDRA, Tabela 4709)
# Malha municipal: IBGE 2022, obtida pelo pacote geobr; se o geobr não estiver
#                  disponível, usa dados/mapa/mg_municipios.json (a mesma do
#                  script 10) e, na falta dele, baixa o GeoJSON do tbrugz/geodata-br.
# Saída: pasta "saida/figuras" (PNG em 300 dpi)
# Roda sozinho: carrega a configuração (pastas relativas à raiz do projeto).
# =============================================================================
source("scripts/00_config.R")

# ---- 0. Pacotes -------------------------------------------------------------
# geobr é opcional (há alternativa); os demais são necessários
for (p in c("sf", "ggrepel")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
if (!requireNamespace("geobr", quietly = TRUE)) try(install.packages("geobr"), silent = TRUE)
suppressPackageStartupMessages({
  library(sf); library(ggrepel)
})
sf_use_s2(FALSE)   # evita erros de geometria ao dissolver polígonos

# ---- 1. Caminhos ------------------------------------------------------------
dir_dados <- DADOS
dir_saida <- FIG

arq_pdr <- file.path(dir_dados, "macrorregioes_pdr2023.xlsx")
arq_pop <- file.path(dir_dados, "tabela4709.xlsx")

# ---- 2. Função auxiliar: padroniza nomes para o cruzamento ------------------
# Remove acentos, apóstrofos, hífens e espaços ("Olhos-d'Água" -> "olhosdagua")
padroniza <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  gsub("[^a-z]", "", x)
}

# Grafias divergentes entre as fontes (chave padronizada -> grafia IBGE)
correcoes <- c(
  "baraodemontealto"  = "baraodomontealto",   # PDR 2023 x IBGE
  "saothomedasletras" = "saotomedasletras",
  "brasopolis"        = "brazopolis",
  "gouvea"            = "gouveia",
  "donaeusebia"       = "donaeuzebia"
)
corrige <- function(chave) ifelse(chave %in% names(correcoes),
                                  correcoes[chave], chave)

# ---- 3. Macrorregiões de saúde (PDR 2023) -----------------------------------
pdr <- read_excel(arq_pdr, sheet = 1,   # aba "Municípios (Lista Completa)"
                  skip = 3, col_types = "text")
names(pdr)[1:4] <- c("n", "macro", "micro", "municipio")

pdr <- pdr |>
  filter(!is.na(municipio), grepl("^[0-9]+$", n),
         !grepl("^Total", macro)) |>
  transmute(macro, municipio, chave = corrige(padroniza(municipio)))

stopifnot(nrow(pdr) == 853, n_distinct(pdr$macro) == 16)

# ---- 4. População - Censo 2022 ----------------------------------------------
pop <- read_excel(arq_pop, sheet = 1, col_names = FALSE, col_types = "text")
names(pop)[1:2] <- c("local", "pop")

pop <- pop |>
  filter(grepl("\\(MG\\)$", local)) |>
  transmute(municipio_ibge = sub("\\s*\\(MG\\)$", "", local),
            pop   = as.numeric(pop),
            chave = corrige(padroniza(municipio_ibge)))

stopifnot(nrow(pop) == 853)

# ---- 5. Malha municipal de MG -----------------------------------------------
obter_malha <- function() {
  if (requireNamespace("geobr", quietly = TRUE)) {
    m <- tryCatch(
      geobr::read_municipality(code_muni = "MG", year = 2022,
                               showProgress = FALSE),
      error = function(e) NULL)
    if (!is.null(m)) {
      return(m |> transmute(nome_malha = name_muni))
    }
  }
  # Alternativa: malha municipal em GeoJSON (repositório tbrugz/geodata-br),
  # primeiro a cópia local usada no script 10; se não houver, baixa da internet
  message("geobr indisponível; usando a malha alternativa (GeoJSON).")
  local <- file.path(dir_dados, "mapa", "mg_municipios.json")
  fonte <- if (file.exists(local)) local else
    paste0("https://raw.githubusercontent.com/tbrugz/geodata-br/",
           "master/geojson/geojs-31-mun.json")
  st_read(fonte, quiet = TRUE) |> transmute(nome_malha = name)
}

mun <- obter_malha() |>
  st_make_valid() |>
  mutate(chave = corrige(padroniza(nome_malha)))

# ---- 6. Cruzamento das três bases -------------------------------------------
mapa <- mun |>
  left_join(pdr, by = "chave") |>
  left_join(pop |> select(chave, pop), by = "chave")

# Diagnóstico: todos os 853 municípios precisam ter macro e população
sem_macro <- mapa$nome_malha[is.na(mapa$macro)]
sem_pop   <- mapa$nome_malha[is.na(mapa$pop)]
if (length(sem_macro) > 0 || length(sem_pop) > 0) {
  message("Sem macrorregião: ", paste(sem_macro, collapse = "; "))
  message("Sem população:    ", paste(sem_pop,   collapse = "; "))
  stop("Há municípios não pareados. Acrescente a grafia ao vetor 'correcoes'.")
}

# ---- 7. Faixas de porte (as mesmas usadas nas análises do TCC) --------------
rotulos <- c("Até 5 mil", "5 a 10 mil", "10 a 20 mil",
             "20 a 50 mil", "50 a 100 mil", "Acima de 100 mil")

mapa <- mapa |>
  mutate(faixa = cut(pop,
                     breaks = c(0, 5000, 10000, 20000, 50000, 100000, Inf),
                     labels = rotulos, right = TRUE))

contagem <- table(mapa$faixa)
rotulos_legenda <- sprintf("%s  (%d)", names(contagem), as.integer(contagem))
print(contagem)

# ---- 8. Projeção e limites das macrorregiões --------------------------------
# SIRGAS 2000 / Brazil Polyconic (EPSG:5880): projeção métrica, permite a
# escala gráfica em km
mapa <- st_transform(mapa, 5880)

# Dissolve os municípios de cada macro. A malha simplificada deixa pequenas frestas
# entre municípios vizinhos; sem tratamento, a união desenharia o contorno de cada
# município. Por isso: expande 300 m, une, contrai 300 m e mantém só o anel externo
# de cada parte (descartando lascas menores que 1 km²).
sem_buracos <- function(g) {
  st_sfc(lapply(g, function(x) {
    partes <- lapply(x, function(p) p[1])
    area <- vapply(partes, function(p) as.numeric(st_area(st_polygon(p))), numeric(1))
    st_multipolygon(partes[area >= 1e6])
  }), crs = st_crs(g))
}
macros <- mapa |>
  st_buffer(300) |>
  group_by(macro) |>
  summarise(.groups = "drop") |>
  st_buffer(-300) |>
  st_cast("MULTIPOLYGON")
st_geometry(macros) <- sem_buracos(st_geometry(macros))

contorno_mg <- st_union(macros)

# Ponto interno de cada macrorregião para o rótulo
# (não depende do nome da coluna de geometria: "geom" no geobr,
#  "geometry" em outras malhas)
pontos_macro <- st_point_on_surface(macros)
coords_macro <- st_coordinates(pontos_macro)
rot_macro <- data.frame(macro = pontos_macro$macro,
                        x = coords_macro[, 1],
                        y = coords_macro[, 2])

# ---- 9. Elementos cartográficos (sem dependências extras) -------------------
bb <- st_bbox(contorno_mg)
larg <- bb["xmax"] - bb["xmin"]
alt  <- bb["ymax"] - bb["ymin"]

# Escala gráfica: 0-100-200 km, no canto inferior direito
x0 <- bb["xmax"] - 0.30 * larg
y0 <- bb["ymin"] + 0.02 * alt
seg <- 100000
escala <- data.frame(xmin = unname(x0 + c(0, seg)),
                     xmax = unname(x0 + c(seg, 2 * seg)),
                     ymin = unname(y0), ymax = unname(y0 + 0.012 * alt))
escala_txt <- data.frame(x = unname(x0 + c(0, seg, 2 * seg)),
                         y = unname(y0 + 0.032 * alt),
                         lab = c("0", "100", "200 km"))

# Seta norte, no canto superior direito
xn <- bb["xmax"] - 0.04 * larg
yn <- bb["ymax"] - 0.10 * alt
seta_esq <- data.frame(x = c(xn, xn - 0.018 * larg, xn),
                       y = c(yn + 0.07 * alt, yn, yn + 0.018 * alt))
seta_dir <- data.frame(x = c(xn, xn + 0.018 * larg, xn),
                       y = c(yn + 0.07 * alt, yn, yn + 0.018 * alt))

# ---- 10. Paleta sequencial (clara = menor porte; escura = maior porte) ------
paleta <- c("#fff3d6", "#fdd49e", "#f4a582", "#d6604d", "#a63253", "#5c1a4a")
names(paleta) <- rotulos

# ---- 11. Mapa ---------------------------------------------------------------
fig <- ggplot() +
  # municípios, coloridos pelo porte
  geom_sf(data = mapa, aes(fill = faixa), colour = "white", linewidth = 0.06) +
  # limites das macrorregiões
  geom_sf(data = macros, fill = NA, colour = "#1f2d3d", linewidth = 0.55) +
  # contorno do estado
  geom_sf(data = contorno_mg, fill = NA, colour = "#1f2d3d", linewidth = 0.9) +
  # nomes das macrorregiões, com halo branco
  geom_text_repel(data = rot_macro, aes(x = x, y = y, label = macro),
                  size = 3.1, fontface = "bold", colour = "#1f2d3d",
                  bg.colour = "white", bg.r = 0.15,
                  force = 0.4, min.segment.length = Inf,
                  seed = 2023) +
  # escala gráfica
  geom_rect(data = escala[1, ], aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax),
            fill = "#2b2b2b", colour = "#2b2b2b", linewidth = 0.3) +
  geom_rect(data = escala[2, ], aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax),
            fill = "white", colour = "#2b2b2b", linewidth = 0.3) +
  geom_text(data = escala_txt, aes(x = x, y = y, label = lab),
            size = 2.6, colour = "#2b2b2b") +
  # seta norte
  geom_polygon(data = seta_esq, aes(x = x, y = y),
               fill = "#2b2b2b", colour = "#2b2b2b", linewidth = 0.3) +
  geom_polygon(data = seta_dir, aes(x = x, y = y),
               fill = "white", colour = "#2b2b2b", linewidth = 0.3) +
  annotate("text", x = xn, y = yn + 0.095 * alt, label = "N",
           fontface = "bold", size = 3.6, colour = "#2b2b2b") +
  scale_fill_manual(values = paleta, labels = rotulos_legenda, drop = FALSE,
                    name = "Porte populacional\n(habitantes, Censo 2022)") +
  coord_sf(crs = 5880, datum = NA, expand = FALSE,
           xlim = c(bb["xmin"] - 0.01 * larg, bb["xmax"] + 0.01 * larg),
           ylim = c(bb["ymin"] - 0.01 * alt,  bb["ymax"] + 0.01 * alt)) +
  guides(fill = guide_legend(override.aes = list(colour = "grey40",
                                                 linewidth = 0.2))) +
  theme_void(base_size = 11) +
  theme(
    legend.position      = "right",
    legend.justification = c(0, 0.5),
    legend.title         = element_text(face = "bold", size = 9.5,
                                        lineheight = 1.1),
    legend.text          = element_text(size = 8.8),
    legend.key.size      = unit(0.48, "cm"),
    legend.margin        = margin(0, 0, 0, 8),
    plot.background      = element_rect(fill = "white", colour = NA),
    plot.margin          = margin(5, 5, 5, 5)
  )

# Nota: o título ("Figura 1 - ...") e a fonte ficam no corpo do TCC, conforme a
# ABNT, e não dentro da imagem.

# ---- 12. Exportação ---------------------------------------------------------
ggsave(file.path(dir_saida, "figura1_porte_macrorregioes_pdr2023.png"),
       fig, width = 18, height = 12.5, units = "cm", dpi = 300, bg = "white")

message("Figura salva em: ", dir_saida)
if (interactive()) print(fig)   # fora do RStudio (Rscript), não gera Rplots.pdf
# ------------------------------------------------------------------
# Script 01b - Figura 2: distribuição dos municípios segundo o
# número de resoluções da SES-MG com saldo declarado (LC nº 171/2023)
# Entrada : dados/Formulario_A.xlsx (lê o arquivo bruto; não depende do script 01)
# Saída   : saida/figuras/figura_2_resolucoes_por_municipio.png
# Roda sozinho: carrega a configuração.
# ------------------------------------------------------------------

source("scripts/00_config.R")

# 1. Leitura (mesmo padrão dos demais scripts) ------------------------
A <- read_excel(file.path(DADOS, "Formulario_A.xlsx"), sheet = 1, col_types = "text") |>
  select(-any_of("ENTE_SENHA_REGISTRADA"))

# Como tudo é lido como texto, converte as colunas numéricas usadas aqui
A <- A |>
  mutate(
    ano         = suppressWarnings(as.numeric(INSTRUMENTO_ANO)),
    SALDO_FIN   = suppressWarnings(as.numeric(SALDO_FIN)),
    SALDO_CONST = suppressWarnings(as.numeric(SALDO_CONST))
  )

# 2. Tratamento -------------------------------------------------------
base <- A |>
  filter(
    ENTE_REGISTRO_VALIDO == "Sim",              # credencial validada
    INSTRUMENTO_ELEGIVEL == "Sim",              # instrumento elegível
    str_detect(ENTE_COD, "^31\\d{5}$")          # apenas municípios (IBGE-MG)
  ) |>
  distinct()                                    # remove duplicatas exatas

# Conferência: deve resultar 43.475 linhas, R$ 5,27 bi e 842 municípios
print(base |> summarise(
  linhas     = n(),
  total_bi   = sum(coalesce(SALDO_FIN, 0) + coalesce(SALDO_CONST, 0)) / 1e9,
  municipios = n_distinct(ENTE_COD)
))

# 3. Contagem de resoluções distintas por município (2016-2022) ------
contagem <- base |>
  filter(between(ano, 2016, 2022)) |>
  distinct(ENTE_COD, ENTE_NOME, INSTRUMENTO_COD) |>
  count(ENTE_COD, ENTE_NOME, name = "n_resolucoes")

# Estatísticas para o texto (esperado: 841 municípios, média 47,
# mediana 46, 31 com 100 ou mais, máximo 212)
print(contagem |>
  summarise(municipios  = n(),
            media       = mean(n_resolucoes),
            mediana     = median(n_resolucoes),
            p25         = quantile(n_resolucoes, 0.25),
            p75         = quantile(n_resolucoes, 0.75),
            ate_20      = sum(n_resolucoes <= 20),
            cem_ou_mais = sum(n_resolucoes >= 100),
            maximo      = max(n_resolucoes)))

print(contagem |> arrange(desc(n_resolucoes)) |> head(10))

# 4. Histograma ------------------------------------------------------
media_res   <- mean(contagem$n_resolucoes)
mediana_res <- median(contagem$n_resolucoes)

# Criação do gráfico com ajustes de legibilidade e cores
fig2 <- ggplot(contagem, aes(x = n_resolucoes)) +
  geom_histogram(binwidth = 10, boundary = 0,
                 fill = "#2B6CB0", color = "white") +
  geom_vline(xintercept = media_res, linetype = "dashed", color = "black", linewidth = 0.8) +
  # Anotação com tamanho maior e cor preta ("sans" = Arial no Windows; funciona em qualquer sistema)
  annotate("text", x = 95, y = 150, hjust = 0, vjust = 1, size = 5,
           color = "black", family = "sans",
           label = paste0("Linha tracejada: média (", round(media_res), ")\n",
                          "Mediana: ", round(mediana_res))) +
  # O eixo x é o número de resoluções por município (não a TRF)
  labs(x = "Número de resoluções com saldo declarado",
       y = "Número de municípios") +
  # base_size = 16 aumenta tudo proporcionalmente; base_family fixa a fonte
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    # Forçando todos os textos para preto
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    # Limpeza visual
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

if (interactive()) print(fig2)   # fora do RStudio (Rscript), não gera Rplots.pdf

# Salvando com dpi = 600 (nitidez máxima) e dimensões levemente maiores
ggsave(file.path(FIG, "figura_2_resolucoes_por_municipio.png"), fig2,
       width = 20, height = 12, units = "cm", dpi = 600, bg = "white")
message("Script 01b concluído.")
# =====================================================================
# Roda todos os scripts na ordem certa.
# Uso (com o projeto TCC_R.Rproj aberto no RStudio):
#     source("scripts/rodar_tudo.R")
# Cada script roda na sessão atual, como se fosse aberto pelo botão "Source";
# antes de cada um, os objetos do Environment são apagados, para que nenhum
# script dependa de algo deixado pelo anterior (só as bases gravadas em disco).
# ATENÇÃO: isso apaga os objetos que estiverem no seu Environment.
# Se um script falhar, a execução para e o erro é mostrado.
# =====================================================================
if (!dir.exists("scripts") || !dir.exists("dados"))
  stop("Rode a partir da pasta do projeto (abra o TCC_R.Rproj no RStudio).")
options(encoding = "UTF-8")   # os scripts estão gravados em UTF-8

# Os objetos de controle começam com ponto: ls() não os lista, então a limpeza não os apaga
.roda <- function(arq) {
  message("\n==================== ", arq, " ====================")
  rm(list = ls(envir = globalenv()), envir = globalenv())
  ok <- tryCatch({
    source(file.path("scripts", arq), local = globalenv(), encoding = "UTF-8")
    TRUE
  }, error = function(e) {
    message("\nERRO em ", arq, ":\n", conditionMessage(e))
    FALSE
  })
  if (!ok) stop("O script ", arq, " terminou com erro (mensagem acima).", call. = FALSE)
}

# 1. Formulários, repasses, porte, capacidade, TRF, políticas e complementos
for (.s in c("01_formularios.R", "01b_figura2_resolucoes.R", "02_repasses.R", "02b_participacao_fms_fes.R",
             "03_porte.R", "04_capacidade_siops.R", "05_trf.R", "06_politicas.R",
             "07_complementos.R", "07b_conferencia_periodos.R")) .roda(.s)

# 2. Planilhas mensais do SIAFI-MG: conversão para CSV (Python) e script 08
.mensal <- file.path("dados", "mensal")
.csv_mensais <- file.path(.mensal, c("exercicio_mensal_2016_2021.csv", "exercicio_mensal_2022_2026.csv",
                                     "rp_mensal_2016_2026.csv"))
if (length(list.files(.mensal, pattern = "^[^~].*\\.xlsx$")) > 0) {
  message("\n==================== 08a_converter_mensal_csv.py ====================")
  .py <- Sys.which(c("python", "py", "python3")); .py <- .py[.py != ""]
  .ok_py <- FALSE
  for (.p in .py) {
    .st <- system2(shQuote(.p), shQuote(file.path("scripts", "08a_converter_mensal_csv.py")))
    if (identical(as.integer(.st), 0L)) { .ok_py <- TRUE; break }
  }
  if (!.ok_py) warning("Não foi possível rodar o Python. Rode no Terminal: python scripts/08a_converter_mensal_csv.py",
                       call. = FALSE)
}
.tem_mensal <- all(file.exists(.csv_mensais))
if (.tem_mensal) .roda("08_empenho_mensal.R") else
  warning("Script 08 não rodado: faltam os CSV mensais em dados/mensal (ver README).", call. = FALSE)

# 3. Tabelas e figuras dos resultados
.roda("09_tabelas_secao9.R")
.roda("10_figuras_secao9.R")

# 4. Análise documental (precisa dos CSV mensais e das planilhas em dados/resolucoes)
if (.tem_mensal && dir.exists(file.path("dados", "resolucoes"))) .roda("12_tabelas_secao10.R") else
  warning("Script 12 não rodado: faltam os CSV mensais ou a pasta dados/resolucoes (ver README).", call. = FALSE)
.roda("13_tabela_resolucoes.R")

# 5. Mapa da Figura 1 (independe dos demais)
.roda("Figura1.R")

message("\nFluxo concluído. Tabelas em saida/tabelas, figuras em saida/figuras.")

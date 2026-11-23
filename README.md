# Recursos represados no SUS: scripts de tratamento e análise dos dados

Scripts do Trabalho de Conclusão de Curso "Recursos represados no SUS: federalismo, capacidade estatal municipal e baixa execução financeira dos repasses estaduais em Minas Gerais (2016-2022)", apresentado ao curso de Administração Pública da Escola de Governo Professor Paulo Neves de Carvalho, da Fundação João Pinheiro (FJP), em 2026.

**Autor:** Matheus Magalhães Avelar

## Sobre a pesquisa

A pesquisa investiga por que parte dos recursos repassados pela Secretaria de Estado de Saúde de Minas Gerais (SES-MG) aos fundos municipais de saúde permaneceu sem execução entre 2016 e 2022, e em que medida isso variou conforme o porte municipal e a política de saúde financiada.

A base empírica central são os saldos declarados pelos municípios na adesão à Lei Complementar Estadual nº 171/2023, ligados aos pagamentos da SES-MG pelo número de cada resolução. A partir dessa ligação, são calculados dois indicadores:

- **Taxa de represamento financeiro (TRF):** parcela do valor recebido pelo fundo municipal que permanecia em conta quando os saldos foram declarados (represamento no município).
- **Taxa de não repasse (TNR):** parcela do valor pactuado que o Estado ainda não havia pago nesse momento (represamento na origem).

As comparações entre grupos (faixas de porte, macrorregiões, áreas de política e períodos) são descritivas. As associações com características dos municípios e dos instrumentos de repasse são estimadas por modelos de resposta fracionária (Papke; Wooldridge, 1996), com erros-padrão robustos e, no modelo por combinação município-resolução, agrupados por município e por resolução.

## Estrutura do repositório

```
scripts-TCC/
├── TCC_R.Rproj          projeto do RStudio
├── README.md
├── LICENSE
├── scripts/             scripts de tratamento e análise
├── dados/               dados de entrada (não versionados, ver abaixo)
└── saida/               criada pelos scripts (não versionada)
    ├── tabelas/         tabelas em .xlsx
    ├── figuras/         figuras em .png
    └── bases/           bases intermediárias em .rds
```

Todos os caminhos são relativos à pasta do projeto: nenhum script aponta para uma pasta específica de computador. As pastas de saída são criadas automaticamente pelo `00_config.R`.

## Scripts

Os scripts são executados em sequência. Cada um lê as bases produzidas pelos anteriores e grava as suas em `saida/`. O `00_config.R` é carregado automaticamente pelos demais.

| Script | Conteúdo | Depende de |
|---|---|---|
| `00_config.R` | Pacotes, pastas, faixas de porte, período de análise e funções auxiliares, entre elas o modelo de resposta fracionária com erros-padrão robustos e agrupados | - |
| `01_formularios.R` | Tratamento dos Formulários A (saldos declarados) e B (destino pretendido) da LC nº 171/2023 | dados |
| `01b_figura2_resolucoes.R` | Número de resoluções com saldo declarado por município | dados |
| `02_repasses.R` | Pagamentos da SES-MG aos municípios (exercício e restos a pagar), dicionário de instrumentos e base de pares município-instrumento | dados |
| `02b_participacao_fms_fes.R` | Participação dos repasses aos fundos municipais na despesa empenhada do Fundo Estadual de Saúde | dados |
| `03_porte.R` | População média de 2016 a 2022, faixas de porte e perfil das faixas | 01 |
| `04_capacidade_siops.R` | Capacidade estatal (MUNIC e IFGF), indicadores fiscais da saúde (SIOPS), PIB per capita e macrorregião de saúde | 03 |
| `05_trf.R` | Cálculo da TRF e da TNR, comparação por porte e macrorregião, modelos municipais e verificações de robustez | 01 a 04 |
| `06_politicas.R` | TRF por área de política, modalidade e objeto do instrumento e modelos por combinação município-resolução | 01, 02, 05 |
| `07_complementos.R` | Ritmo dos pagamentos por ano do instrumento, concentração do saldo constante e comparação entre 2016-2019 e 2020-2022 | 01, 02, 05 |
| `07b_conferencia_periodos.R` | Conferência, no console, dos números da comparação entre períodos citados no texto | 01, 02, 05 |
| `08a_converter_mensal_csv.py` | Conversão das planilhas mensais do SIAFI-MG para CSV, lidas linha a linha para contornar o limite de memória | dados |
| `08_empenho_mensal.R` | Calendário dos empenhos e parcela paga no próprio exercício conforme o mês do empenho | 02, 05, 08a |
| `09_tabelas_secao9.R` | Reúne em um único arquivo as tabelas da seção de resultados quantitativos | 01 a 08 |
| `10_figuras_secao9.R` | Figuras da seção de resultados quantitativos, inclusive mapas por macrorregião e gráficos de efeitos marginais | 01 a 08 |
| `12_tabelas_secao10.R` | Tabelas da análise documental (forma de pagamento por área, valores das resoluções de vigilância, investimentos por modalidade) | 02, 06, 08a |
| `13_tabela_resolucoes.R` | Tabela com uma linha por resolução e resumo por área de política | 01, 02, 05, 06 |
| `Figura1.R` | Mapa dos municípios por faixa populacional com os limites das macrorregiões de saúde | dados |
| `rodar_tudo.R` | Roda todos os scripts acima, na ordem | - |

Os scripts 01 a 08 gravam também figuras exploratórias (`fig1` a `fig14`), que não aparecem no trabalho.

## Onde está cada tabela e figura do trabalho

| No trabalho | Script | Arquivo e aba (em `saida/`) |
|---|---|---|
| Tabela 1 | `02b` | `tabelas/repasses_FMS_FES_2016_2022.xlsx`, abas `Tabela_anual` e `Periodo` |
| Tabela 2 | `04` | `tabelas/parte4_capacidade.xlsx`, aba `T2_recursos_proprios_ano` |
| Tabela 3 | `01` | `tabelas/parte1_formularios.xlsx`, aba `T2_ano_origem` |
| Tabela 4 | - | Dados publicados pela SES-MG (Minas Gerais, 2025c) |
| Tabela 5 | `01` | `tabelas/parte1_formularios.xlsx`, aba `T1_tratamento` |
| Tabela 6 | `07` | `tabelas/parte7_complementos.xlsx`, aba `T1_por_ano_instrumento` |
| Tabela 7 | `05` e `03` | `tabelas/parte5_trf.xlsx`, aba `T3_TRF_por_faixa`; `tabelas/parte3_porte.xlsx`, aba `T8_perfil_por_faixa` |
| Tabela 8 | `06` | `tabelas/parte6_politicas.xlsx`, abas `T2_por_modalidade`, `T3_por_objeto`, `T5b_modalidade_x_porte` e `T5c_objeto_x_porte` |
| Tabela 9 | `13` | `tabelas/parte13_resolucoes.xlsx`, aba `Corpus` |
| Tabela 10 | `12` | `tabelas/parte12_tabelas_secao9.xlsx`, aba `Tab11` |
| Tabelas A.1, A.2, A.3 e A.8 | `05` | `tabelas/parte5_trf.xlsx`, abas `T1_ligacao_formA_pagtos`, `T2b_repasses_x_siops_ano`, `T6_macrorregiao` e `T4_modelos_TRF` (robustez em `T5_robustez` e `T6c_modelo_com_macro`) |
| Tabela A.4 | `07` | `tabelas/parte7_complementos.xlsx`, aba `T2b_top15_constante` |
| Tabelas A.5 e A.6 | `08` | `tabelas/parte8_empenho_mensal.xlsx`, abas `T1_calendario_fms_x_ses`, `T2b_pago_por_mes_empenho_largo`, `T3_decomposicao_nao_pago`, `T4_modelo_tardio` e `T5_restos_a_pagar_origem` |
| Tabela A.7 | `04` | `tabelas/parte4_capacidade.xlsx`, aba `T11_capacidade_por_faixa` |
| Tabelas A.9 e A.12 | `09` | `tabelas/tabelas_secao8.xlsx`, abas `Tab17` e `Tab21` |
| Tabelas A.10 e A.11 | `06` | `tabelas/parte6_politicas.xlsx`, abas `T5_TRF_politica_x_porte`, `T6_modelos_par`, `T6b_agrupamento_simples_x_duplo` e `T7_poder_explicativo` |
| Tabela A.13 | `03` e `01` | `tabelas/parte3_porte.xlsx`, aba `T10_destino_por_faixa`; `tabelas/parte1_formularios.xlsx`, aba `T7_origem_destino` |
| Tabelas A.14 e A.15 | `12` | `tabelas/parte12_tabelas_secao9.xlsx`, abas `Tab10` e `Tab12` |
| Figura 1 | `Figura1.R` | `figuras/figura1_porte_macrorregioes_pdr2023.png` |
| Gráfico 1 | `01b` | `figuras/figura_2_resolucoes_por_municipio.png` |
| Gráfico 2 | `10` | `figuras/fig_s8_histograma_trf.png` |
| Figura 2 | `10` | `figuras/fig_s8_mapa_macrorregioes.png` |
| Gráfico 3 | `10` | `figuras/fig_s8_pagamentos_ano.png` |
| Gráfico 4 | `10` | `figuras/fig_s8_h3_mes_empenho.png` |
| Gráfico 5 | `10` | `figuras/fig_s8_trf_faixa.png` |
| Figura 3 | `10` | `figuras/fig_s8_coef_municipal.png` |
| Gráfico 6 | `10` | `figuras/fig_s8_trf_subfuncao.png` |
| Figura 4 | `10` | `figuras/fig_s8_politica_porte.png` |
| Figura 5 | `10` | `figuras/fig_s8_coef_pares.png` |
| Gráfico 7 | `10` | `figuras/fig_s8_motivos.png` |
| Gráfico 8 | `10` | `figuras/fig_s8_destino.png` |

Os nomes `tabelas_secao8.xlsx`, `fig_s8_*` e as abas `Tab06` a `Tab22` vêm de uma versão anterior do texto, com outra numeração de seções e tabelas; a correspondência correta é a da tabela acima.

## Como reproduzir

1. Coloque os arquivos de entrada na pasta `dados/`, com os nomes indicados na seção seguinte.
2. Abra o projeto `TCC_R.Rproj` no RStudio, para que os caminhos relativos funcionem (ou use `setwd()` para apontar para a pasta do projeto).
3. Rode tudo de uma vez, no Console:
   ```r
   source("scripts/rodar_tudo.R")
   ```
   Os scripts rodam na sessão atual, na ordem da tabela acima; antes de cada um, o Environment é limpo, de modo que cada script use apenas o que os anteriores gravaram em disco. Se um script falhar, a execução para e o erro é mostrado.

Também é possível rodar os scripts um a um, na mesma ordem. Nesse caso, antes do script 08, converta as planilhas mensais no Terminal do RStudio (não no Console), a partir da pasta do projeto:

```
python scripts/08a_converter_mensal_csv.py
```

No Windows, se aparecer a mensagem "Python não foi encontrado", use `py` no lugar de `python`. O `rodar_tudo.R` tenta os dois comandos. Se os arquivos `.csv` mensais já existirem em `dados/mensal/`, a conversão pode ser dispensada.

Os pacotes ausentes são instalados automaticamente na primeira execução.

## Requisitos

- R 4.2 ou superior. Pacotes: readxl, dplyr, tidyr, stringr, stringi, ggplot2, writexl, sandwich, lmtest e readr (todos os scripts); sf, ggforce, ggrepel, ragg, systemfonts, forcats e scales (figuras); openxlsx (`02b`); geobr (opcional, `Figura1.R`).
- Python 3, com o pacote openpyxl (`python -m pip install openpyxl`), apenas para o script `08a`.

## Dados de entrada

Os dados não fazem parte do repositório. Parte deles é pública; parte foi cedida pelo Núcleo de Economia da Saúde da SES-MG (NES/SES-MG) exclusivamente para fins acadêmicos e não pode ser redistribuída.

**Dados cedidos pelo NES/SES-MG (não públicos):**

| Arquivo em `dados/` | Conteúdo |
|---|---|
| `Formulario_A.xlsx` | Saldos financeiros e constantes declarados por instrumento (LC nº 171/2023) |
| `Formulario_B.xlsx` | Planejamento de transposições e transferências (LC nº 171/2023) |
| `Exercicio_2016_2021.xlsx`, `Exercicio_2022_2026.xlsx` | Pagamentos da SES-MG no exercício (SIAFI-MG) |
| `RP_2016_2025.xlsx` | Pagamentos de restos a pagar (SIAFI-MG) |
| `mensal/exercicio_mensal_2016_2021.xlsx`, `mensal/exercicio_mensal_2022_2026.xlsx`, `mensal/rp_mensal_2016_2026.xlsx` | Execução orçamentária mensal da SES-MG (SIAFI-MG); o script `08a` gera os `.csv` de mesmo nome |

**Dados públicos:**

| Arquivo em `dados/` | Fonte |
|---|---|
| `tabela6579.xlsx` | IBGE/SIDRA, Tabela 6579 (estimativas populacionais, 2016-2021) |
| `tabela4709.xlsx` | IBGE/SIDRA, Tabela 4709 (Censo Demográfico 2022) |
| `tabela5938.xlsx` | IBGE/SIDRA, Tabela 5938 (PIB dos municípios) |
| `ipca_tabela_1737.xlsx` | IBGE/SIDRA, Tabela 1737 (IPCA) |
| `Base_MUNIC_2017.xls`, `Base_MUNIC_2021.xlsx` | IBGE, Pesquisa de Informações Básicas Municipais (MUNIC) |
| `IFGF.xlsx` | Firjan, Índice Firjan de Gestão Fiscal |
| `siops/*.txt` | Ministério da Saúde, SIOPS (exportações do TabNet, 2016-2022) |
| `macrorregioes_pdr2023.xlsx` | SES-MG, Plano Diretor de Regionalização 2023 |
| `mapa/mg_municipios.json` | Malha municipal de Minas Gerais em GeoJSON (repositório tbrugz/geodata-br, a partir da malha do IBGE) |
| `resolucoes/anexo_I_res_7153_2020.xlsx` | Anexo I da Resolução SES/MG nº 7.153/2020, transcrito (colunas `cod6`, `pop_2019`, `valor_por_eixo`) |
| `resolucoes/anexo_I_res_7799_2021.xlsx` | Anexo I da Resolução SES/MG nº 7.799/2021, transcrito (colunas `cod6`, `pop_2019`, `valor_anexo_I`) |

As colunas de senha dos formulários são descartadas logo na leitura e nunca são gravadas.

## Aviso

As análises e interpretações são de responsabilidade do autor e não representam posição da Secretaria de Estado de Saúde de Minas Gerais.

## Como citar

AVELAR, Matheus Magalhães. **Recursos represados no SUS**: federalismo, capacidade estatal municipal e baixa execução financeira dos repasses estaduais em Minas Gerais (2016-2022). 2026. Trabalho de Conclusão de Curso (Graduação em Administração Pública) - Escola de Governo Professor Paulo Neves de Carvalho, Fundação João Pinheiro, Belo Horizonte, 2026.

## Licença

O código está disponível sob a licença MIT (ver o arquivo `LICENSE`). A licença não se aplica aos dados de entrada.

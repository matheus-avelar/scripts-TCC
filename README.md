# Recursos represados no SUS: scripts de tratamento e análise dos dados

Scripts do Trabalho de Conclusão de Curso **"Recursos represados no SUS: federalismo, capacidade estatal municipal e baixa execução financeira dos repasses estaduais em Minas Gerais (2016-2022)"**, apresentado ao curso de Administração Pública da Escola de Governo da Fundação João Pinheiro (FJP), em 2026.

Autor: Matheus Magalhães Avelar

## Sobre a pesquisa

A pesquisa investiga por que parte dos recursos repassados pela Secretaria de Estado de Saúde de Minas Gerais (SES-MG) aos fundos municipais de saúde permaneceu sem execução entre 2016 e 2022, e em que medida isso variou conforme o porte municipal e a política de saúde financiada.

A base empírica central são os saldos declarados pelos municípios na adesão à Lei Complementar Estadual nº 171/2023, ligados aos pagamentos da SES-MG pelo número de cada resolução. A partir dessa ligação, são calculados dois indicadores:

- **Taxa de represamento financeiro (TRF):** parcela do valor recebido pelo fundo municipal que permanecia em conta quando os saldos foram declarados (represamento no município).
- **Taxa de não repasse (TNR):** parcela do valor pactuado que o Estado ainda não havia pago nesse momento (represamento na origem).

As comparações entre grupos (faixas de porte, macrorregiões, áreas de política e períodos) são descritivas. As associações com características dos municípios e dos instrumentos de repasse são estimadas por modelos de resposta fracionária (Papke; Wooldridge, 1996), com erros-padrão robustos e, no modelo por combinação município-resolução, agrupados por município e por resolução.

## Estrutura do repositório

```
TCC_R/
├── TCC_R.Rproj          projeto do RStudio
├── scripts/             scripts de tratamento e análise
├── dados/               dados de entrada (não versionados, ver abaixo)
└── saida/
    ├── tabelas/         tabelas em .xlsx geradas pelos scripts
    ├── figuras/         figuras em .png (300 dpi)
    └── bases/           bases intermediárias em .rds
```

As pastas de saída são criadas automaticamente pelo script `00_config.R`.

## Scripts

Os scripts são executados em sequência. Cada um lê as bases produzidas pelos anteriores e grava as suas em `saida/`. O `00_config.R` é carregado automaticamente pelos demais.

| Script | Conteúdo |
| --- | --- |
| `00_config.R` | Pacotes, pastas, faixas de porte, período de análise e funções auxiliares, entre elas o modelo de resposta fracionária com erros-padrão robustos e agrupados |
| `01_formularios.R` | Tratamento dos Formulários A (saldos declarados) e B (destino pretendido) da LC nº 171/2023 |
| `01b_figura2_resolucoes.R` | Número de resoluções com saldo declarado por município (figura auxiliar) |
| `02_repasses.R` | Pagamentos da SES-MG aos municípios (exercício e restos a pagar), dicionário de instrumentos e base de pares município-instrumento |
| `03_porte.R` | População média de 2016 a 2022, faixas de porte e perfil das faixas |
| `04_capacidade_siops.R` | Indicadores de capacidade estatal (MUNIC e IFGF), indicadores fiscais da saúde (SIOPS), PIB per capita e macrorregião de saúde |
| `05_trf.R` | Cálculo da TRF e da TNR, comparação por porte e macrorregião, modelos municipais e verificações de robustez |
| `06_politicas.R` | TRF por área de política, modalidade e objeto do instrumento e modelos por combinação município-resolução |
| `07_complementos.R` | Ritmo dos pagamentos por ano do instrumento, concentração do saldo constante e comparação entre os períodos 2016-2019 e 2020-2022 |
| `08a_converter_mensal_csv.py` | Conversão das planilhas mensais do SIAFI-MG para CSV, lidas linha a linha para contornar o limite de memória |
| `08_empenho_mensal.R` | Calendário dos empenhos e parcela paga no próprio exercício conforme o mês do empenho |
| `09_tabelas_secao8.R` | Reúne em um único arquivo as tabelas apresentadas nos resultados |
| `10_figuras_secao8.R` | Figuras dos resultados, inclusive mapas por macrorregião e gráficos de efeitos marginais |
| `Figura1.R` | Mapa dos municípios por faixa populacional com os limites das macrorregiões de saúde |

## Como reproduzir

1. Coloque os arquivos de entrada na pasta `dados/`, com os nomes indicados na seção seguinte.
2. Abra o projeto `TCC_R.Rproj` no RStudio, para que os caminhos relativos funcionem.
3. Na primeira vez, rode `scripts/00_config.R` para instalar os pacotes que faltarem.
4. Rode os scripts de `01` a `07`, nesta ordem.
5. Converta as planilhas mensais do SIAFI-MG, no terminal, a partir da pasta do projeto:
   ```
   python scripts/08a_converter_mensal_csv.py
   ```
6. Rode `08_empenho_mensal.R`, depois `09_tabelas_secao8.R` e `10_figuras_secao8.R`.

## Requisitos

- **R** (versão 4.2 ou superior), com os pacotes `readxl`, `dplyr`, `tidyr`, `stringr`, `stringi`, `ggplot2`, `writexl`, `sandwich`, `lmtest` e `readr`. Os scripts de figuras usam também `sf`, `ggforce`, `ggrepel` e `geobr`. Os pacotes ausentes são instalados automaticamente.
- **Python 3**, com o pacote `openpyxl`, apenas para o script `08a`.

## Dados de entrada

Os dados não fazem parte do repositório. Parte deles é pública; parte foi cedida pelo Núcleo de Economia da Saúde da SES-MG (NES/SES-MG) exclusivamente para fins acadêmicos e não pode ser redistribuída.

**Dados cedidos pelo NES/SES-MG (não públicos):**

| Arquivo em `dados/` | Conteúdo |
| --- | --- |
| `Formulario_A.xlsx` | Saldos financeiros e constantes declarados por instrumento (LC nº 171/2023) |
| `Formulario_B.xlsx` | Planejamento de transposições e transferências (LC nº 171/2023) |
| `Exercicio_2016_2021.xlsx`, `Exercicio_2022_2026.xlsx` | Pagamentos da SES-MG no exercício (SIAFI-MG) |
| `RP_2016_2025.xlsx` | Pagamentos de restos a pagar (SIAFI-MG) |
| `mensal/*.xlsx` | Execução orçamentária mensal da SES-MG (SIAFI-MG) |

**Dados públicos:**

| Arquivo em `dados/` | Fonte |
| --- | --- |
| `tabela6579.xlsx` | IBGE/SIDRA, Tabela 6579 (estimativas populacionais, 2016-2021) |
| `tabela4709.xlsx` | IBGE/SIDRA, Tabela 4709 (Censo Demográfico 2022) |
| `tabela5938.xlsx` | IBGE/SIDRA, Tabela 5938 (PIB dos municípios) |
| `ipca_tabela_1737.xlsx` | IBGE/SIDRA, Tabela 1737 (IPCA) |
| `Base_MUNIC_2017.xls`, `Base_MUNIC_2021.xlsx` | IBGE, Pesquisa de Informações Básicas Municipais (MUNIC) |
| `IFGF.xlsx` | Firjan, Índice Firjan de Gestão Fiscal |
| `siops/*.txt` | Ministério da Saúde, SIOPS (exportações do TabNet, 2016-2022) |
| `macrorregioes_pdr2023.xlsx` | SES-MG, Plano Diretor de Regionalização 2023 |
| `mapa/mg_municipios.json` | Malha municipal de Minas Gerais em GeoJSON (repositório tbrugz/geodata-br, a partir da malha do IBGE) |

As colunas de senha dos formulários não são lidas pelos scripts.

## Aviso

As análises e interpretações são de responsabilidade do autor e não representam posição da Secretaria de Estado de Saúde de Minas Gerais.

## Como citar

AVELAR, Matheus. **Recursos represados no SUS**: federalismo, capacidade estatal municipal e baixa execução financeira dos repasses estaduais em Minas Gerais (2016-2022). 2026. Trabalho de Conclusão de Curso (Graduação em Administração Pública) - Escola de Governo, Fundação João Pinheiro, Belo Horizonte, 2026.

## Licença

O código está disponível sob a licença MIT (ver o arquivo `LICENSE`). A licença não se aplica aos dados de entrada.

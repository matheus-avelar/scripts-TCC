"""
Converte as planilhas mensais do SIAFI-MG (dados/mensal/*.xlsx) em CSV, lendo linha a linha.
Motivo: a planilha de restos a pagar tem cerca de 560 mil linhas e 540 MB de XML interno;
lê-la inteira com o readxl esgota a memória. Rode uma vez antes dos scripts 08 e 12:
    python scripts/08a_converter_mensal_csv.py
(a pasta "dados" é localizada a partir da posição deste arquivo, então o comando
funciona de qualquer pasta).
Gera dados/mensal/<nome>.csv (separador ';', UTF-8), a partir da linha de cabeçalho.
Os scripts 08 e 12 esperam: exercicio_mensal_2016_2021.csv, exercicio_mensal_2022_2026.csv
e rp_mensal_2016_2026.csv (ou seja, planilhas com esses nomes, em .xlsx).
Requer: openpyxl (pip install openpyxl).
"""
import csv, glob, os, sys
from openpyxl import load_workbook

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # pasta do projeto
pasta = os.path.join(RAIZ, "dados", "mensal")
if not os.path.isdir(pasta):
    sys.exit(f"Pasta não encontrada: {pasta}. Coloque as planilhas mensais em dados/mensal.")

arquivos = [a for a in sorted(glob.glob(os.path.join(pasta, "*.xlsx")))
            if not os.path.basename(a).startswith("~$")]   # ignora arquivos temporários do Excel
if not arquivos:
    sys.exit(f"Nenhuma planilha .xlsx em {pasta}.")

for arq in arquivos:
    destino = arq[:-5] + ".csv"
    if os.path.exists(destino) and os.path.getmtime(destino) > os.path.getmtime(arq):
        print("já convertido:", destino); continue
    wb = load_workbook(arq, read_only=True, data_only=True)
    ws = wb.worksheets[0]                       # só a 1ª aba
    n, achou, ini = 0, False, 0
    with open(destino, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        for linha in ws.iter_rows(values_only=True):
            vals = ["" if v is None else v for v in linha]
            if not achou:                       # cabeçalho = linha que contém "Ano de Exercício"
                if any(str(v).strip() == "Ano de Exercício" for v in vals):
                    achou = True
                    ini = next(i for i, v in enumerate(vals) if str(v).strip() != "")
                    w.writerow(vals[ini:])
                continue
            if all(v == "" for v in vals):
                continue
            w.writerow(vals[ini:]); n += 1
    wb.close()
    if not achou:                               # sem cabeçalho: não deixa um CSV vazio para trás
        os.remove(destino)
        print(f"AVISO: {arq} não tem a linha de cabeçalho com 'Ano de Exercício'; nada foi gravado.")
        continue
    print(f"{arq} -> {destino}: {n} linhas")

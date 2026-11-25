"""
Trabalho Final EEE933 — Grupo E
00_extrair_estacoes.py — extrai a vazão DIÁRIA das estações das planilhas da empresa

Não há leitor de xlsx no R deste ambiente (só data.table), então este script usa
apenas a biblioteca padrão do Python (zipfile + xml) para gravar um CSV.

Entradas (cópia local da pasta "Base Dados" do Drive):
  dados/raw/base_dados/01_historico/plano_2026/Indicadores_2026_Meta_Geracao_Eficacia_jul25.xlsx
    aba "Vazão_Histórica_Classificada": Data, Bruto, Consistido, Revisado, Estacao ...
        (vazão diária por CÓDIGO de estação, até 31/12/2024)
    aba "Vazão_Exceções": usina, vazao, data ...
        (vazão já ajustada para usinas sem estação própria: Henrique Portugal,
         Congonhal II, Pirambeira, Ribeirão e Cachoeira dos Prazeres; até mai/2025)

Saída:
  dados/raw/derivados/estacoes_vazao_diaria.csv
    aba, estacao, usina_aba, data, bruto, consistido, revisado
    (sem decidir nada: o mapa estação → usina e a escolha da coluna ficam no R)

Uso: python3 "Trabalho Final/codigo/00_extrair_estacoes.py"
"""
import csv
import datetime as dt
import os
import re
import sys
import zipfile
import xml.etree.ElementTree as ET

M = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
R_ID = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id"
REL = "{http://schemas.openxmlformats.org/package/2006/relationships}Relationship"
BASE_EXCEL = dt.date(1899, 12, 30)

DIR_TF = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
ARQ = os.path.join(DIR_TF, "dados", "raw", "base_dados", "01_historico", "plano_2026",
                   "Indicadores_2026_Meta_Geracao_Eficacia_jul25.xlsx")
SAIDA = os.path.join(DIR_TF, "dados", "raw", "derivados", "estacoes_vazao_diaria.csv")


def col_idx(ref):
    n = 0
    for ch in re.match(r"[A-Z]+", ref).group(0):
        n = n * 26 + ord(ch) - 64
    return n - 1


def abrir(caminho):
    z = zipfile.ZipFile(caminho)
    wb = ET.fromstring(z.read("xl/workbook.xml"))
    rels = {r.get("Id"): r.get("Target")
            for r in ET.fromstring(z.read("xl/_rels/workbook.xml.rels")).iter(REL)}
    abas = {}
    for s in wb.find(M + "sheets"):
        alvo = rels[s.get(R_ID)].lstrip("/")
        abas[s.get("name")] = alvo if alvo.startswith("xl/") else "xl/" + alvo
    ss = []
    if "xl/sharedStrings.xml" in z.namelist():
        for si in ET.fromstring(z.read("xl/sharedStrings.xml")).findall(M + "si"):
            ss.append("".join(t.text or "" for t in si.iter(M + "t")))
    return z, abas, ss


def linhas(z, alvo, ss):
    with z.open(alvo) as fh:
        for _, el in ET.iterparse(fh):
            if el.tag != M + "row":
                continue
            vals = {}
            for c in el.findall(M + "c"):
                v, t = c.find(M + "v"), c.get("t")
                if t == "inlineStr":
                    val = "".join(x.text or "" for x in c.iter(M + "t"))
                elif v is None:
                    continue
                else:
                    val = ss[int(v.text)] if t == "s" else v.text
                vals[col_idx(c.get("r"))] = val
            el.clear()
            if vals:
                yield [vals.get(i, "") for i in range(max(vals) + 1)]


def data_excel(x):
    return (BASE_EXCEL + dt.timedelta(days=int(float(x)))).isoformat()


def ler_aba(z, abas, ss, nome, cabecalho_obrigatorio):
    """Pula linhas até achar o cabeçalho (as abas têm 'Arquivo base:' na 1ª linha)."""
    it = linhas(z, abas[nome], ss)
    for cab in it:
        if all(c in cab for c in cabecalho_obrigatorio):
            break
    else:
        sys.exit(f"cabeçalho {cabecalho_obrigatorio} não encontrado na aba {nome}")
    ix = {c: cab.index(c) for c in cab if c}
    for r in it:
        r = r + [""] * (len(cab) - len(r))
        yield ix, r


def main():
    z, abas, ss = abrir(ARQ)
    os.makedirs(os.path.dirname(SAIDA), exist_ok=True)
    n = {"Vazão_Histórica_Classificada": 0, "Vazão_Exceções": 0}
    with open(SAIDA, "w", newline="", encoding="utf-8") as out:
        w = csv.writer(out, lineterminator="\n")
        w.writerow(["aba", "estacao", "usina_aba", "data", "bruto", "consistido", "revisado"])
        aba = "Vazão_Histórica_Classificada"
        for ix, r in ler_aba(z, abas, ss, aba, ["Data", "Consistido", "Estacao"]):
            if not r[ix["Data"]]:
                continue
            w.writerow([aba, r[ix["Estacao"]], "", data_excel(r[ix["Data"]]),
                        r[ix["Bruto"]], r[ix["Consistido"]], r[ix["Revisado"]]])
            n[aba] += 1
        aba = "Vazão_Exceções"
        for ix, r in ler_aba(z, abas, ss, aba, ["usina", "vazao", "data"]):
            if not r[ix["data"]]:
                continue
            # a aba não diz se é bruto ou consistido: vai na coluna "revisado"
            w.writerow([aba, "", r[ix["usina"]], data_excel(r[ix["data"]]),
                        "", "", r[ix["vazao"]]])
            n[aba] += 1
    print(f"gravado {SAIDA}")
    for k, v in n.items():
        print(f"  {k}: {v} linhas")


if __name__ == "__main__":
    main()

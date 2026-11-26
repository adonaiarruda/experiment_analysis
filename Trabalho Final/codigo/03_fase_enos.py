"""
Trabalho Final EEE933 — Grupo E
03_fase_enos.py — fase do ENOS por ano hidrológico (fator B da P3)

Regra da PROPOSTA apresentada em 10/11/2026 (confirmada pelo grupo em 27/11/2026):
  ONI médio dos trimestres OND, NDJ e DJF do ano hidrológico T (ago/T – jul/T+1):
    El Niño  se ONI médio >= +0,5 °C
    La Niña  se ONI médio <= −0,5 °C
    Neutro   caso contrário
Na tabela da NOAA, NDJ do ano T = nov/T–jan/T+1 e DJF do ano T+1 = dez/T–fev/T+1.
Ano hidrológico sem os três trimestres fica sem fase.
(Entre 25 e 27/11 foi usada a regra de episódio da NOAA, 5 trimestres seguidos;
 o grupo voltou para a regra da proposta.)

Sensibilidade (proposta): fase pelo catálogo do CPTEC/INPE, que lista os episódios
por ano hidrológico ago–jul, com intensidade. Ano listado → a fase do CPTEC; ano não
listado até o fim do catálogo (2017/18) → Neutro; depois disso → vazio.

Entrada: dados/raw/ENOS_Monitoramento_Nino34.xlsx › abas NOAA_CPC_ONI e CPTEC_INPE_Episodios
Saída  : dados/fase_enos_anual.csv
         ano_hidro, periodo, oni_ond, oni_ndj, oni_djf, oni_medio, fase_noaa,
         fase_cptec, intensidade_cptec

Uso: python3 "Trabalho Final/codigo/03_fase_enos.py"
"""
import csv
import importlib.util
import os
import sys

sys.dont_write_bytecode = True   # não deixa __pycache__ em codigo/ ao importar o 00

LIMIAR = 0.5
DIR_COD = os.path.dirname(os.path.abspath(__file__))
DIR_TF = os.path.normpath(os.path.join(DIR_COD, ".."))
ARQ = os.path.join(DIR_TF, "dados", "raw", "ENOS_Monitoramento_Nino34.xlsx")
SAIDA = os.path.join(DIR_TF, "dados", "fase_enos_anual.csv")

# reaproveita o leitor de xlsx (só biblioteca padrão) do 00_extrair_estacoes.py
_spec = importlib.util.spec_from_file_location("extrair", os.path.join(DIR_COD, "00_extrair_estacoes.py"))
xl = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(xl)


def fase(oni_medio):
    if oni_medio >= LIMIAR:
        return "El Niño"
    if oni_medio <= -LIMIAR:
        return "La Niña"
    return "Neutro"


def main():
    z, abas, ss = xl.abrir(ARQ)
    it = xl.linhas(z, abas["NOAA_CPC_ONI"], ss)
    cab = next(it)
    i_ano, i_tri, i_oni = cab.index("Ano"), cab.index("Trimestre (3 meses)"), cab.index("ONI (°C)")
    oni = {}
    for r in it:
        if len(r) > i_oni and r[i_oni] not in ("", None):
            oni[(int(float(r[i_ano])), r[i_tri].strip())] = float(r[i_oni])

    # catálogo do CPTEC: "1997-1998" → ano hidrológico 1997
    it = xl.linhas(z, abas["CPTEC_INPE_Episodios"], ss)
    cab = next(it)
    i_fen, i_per, i_int = 0, 1, 2
    cptec = {}
    for r in it:
        if len(r) > i_per and r[i_per]:
            t = int(r[i_per].split("-")[0])
            cptec[t] = (r[i_fen].strip(), r[i_int].strip() if len(r) > i_int else "")
    fim_cptec = max(cptec)

    anos = sorted({a for a, _ in oni})
    linhas = []
    for t in range(anos[0] - 1, anos[-1] + 1):
        v = (oni.get((t, "OND")), oni.get((t, "NDJ")), oni.get((t + 1, "DJF")))
        if any(x is None for x in v):
            continue
        media = round(sum(v) / 3, 4)
        if t in cptec:
            f_cptec, i_cptec = cptec[t]
        elif t <= fim_cptec:
            f_cptec, i_cptec = "Neutro", ""
        else:
            f_cptec, i_cptec = "", ""            # fora do catálogo
        linhas.append([t, f"{t}-{t + 1}", *v, media, fase(media), f_cptec, i_cptec])

    with open(SAIDA, "w", newline="", encoding="utf-8") as out:
        w = csv.writer(out, lineterminator="\n")
        w.writerow(["ano_hidro", "periodo", "oni_ond", "oni_ndj", "oni_djf", "oni_medio", "fase_noaa",
                    "fase_cptec", "intensidade_cptec"])
        w.writerows(linhas)
    cont = {f: sum(l[6] == f for l in linhas) for f in ("El Niño", "La Niña", "Neutro")}
    print(f"gravado {SAIDA}: anos hidrológicos {linhas[0][0]}–{linhas[-1][0]} ({len(linhas)}) {cont}")
    print(f"CPTEC: catálogo até {fim_cptec}-{fim_cptec + 1}; anos depois ficam sem fase_cptec")


if __name__ == "__main__":
    main()

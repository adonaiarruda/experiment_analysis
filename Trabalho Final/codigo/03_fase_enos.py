"""
Trabalho Final EEE933 — Grupo E
03_fase_enos.py — fase do ENOS por ano hidrológico (fator B da P3)

Regra (NOAA/CPC, decidida com o grupo em 25/11/2026):
  1. Episódio de El Niño = pelo menos 5 trimestres móveis SEGUIDOS com ONI >= +0,5 °C.
     Episódio de La Niña = pelo menos 5 trimestres móveis seguidos com ONI <= −0,5 °C.
  2. O ano hidrológico T (ago/T – jul/T+1) recebe a fase do episódio que cobre a
     maioria (>= 2) dos trimestres do pico do verão: OND/T, NDJ/T e DJF/T+1.
     Se nenhum episódio cobre 2 desses 3 trimestres → Neutro.
Na tabela da NOAA, NDJ do ano T = nov/T–jan/T+1 e DJF do ano T+1 = dez/T–fev/T+1.
Um trecho que ainda não fechou 5 trimestres no fim da série não conta como episódio.

Entrada: dados/raw/ENOS_Monitoramento_Nino34.xlsx › aba NOAA_CPC_ONI
         (Ano, Trimestre, ONI (°C); começa em DJF/1950)
Saída  : dados/fase_enos_anual.csv
         ano_hidro, periodo, oni_ond, oni_ndj, oni_djf, oni_medio,
         tri_el_nino, tri_la_nina (quantos dos 3 trimestres estão em episódio), fase

Uso: python3 "Trabalho Final/codigo/03_fase_enos.py"
"""
import csv
import importlib.util
import os
import sys

sys.dont_write_bytecode = True   # não deixa __pycache__ em codigo/ ao importar o 00

LIMIAR = 0.5
MIN_TRIMESTRES = 5
TRIMESTRES = ["DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS", "ASO", "SON", "OND", "NDJ"]
DIR_COD = os.path.dirname(os.path.abspath(__file__))
DIR_TF = os.path.normpath(os.path.join(DIR_COD, ".."))
ARQ = os.path.join(DIR_TF, "dados", "raw", "ENOS_Monitoramento_Nino34.xlsx")
SAIDA = os.path.join(DIR_TF, "dados", "fase_enos_anual.csv")

# reaproveita o leitor de xlsx (só biblioteca padrão) do 00_extrair_estacoes.py
_spec = importlib.util.spec_from_file_location("extrair", os.path.join(DIR_COD, "00_extrair_estacoes.py"))
xl = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(xl)


def ler_oni():
    z, abas, ss = xl.abrir(ARQ)
    it = xl.linhas(z, abas["NOAA_CPC_ONI"], ss)
    cab = next(it)
    i_ano, i_tri, i_oni = cab.index("Ano"), cab.index("Trimestre (3 meses)"), cab.index("ONI (°C)")
    oni = {}
    for r in it:
        if len(r) > i_oni and r[i_oni] not in ("", None):
            oni[(int(float(r[i_ano])), r[i_tri].strip())] = float(r[i_oni])
    return oni


def episodios(oni):
    """Marca cada trimestre como 'El Niño', 'La Niña' ou None pela regra dos 5 seguidos."""
    seq = sorted(oni, key=lambda k: (k[0], TRIMESTRES.index(k[1])))
    sinal = ["El Niño" if oni[k] >= LIMIAR else "La Niña" if oni[k] <= -LIMIAR else None for k in seq]
    marca, i = {}, 0
    while i < len(seq):
        j = i
        while j + 1 < len(seq) and sinal[j + 1] == sinal[i]:
            j += 1
        em_episodio = sinal[i] is not None and (j - i + 1) >= MIN_TRIMESTRES
        for k in seq[i:j + 1]:
            marca[k] = sinal[i] if em_episodio else None
        i = j + 1
    return marca


def main():
    oni = ler_oni()
    marca = episodios(oni)
    anos = sorted({a for a, _ in oni})
    linhas = []
    for t in range(anos[0] - 1, anos[-1] + 1):
        chaves = [(t, "OND"), (t, "NDJ"), (t + 1, "DJF")]
        if any(k not in oni for k in chaves):
            continue
        v = [oni[k] for k in chaves]
        n_en = sum(marca[k] == "El Niño" for k in chaves)
        n_ln = sum(marca[k] == "La Niña" for k in chaves)
        fase = "El Niño" if n_en >= 2 else "La Niña" if n_ln >= 2 else "Neutro"
        linhas.append([t, f"{t}-{t + 1}", *v, round(sum(v) / 3, 4), n_en, n_ln, fase])

    with open(SAIDA, "w", newline="", encoding="utf-8") as out:
        w = csv.writer(out, lineterminator="\n")
        w.writerow(["ano_hidro", "periodo", "oni_ond", "oni_ndj", "oni_djf", "oni_medio",
                    "tri_el_nino", "tri_la_nina", "fase"])
        w.writerows(linhas)
    cont = {f: sum(l[-1] == f for l in linhas) for f in ("El Niño", "La Niña", "Neutro")}
    print(f"gravado {SAIDA}: anos hidrológicos {linhas[0][0]}–{linhas[-1][0]} ({len(linhas)}) {cont}")


if __name__ == "__main__":
    main()

# Dados do trabalho final — guia rápido

Dados da NEC Energia. No git estão só este guia e as **duas bases compiladas** abaixo; os
arquivos originais em `raw/` ficam só na máquina (ver `.gitignore`).

## O que usar

| Arquivo | O que é | Linha = |
|---|---|---|
| `base_vazao_mensal.csv` | vazão medida de 17 usinas (1930–2026) | usina × mês |
| `fase_enos_anual.csv` | fase do ENOS (El Niño / La Niña / Neutro) | ano hidrológico |

As duas se ligam pela coluna `ano_hidro`. **Ano hidrológico** T = agosto de T a julho de T+1.
Todo o resto está em `raw/` e só é preciso para regenerar estes dois arquivos.

## `base_vazao_mensal.csv`

| Coluna | Significado |
|---|---|
| `usina`, `mes_ano`, `ano`, `mes`, `ano_hidro` | identificação |
| `q_soma_m3s_dia` | **vazão do mês = soma das vazões diárias** (m³/s·dia) |
| `volume_hm3` | o mesmo em volume: soma × 86 400 / 10⁶ |
| `q_media_m3s` | média dos dias medidos |
| `dias_mes`, `dias_medidos` | dias do mês e dias com medição |
| `completo` | todos os dias do mês medidos |
| `valido` | pelo menos 20 dias medidos |
| `fonte` | `mlt`, `estacao` ou `estacao_ajustada` (ver abaixo) |

Regras:

- Só entra vazão **medida**. Dia preenchido ou sintético não entra.
- **Dia sem medição conta como 0** na soma: mês incompleto fica com soma menor.
- **Mês sem nenhum dia medido = NA** (vazio). Meses parciais no começo e no fim de cada série foram tirados.
- Cada usina usa **uma fonte principal**, a de mais anos completos:
  - `mlt`: séries diárias longas (Lucas Horta), medidas numa estação de referência e
    transferidas para a usina pela razão de áreas;
  - `estacao`: estação da empresa junto à usina (2013–2024).
- A outra fonte só completa dias faltantes se as duas baterem (razão mediana a ±6% de 1).
  Isso acontece em Areal, Brito, Glória, Mello e NPQ II.
- A usina só fica na base com **pelo menos 8 anos hidrológicos completos**.
- A escala muda de uma usina para outra (Prazeres é medida a 437 km², ~7× a usina). Isso não
  afeta o erro ln(Q_obs/Q_prev), que é calculado dentro da mesma série.

| Usinas | Fonte | Período | Anos completos |
|---|---|---|---|
| Areal, Henrique Portugal, NPQ II, Nova Maurício, Pacífico Mascarenhas, Furquim | MLT | 1930/40 – 2025/26 | 80–88 |
| Glória, Brito, Costa Rica | MLT | 1964/84 – 2025/26 | 35–60 |
| Mello | estação + MLT | 2014 – 2026 | 11 |
| Fumaça, Ituerê, Funil, Salto, Paes Leme, Caboclo, Prazeres | estação | 2014/16 – 2024 | 8–10 |

**Fora da base:** BRC, Congonhal I/II, Pirambeira e Ribeirão (1 a 2 anos completos).
**Paes Leme** usa a estação própria porque na MLT ela é cópia de Henrique Portugal.

## `fase_enos_anual.csv`

| Coluna | Significado |
|---|---|
| `ano_hidro`, `periodo` | ano hidrológico (ex.: `1997-1998`) |
| `oni_ond`, `oni_ndj`, `oni_djf` | ONI (°C) dos 3 trimestres do pico do verão |
| `oni_medio` | média dos três (indica a intensidade) |
| `tri_el_nino`, `tri_la_nina` | quantos dos 3 trimestres estão dentro de um episódio |
| `fase` | `El Niño`, `La Niña` ou `Neutro` |

Regra da NOAA:

1. **Episódio** de El Niño: pelo menos **5 trimestres seguidos** com ONI ≥ +0,5 °C.
   Episódio de La Niña: pelo menos 5 trimestres seguidos com ONI ≤ −0,5 °C.
2. O ano hidrológico recebe a fase do episódio que cobre **pelo menos 2** dos trimestres
   OND, NDJ e DJF. Se nenhum episódio cobre, o ano é **Neutro**.

Resultado: 1950/51 a 2025/26, com 27 anos El Niño, 25 La Niña e 24 Neutro. **Anos antes de
1950 não têm fase**, porque o ONI começa em 1950.

## Como regenerar

Na raiz do repositório:

```bash
python3 "Trabalho Final/codigo/00_extrair_estacoes.py"
```

```bash
Rscript "Trabalho Final/codigo/02_base_unica.R"
```

```bash
python3 "Trabalho Final/codigo/03_fase_enos.py"
```

`00` lê a vazão diária das estações do xlsx. `02` monta a base de vazão. `03` monta a fase
do ENOS. O `01_backcast.R` (previsões antigas, por série diária) é separado.

## O que está em `raw/`

| Caminho | O que é |
|---|---|
| `serie_mlt_diaria/<usina>.csv` | séries diárias MLT (19 usinas); `Tipo` = Original / Preenchido / Sintético |
| `ENOS_Monitoramento_Nino34.xlsx` | ONI e Niño 3.4 de várias fontes (aba `NOAA_CPC_ONI` é a usada) |
| `Estacoes_*.xlsx`, `MLT_*.xlsx`, `Resumo_dados_por_usina.xlsx` | resumos mensais e estatísticas por posição |
| `Cadastro_usinas_vazao_turbinavel.xlsx` | vazão máxima turbinável por usina |
| `ENOS_Correlacao_com_vazao.xlsx`, `ENOS_Vazao_diaria_x_fases.xlsx` | análises prontas da empresa |
| `base_dados/01_historico/` | planos 2024 e 2026 (o de 2026 tem a vazão diária das estações) |
| `base_dados/02_lognormal/` | planilha original da log-normal |
| `base_dados/03_plano_2027/` | plano 2027 e previsto × realizado por usina × mês |
| `base_dados/06_scripts/` | scripts Python da empresa (só leitura) |
| `derivados/` | arquivos intermediários dos scripts (base diária, diagnósticos, backcast) |

Origem: pasta **"Base Dados"** do Google Drive do João, baixada em 22/11/2026 e com nomes
sem acento. Os arquivos soltos em `raw/` e `serie_mlt_diaria/` vêm da subpasta
"05 - Bases Hidrológicas". Ficaram de fora: o Memorial de Cálculo 2027 (18 MB), a pasta
"04 - Comparativo" (não compartilhada) e os arquivos de vazão realizada 2025–26 citados nos
scripts da empresa.

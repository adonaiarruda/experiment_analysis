# Dados do trabalho final — guia rápido

Dados da NEC Energia para a P3: *o erro da previsão de vazão depende da fase do ENOS?*

- **Dados brutos:** no Google Drive, pasta **"Base Dados"**:
  https://drive.google.com/drive/folders/13SFYfISbmfvI3B9waTVCh6oPJRN_Snkw
  Localmente ficam em `raw/`, que **não vai para o git**.
- **Dados consolidados:** os 5 CSVs desta pasta, que **vão para o git**. Saem dos brutos pelos
  scripts de `../codigo/` (ver "Como reproduzir").

## O que usar

| Arquivo | O que é | Linha = |
|---|---|---|
| **`tabela_experimento.csv`** | **tabela do fatorial**: erro `e` e `abs_e` de cada metodologia, com a fase do ENOS | usina × ano hidrológico × metodologia |
| `previsoes_mensais.csv` | previsão de cada mês pelas 3 metodologias, com observado e erro | usina × mês × metodologia |
| `base_vazao_mensal.csv` | vazão medida de 17 usinas, ago/1975–jun/2026 | usina × mês |
| `base_vazao_diaria.csv` | a mesma vazão, dia a dia (usada na log-normal) | usina × dia |
| `fase_enos_anual.csv` | fase do ENOS pelo ONI da NOAA e pelo CPTEC | ano hidrológico |

- **Ano hidrológico** T = agosto de T a julho de T+1 (coluna `ano_hidro`, que liga os arquivos).
- **Erro:** e = ln(Q_obs / Q_prev). e < 0 quer dizer que a metodologia previu acima do que
  aconteceu (viés); |e| é a acurácia.

## De onde vêm os dados

Pasta **"Base Dados"** do Google Drive ([link](https://drive.google.com/drive/folders/13SFYfISbmfvI3B9waTVCh6oPJRN_Snkw)),
compartilhada pelo João (NEC Energia) e baixada em 22/11/2026.

| Fonte | O que é | Onde está em `raw/` |
|---|---|---|
| `mlt` | Séries diárias longas (Lucas Horta): vazão **medida numa estação de referência da ANA** e **convertida para a usina pela razão entre as áreas das bacias**. Cada dia vem marcado como `Original` (medido), `Preenchido` ou `Sintético`. | `serie_mlt_diaria/` |
| `estacao` | **Estações da própria empresa junto à usina**, dados consistidos de 2002–2016 até 2024. | aba `Vazão_Histórica_Classificada` de `base_dados/01_historico/plano_2026/Indicadores_2026_Meta_Geracao_Eficacia_jul25.xlsx` |
| `estacao_ajustada` | Vazão que a empresa calculou para usinas sem estação própria. Na base, só Henrique Portugal (2013–2024) usa. | aba `Vazão_Exceções` do mesmo arquivo |
| ONI da NOAA/CPC | Anomalia da temperatura do mar na região Niño 3.4, média de 3 meses, desde 1950. | aba `NOAA_CPC_ONI` de `ENOS_Monitoramento_Nino34.xlsx` |
| CPTEC/INPE | Catálogo de episódios de El Niño e La Niña por ano ago–jul, com intensidade, até 2017/18. | aba `CPTEC_INPE_Episodios` do mesmo arquivo |

## Como os dados foram tratados

### 1. Vazão (`00_extrair_estacoes.py`, `02_base_unica.R`)

1. **Só dado medido:** dias `Preenchido` ou `Sintético` da MLT e vazões ≤ 0 saem.
2. **Estação tem prioridade:** em cada dia vale a estação; sem ela, a vazão ajustada; sem
   nenhuma das duas, a MLT, **sem ajuste de escala**.
3. **Horizonte:** a base começa em **ago/1975**. Com 10 anos de histórico para prever, os anos
   testados vão de 1985/86 a 2024/25 (40 anos, com 14 El Niño, 14 La Niña e 12 Neutro). Com a
   regra atual, 1980 também deixaria pelo menos 10 anos de cada fase, mas o grupo manteve 1975
   para ter mais anos de teste (27/11/2026).
4. **Usinas:** fica quem tem **pelo menos 8 anos hidrológicos completos**. Saíram BRC,
   Congonhal I/II, Pirambeira e Ribeirão.
5. **Do dia para o mês:** vazão do mês = **soma das vazões diárias**; dia sem medição conta
   como 0.
6. **Meses:** só ficam meses com **pelo menos 20 dias medidos**; meses parciais no início e no
   fim de cada série também saem.

### 2. Previsões (`04_previsoes.R`)

Cada mês de 1985/86 a 2024/25 é previsto só com anos anteriores (backcast):

| Metodologia | Como prevê o mês m do ano T |
|---|---|
| `media_4anos` | média do mês m nos anos T−4 a T−1 (aceita 3 de 4) |
| `mlt` | média do mês m em **todos** os anos desde 1975 até T−1 (mínimo de 10) |
| `lognormal` | **regra da empresa (Plano 2027), sobre a base diária:** em cada mês os dias são ordenados (posição k = k-ésimo dia mais seco); para cada posição juntam-se os anos T−10 a T−1 (mínimo de 8, só meses completos); σ = (ln máx − ln mín)/(2·1,96), μ = ln(média) − σ²/2; dia previsto = exp(μ + σ·Φ⁻¹(k/(n+1))); mês previsto = soma dos dias. Fevereiro usa 28 dias. |

### 3. Fase do ENOS (`03_fase_enos.py`)

- **`fase_noaa` (principal, regra da proposta):** média do ONI em OND/T, NDJ/T e DJF/T+1
  (pico do ENOS e da chuva no Sudeste). El Niño se ≥ +0,5 °C; La Niña se ≤ −0,5 °C; senão,
  Neutro. Exemplos: 1997/98 = +2,33 (El Niño); 2016/17 = −0,43 (Neutro); 2025/26 = −0,50
  (La Niña, no limite). Entre 25 e 27/11 foi testada a regra de episódio da NOAA (5 trimestres
  seguidos); o grupo voltou para a da proposta.
- **`fase_cptec` (sensibilidade):** ano no catálogo do CPTEC → a fase dele; ano fora do
  catálogo até 2017/18 → Neutro; depois disso → vazio. Concorda com a `fase_noaa` em 71% dos
  anos de 1950–2017 (o mesmo número da proposta). O CPTEC é mais conservador: quando discorda,
  quase sempre dá Neutro onde o ONI dá El Niño ou La Niña.

### 4. Tabela do experimento (`05_tabela_experimento.R`)

- Unidade da proposta: **usina × ano hidrológico**, com as 3 metodologias.
- `q_prev` = soma das 12 previsões mensais; `q_obs` = soma dos 12 meses observados.
- Só entra o usina-ano com os 12 meses observados e as 3 metodologias previstas (blocos
  balanceados).
- **Paes Leme:** saíram os 30 anos (1985–2014) em que a série dela é a MLT, que é Henrique
  Portugal × 2,9 e daria o mesmo erro (pseudo-replicação). Ficam os 10 anos da estação própria.

## Colunas

**`tabela_experimento.csv`** (1.023 linhas = 341 usina-anos × 3 metodologias)

| Coluna | Significado |
|---|---|
| `usina`, `ano_hidro`, `periodo` | bloco e ano (ex.: `2000-2001`) |
| `fase_noaa`, `fase_cptec`, `oni_medio` | fase pelo ONI (principal), pelo CPTEC (vazia de 2018/19 em diante) e ONI médio |
| `metodologia` | `media_4anos`, `mlt` ou `lognormal` |
| `q_prev`, `q_obs` | soma anual prevista e observada (m³/s·dia); `q_obs` é igual nas 3 metodologias |
| `e`, `abs_e` | ln(q_obs/q_prev) e \|e\| |
| `fonte_ano` | fontes do observado no ano (`mlt`, `estacao`, `estacao+mlt`...) |
| `meses_incompletos` | meses do ano com 20 a 30 dias medidos |

**`previsoes_mensais.csv`**: `usina`, `ano_hidro`, `mes`, `mes_ano`, `metodologia`, `n_anos`
(anos usados no ajuste), `q_prev`, `q_obs`, `e`.

**`base_vazao_mensal.csv`**: `usina`, `mes_ano`, `ano`, `mes`, `ano_hidro`, `dias_mes`,
`dias_medidos`, `q_soma_m3s_dia` (**a vazão do mês**), `volume_hm3` (soma × 86 400 / 10⁶),
`q_media_m3s` (média dos dias medidos), `completo` (todos os dias medidos) e `fonte`.

**`base_vazao_diaria.csv`**: `usina`, `data`, `ano`, `mes`, `mes_ano`, `ano_hidro`, `q_m3s`
(vazio se o dia não foi medido), `medido`, `fonte` e `codigo` (estação). Tem os mesmos
usina-meses da mensal, e a soma dos dias bate com `q_soma_m3s_dia`.

**`fase_enos_anual.csv`**: `ano_hidro`, `periodo`, `oni_ond`, `oni_ndj`, `oni_djf`,
`oni_medio`, `fase_noaa`, `fase_cptec`, `intensidade_cptec`. Vai de 1950/51 a 2025/26
(`fase_noaa`: 27 El Niño, 26 La Niña, 23 Neutro).

## Usinas

| Usinas | Base de vazão | Fonte | Na tabela do experimento |
|---|---|---|---|
| Glória, Henrique Portugal, Nova Maurício | ago/1975–2026 | MLT; estação desde 2013–2024 | 40 anos |
| Areal, Brito, Furquim, Pacífico Mascarenhas | ago/1975–2026 | MLT; estação desde 2002–2020 | 37–39 anos |
| NPQ II | ago/1975–2025 | MLT; estação desde 2021 | 30 anos (até 2016) |
| Costa Rica | fev/1984–2025 | MLT; estação desde 2002 | 25 anos (2000–2024) |
| Paes Leme | ago/1975–2026 | MLT (= Henrique Portugal); estação desde 2015 | 10 anos (2015–2024) |
| Fumaça, Ituerê, Mello, Funil, Salto, Caboclo, Prazeres | 2014/16–2026 | estação | 0–1 ano (histórico curto) |

Na prática, o fatorial tem **10 usinas** (119 anos El Niño, 118 La Niña e 104 Neutro no total).

## Limitações e análises de sensibilidade

| Limitação | O que fazer na análise |
|---|---|
| **Degrau na troca de fonte:** estação e MLT têm escalas diferentes e a MLT entra sem ajuste. Ex.: Henrique Portugal cai para ~0,64× em 2013; Pacífico sobe ~1,7× em 2020. Afeta mais a MLT, que carrega décadas na escala antiga. | rodar sem Henrique Portugal, Paes Leme, Nova Maurício e Pacífico |
| **MLT descreve outro rio:** em Costa Rica, Furquim e Pacífico a razão estação/MLT varia muito de mês para mês. Só em Brito (e quase em Areal, Glória, Mello e NPQ II) as duas fontes batem. | rodar sem Costa Rica, Furquim e Pacífico |
| **Classificação do ENOS:** depende de quem classifica. | repetir com `fase_cptec` (só até 2017/18) |
| **Escala varia entre usinas** (ex.: Prazeres é medida numa bacia ~7× a da usina). | nada: o erro em log cancela a escala |
| **Dia sem medição = 0** na soma do mês (172 meses da base têm 20–30 dias). | filtrar por `meses_incompletos` |
| **A fase só é conhecida no meio do próprio ano** (ONI de out–fev). | no backcast é só rótulo; um reajuste real exigiria previsão do ENOS |
| **Log-normal da empresa:** o dia mais cheio usa o quantil n/(n+1) e pode passar do máximo já observado. | característica da regra, não erro |

**Pendente:** definir os anos guardados (hold-out) para a etapa 2 da proposta (t pareado).

## Como reproduzir

Os 5 CSVs consolidados saem de **só 3 itens** da pasta do Drive. Testado em 27/11/2026: numa
pasta limpa, só com esses 3 itens, os scripts geraram arquivos **idênticos** aos desta pasta.

**1. Baixar do Drive e salvar em `raw/` com estes nomes** (o nome local importa: nas séries MLT
ele vira o nome da usina):

| No Drive ("Base Dados" / …) | Em `dados/raw/` |
|---|---|
| `05 - Bases Hidrológicas/Série diária MLT (Lucas Horta)/` (19 CSVs) | `serie_mlt_diaria/<usina>.csv`, conforme a tabela abaixo |
| `05 - Bases Hidrológicas/ENOS - Monitoramento Niño 3.4.xlsx` | `ENOS_Monitoramento_Nino34.xlsx` |
| `01 - Histórico das Metodologias/Plano 2026 - Metas aprovadas/Indicadores 2026 - Meta de Geração e Eficácia (jul.25).xlsx` | `base_dados/01_historico/plano_2026/Indicadores_2026_Meta_Geracao_Eficacia_jul25.xlsx` |

Séries MLT: o arquivo `Série Hidrológica - <nome> (<área> km²).csv` vira `<usina>.csv`:

| Nome no Drive | `<usina>.csv` | Nome no Drive | `<usina>.csv` |
|---|---|---|---|
| PCH Areal | `Areal` | PCH Mello | `Mello` |
| CGH BRC | `BRC` | PCH Nova Maurício | `Nova Mauricio` |
| PCH BRITO | `Brito` | Nova Ponte Queimada II | `Nova Ponte Queimada II` |
| PCH CABOCLO | `Caboclo` | Pacífico Mascarenhas | `Pacifico Mascarenhas` |
| CGH Congonhal | `Congonhal` | CGH Paes Leme | `Paes Leme` |
| Costa Rica | `Costa Rica` | CGH PRAZERES | `Prazeres` |
| PCH FUMAÇA | `Fumaca` | PCH SALTO | `Salto` |
| PCH FUNIL | `Funil` | Glória | `Gloria` |
| PCH FURQUIM | `Furquim` | CGH Henrique Portugal | `Henrique Portugal` |
| Ituerê | `Ituere` | | |

**2. Rodar, na raiz do repositório, nesta ordem** (Python 3, só biblioteca padrão; R com o
pacote `data.table`; testado com Python 3.14, R 4.6 e data.table 1.18):

```bash
python3 "Trabalho Final/codigo/00_extrair_estacoes.py"
```

```bash
Rscript "Trabalho Final/codigo/02_base_unica.R"
```

```bash
python3 "Trabalho Final/codigo/03_fase_enos.py"
```

```bash
Rscript "Trabalho Final/codigo/04_previsoes.R"
```

```bash
Rscript "Trabalho Final/codigo/05_tabela_experimento.R"
```

| Script | Lê | Grava |
|---|---|---|
| `00_extrair_estacoes.py` | xlsx do Plano 2026 | `raw/derivados/estacoes_vazao_diaria.csv` |
| `02_base_unica.R` | séries MLT + estações | `base_vazao_mensal.csv`, `base_vazao_diaria.csv` (+ diagnósticos em `raw/derivados/`) |
| `03_fase_enos.py` | xlsx de ENOS | `fase_enos_anual.csv` |
| `04_previsoes.R` | bases mensal e diária | `previsoes_mensais.csv` |
| `05_tabela_experimento.R` | previsões, base mensal e fases | `tabela_experimento.csv` |

As regras (prioridade de fonte, horizonte, cortes, janelas) são parâmetros no topo de cada
script e estão descritas em "Como os dados foram tratados". O `01_backcast.R` é uma versão
antiga e não entra no pipeline.

## Outros arquivos do Drive (não usados no pipeline)

O restante da pasta do Drive foi consultado na exploração e também está em `raw/`, com nomes
sem acento: resumos das estações e da MLT, cadastro de vazão turbinável, análises de ENOS da
empresa, planos 2024 e 2027, a planilha original da log-normal e os scripts da empresa
(`base_dados/06_scripts/`). Ficaram de fora o Memorial de Cálculo 2027 (18 MB), a pasta
"04 - Comparativo" (não compartilhada) e a vazão realizada de 2025–26 citada nos scripts da
empresa.

# Dados do trabalho final

Cópia local da pasta do Google Drive compartilhada pelo líder de dados do grupo
(baixada em 22/11/2026). Os arquivos **não vão para o git** (ver `.gitignore`):
são dados da empresa. Para recriar a pasta, baixe de novo do Drive e renomeie conforme a tabela.

| Arquivo local | Nome no Drive |
|---|---|
| `serie_mlt_diaria/<usina>.csv` | Série diária MLT (Lucas Horta) / `Série Hidrológica - <usina> (<área> km²).csv` |
| `Cadastro_usinas_vazao_turbinavel.xlsx` | Cadastro das usinas - vazões mín. e máx. turbináveis.xlsx |
| `ENOS_Correlacao_com_vazao.xlsx` | ENOS - Correlação com vazão.xlsx |
| `ENOS_Monitoramento_Nino34.xlsx` | ENOS - Monitoramento Niño 3.4.xlsx |
| `ENOS_Vazao_diaria_x_fases.xlsx` | ENOS - Vazão diária x fases.xlsx |
| `Estacoes_Disponibilidade_hidrica.xlsx` | Estações - Disponibilidade hídrica.xlsx |
| `Estacoes_Estatisticas_por_posicao.xlsx` | Estações - Estatísticas por posição.xlsx |
| `MLT_Disponibilidade_hidrica.xlsx` | MLT - Disponibilidade hídrica.xlsx |
| `MLT_Estatisticas_por_posicao.xlsx` | MLT - Estatísticas por posição (média, P05, P95, sigma log).xlsx |
| `Resumo_dados_por_usina.xlsx` | Resumo dos dados por usina.xlsx |

A pasta acima é a "05 - Bases Hidrológicas" da pasta-mãe **"Base Dados"** do João (que tem um
`00 - LEIA-ME.md` próprio). Os arquivos das outras subpastas estão em `base_dados/`:

| Arquivo local | Origem em "Base Dados" | O que tem |
|---|---|---|
| `base_dados/03_plano_2027/Indicadores_Operacionais_Plano_2027.xlsm` | 03 - Plano de Geração 2027 | aba `Dados`: Plano/Forecast/Realizado por usina × mês (MWmed, EVT, indisponibilidades). Realizado 2023–mai/2026 |
| `base_dados/03_plano_2027/Plano_2027_Resumo_Geracao_EVT_Disponibilidade.xlsx` | idem | plano 2027 mensal por usina |
| `base_dados/03_plano_2027/Geracao_Energia_Budget_2027-36.xlsx` | idem | budget 2027–36 |
| `base_dados/01_historico/plano_2024/*` | 01 - Histórico / Plano 2024 | vazão planejada 2024 (m³/s) e projeções v0–v3 × realizado jan–jul/2024 |
| `base_dados/01_historico/plano_2026/*` | 01 - Histórico / Plano 2026 | metas 2026; aba `Vazão_Histórica_Classificada` = vazão diária das estações ranqueada |
| `base_dados/02_lognormal/Calculo_Lognormal_planilha_original.xlsx` | 02 - Metodologia Log-normal | planilha original do cálculo |
| `base_dados/06_scripts/*.py` | 06 - Scripts Python | ETLs e simulação (só leitura; não executar sem revisar) |

Não baixados: `Planejamento 2027 - Memorial de Cálculo.xlsx` (18 MB, acima do limite do conector),
PDF consolidado do plano 2027, pptx de 2023 e o e-mail do Budget. A pasta "04 - Comparativo
Log-normal x MLT", citada no LEIA-ME do João, não foi compartilhada.

## Derivados (gerados por `Trabalho Final/codigo/01_backcast.R`)

- `derivados/backcast_anual.csv`: usina × ano hidrológico (ago–jul) × metodologia
  (`media_4anos`, `mlt`, `lognormal`), com q_prev, q_obs, e = ln(q_obs/q_prev), |e|,
  `pct_original` (fração de dias medidos no ano) e `grupo_ref` (estação de referência).
- `derivados/backcast_mensal.csv`: o mesmo por mês.
- `derivados/estatisticas_posicao_periodo_todo.csv`: estatísticas por posição com o período
  todo, usadas para conferir a lógica contra `MLT_Estatisticas_por_posicao.xlsx`
  (bateu 100% nas 18 usinas com par).

`serie_mlt_diaria/Ituere.csv` = Ituerê.

## Formatos das séries diárias

- 15 arquivos: cabeçalho com `#` (exutório, área, ERV, FARE), separador `,`,
  colunas `Data, Vazao_Exutorio_m3s, Vazao_ERV_m3s, Tipo`. Vão até jul/2026.
- 4 arquivos (Costa Rica, Glória, Nova Ponte Queimada II, Pacífico Mascarenhas): sem
  cabeçalho `#`, separador `;`, colunas `Data; Vazao_Exutorio_m3s; Vazao_VRS_m3s; Origem_VRS;
  Tipo_Registro`. Vão até 31/12/2025.
- `Tipo` / `Tipo_Registro`: `Original` (medido), `Preenchido`, `Sintético`.

## Cuidados antes de usar

- Funil, Salto e Fumaça são a mesma série multiplicada por uma constante; Paes Leme é
  Henrique Portugal × constante. Como o erro é ln(Q_obs/Q_prev), essas usinas dão o mesmo erro.
- BRC, Caboclo, Fumaça, Funil, Prazeres e Salto (ERV 56220900), Mello e Congonhal só têm
  2021–2025 medidos; Ituerê, 2016–2025. O resto da série é preenchido.
- `*_Estatisticas_por_posicao.xlsx` usam o período inteiro: não servem como backcast.
- CPTEC (`ENOS_Monitoramento_Nino34.xlsx` › `CPTEC_INPE_Episodios`) só vai até 2017–18.

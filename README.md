# Planejamento e Análise de Experimentos (EEE933 — UFMG)

Materiais e estudos de caso da disciplina de Planejamento e Análise de Experimentos
do PPGEE-UFMG.

## Estrutura

| Pasta | Conteúdo |
|---|---|
| `Slides/` | Slides das aulas (PDF) |
| `codigos/` | Scripts R de apoio das aulas, com dados em `codigos/data/` |
| `ec1/` | Estudo de Caso 01 — comparação do IMC médio de alunos do PPGEE entre 2016-2 e 2017-2 |
| `Template Relatórios/` | Template R Markdown usado como base para os relatórios |

Na raiz ficam o `plano de ensino.pdf` e a divisão dos grupos (`Grupos-V1.pdf`).

## Estudo de Caso 01

- `ec1/ec1_report.Rmd` — relatório final (fonte)
- `ec1/ec1_report.pdf` — relatório renderizado
- `ec1/processamento_separtado.R` — análise exploratória e testes feitos separadamente
- `ec1/EC01.pdf` — enunciado

## Como gerar os relatórios

Os relatórios são R Markdown. Para renderizar em PDF:

```r
rmarkdown::render("ec1/ec1_report.Rmd", "pdf_document")
```

Também funciona pelo botão **Knit** do RStudio. Requer R, pandoc e uma distribuição
LaTeX (TinyTeX), além do pacote `car`.

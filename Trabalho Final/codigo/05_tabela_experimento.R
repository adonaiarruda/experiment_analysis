# =============================================================================
# Trabalho Final EEE933 — Grupo E
# 05_tabela_experimento.R — tabela do experimento: usina × ano hidrológico × metodologia
#
# Unidade da proposta (10/11/2026): usina × ano hidrológico (ago/T – jul/T+1), que
# recebe as 3 metodologias; a fase do ENOS é atributo do ano.
#   q_prev  = soma das 12 previsões mensais da metodologia (m³/s·dia)
#   q_obs   = soma dos 12 meses observados (m³/s·dia)
#   e       = ln(q_obs / q_prev)   (viés; e < 0 → a metodologia superestimou)
#   abs_e   = |e|                  (acurácia)
# Só entra o usina-ano com os 12 meses observados E as 3 metodologias com previsão
# nos 12 meses (blocos balanceados).
#
# Exclusão (decisão do grupo, 27/11/2026): saem os anos de Paes Leme em que a série é
# a MLT, que é Henrique Portugal × 2,9 (1985–2014): dariam o mesmo erro que HP
# (pseudo-replicação). Paes Leme fica só com os anos da estação própria.
#
# Marcas para análises de sensibilidade:
#   fonte_ano          fontes da vazão observada no ano (estacao, mlt, ...)
#   meses_incompletos  meses do ano com 20 a 30 dias medidos (dia faltante = 0)
#
# Entradas: dados/previsoes_mensais.csv, dados/base_vazao_mensal.csv, dados/fase_enos_anual.csv
# Saída   : dados/tabela_experimento.csv
#
# Uso: Rscript "Trabalho Final/codigo/05_tabela_experimento.R"
# =============================================================================

suppressPackageStartupMessages(library(data.table))

METODOLOGIAS <- c("media_4anos", "mlt", "lognormal")

achar_dir_tf <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- gsub("~+~", " ", sub("^--file=", "", a[grep("^--file=", a)]), fixed = TRUE)
  if (length(f)) return(normalizePath(file.path(dirname(f), "..")))
  if (basename(getwd()) == "codigo") return(normalizePath(".."))
  if (dir.exists("Trabalho Final")) return(normalizePath("Trabalho Final"))
  normalizePath(".")
}
DIR_DADOS <- file.path(achar_dir_tf(), "dados")

prev  <- fread(file.path(DIR_DADOS, "previsoes_mensais.csv"))
base  <- fread(file.path(DIR_DADOS, "base_vazao_mensal.csv"))
fases <- fread(file.path(DIR_DADOS, "fase_enos_anual.csv"))

# ---- 1. Soma anual por usina × ano × metodologia (só meses com observado) ------
anual <- prev[!is.na(q_obs), .(meses = .N, q_prev = sum(q_prev), q_obs = sum(q_obs)),
              by = .(usina, ano_hidro, metodologia)]
anual <- anual[meses == 12L][, meses := NULL]

# ---- 2. Blocos balanceados: usina-ano com as 3 metodologias -------------------
completos <- anual[, .(n = uniqueN(metodologia)), by = .(usina, ano_hidro)][n == length(METODOLOGIAS)]
anual <- anual[completos[, .(usina, ano_hidro)], on = .(usina, ano_hidro)]
# o observado tem de ser o mesmo nas 3 metodologias
stopifnot(anual[, uniqueN(round(q_obs, 6)), by = .(usina, ano_hidro)][, all(V1 == 1L)])

anual[, `:=`(e = log(q_obs / q_prev))][, abs_e := abs(e)]

# ---- 3. Fase do ENOS e marcas ---------------------------------------------------
anual <- merge(anual, fases[, .(ano_hidro, periodo, oni_medio, fase_noaa, fase_cptec)], by = "ano_hidro")
anual[fase_cptec == "", fase_cptec := NA_character_]          # catálogo do CPTEC acaba em 2017/18
marcas <- base[, .(fonte_ano = paste(sort(unique(unlist(strsplit(fonte, "+", fixed = TRUE)))), collapse = "+"),
                   meses_incompletos = sum(!completo)),
               by = .(usina, ano_hidro)]
anual <- merge(anual, marcas, by = c("usina", "ano_hidro"), all.x = TRUE)
# Paes Leme: tira os anos em que a série é a cópia de Henrique Portugal (só MLT)
copia <- anual[usina == "Paes Leme" & fonte_ano == "mlt", unique(ano_hidro)]
anual <- anual[!(usina == "Paes Leme" & ano_hidro %in% copia)]
completos <- completos[!(usina == "Paes Leme" & ano_hidro %in% copia)]

anual[, metodologia := factor(metodologia, levels = METODOLOGIAS)]
setcolorder(anual, c("usina", "ano_hidro", "periodo", "fase_noaa", "fase_cptec", "oni_medio", "metodologia",
                     "q_prev", "q_obs", "e", "abs_e", "fonte_ano", "meses_incompletos"))
setorder(anual, usina, ano_hidro, metodologia)
fwrite(anual, file.path(DIR_DADOS, "tabela_experimento.csv"))

# ---- Relatório ----------------------------------------------------------------
cat(sprintf("tabela_experimento: %d linhas = %d usina-anos × %d metodologias\n",
            nrow(anual), nrow(completos), length(METODOLOGIAS)))
cat("\nUsina-anos por usina e fase:\n")
res <- dcast(unique(anual[, .(usina, ano_hidro, fase_noaa)])[, .N, by = .(usina, fase_noaa)],
             usina ~ fase_noaa, value.var = "N", fill = 0L)
res[, total := rowSums(.SD), .SDcols = -1]
print(res[order(-total)], class = FALSE)
cat(sprintf("\nPaes Leme: %d anos copiados de Henrique Portugal removidos (%d–%d)\n",
            length(copia), min(copia), max(copia)))
cat(sprintf("Usina-anos com algum mês incompleto: %d\n",
            unique(anual[, .(usina, ano_hidro, meses_incompletos)])[meses_incompletos > 0, .N]))

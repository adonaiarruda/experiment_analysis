# =============================================================================
# Trabalho Final EEE933 — Grupo E
# 04_previsoes.R — previsão mensal de vazão pelas 3 metodologias (backcast)
#
# Para cada usina, ano hidrológico T testado (ago/T – jul/T+1) e mês m, prevê a
# vazão do mês usando SÓ anos hidrológicos anteriores a T.
# Variável: q_soma_m3s_dia (soma das vazões diárias do mês) da base mensal.
#
# Metodologias (uma a uma, decididas com o grupo):
#   media_4anos  média do mesmo mês nos 4 anos hidrológicos imediatamente
#                anteriores (T−4 a T−1); aceita 3 de 4 se faltar um mês.  [26/11/2026]
#   mlt          média do mesmo mês em TODOS os anos hidrológicos anteriores a T
#                disponíveis na base (desde 1975, janela crescente); mínimo de
#                10 anos com dado.                                         [26/11/2026]
#   lognormal    regra da empresa (Plano 2027), sobre a base DIÁRIA:     [27/11/2026]
#                  1. em cada mês, ordena os dias: posição k = k-ésimo menor dia;
#                  2. para (mês, k), junta o k-ésimo dia de cada ano da janela
#                     T−10..T−1 (mín. 8 anos), só com meses COMPLETOS;
#                  3. σ = (ln máx − ln mín)/(2·1,96); μ = ln(média) − σ²/2;
#                  4. dia da posição k = exp(μ + σ·Φ⁻¹(k/(n+1))), n = dias do mês
#                     (fevereiro = 28; o 29º dia de fevereiro bissexto fica fora);
#                  5. previsão do mês = soma dos n dias previstos.
#
# Erro: e = ln(q_obs / q_prev)  (e < 0 → a previsão superestimou).
#
# Entrada: dados/base_vazao_mensal.csv e dados/base_vazao_diaria.csv
# Saída  : dados/previsoes_mensais.csv
#          usina, ano_hidro, mes, mes_ano, metodologia, n_anos, q_prev, q_obs, e
#
# Uso: Rscript "Trabalho Final/codigo/04_previsoes.R"
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- Parâmetros ---------------------------------------------------------------
ANOS_TESTE     <- 1985:2024   # anos hidrológicos que recebem previsão
JANELA_MEDIA   <- 4L          # anos da média simples
MIN_ANOS_MEDIA <- 3L          # mínimo de anos com dado dentro da janela
MLT_MIN_ANOS   <- 10L         # mínimo de anos de histórico para a MLT
JANELA_LN      <- 10L         # anos da janela móvel da log-normal
MIN_ANOS_LN    <- 8L          # mínimo de anos (meses completos) na janela
Z95            <- qnorm(0.975)
N_DIAS_MES     <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

# ---- Caminhos -----------------------------------------------------------------
achar_dir_tf <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- gsub("~+~", " ", sub("^--file=", "", a[grep("^--file=", a)]), fixed = TRUE)
  if (length(f)) return(normalizePath(file.path(dirname(f), "..")))
  if (basename(getwd()) == "codigo") return(normalizePath(".."))
  if (dir.exists("Trabalho Final")) return(normalizePath("Trabalho Final"))
  normalizePath(".")
}
DIR_DADOS <- file.path(achar_dir_tf(), "dados")

base <- fread(file.path(DIR_DADOS, "base_vazao_mensal.csv"))
obs  <- base[, .(usina, ano_hidro, mes, q = q_soma_m3s_dia)]

# grade de previsão: toda usina × ano testado × mês
grade <- CJ(usina = unique(obs$usina), ano_hidro = ANOS_TESTE, mes = 1:12)

# ---- Metodologia 1: média dos 4 anos anteriores -------------------------------
# Para (usina, T, m): junta o mês m dos anos T−4..T−1 e tira a média.
janela <- grade[, .(ano_janela = ano_hidro - seq_len(JANELA_MEDIA)), by = .(usina, ano_hidro, mes)]
janela <- merge(janela, obs, by.x = c("usina", "ano_janela", "mes"),
                by.y = c("usina", "ano_hidro", "mes"))
media4 <- janela[, .(n_anos = .N, q_prev = mean(q)), by = .(usina, ano_hidro, mes)]
media4 <- media4[n_anos >= MIN_ANOS_MEDIA][, metodologia := "media_4anos"]

# ---- Metodologia 2: MLT (todos os anos anteriores) ----------------------------
# Para (usina, T, m): junta o mês m de todos os anos da base com ano < T.
hist <- merge(grade, obs[, .(usina, mes, ano_obs = ano_hidro, q)], by = c("usina", "mes"),
              allow.cartesian = TRUE)[ano_obs < ano_hidro]
mlt <- hist[, .(n_anos = .N, q_prev = mean(q)), by = .(usina, ano_hidro, mes)]
mlt <- mlt[n_anos >= MLT_MIN_ANOS][, metodologia := "mlt"]

# ---- Metodologia 3: log-normal por posição (regra da empresa) -----------------
diaria <- fread(file.path(DIR_DADOS, "base_vazao_diaria.csv"))
completos <- base[completo == TRUE, .(usina, ano, mes)]
d <- diaria[medido == TRUE][completos, on = .(usina, ano, mes), nomatch = 0L]
setorder(d, usina, ano, mes, q_m3s)
d[, pos := seq_len(.N), by = .(usina, ano, mes)]          # 1 = dia mais seco do mês
d <- d[pos <= N_DIAS_MES[mes]]                              # fev bissexto: 29º dia fora

# janela T−10..T−1 de cada previsão, por join de intervalo
gj <- grade[, .(usina, mes, ano_prev = ano_hidro, ini = ano_hidro - JANELA_LN, fim = ano_hidro - 1L)]
jan <- d[gj, on = .(usina, mes, ano_hidro >= ini, ano_hidro <= fim), nomatch = 0L,
         .(usina, mes, ano_prev, pos, q = x.q_m3s, ano_obs = x.ano_hidro)]
st <- jan[, .(media = mean(q), minimo = min(q), maximo = max(q), n_anos = .N),
          by = .(usina, ano_prev, mes, pos)]
st <- st[n_anos >= MIN_ANOS_LN]
st[, s   := (log(maximo) - log(minimo)) / (2 * Z95)]
st[, mu  := log(media) - s^2 / 2]
st[, q_k := exp(mu + s * qnorm(pos / (N_DIAS_MES[mes] + 1L)))]
lognormal <- st[, .(n_pos = .N, n_anos = min(n_anos), q_prev = sum(q_k)), by = .(usina, ano_prev, mes)]
lognormal <- lognormal[n_pos == N_DIAS_MES[mes]][, n_pos := NULL]   # todas as posições do mês
setnames(lognormal, "ano_prev", "ano_hidro")
lognormal[, metodologia := "lognormal"]

# ---- Junta as metodologias e o observado --------------------------------------
prev <- rbindlist(list(media4, mlt, lognormal), use.names = TRUE)
prev <- merge(prev, obs, by = c("usina", "ano_hidro", "mes"), all.x = TRUE)
setnames(prev, "q", "q_obs")
prev[, `:=`(mes_ano = sprintf("%d-%02d", fifelse(mes >= 8L, ano_hidro, ano_hidro + 1L), mes),
            e       = log(q_obs / q_prev))]
setcolorder(prev, c("usina", "ano_hidro", "mes", "mes_ano", "metodologia", "n_anos",
                    "q_prev", "q_obs", "e"))
setorder(prev, usina, metodologia, ano_hidro, mes)
fwrite(prev, file.path(DIR_DADOS, "previsoes_mensais.csv"))

# ---- Relatório ----------------------------------------------------------------
cat(sprintf("Previsões gravadas: %d linhas\n", nrow(prev)))
print(prev[, .(meses_previstos = .N, com_observado = sum(!is.na(q_obs)),
               de = min(mes_ano), ate = max(mes_ano)), by = .(metodologia, usina)],
      nrows = 100, class = FALSE)

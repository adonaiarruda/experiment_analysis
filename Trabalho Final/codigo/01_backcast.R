# =============================================================================
# Trabalho Final EEE933 — Grupo E
# 01_backcast.R — backcast das 3 metodologias de previsão de vazão
#
# Para cada usina e ano hidrológico T (ago/T a jul/T+1) prevê a vazão usando
# SÓ anos hidrológicos anteriores a T ("só no passado"), por três metodologias:
#
#   media_4anos  média da vazão diária dos 4 anos hidrológicos anteriores
#                (ideia do Plano 2024, que usou a média 2018–2021)
#   mlt          média de todo o histórico anterior a T (janela crescente)
#   lognormal    log-normal por posição no mês (Plano 2027), janela móvel:
#                  - posição k = k-ésimo menor dia do mês;
#                  - para cada mês e posição, junta o k-ésimo dia de cada ano
#                    da janela: média, mínimo e máximo;
#                  - s  = (ln máx − ln mín) / (2 · 1,96)
#                  - mu = ln(média) − s²/2
#                  - previsão da posição k = exp(mu + s · Φ⁻¹(k / (n + 1))),
#                    n = dias do mês (fevereiro = 28)
#                Regra lida da aba "Param.Distrib. e Vazao Prevista" do Plano
#                2026 e das planilhas "Estatísticas por posição". Falta
#                confirmar no etl-previsao.py se o Plano 2027 usou a mesma.
#
# O observado vem da mesma série (exutório), sem conversão de escala.
# Resposta: e = ln(Q_obs / Q_prev)  (e < 0 → a metodologia superestimou).
#
# Entrada : Trabalho Final/dados/serie_mlt_diaria/*.csv
# Saídas  : Trabalho Final/dados/derivados/backcast_mensal.csv
#           Trabalho Final/dados/derivados/backcast_anual.csv
#           Trabalho Final/dados/derivados/estatisticas_posicao_periodo_todo.csv
#             (só para validar a lógica de posições contra a planilha da empresa)
# Os arquivos de dados/ não vão para o git.
#
# Uso: Rscript "Trabalho Final/codigo/01_backcast.R"   (ou source() no RStudio)
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- Parâmetros ---------------------------------------------------------------
JANELA_MEDIA  <- 4L    # anos da média simples
JANELA_LN     <- 10L   # anos da janela móvel da log-normal
MLT_MIN_ANOS  <- 10L   # mínimo de anos para a MLT (abaixo disso ela ≈ média curta)
MIN_DIAS_MES  <- 20L   # mês válido precisa de >= 20 dias com vazão > 0
Z95           <- qnorm(0.975)
N_DIAS_MES    <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

# ---- Caminhos -----------------------------------------------------------------
achar_dir_tf <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- gsub("~+~", " ", sub("^--file=", "", a[grep("^--file=", a)]), fixed = TRUE)  # Rscript codifica espaços
  if (length(f)) return(normalizePath(file.path(dirname(f), "..")))
  if (basename(getwd()) == "codigo") return(normalizePath(".."))
  if (dir.exists("Trabalho Final")) return(normalizePath("Trabalho Final"))
  normalizePath(".")
}
DIR_TF    <- achar_dir_tf()
DIR_SERIE <- file.path(DIR_TF, "dados", "serie_mlt_diaria")
DIR_SAIDA <- file.path(DIR_TF, "dados", "derivados")
dir.create(DIR_SAIDA, showWarnings = FALSE, recursive = TRUE)

# ---- Leitura das 19 séries (dois formatos) ------------------------------------
le_serie <- function(caminho) {
  linhas <- readLines(caminho, encoding = "UTF-8", warn = FALSE)
  meta   <- grep("^#", linhas, value = TRUE)
  erv    <- grep("ERV \\(", meta, value = TRUE)
  erv    <- if (length(erv)) sub(".*ERV \\(([0-9]+)\\).*", "\\1", erv) else NA_character_
  x      <- fread(text = linhas[!grepl("^#", linhas)], encoding = "UTF-8")
  col_tipo <- intersect(c("Tipo", "Tipo_Registro"), names(x))
  usina  <- tools::file_path_sans_ext(basename(caminho))
  data.table(usina     = usina,
             grupo_ref = if (is.na(erv)) paste0("propria:", usina) else paste0("ERV ", erv),
             data      = as.IDate(x[[1]]),
             q         = as.numeric(x[[2]]),
             tipo      = x[[col_tipo]])
}

arquivos <- list.files(DIR_SERIE, pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(arquivos) > 0)
d <- rbindlist(lapply(arquivos, le_serie))
d[, `:=`(ano = year(data), mes = month(data))]
d[, ano_hidro := fifelse(mes >= 8L, ano, ano - 1L)]
d <- d[!is.na(q) & q > 0]                       # vazão nula/negativa é inválida

# posição do dia dentro do mês (1 = menor vazão)
setorder(d, usina, ano, mes, q)
d[, pos := seq_len(.N), by = .(usina, ano, mes)]

# ---- Validação da lógica de posições (período todo, como a planilha da empresa)
est_todo <- d[, .(media = mean(q), minimo = min(q), maximo = max(q), n_anos = .N),
              by = .(usina, mes, pos)]
est_todo[, sigma_log_minmax := (log(maximo) - log(minimo)) / (2 * Z95)]
fwrite(est_todo, file.path(DIR_SAIDA, "estatisticas_posicao_periodo_todo.csv"))

# ---- Anos hidrológicos completos ---------------------------------------------
mensal <- d[, .(soma_q = sum(q), n_dias = .N,
                n_original = sum(tipo == "Original")),
            by = .(usina, grupo_ref, ano_hidro, ano, mes)]
anos_ok <- mensal[n_dias >= MIN_DIAS_MES, .N, by = .(usina, ano_hidro)][N == 12L]
anos_ok[, N := NULL]
mensal <- mensal[anos_ok, on = .(usina, ano_hidro)]
d_ok   <- d[anos_ok, on = .(usina, ano_hidro)]

# ---- Backcast -----------------------------------------------------------------
media_janela <- function(m_u, anos) {
  m_u[ano_hidro %in% anos, .(q_prev = sum(soma_q) / sum(n_dias)), by = mes]
}

lognormal_janela <- function(d_u, anos) {
  st <- d_u[ano_hidro %in% anos,
            .(media = mean(q), minimo = min(q), maximo = max(q), n = .N),
            by = .(mes, pos)]
  st <- st[pos <= N_DIAS_MES[mes]]
  st[, s  := (log(maximo) - log(minimo)) / (2 * Z95)]
  st[, mu := log(media) - s^2 / 2]
  st[, q_k := exp(mu + s * qnorm(pos / (N_DIAS_MES[mes] + 1L)))]
  st[, .(q_prev = mean(q_k), n_pos = .N), by = mes]
}

resultado <- list()
for (u in sort(unique(anos_ok$usina))) {
  m_u   <- mensal[usina == u]
  d_u   <- d_ok[usina == u]
  anos  <- sort(unique(m_u$ano_hidro))
  for (T in anos) {
    hist <- anos[anos < T]
    if (length(hist) < max(JANELA_LN, MLT_MIN_ANOS, JANELA_MEDIA)) next
    jan_media <- tail(hist, JANELA_MEDIA)
    jan_ln    <- tail(hist, JANELA_LN)
    obs <- m_u[ano_hidro == T, .(grupo_ref, ano, mes, soma_q, n_dias, n_original)]
    prev <- rbindlist(list(
      media_janela(m_u, jan_media)[, `:=`(metodologia = "media_4anos",
                                          janela_ini = min(jan_media), janela_fim = max(jan_media),
                                          n_anos_janela = length(jan_media))],
      media_janela(m_u, hist)[, `:=`(metodologia = "mlt",
                                     janela_ini = min(hist), janela_fim = max(hist),
                                     n_anos_janela = length(hist))],
      lognormal_janela(d_u, jan_ln)[, n_pos := NULL][, `:=`(metodologia = "lognormal",
                                     janela_ini = min(jan_ln), janela_fim = max(jan_ln),
                                     n_anos_janela = length(jan_ln))]
    ))
    resultado[[length(resultado) + 1L]] <-
      merge(obs, prev, by = "mes")[, `:=`(usina = u, ano_hidro = T)]
  }
}
bm <- rbindlist(resultado)
bm[, q_obs := soma_q / n_dias]
bm[, e := log(q_obs / q_prev)]
bm[, rotulo_ano_hidro := sprintf("%d-%02d", ano_hidro, (ano_hidro + 1L) %% 100L)]

# ---- Tabela mensal ------------------------------------------------------------
backcast_mensal <- bm[, .(usina, grupo_ref, ano_hidro, rotulo_ano_hidro, ano, mes,
                          metodologia, q_prev, q_obs, e, n_dias,
                          pct_original = n_original / n_dias,
                          janela_ini, janela_fim, n_anos_janela)]
setorder(backcast_mensal, usina, ano_hidro, metodologia, ano, mes)

# ---- Tabela anual (unidade do fatorial: usina × ano hidrológico) ---------------
backcast_anual <- bm[, .(q_prev = sum(q_prev * n_dias) / sum(n_dias),
                         q_obs  = sum(soma_q) / sum(n_dias),
                         n_dias = sum(n_dias),
                         pct_original = sum(n_original) / sum(n_dias)),
                     by = .(usina, grupo_ref, ano_hidro, rotulo_ano_hidro, metodologia,
                            janela_ini, janela_fim, n_anos_janela)]
backcast_anual[, e := log(q_obs / q_prev)]
backcast_anual[, abs_e := abs(e)]
setcolorder(backcast_anual, c("usina", "grupo_ref", "ano_hidro", "rotulo_ano_hidro",
                              "metodologia", "q_prev", "q_obs", "e", "abs_e"))
setorder(backcast_anual, usina, ano_hidro, metodologia)

fwrite(backcast_mensal, file.path(DIR_SAIDA, "backcast_mensal.csv"))
fwrite(backcast_anual,  file.path(DIR_SAIDA, "backcast_anual.csv"))

# ---- Conferências básicas -----------------------------------------------------
stopifnot(backcast_anual[, all(is.finite(e))])
blocos <- backcast_anual[, .N, by = .(usina, ano_hidro)]
stopifnot(blocos[, all(N == 3L)])                 # todo bloco tem as 3 metodologias
cat(sprintf("backcast_anual: %d linhas (%d usinas × anos hidrológicos, 3 metodologias)\n",
            nrow(backcast_anual), nrow(blocos)))
print(backcast_anual[metodologia == "lognormal",
                     .(anos = .N, primeiro = min(rotulo_ano_hidro), ultimo = max(rotulo_ano_hidro),
                       pct_original_medio = round(mean(pct_original), 2)),
                     keyby = .(usina, grupo_ref)])

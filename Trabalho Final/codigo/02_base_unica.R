# =============================================================================
# Trabalho Final EEE933 — Grupo E
# 02_base_unica.R — base única de vazão diária MEDIDA por usina
#
# Junta as duas famílias de histórico de vazão da base da empresa:
#   mlt      séries diárias longas (Lucas Horta), vazão no exutório da usina,
#            transferida de uma estação de referência (ERV) por razão de áreas.
#            Tipo: Original (medido na ERV), Preenchido, Sintético.
#   estacao  estações da empresa junto à usina (consistido, até 2024) e
#            vazões "ajustadas" das usinas sem estação própria (até mai/2025).
# Só entra dia MEDIDO (Original / Consistido / Ajustada, q > 0). Dia preenchido
# ou sintético vira NA na base final (continua em base_vazao_fontes.csv).
#
# Regras (decididas com o grupo em 22/11/2026):
#   1. Uma fonte principal por usina: a que tem mais anos hidrológicos completos.
#      A outra família só completa os dias sem medição na principal se as duas
#      baterem na sobreposição (razão mediana estação/MLT a ±TOL_EMENDA de 1).
#      Estação e MLT medem em pontos/áreas diferentes: emendar fontes que não
#      batem criaria um degrau na série.
#   2. Usina fica na base se tiver >= MIN_ANOS anos hidrológicos completos
#      (ago/T–jul/T+1, os 12 meses com >= MIN_DIAS_MES dias medidos).
#   3. Paes Leme usa a estação própria: na MLT ela é Henrique Portugal × 2,9.
#   4. Prazeres "ajustada" (aba Vazão_Exceções) é descartada: não bate com a
#      estação 56220900 nem com a MLT.
#
# Entradas: dados/raw/serie_mlt_diaria/*.csv
#           dados/raw/derivados/estacoes_vazao_diaria.csv  (de 00_extrair_estacoes.py)
# Saídas  : dados/base_vazao_mensal.csv           ← BASE ÚNICA MENSAL
#           dados/raw/derivados/base_vazao_diaria.csv
#           dados/raw/derivados/base_usinas.csv            ← escolha e cobertura por usina
#           dados/raw/derivados/base_vazao_fontes.csv      (todas as fontes, sem filtro)
#           dados/raw/derivados/comparacao_estacao_mlt.csv
#           dados/raw/derivados/cobertura_usina_ano.csv
#
# Uso: python3 "Trabalho Final/codigo/00_extrair_estacoes.py"
#      Rscript "Trabalho Final/codigo/02_base_unica.R"
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- Parâmetros ---------------------------------------------------------------
MIN_DIAS_MES <- 20L    # mesmo critério do 01_backcast.R
MIN_ANOS     <- 8L     # anos hidrológicos completos para a usina ficar
TOL_EMENDA   <- 0.06   # |razão mediana − 1| máxima para emendar estação e MLT
FORCAR_FONTE <- c("Paes Leme" = "estacao")
DESCARTAR    <- data.table(usina = "Prazeres", fonte = "estacao_ajustada")

# ---- Caminhos -----------------------------------------------------------------
achar_dir_tf <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- gsub("~+~", " ", sub("^--file=", "", a[grep("^--file=", a)]), fixed = TRUE)
  if (length(f)) return(normalizePath(file.path(dirname(f), "..")))
  if (basename(getwd()) == "codigo") return(normalizePath(".."))
  if (dir.exists("Trabalho Final")) return(normalizePath("Trabalho Final"))
  normalizePath(".")
}
DIR_TF    <- achar_dir_tf()
DIR_SERIE <- file.path(DIR_TF, "dados", "raw", "serie_mlt_diaria")
DIR_DER   <- file.path(DIR_TF, "dados", "raw", "derivados")
ARQ_EST   <- file.path(DIR_DER, "estacoes_vazao_diaria.csv")
if (!file.exists(ARQ_EST)) stop("rode antes: python3 'Trabalho Final/codigo/00_extrair_estacoes.py'")

# ---- MLT (dois formatos de arquivo) -------------------------------------------
le_mlt <- function(caminho) {
  linhas <- readLines(caminho, encoding = "UTF-8", warn = FALSE)
  meta   <- grep("^#", linhas, value = TRUE)
  erv    <- grep("ERV \\(", meta, value = TRUE)
  erv    <- if (length(erv)) sub(".*ERV \\(([0-9]+)\\).*", "\\1", erv) else NA_character_
  x      <- fread(text = linhas[!grepl("^#", linhas)], encoding = "UTF-8", colClasses = "character")
  tipo   <- x[[intersect(c("Tipo", "Tipo_Registro"), names(x))]]
  codigo <- if ("Origem_VRS" %in% names(x)) x$Origem_VRS else erv
  data.table(usina  = tools::file_path_sans_ext(basename(caminho)),
             data   = as.IDate(x[[1]]),
             fonte  = "mlt",
             codigo = codigo,
             tipo   = tipo,
             q      = as.numeric(x[[2]]))
}
mlt <- rbindlist(lapply(list.files(DIR_SERIE, "\\.csv$", full.names = TRUE), le_mlt))

# ---- Estações -----------------------------------------------------------------
# Mapa código → usina: abas "Planilha6" e "estacao" da planilha original da log-normal.
# 56220900 é a estação do rio Gualaxo do Sul em Cachoeira dos Prazeres (437 km²),
# a mesma usada como ERV da MLT de BRC/Caboclo/Fumaça/Funil/Prazeres/Salto.
mapa <- data.table(
  estacao = c("58544000", "56097000", "56100450", "58713600", "63000100",
              "56415004", "56415006", "61465500", "58536500", "56194800",
              "56230100", "56212000", "56149000", "56208000", "41713000",
              "58548500", "58918140", "58764800", "56220900"),
  usina   = c("Areal", "Brito", "Brito", "Ituere", "Costa Rica",
              "Nova Ponte Queimada II", "Nova Ponte Queimada II", "Congonhal", "Paes Leme", "Caboclo",
              "Fumaca", "Funil", "Furquim", "Salto", "Pacifico Mascarenhas",
              "Mello", "Gloria", "Nova Mauricio", "Prazeres"))
# Nomes da aba Vazão_Exceções → nome usado nos arquivos MLT
mapa_exc <- c("Henrique Portugal" = "Henrique Portugal", "Cachoeira dos Prazeres" = "Prazeres",
              "Congonhal II" = "Congonhal II", "Pirambeira" = "Pirambeira", "Ribeirão" = "Ribeirao")

e <- fread(ARQ_EST, colClasses = "character", encoding = "UTF-8")
e_hist <- merge(e[aba == "Vazão_Histórica_Classificada"], mapa, by = "estacao", all.x = TRUE)
stopifnot(!anyNA(e_hist$usina))
e_hist <- e_hist[, .(usina, data = as.IDate(data), fonte = "estacao", codigo = estacao,
                     tipo = "Consistido", q = as.numeric(consistido))]
e_exc <- e[aba == "Vazão_Exceções"]
stopifnot(all(e_exc$usina_aba %in% names(mapa_exc)))
e_exc <- e_exc[, .(usina = unname(mapa_exc[usina_aba]), data = as.IDate(data),
                   fonte = "estacao_ajustada", codigo = NA_character_,
                   tipo = "Ajustada", q = as.numeric(revisado))]

# ---- Passo 1: todas as fontes, sem descartar nada -----------------------------
fontes <- rbindlist(list(mlt, e_hist, e_exc))
fontes[, medido := tipo %in% c("Original", "Consistido", "Ajustada") & !is.na(q) & q > 0]
setorder(fontes, usina, fonte, data)
stopifnot(!anyDuplicated(fontes, by = c("usina", "fonte", "data")))
fwrite(fontes, file.path(DIR_DER, "base_vazao_fontes.csv"))

ano_hidro <- function(d) fifelse(month(d) >= 8L, year(d), year(d) - 1L)

# anos hidrológicos completos de uma tabela (chaves..., data) só com dias medidos
anos_completos <- function(x, chaves) {
  m <- x[, .(dias = .N), by = c(chaves, "ah", "mes")][dias >= MIN_DIAS_MES]
  m[, .(meses_ok = .N), by = c(chaves, "ah")][, completo := meses_ok == 12L][]
}
med <- fontes[medido == TRUE]
med[, `:=`(ah = ano_hidro(data), mes = month(data))]

# ---- Passo 2: estação × MLT nos dias em que as duas são medidas ---------------
comp <- merge(med[fonte != "mlt", .(usina, fonte, data, q_est = q)],
              med[fonte == "mlt", .(usina, data, q_mlt = q)], by = c("usina", "data"))
comp <- comp[, {
  r <- q_est / q_mlt
  .(n_dias = .N, de = min(data), ate = max(data),
    razao_med = median(r), razao_p10 = quantile(r, 0.10), razao_p90 = quantile(r, 0.90),
    cor_log = cor(log(q_est), log(q_mlt)))
}, by = .(usina, fonte)]
fwrite(comp, file.path(DIR_DER, "comparacao_estacao_mlt.csv"))

# ---- Passo 3: cobertura por usina × fonte × ano hidrológico -------------------
cob <- anos_completos(med, c("usina", "fonte"))
setnames(cob, "ah", "ano_hidro")
fwrite(cob, file.path(DIR_DER, "cobertura_usina_ano.csv"))

# ---- Passo 4: fonte principal e complemento por usina -------------------------
cand <- cob[completo == TRUE, .(n_anos = .N), by = .(usina, fonte)][!DESCARTAR, on = .(usina, fonte)]
cand[, pref := fifelse(fonte == "mlt", 1L, 2L)]          # empate → MLT (série mais longa)
setorder(cand, usina, -n_anos, pref)
esc <- cand[, .(principal = fonte[1]), by = usina]
esc[usina %in% names(FORCAR_FONTE), principal := FORCAR_FONTE[usina]]
esc <- merge(data.table(usina = sort(unique(fontes$usina))), esc, by = "usina", all.x = TRUE)

# complemento = a outra família, se bater na sobreposição
compat <- comp[!DESCARTAR, on = .(usina, fonte)]
esc <- merge(esc, compat[, .(usina, fonte_est = fonte, razao_med, cor_log)], by = "usina", all.x = TRUE)
esc[, complemento := fcase(
  is.na(principal) | is.na(razao_med),                   NA_character_,
  abs(razao_med - 1) > TOL_EMENDA,                       NA_character_,
  principal == "mlt",                                    fonte_est,
  default =                                              "mlt")]
esc[usina %in% names(FORCAR_FONTE), complemento := NA_character_]

# ---- Passo 5: série final (principal; complemento só nos dias sem principal) --
pega <- function(col, ordem) {
  x <- esc[!is.na(get(col)), .(usina, fonte = get(col))]
  med[x, on = .(usina, fonte), nomatch = 0L, .(usina, data, q, fonte, codigo, ordem = ordem)]
}
serie <- rbind(pega("principal", 1L), pega("complemento", 2L))
setorder(serie, usina, data, ordem)
serie <- unique(serie, by = c("usina", "data"))[, ordem := NULL]

# anos completos da série final e corte
serie[, `:=`(ah = ano_hidro(data), mes = month(data))]
cob_final <- anos_completos(serie, "usina")
resumo <- cob_final[completo == TRUE, .(anos_completos = .N,
                                        primeiro_ano = min(ah), ultimo_ano = max(ah)), by = usina]
esc <- merge(esc, resumo, by = "usina", all.x = TRUE)
esc[is.na(anos_completos), anos_completos := 0L]
esc[, incluida := anos_completos >= MIN_ANOS]
esc[, dias_complemento := serie[esc, on = "usina", sum(fonte == complemento, na.rm = TRUE), by = .EACHI]$V1]

# calendário completo (dia sem medição = NA) só para as usinas incluídas
base <- serie[usina %in% esc[incluida == TRUE, usina]]
cal  <- base[, .(data = seq(min(data), max(data), by = 1L)), by = usina]
base <- merge(cal, base[, .(usina, data, q, fonte, codigo)], by = c("usina", "data"), all.x = TRUE)
base[, ano_hidro := ano_hidro(data)]
setcolorder(base, c("usina", "data", "ano_hidro", "q", "fonte", "codigo"))
setnames(base, "q", "q_m3s")
fwrite(base, file.path(DIR_DER, "base_vazao_diaria.csv"))

# ---- Passo 5b: base mensal (soma das vazões diárias do mês) -------------------
# q_soma_m3s_dia = Σ vazão diária (m³/s·dia); volume_hm3 = q_soma · 86400 / 1e6.
# Regras (decididas com o grupo em 25/11/2026):
#   - dia sem medição conta como 0 na soma (mês incompleto fica com soma menor);
#   - mês SEM nenhum dia medido fica NA (0 seria um mês seco falso e ln(0) = −∞);
#   - o 1º e o último mês de cada usina saem se forem parciais: ali a série só
#     começa/termina no meio do mês, não é dia faltando.
# dias_medidos, dias_mes, completo (todos os dias) e valido (>= MIN_DIAS_MES) vêm junto.
base_mensal <- base[, .(dias_medidos   = sum(!is.na(q_m3s)),
                        q_soma_m3s_dia = sum(q_m3s, na.rm = TRUE),
                        fonte          = paste(sort(unique(na.omit(fonte))), collapse = "+")),
                    by = .(usina, ano = year(data), mes = month(data))]
base_mensal[, dias_mes := as.integer(format(as.IDate(sprintf("%d-%02d-01", ano, mes)) + 31L -
                                       as.integer(format(as.IDate(sprintf("%d-%02d-01", ano, mes)) + 31L, "%d")),
                                     "%d"))]
base_mensal[, `:=`(mes_ano     = sprintf("%d-%02d", ano, mes),
                   ano_hidro   = fifelse(mes >= 8L, ano, ano - 1L),
                   q_media_m3s = fifelse(dias_medidos > 0, q_soma_m3s_dia / dias_medidos, NA_real_),
                   volume_hm3  = q_soma_m3s_dia * 86400 / 1e6,
                   completo    = dias_medidos == dias_mes,
                   valido      = dias_medidos >= MIN_DIAS_MES)]
base_mensal[dias_medidos == 0L, `:=`(q_soma_m3s_dia = NA_real_, volume_hm3 = NA_real_, fonte = NA_character_)]
setcolorder(base_mensal, c("usina", "mes_ano", "ano", "mes", "ano_hidro", "dias_mes", "dias_medidos",
                           "q_soma_m3s_dia", "volume_hm3", "q_media_m3s", "completo", "valido", "fonte"))
setorder(base_mensal, usina, ano, mes)
base_mensal[, ponta := seq_len(.N) %in% c(1L, .N), by = usina]
n_pontas <- base_mensal[ponta & !completo, .N]
base_mensal <- base_mensal[!(ponta & !completo)][, ponta := NULL]
cat(sprintf("Base mensal: %d meses parciais nas pontas das séries removidos\n", n_pontas))
fwrite(base_mensal, file.path(DIR_TF, "dados", "base_vazao_mensal.csv"))

# ---- Checagem: alguma usina incluída é cópia escalada de outra? ---------------
inc <- esc[incluida == TRUE, usina]
pares <- CJ(a = inc, b = inc)[a < b]
dup <- pares[, {
  m <- merge(base[usina == a & !is.na(q_m3s), .(data, qa = q_m3s)],
             base[usina == b & !is.na(q_m3s), .(data, qb = q_m3s)], by = "data")
  r <- m$qb / m$qa
  .(n = nrow(m), dispersao = if (nrow(m) > 30) unname(quantile(r, .9) / quantile(r, .1)) else NA_real_,
    cor_log = if (nrow(m) > 30) cor(log(m$qa), log(m$qb)) else NA_real_)
}, by = .(a, b)]
esc[, copia_de := NA_character_]
for (i in which(dup$dispersao < 1.01)) esc[usina == dup$b[i], copia_de := dup$a[i]]

fwrite(esc[, .(usina, incluida, principal, complemento, razao_est_mlt = round(razao_med, 3),
               cor_log_est_mlt = round(cor_log, 3), dias_complemento, anos_completos,
               primeiro_ano, ultimo_ano, copia_de)],
       file.path(DIR_DER, "base_usinas.csv"))

# ---- Relatório no console -----------------------------------------------------
faixas <- function(anos) {
  if (!length(anos)) return("-")
  anos <- sort(anos); g <- cumsum(c(1, diff(anos) != 1))
  paste(tapply(anos, g, function(a) if (length(a) == 1) a else paste0(a[1], "-", a[length(a)])),
        collapse = ",")
}
anos_txt <- cob_final[completo == TRUE, .(anos = faixas(ah)), by = usina]
rel <- merge(esc, anos_txt, by = "usina", all.x = TRUE)
setorder(rel, -incluida, -anos_completos)
cat("\n== Base única: escolha por usina ==\n")
print(rel[, .(usina, incluida, principal, complemento, razao = round(razao_med, 3),
              dias_compl = dias_complemento, n_anos = anos_completos, anos, copia_de)],
      nrows = 100, class = FALSE)
cat(sprintf("\nUsinas na base: %d | linhas: %d | dias medidos: %d (%.1f%%)\n",
            uniqueN(base$usina), nrow(base), base[!is.na(q_m3s), .N], 100 * base[, mean(!is.na(q_m3s))]))
cat("Pares com correlação log > 0,95 (parecidos, mas não cópias):\n")
print(dup[cor_log > 0.95, .(a, b, n, dispersao = round(dispersao, 3), cor_log = round(cor_log, 3))])

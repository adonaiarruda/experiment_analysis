# clean workspace
rm(list = ls())


# install required packages if needed
if (!("ExpDE" %in% rownames(installed.packages()))){
  install.packages("ExpDE")
}

if (!("smoof" %in% rownames(installed.packages()))){
  install.packages("smoof")
}

#====================================================

suppressWarnings(suppressPackageStartupMessages(library(smoof)))
suppressPackageStartupMessages(library(ExpDE))


# ---------------------------------------------------------------------------
# 1. Configurações (Equipe E)
# ---------------------------------------------------------------------------
## Config 1
recpars1 <- list(name = "recombination_lbga")
mutpars1 <- list(name = "mutation_rand", f = 4.5)
## Config 2
recpars2 <- list(name = "recombination_blxAlphaBeta", alpha = 0.1, beta = 0.4)
mutpars2 <- list(name = "mutation_rand", f = 3)

config <- list(list(rec = recpars1, mut = mutpars1),
               list(rec = recpars2, mut = mutpars2))

selpars <- list(name = "selection_standard")


# ---------------------------------------------------------------------------
# 2. Função-problema (Rosenbrock) vetorizada e execução do ExpDE
#    (a função precisa se chamar 'fn' e ficar no ambiente global, como no
#    enunciado)
# ---------------------------------------------------------------------------
set_instance <- function(dim) {
  assign("fn", function(X) {
    if (!is.matrix(X)) X <- matrix(X, nrow = 1)
    n  <- ncol(X)
    xa <- X[, -n, drop = FALSE]
    xb <- X[, -1, drop = FALSE]
    rowSums(100 * (xa^2 - xb)^2 + (xa - 1)^2)
  }, envir = .GlobalEnv)
}

# Verificação: a versão vetorizada deve coincidir com a do smoof
set_instance(10)
Xchk <- matrix(runif(20, -5, 10), nrow = 2)
f_smoof <- smoof::makeRosenbrockFunction(dimensions = 10)
stopifnot(isTRUE(all.equal(as.numeric(fn(Xchk)),
                           as.numeric(apply(Xchk, 1, f_smoof)))))

# Uma execução do DE na instância 'dim' com a configuração 'cfg': devolve Fbest
run_de <- function(dim, cfg) {
  out <- ExpDE(mutpars  = cfg$mut,
               recpars  = cfg$rec,
               popsize  = 5 * dim,
               selpars  = selpars,
               stopcrit = list(names = "stop_maxeval",
                               maxevals = 5000 * dim,
                               maxiter  = 100 * dim),
               probpars = list(name = "fn",
                               xmin = rep(-5, dim), xmax = rep(10, dim)),
               showpars = list(show.iters = "none"))
  out$Fbest
}


# ---------------------------------------------------------------------------
# 3. Algoritmo 1 (Campelo & Takahashi, 2019): amostragem em UMA instância
#
# Roda as duas configs (n0 vezes cada) e depois acrescenta uma execução por
# vez à config que está abaixo da razão ótima r_opt = s1/s2 (Eq. 11), até que
# o erro padrão de psi_j fique <= se_star ou n1 + n2 atinja nmax.
# Diferença: psi_j = mean(transf(Y2)) - mean(transf(Y1)).
#
# 'semente': cada instância usa sua própria semente, então o resultado de uma
# dimensão não depende de quais outras foram rodadas antes (permite rodar as
# instâncias em partes e em qualquer ordem).
# ---------------------------------------------------------------------------
algoritmo1 <- function(dim, se_star, n0 = 2, nmax = 200, transf = log,
                       semente = 1000 + dim) {
  set.seed(semente)
  set_instance(dim)

  se_fun <- function(x1, x2) {
    sqrt(var(transf(x1)) / length(x1) + var(transf(x2)) / length(x2))   # Eq. (12)
  }

  x1 <- replicate(n0, run_de(dim, config[[1]]))
  x2 <- replicate(n0, run_de(dim, config[[2]]))
  se <- se_fun(x1, x2)

  while (isTRUE(se > se_star) && (length(x1) + length(x2)) < nmax) {
    r_opt <- sd(transf(x1)) / sd(transf(x2))          # Eq. (11)
    if (length(x1) / length(x2) < r_opt) {
      x1 <- c(x1, run_de(dim, config[[1]]))
    } else {
      x2 <- c(x2, run_de(dim, config[[2]]))
    }
    se <- se_fun(x1, x2)
  }

  list(
    resumo = data.frame(dim = dim,
                        psi = mean(transf(x2)) - mean(transf(x1)),
                        se = se, n1 = length(x1), n2 = length(x2),
                        bateu_nmax = isTRUE(se > se_star)),
    brutos = rbind(
      data.frame(dim = dim, config = 1, rep = seq_along(x1), Fbest = x1),
      data.frame(dim = dim, config = 2, rep = seq_along(x2), Fbest = x2)),
    params = list(se_star = se_star, n0 = n0, nmax = nmax)
  )
}


# ---------------------------------------------------------------------------
# 4. Sorteio das dimensões (fixo e salvo em arquivo)
#    As dimensões do experimento são sorteadas PRIMEIRO e as do piloto depois,
#    a partir das restantes. Assim, o sorteio do experimento não depende de o
#    piloto ter sido rodado ou não.
# ---------------------------------------------------------------------------
calc_N_star <- function(d_star = 0.5, alpha = 0.05, potencia = 0.8) {
  ceiling(power.t.test(delta = d_star, sd = 1, sig.level = alpha,
                       power = potencia, type = "paired",
                       alternative = "two.sided")$n)             # Eq. (23)
}

sortear_dims <- function(N_inst, instancias = 2:150, semente = 2026,
                         arquivo = "dims_ec03.csv") {
  if (file.exists(arquivo)) {
    dims <- read.csv(arquivo)$dim
    if (length(dims) != N_inst) {
      stop("O arquivo ", arquivo, " tem ", length(dims), " dimensões, mas N_inst = ",
           N_inst, ". Apague o arquivo (e a pasta de resultados) para sortear de novo.")
    }
    return(dims)
  }
  set.seed(semente)
  dims <- sort(sample(instancias, size = N_inst, replace = FALSE))
  write.csv(data.frame(dim = dims), arquivo, row.names = FALSE)
  dims
}


# ---------------------------------------------------------------------------
# 5. Piloto: estima sigma_Phi e define se_star (Seção 4.6 do artigo):
#    (se_star)^2 <= 0.1 * sigma_Phi^2  =>  se_star = sqrt(0.1) * sigma_Phi
#
# O resultado é salvo em 'arquivo'. Se o arquivo já existir, o piloto NÃO é
# rodado de novo: o se_star é recalculado a partir dos psi salvos.
# Também é possível informar o se_star diretamente (se_star_manual).
# ---------------------------------------------------------------------------
obter_se_star <- function(dims_piloto, n_rep = 15, transf = log,
                          arquivo = "piloto_ec03.csv", forcar = FALSE,
                          se_star_manual = NULL) {

  if (!is.null(se_star_manual)) {
    cat(sprintf("[piloto] usando se_star informado: %.4f\n", se_star_manual))
    return(list(se_star = se_star_manual, sigma_phi = NA, dims = NA, psi = NA))
  }

  if (file.exists(arquivo) && !forcar) {
    p <- read.csv(arquivo)
    cat(sprintf("[piloto] lido de '%s' (%d dimensões); piloto NÃO foi rodado de novo\n",
                arquivo, nrow(p)))
  } else {
    psi <- numeric(length(dims_piloto))
    for (k in seq_along(dims_piloto)) {
      d <- dims_piloto[k]
      set.seed(5000 + d)
      set_instance(d)
      y1 <- transf(replicate(n_rep, run_de(d, config[[1]])))
      y2 <- transf(replicate(n_rep, run_de(d, config[[2]])))
      psi[k] <- mean(y2) - mean(y1)
      cat(sprintf("[piloto] dim = %3d (%d/%d) psi = %.3f\n",
                  d, k, length(dims_piloto), psi[k]))
      flush.console()
    }
    p <- data.frame(dim = dims_piloto, psi = psi)
    write.csv(p, arquivo, row.names = FALSE)
  }

  sigma_phi <- sd(p$psi)
  list(se_star = sqrt(0.1) * sigma_phi, sigma_phi = sigma_phi,
       dims = p$dim, psi = p$psi)
}


# ---------------------------------------------------------------------------
# 6. Algoritmo 2 (Campelo & Takahashi, 2019): procedimento completo
#
# 1. Calcula N* (teste t pareado, Eq. 23).
# 2. Verifica se as instâncias sorteadas (dims) são suficientes.
# 3. Aplica o Algoritmo 1 em cada instância e obtém psi_j.
#    Cada instância concluída é salva em 'pasta' (um .rds por dimensão).
#    Se o script for interrompido ou rodado em partes, as instâncias já
#    concluídas são lidas da pasta e NÃO são recalculadas.
# 4. Quando todas as instâncias estiverem prontas: teste t sobre os psi_j
#    e verificação da normalidade.
#
# dims_rodar: subconjunto das dimensões a rodar nesta chamada (padrão: todas).
# ---------------------------------------------------------------------------
algoritmo2 <- function(se_star, dims, dims_rodar = dims,
                       pasta = "resultados_ec03",
                       d_star = 0.5, alpha = 0.05, potencia = 0.8,
                       n0 = 2, nmax = 200, transf = log) {

  N_star <- calc_N_star(d_star, alpha, potencia)
  if (length(dims) < N_star) {
    warning("Foram sorteadas ", length(dims), " instâncias, menos que N* = ", N_star)
  }

  dir.create(pasta, showWarnings = FALSE)
  arq_dim <- function(d) file.path(pasta, sprintf("dim_%03d.rds", d))

  # Algoritmo 1 em cada instância pedida (pulando as já concluídas)
  for (k in seq_along(dims_rodar)) {
    d <- dims_rodar[k]
    if (file.exists(arq_dim(d))) {
      p <- readRDS(arq_dim(d))$params
      if (!isTRUE(all.equal(unlist(p), c(se_star = se_star, n0 = n0, nmax = nmax)))) {
        stop("dim = ", d, " foi rodada com outros parâmetros (se_star, n0, nmax). ",
             "Apague a pasta '", pasta, "' para recomeçar.")
      }
      cat(sprintf("[exp] dim = %3d já concluída, pulando\n", d))
      next
    }
    t0 <- Sys.time()
    r  <- algoritmo1(d, se_star, n0 = n0, nmax = nmax, transf = transf)
    saveRDS(r, arq_dim(d))
    cat(sprintf("[exp] dim = %3d (%d/%d) n1 = %3d n2 = %3d | %.1f s\n",
                d, k, length(dims_rodar), r$resumo$n1, r$resumo$n2,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    flush.console()
  }

  # Faltam instâncias? Então ainda não dá para testar.
  prontas   <- file.exists(sapply(dims, arq_dim))
  pendentes <- dims[!prontas]
  if (length(pendentes) > 0) {
    cat(sprintf("\n%d de %d instâncias concluídas. Pendentes: %s\n",
                sum(prontas), length(dims), paste(pendentes, collapse = ", ")))
    return(invisible(list(pendentes = pendentes, N_star = N_star)))
  }

  # Consolidação
  res    <- lapply(dims, function(d) readRDS(arq_dim(d)))
  tabela <- do.call(rbind, lapply(res, `[[`, "resumo"))
  brutos <- do.call(rbind, lapply(res, `[[`, "brutos"))

  # Teste de hipóteses (bilateral) e premissas
  teste <- t.test(tabela$psi, mu = 0, alternative = "two.sided",
                  conf.level = 1 - alpha)
  shap  <- shapiro.test(tabela$psi)
  qqnorm(tabela$psi, main = "QQ-plot dos psi_j"); qqline(tabela$psi)

  list(pendentes = NULL, N_star = N_star, dims = dims,
       tabela = tabela, brutos = brutos, teste = teste, shapiro = shap)
}


# ===========================================================================
# EXECUÇÃO
# ===========================================================================

# 0. (Opcional) teste de tempo de uma execução. Depois da primeira vez, pode
#    colocar FALSE.
TESTE_TEMPO <- TRUE
if (TESTE_TEMPO) {
  for (d in c(10, 150)) {
    set_instance(d)
    tt <- system.time(run_de(d, config[[1]]))[["elapsed"]]
    cat(sprintf("Uma execução com dim = %3d levou %.1f s\n", d, tt))
  }
}

# 1. Dimensões do experimento (sorteio fixo, salvo em dims_ec03.csv)
#    Use N_inst = 40 ou 54 se precisar da amostra maior (não normalidade).
N_inst   <- calc_N_star()
dims_exp <- sortear_dims(N_inst)

# 2. Piloto -> se_star
#    - Se 'piloto_ec03.csv' existir, o piloto NÃO é rodado de novo.
#    - Para pular o piloto informando o valor: se_star_manual = 0.25 (exemplo)
#    - Para refazer o piloto: forcar = TRUE
set.seed(2026)
dims_piloto <- sort(sample(setdiff(2:150, dims_exp), size = 8))
pil <- obter_se_star(dims_piloto, n_rep = 15)
cat(sprintf("se_star = %.4f\n", pil$se_star))

# 3. Experimento (Algoritmo 2)  
#    Rodar tudo (pula o que já estiver em 'resultados_ec03/'):
exp <- algoritmo2(se_star = pil$se_star, dims = dims_exp, n0 = 2, nmax = 200)

#    Ou rodar só algumas dimensões agora e o resto depois, por exemplo:
# exp <- algoritmo2(pil$se_star, dims_exp, dims_rodar = dims_exp[1:10])
# exp <- algoritmo2(pil$se_star, dims_exp, dims_rodar = dims_exp[11:20])
# ...
# exp <- algoritmo2(pil$se_star, dims_exp)       # consolida e testa quando tudo estiver pronto

# 4. Resultados (só quando todas as instâncias estiverem concluídas)
if (is.null(exp$pendentes)) {
  print(exp$N_star)
  print(exp$tabela)
  print(exp$teste)
  print(exp$shapiro)

  # mu_D está em escala log: converte para variação percentual de Fbest (Config 2 vs 1)
  mu <- exp$teste$estimate
  ic <- exp$teste$conf.int
  cat(sprintf("Variação média de Fbest (config 2 vs 1): %.1f%%  IC95%% [%.1f%%, %.1f%%]\n",
              100 * (exp(mu) - 1), 100 * (exp(ic[1]) - 1), 100 * (exp(ic[2]) - 1)))

  write.csv(exp$brutos, "dados_ec03.csv", row.names = FALSE)          # x1, x2 (Fbest de cada execução)
  write.csv(exp$tabela, "tabela_instancias_ec03.csv", row.names = FALSE)  # dim, psi, se, n1, n2, bateu_nmax
}

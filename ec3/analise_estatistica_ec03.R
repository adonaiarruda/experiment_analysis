###############################################################################
#  EEE933 - Estudo de Caso 03 - Análise estatística pareada (escala log)
#  Config 1 (lbga, F = 4.5) x Config 2 (blxAlphaBeta, F = 3)
#  Base: slides 06 e 9 da disciplina; Campelo & Takahashi (2019).
#  Execute com o diretório de trabalho na pasta ec3.
###############################################################################

# clean workspace
rm(list = ls())

suppressPackageStartupMessages(library(car))   # qqPlot

alpha  <- 0.05   # enunciado
d_star <- 0.5    # enunciado: menor diferença relevante, em desvios padrão


# Passo 1 - Ler os dados (uma linha por execução) e aplicar o log
res <- read.csv("dados_ec03.csv", sep = ";", dec = ".", header = TRUE)
res$log_Fbest <- log(res$Fbest)


# Passo 2 - Média das repetições de cada par dimensão:configuração
aggdata <- aggregate(log_Fbest ~ dim + config, data = res, FUN = mean)


# Passo 3 - Uma linha por dimensão, com as duas configurações lado a lado
aggdata2 <- reshape(aggdata, direction = "wide",
                    idvar = "dim", timevar = "config")
aggdata2 <- aggdata2[order(aggdata2$dim), ]

medias <- data.frame(dim = aggdata2$dim,
                     Y1  = aggdata2$log_Fbest.1,
                     Y2  = aggdata2$log_Fbest.2)
stopifnot(!anyNA(medias))
N <- nrow(medias)

# Conferência com os psi salvos pelo ec03_experimento.R
tab <- read.csv("tabela_instancias_ec03.csv")
stopifnot(identical(tab$dim, medias$dim),
          isTRUE(all.equal(tab$psi, medias$Y2 - medias$Y1, tolerance = 1e-6)))


# Passo 4 - Diferença em cada dimensão (psi > 0: Config 1 melhor)
psi <- medias$Y2 - medias$Y1


# Passo 5 - Teste t pareado (H0: mu_D = 0 x H1: mu_D != 0)
cat("\n== Passo 5: teste t pareado ==\n")
teste_t <- t.test(medias$Y2, medias$Y1, paired = TRUE,
                  alternative = "two.sided", conf.level = 1 - alpha)
print(teste_t)


# Passo 6 - Conferir se o teste é confiável: normalidade, bootstrap,
#           Wilcoxon (exige simetria) e sinal (exige só independência)
cat("\n== Passo 6: conferências ==\n")
print(shapiro.test(psi))

cat(sprintf("média = %.4f | mediana = %.4f | desvio padrão = %.4f -> %s\n",
            mean(psi), median(psi), sd(psi),
            ifelse(abs(mean(psi) - median(psi)) > 0.1 * sd(psi),
                   "indício de assimetria", "indício de simetria")))

set.seed(2026)
media_boot <- replicate(9999, mean(sample(psi, N, replace = TRUE)))
ic_boot    <- quantile(media_boot, c(alpha / 2, 1 - alpha / 2))
cat(sprintf("IC 95%% bootstrap (percentil) para mu_D: [%.4f; %.4f]\n",
            ic_boot[1], ic_boot[2]))

op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
qqPlot(psi, id = list(n = 3, labels = medias$dim), pch = 16, las = 1,
       ylab = "psi_j", main = "psi_j")
qqnorm(media_boot, pch = ".", las = 1, main = "Médias bootstrap")
qqline(media_boot, col = "red", lwd = 2)
par(op)

print(wilcox.test(medias$Y2, medias$Y1, paired = TRUE,
                  alternative = "two.sided", conf.int = TRUE,
                  conf.level = 1 - alpha))

n_sinal <- sum(psi != 0)
r       <- sum(psi > 0)
p_sinal <- min(1, 2 * pbinom(min(r, n_sinal - r), n_sinal, prob = 0.5))
cat(sprintf("Teste do sinal: %d de %d diferenças positivas, p-valor = %.3g\n",
            r, n_sinal, p_sinal))


# Passo 7 - Magnitude: efeito padronizado (x d*) e razão entre os Fbest
cat("\n== Passo 7: magnitude da diferença ==\n")
mu_D  <- unname(teste_t$estimate)
ic_t  <- teste_t$conf.int
d_hat <- mean(psi) / sd(psi)

cat(sprintf("mu_D = %.3f | IC95%% t [%.3f; %.3f] | IC95%% bootstrap [%.3f; %.3f]\n",
            mu_D, ic_t[1], ic_t[2], ic_boot[1], ic_boot[2]))
cat(sprintf("d estimado = %.2f (d* = %.1f)\n", d_hat, d_star))
cat(sprintf("Fbest(Config 2) / Fbest(Config 1) = %.2f | IC95%% [%.2f; %.2f]\n",
            exp(mu_D), exp(ic_t[1]), exp(ic_t[2])))


# Passo 8 - Extras: sem as dimensões atípicas, sem pareamento e potência
cat("\n== Passo 8: extras ==\n")
indx <- order(psi, decreasing = TRUE)[1:3]
cat("Sem as dimensões", paste(sort(medias$dim[indx]), collapse = ", "), "\n")
print(t.test(psi[-indx], mu = 0, conf.level = 1 - alpha))
print(shapiro.test(psi[-indx]))

cat(sprintf("Correlação entre Y1 e Y2: %.3f\n", cor(medias$Y1, medias$Y2)))
print(t.test(medias$Y2, medias$Y1, conf.level = 1 - alpha))   # não pareado

pot <- power.t.test(n = N, delta = d_star, sd = 1, sig.level = alpha,
                    type = "paired", alternative = "two.sided")$power
cat(sprintf("Potência do teste t pareado (N = %d, d* = %.1f): %.3f\n",
            N, d_star, pot))

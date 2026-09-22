###############################################################################
#  EEE933 - Planejamento e Analise de Experimentos  (PPGEE-UFMG)
#  Aula 07 - Dados nao Normais e Metodos nao Parametricos
#  Script de apoio ao deck "9 - Dados nao Normais e Metodos nao Parametricos"
#
#  Conteudo:
#    0. Verificacao de premissas (normalidade e simetria)
#    1. Teste do Sinal
#    2. Teste de Wilcoxon (uma amostra e pareado) + comparacao com o teste t
#    3. Teste da Permutacao (Monte Carlo e exato)
#    4. Teste de Bootstrap sob H0 + Intervalo de Confianca por bootstrap
#
#  Pacotes:
#    BSDA    - teste do sinal (SIGN.test)
#    moments - assimetria e curtose (verificacao de premissas)
#    install.packages(c("BSDA", "moments"))
#    O script roda mesmo sem os pacotes: as chamadas que dependem deles sao
#    omitidas, com aviso no console.
#
#  OBS: rodando via Rscript (nao interativo) os graficos vao para Rplots.pdf,
#       na pasta de trabalho. No RStudio aparecem no painel Plots.
###############################################################################

# clean workspace
rm(list=ls())

#===================
# Pacotes
#===================
has_BSDA    <- requireNamespace("BSDA", quietly = TRUE)
has_moments <- requireNamespace("moments", quietly = TRUE)

if (!has_BSDA) {
  message("Pacote 'BSDA' nao instalado: SIGN.test() sera omitido ",
          "(o teste do sinal segue calculado com pbinom()).\n",
          "  Instale com: install.packages(\"BSDA\")")
}
if (!has_moments) {
  message("Pacote 'moments' nao instalado: assimetria e curtose serao omitidas.\n",
          "  Instale com: install.packages(\"moments\")")
}

#===================
# 0. Verificacao de premissas (normalidade e simetria)
#===================
# Por que verificar: os metodos vistos ate aqui exigem normalidade. O teste do
# sinal exige apenas amostra iid e distribuicao continua; o teste de Wilcoxon
# (postos sinalizados) exige tambem SIMETRIA. Se essas premissas nao se
# sustentam, os metodos nao parametricos desta aula sao a alternativa.
#
# O roteiro abaixo e indicativo, nao decisorio:
#   - histograma, QQ-plot e boxplot: avaliacao visual;
#   - Shapiro-Wilk: teste formal (H0: os dados vem de uma Normal);
#   - assimetria e curtose: indicadores de forma (Normal: 0 e 3);
#   - media x mediana: indicador robusto de simetria (regra pratica: se a
#     diferenca for grande em relacao ao desvio padrao, ha indicio de
#     assimetria).
premissas <- function(x, nome, alpha = 0.05) {
  n <- length(x)
  cat("\n-----------------------------------------------------------------\n")
  cat("Verificacao de premissas:", nome, "\n")
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("n = %d | media = %.4f | mediana = %.4f | desvio padrao = %.4f\n",
              n, mean(x), median(x), sd(x)))
  cat(sprintf("media x mediana: %s\n",
              ifelse(abs(mean(x) - median(x)) > 0.1 * sd(x),
                     "distantes -> indicio de assimetria",
                     "proximas  -> indicio de simetria")))
  # figuras: histograma, QQ-plot e boxplot
  op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  on.exit(par(op), add = TRUE)
  hist(x, main = paste("Histograma\n", nome), col = "gray85", xlab = "valor")
  qqnorm(x, main = paste("QQ-plot (Normal)\n", nome), pch = 19)
  qqline(x, col = "red", lwd = 2)
  boxplot(x, main = paste("Boxplot\n", nome), col = "gray85", horizontal = TRUE)
  invisible(NULL)
}

#===================
# Sign test
#===================
# median time to failure example
#H0: teta = 200
#H1: teta < 200

X <-c(49, 58, 75, 110, 112, 132, 151, 276, 281, 362)
premissas(X, "tempo ate falha (teste do sinal)")

r <- sum(X>200)
#r <- 3
n <- length(X)
pbinom(r,n,prob = 0.5)  # P(X<=3)

1 - pbinom(6,n,prob = 0.5)  # P(X >= 7) = 1 - P(X <=6)

# 2 samples greater than 200
pbinom(2,n,prob = 0.5)  # P(X<=2), p-value > alpha

# 1 sample greater than 200
pbinom(1,n,prob = 0.5)  # P(X<=1), p-value < alpha

# No sample greater than 200
pbinom(0,n,prob = 0.5)  # P(X<=0), p-value < alpha



# using BSDA package
# (equivale a library(BSDA) + SIGN.test(...), como no slide; o prefixo BSDA::
#  evita anexar o pacote inteiro e funciona com requireNamespace)
if (has_BSDA) {
  # print() explicito: dentro de um bloco if() o R nao auto-imprime os resultados
  print(BSDA::SIGN.test(X, md=200, alternative="less"))

  # two sided H1
  #H0: teta0 = 200
  #H1: teta0 != 200
  print(BSDA::SIGN.test(X, md=200, alternative="two.sided"))
}

# 2 x P(X<=3)
2*pbinom(r, n, prob = 0.5)
# P(X<=3) + P(X >= 7) = P(X<=3) + 1 - P(X <=6)
pbinom(r,n,prob = 0.5)  + 1 - pbinom(6,n,prob = 0.5)

#===================
# Wilcoxon test
#===================

# changes in heart rate example

X <- c(-2, 4, 8, 25, -5, 16, 3, 1, 12, 17, 20, 9)
premissas(X, "mudanca na frequencia cardiaca")
# H0: theta = 15
# H1: theta != 15

wilcox.test(X, alternative="two.sided", mu=15, exact=TRUE,
            conf.int = TRUE)

# comparacao com o teste t (parametrico): exige normalidade
t.test(X, mu = 15)

# paired Wilcoxon twins example

school <- c(82,69,73,43,58,56,76,65)
home <- c(63,42,74,37,51,43,80,62)

response <- school - home

# H0: theta_D = 0
# H1: theta_D > 0

wilcox.test(response, alternative="greater",conf.int=TRUE)

# comparacao com o teste t pareado
t.test(response, alternative = "greater")


#===================
# Permutation test
#===================

# average processing time example
X <- c(1.24, 1.38, 1.36, 1.40, 1.38, 1.48, 1.38, 1.48, 1.38, 1.54, 1.56)
Y <- c(1.14, 1.20, 1.30, 1.26, 1.28, 1.18)

t_hat <- mean(X) - mean(Y)  # observed statistic

# one sided alternative hypothesis
# H0: mu_X - mu_Y = 0
# H1: mu_X - mu_Y > 0

S <- append(X,Y)  # combine X and Y in a single vector
set.seed(1)  # for reproducibility
nperm <- 10000  # number of permutations
n1 <- length(X)
n2 <- length(Y)
t_perm <- numeric(nperm)  # null vector to store permutations results
for (i in 1:nperm) {
  Sperm <- sample(S, n1+n2)  # generate a reordered sample of X and Y
  Xperm <- Sperm[1:n1]  # get the n1 first elements to be the permuted X
  Yperm <- Sperm[(n1+1):(n1+n2)]  # get the remaining n2 elements to be the permuted Y
  # compute & store difference in means
  t_perm[i] <- mean(Xperm) - mean(Yperm)
}
# H_1: mu_X>=mu_y
p.perm = (1 + sum(t_perm >= t_hat))/(1+nperm)  # p-valor
print(p.perm)


histperm <- hist(t_perm, breaks=100, plot=FALSE)
plot(histperm, col=ifelse(histperm$mids > t_hat, "red","gray50"))

plot(histperm$count, type='h', lwd=10, lend=2,
     col=ifelse(histperm$mids > t_hat, "red","gray50"))

# two sided alternative hypothesis
# H_1: mu_X != mu_y
p.perm.twosided = (1 + sum(abs(t_perm) >= abs(t_hat)))/(1+nperm)
print(p.perm.twosided)
histperm <- hist(t_perm, breaks=120, plot=FALSE)

plot(histperm, col=ifelse(abs(histperm$mids) > abs(t_hat), "red","gray50"))

#===================
# Bootstrap test
#===================

# Comparison of two means example

X <- c(82, 79, 81, 79, 77, 79, 79, 78, 79, 82, 76, 73, 64)
Y <- c(84, 86, 85, 82, 77, 76, 77, 80, 83, 81, 78, 78, 78)


# H0: mu_y = mu_x
# H1: mu_y > mu_x

summary(X)
summary(Y)

t_hat <- mean(Y) - mean(X)  # observed statistic

n1 <- length(X)
n2 <- length(Y)
S <- append(X, Y)  # pooled data

# teste de bootstrap SOB H0: reamostragem a partir do pool --------------------
# Se H0 e verdadeira, as duas amostras vem da mesma distribuicao; por isso a
# reamostragem e feita a partir dos dados agrupados (pooled).
set.seed(112)  # for reproducibility
R <- 999  # number of resampling
t_R <- numeric(R)
for (i in 1:R){  # bootstrap
  XX <- sample(S, n1, replace=TRUE)  # sample of size n1 drawn with replacement
  YY <- sample(S, n2, replace=TRUE)  # sample of size n2 drawn with replacement
  t_R[i] <- mean(YY)-mean(XX)  # compute & store difference in means
}

histR <- hist(t_R, breaks=100, plot=FALSE)

plot(histR, col=ifelse(histR$mids > t_hat, "red","gray50"))
# H_1: mu_X > mu_y
p_R = (1 + sum(t_R >= t_hat))/(1+R)
print(p_R)

# increase the number of resampling
set.seed(112)  # for reproducibility
R <- 99999  # number of resampling
t_R <- numeric(R)
for (i in 1:R){  # bootstrap
  XX <- sample(S, n1, replace=TRUE)  # sample of size n1 drawn with replacement
  YY <- sample(S, n2, replace=TRUE)  # sample of size n2 drawn with replacement
  t_R[i] <- mean(YY)-mean(XX)  # compute & store difference in means
}

histR <- hist(t_R, breaks=100, plot=FALSE)

plot(histR, col=ifelse(histR$mids > t_hat, "red","gray50"))
# H_1: mu_X > mu_y
p_R = (1 + sum(t_R >= t_hat))/(1+R)
print(p_R)



#===================
# Bootstrap Intervalo de confianca
#===================

# Diferenca conceitual: no TESTE acima a reamostragem parte do pool, impondo
# H0. Para o INTERVALO DE CONFIANCA nao se impoe H0: reamostra-se DENTRO de
# cada grupo, preservando a estrutura de cada amostra.

set.seed(2024)  # for reproducibility
B <- 10000  # number of resamplings
d_boot <- numeric(B)
for (i in 1:B) {
  Xb <- sample(X, n1, replace=TRUE)  # reamostra da amostra X
  Yb <- sample(Y, n2, replace=TRUE)  # reamostra da amostra Y
  d_boot[i] <- mean(Yb) - mean(Xb)
}

ic <- quantile(d_boot, c(0.025, 0.975))
cat(sprintf("\nIC 95%% por bootstrap para mu_Y - mu_X: [%.4f; %.4f]\n", ic[1], ic[2]))
cat(sprintf("estimativa pontual (t_hat) = %.4f | media das reamostragens = %.4f\n",
            t_hat, mean(d_boot)))

histD <- hist(d_boot, breaks=100, plot=FALSE)
plot(histD, col="gray85", main="Diferenca de medias (reamostragem por grupo)",
     xlab = "mu_Y - mu_X")
abline(v = ic, col = "red", lwd = 2, lty = 2)

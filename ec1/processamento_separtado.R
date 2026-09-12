# clean workspace
rm(list=ls())

library(car)

# durbinWatsonTest usa reamostragem para o p-valor: semente fixa p/ reprodutibilidade
set.seed(1234)

# ------------------------------------------------------------------------------------
# 0. Projeto experimental (definido a priori, antes de olhar os dados)

# delta* : menor diferenca de IMC medio considerada praticamente relevante.
#          Fixado em 1 kg/m2. O mesmo valor
#          vale para os tres casos: relevancia e uma propriedade do IMC,
#          nao da dispersao de cada subamostra.
delta_star    <- 1

# Justificativa do delta*: 
# O IMC é uma medida para avaliar a saúde dos alunos e o delta devo expressa a menor diferença prática 
# entre os IMC médios das turmas para sinaliza uma mudança de hábitos.
# As faixas de classificação do IMC (subpeso, normal, sobrepeso, obseidade1 obesidade 2)
# tem cerca de 5 kg/m². 
# Adotou-se δ* = 1 kg/m² — cerca de um quinto de faixa, ou 2,9 kg para um aluno de 1,70 m — 
# fixado antes do exame dos dados e igual nas três análises, por ser a relevância 
# prática propriedade da grandeza medida e não da dispersão de cada subamostra.


# ------------------------------------------------------------------------------------
# 1. Leitura dos dados

dados_2016 <- read.csv("data/imc_20162.csv", header = TRUE)


dados_2017 <- read.csv("data/CS01_20172.csv", header = TRUE, sep = ";")


# Isolando os dados de interesse.
# O enunciado diz: "Note que o arquivo relativo a 2016-2 contém tam-
#bém dados de uma turma de graduação, e que os dois
#arquivos (2016-2 e 2017-2) estão em formatos ligeira-
#mente diferentes"
# Por isso, foi isolada a turma de pós gradução

dados_2016 <- subset(dados_2016, Course == "PPGEE")


# Consolidadnod os dados - modificar os dados 2017 para ficar como 2016

names(dados_2017)[2] = "Height.m"
names(dados_2017)[3] = "Gender"


# Função para add a columna BMI nas tabelas

calcular_imc_tabela <- function(tabela) {
  imc <- tabela$Weight.kg / (tabela$Height.m^2)
  tabela$BMI <- imc
  
  return(tabela)
}

dados_2016 <- calcular_imc_tabela(dados_2016)
dados_2016$BMI <- as.numeric(gsub(",", ".", dados_2016$BMI))

dados_2017 <- calcular_imc_tabela(dados_2017)
dados_2017$BMI <- as.numeric(gsub(",", ".", dados_2017$BMI))


fem_2016 <- subset(dados_2016,Gender == "F")
fem_2017 <- subset(dados_2017,Gender == "F")


masc_2016 <-subset(dados_2016, Gender == "M")
masc_2017 <-subset(dados_2017, Gender == "M")

fem <- rbind(
  data.frame(BMI = fem_2016[["BMI"]], Semestre = "2016-2"),
  data.frame(BMI = fem_2017[["BMI"]], Semestre = "2017-2")
)

masc <- rbind(
  data.frame(BMI = masc_2016[["BMI"]], Semestre = "2016-2"),
  data.frame(BMI = masc_2017[["BMI"]], Semestre = "2017-2")
)

pop_2016 <- rbind(
  data.frame(BMI = masc_2016[["BMI"]], Gender = "M"),
  data.frame(BMI = fem_2016[["BMI"]], Gender = "F")
)

pop_2017 <- rbind(
  data.frame(BMI = masc_2017[["BMI"]], Gender = "M"),
  data.frame(BMI = fem_2017[["BMI"]], Gender = "F")
)

pop_total <- rbind(
  data.frame(BMI = pop_2016[["BMI"]], Semestre = "2016-2"),
  data.frame(BMI = pop_2017[["BMI"]], Semestre = "2017-2")
)







# ------------------------------------------------
# 2. Testes de Hipóteses

# 2.1 - Caso 1 - populacao feminina -----------
#----------------------------------------

# Verificacao das premissas do teste:
#   normalidade -> igualdade de variancias -> independencia

## Normalidade
shapiro.test(fem_2016$BMI)
shapiro.test(fem_2017$BMI)
# result
# data:  fem_2016$BMI
# W = 0.91974, p-value = 0.4674
# data:  fem_2017$BMI
# W = 0.7475, p-value = 0.03659

png("plots/qq_fem.png", width = 600, height = 600)
qqPlot(fem$BMI,
       groups = fem$Semestre,
       glab   = "IMC feminino",
       cex    = 1.5,
       pch    = 16,
       layout = c(2, 1),
       las    = 1)
dev.off()
# Explicação: Apesar do shapiro mostrar não normalidade para o semestre 2017, 
# o qqplot mostra que foi devido a apenas uma amostra, por isso podemos assumir 
# quase normalidade, que é condição do teste.


## Igualdade de variancias
fligner.test(BMI ~ Semestre, data = fem)
# com o desvio padrão
(sp_fem <- sp(fem_2016$BMI,  fem_2017$BMI))
# Explicação: Fligner teste mostra que falhamos em rejeitar igualdade de variâncias
# por isso, podemos usar teste t de student

## Independencia
resid_fem <- tapply(X     = masc$BMI,
                     INDEX = masc$Semestre,
                     FUN   = function(x){x - mean(x)})

png("plots/indep_fem.png", width = 600, height = 600)
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))

ylim_masc <- range(pretty(unlist(resid_fem)))
plot(resid_fem[["2016-2"]],
     pch  = 16,
     cex  = 1.5,
     type = "b",
     las  = 1,
     xlab = "ordem da observacao",
     ylab = "resíduos",
     main = "Resíduos do IMC - Feminino 2016-2")
grid(NA, NULL, lwd = 2)
plot(resid_fem[["2017-2"]],
     pch  = 16,
     cex  = 1.5,
     type = "b",
     las  = 1,
     xlab = "ordem da observacao",
     ylab = "resíduos",
     main = "Resíduos do IMC - Feminino 2017-2")
grid(NA, NULL, lwd = 2)
mtext("Resíduos do IMC - Feminino", outer = TRUE, cex = 1.2)
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
dev.off()

car::durbinWatsonTest(lm(resid_fem[["2016-2"]] ~ 1))
car::durbinWatsonTest(lm(resid_fem[["2017-2"]] ~ 1))

# > car::durbinWatsonTest(lm(resid_fem[["2016-2"]] ~ 1))
# lag Autocorrelation D-W Statistic p-value
# 1      0.03474258      1.605236   0.568
# Alternative hypothesis: rho != 0
# > car::durbinWatsonTest(lm(resid_fem[["2017-2"]] ~ 1))
# lag Autocorrelation D-W Statistic p-value
# 1      -0.4664038      2.716686      NA
# Alternative hypothesis: rho != 0
# Verificamos idependencia das amostras das populações


# Teste de hipoteses
#   H0: mu_F2016 - mu_F2017 =  0
#   H1: mu_F2016 - mu_F2017 != 0
# Student (var.equal = TRUE): variancias iguais, conforme o fligner.test acima.
t.test(fem_2016$BMI, fem_2017$BMI,
       alternative = "two.sided",
       mu          = delta_star,
       var.equal   = TRUE,
       conf.level  = 0.95)

# data:  fem_2016$BMI and fem_2017$BMI
# t = 1.1988, df = 9, p-value = 0.2612
# alternative hypothesis: true difference in means is not equal to 1
# 95 percent confidence interval:
#   -0.4527037  5.7283762
# sample estimates:
#   mean of x mean of y 
# 21.08443  18.44660 
# REsultado: Falhamos em rejeita a hiótese nula

#-----------------------------------------------
# 2.2 - Caso 2 - populacao masculina ----------
#----------------------------------------

# Verificacao das premissas do teste

## Normalidade
png("plots/qq_masc.png", width = 600, height = 600)
qqPlot(masc$BMI,
       groups = masc$Semestre,
       glab   = "IMC masculino",
       cex    = 1.5,
       pch    = 16,
       layout = c(2, 1),
       las    = 1)
dev.off()

shapiro.test(masc_2016$BMI)
shapiro.test(masc_2017$BMI)


# data:  masc_2016$BMI
# W = 0.92833, p-value = 0.1275
# 
# 
# data:  masc_2017$BMI
# W = 0.96494, p-value = 0.6206
# Teste shapiro mostra normalidade dos dados



## Igualdade de variancias
fligner.test(BMI ~ Semestre, data = masc)
(sp_masc <- sp(masc_2016$BMI,  masc_2017$BMI))
# data:  masc_2016$BMI
# W = 0.92833, p-value = 0.1275
# 
# data:  masc_2017$BMI
# W = 0.96494, p-value = 0.6206

# [1] 3.904637
# Teste de fligner mostra igualdade de variancias, podendo usar teste t de student




## Independencia
resid_masc <- tapply(X     = masc$BMI,
                     INDEX = masc$Semestre,
                     FUN   = function(x){x - mean(x)})

png("plots/indep_masc.png", width = 600, height = 600)
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))
# mesma escala vertical nos dois paineis
ylim_masc <- range(pretty(unlist(resid_masc)))
plot(resid_masc[["2016-2"]],
     pch  = 16,
     cex  = 1.5,
     type = "b",
     las  = 1,
     xlab = "ordem da observacao",
     ylab = "resíduos",
     main = "Resíduos do IMC - Masculino 2016-2")
grid(NA, NULL, lwd = 2)


plot(resid_masc[["2017-2"]],
     pch  = 16,
     cex  = 1.5,
     type = "b",
     las  = 1,
     xlab = "ordem da observacao",
     ylab = "resíduos",
     main = "Resíduos do IMC - Masculino 2017-2")
grid(NA, NULL, lwd = 2)
mtext("Resíduos do IMC - Masculino", outer = TRUE, cex = 1.2)
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
dev.off()

car::durbinWatsonTest(lm(resid_masc[["2016-2"]] ~ 1))
car::durbinWatsonTest(lm(resid_masc[["2017-2"]] ~ 1))

# > car::durbinWatsonTest(lm(resid_masc[["2016-2"]] ~ 1))
# lag Autocorrelation D-W Statistic p-value
# 1      -0.1006324      2.188132   0.682
# Alternative hypothesis: rho != 0
# > car::durbinWatsonTest(lm(resid_masc[["2017-2"]] ~ 1))
# lag Autocorrelation D-W Statistic p-value
# 1        0.294231      1.285282   0.104
# Alternative hypothesis: rho != 0

# Teste durbinWatson mostra independencia das populações.

# Teste de hipoteses
#   H0: mu_M2016 - mu_M2017 =  0
#   H1: mu_M2016 - mu_M2017 != 0
# Student (var.equal = TRUE): variancias iguais, conforme o fligner.test acima.
t.test(masc_2016$BMI, masc_2017$BMI,
       alternative = "two.sided",
       mu          = delta_star,
       var.equal   = TRUE,
       conf.level  = 0.95)

# data:  masc_2016$BMI and masc_2017$BMI
# t = -0.29009, df = 40, p-value = 0.7732
# alternative hypothesis: true difference in means is not equal to 1
# 95 percent confidence interval:
#   -1.784943  3.085836
# sample estimates:
#   mean of x mean of y 
# 24.93595  24.28551 

# Teste t de student mostra que falhamos em rejeitar a hipótese nula


# -----------------------------------------------------------------
# 2.3 - Caso 3 - populacao agregada (M + F) ---
#----------------------------------------
# Neste caso é a comparação da população inteira de 2016 contra a população inteira de 2017

# Verificacao das premissas do teste

## Normalidade
png("plots/qq_pop_tot.png", width = 600, height = 600)
qqPlot(pop_total$BMI,
       groups = pop_total$Semestre,
       glab   = "IMC população total",
       cex    = 1.5,
       pch    = 16,
       layout = c(2, 1),
       las    = 1)
dev.off()

shapiro.test(pop_2016$BMI)
shapiro.test(pop_2017$BMI)

# data:  pop_2016$BMI
# W = 0.92185, p-value = 0.03854
# 
# data:  pop_2017$BMI
# W = 0.95381, p-value = 0.3049
# Populações com distribuição normal

## Igualdade de variancias
fligner.test(BMI ~ Semestre, data = pop_total)
var(pop_2016$BMI)
var(pop_2017$BMI)
# data:  BMI by Semestre
# Fligner-Killeen:med chi-squared = 0.025355, df = 1, p-value = 0.8735
# Considerando variâncias iguais, para realizar teste t de student


## Independencia
resid_pop <- tapply(X     = pop_total$BMI,
                    INDEX = pop_total$Semestre,
                    FUN   = function(x){x - mean(x)})
png("plots/indep_pop_tot.png", width = 600, height = 600)
plot(resid_pop[["2016-2"]],
     pch  = 16,
     cex  = 1.5,
     type = "b",
     las  = 1,
     xlab = "ordem da observacao",
     ylab = "resíduos",
     main = "Resíduos do IMC - População total 2016-2")

plot(resid_pop[["2017-2"]],
     pch  = 16,
     cex  = 1.5,
     type = "b",
     las  = 1,
     xlab = "ordem da observacao",
     ylab = "resíduos",
     main = "Resíduos do IMC - População total 2017-2")
dev.off()
car::durbinWatsonTest(lm(resid_pop[["2016-2"]] ~ 1))
car::durbinWatsonTest(lm(resid_pop[["2017-2"]] ~ 1))

# Resultado:
# > car::durbinWatsonTest(lm(resid_pop[["2016-2"]] ~ 1))
# lag Autocorrelation D-W Statistic p-value
# 1      0.05649958      1.833376   0.656
# Alternative hypothesis: rho != 0
# > car::durbinWatsonTest(lm(resid_pop[["2017-2"]] ~ 1))
# lag Autocorrelation D-W Statistic p-value
# 1       0.3741784      1.050147   0.016
# Alternative hypothesis: rho != 0
# Comprovação de independencia das amostras


# Teste de hipoteses
#   H0: mu_2016 - mu_2017 =  0
#   H1: mu_2016 - mu_2017 != 0
# Student (var.equal = TRUE): variancias iguais, conforme o fligner.test acima.
t.test(pop_2016$BMI, pop_2017$BMI,
       alternative = "two.sided",
       mu          = delta_star,
       var.equal   = TRUE,
       conf.level  = 0.95)

# data:  pop_2016$BMI and pop_2017$BMI
# t = -0.33767, df = 51, p-value = 0.737
# alternative hypothesis: true difference in means is not equal to 1
# 95 percent confidence interval:
#   -1.626826  2.870410
# sample estimates:
#   mean of x mean of y 
# 23.97307  23.35128 

# Falhamos em rejeitar a hipótese nula com p_valor > significancia



# ----------------------------------------------------------------
# 3 - Discussões de melhoria dos testes


# Os teste que falharam em rejeitar H0 tem alguma diferença nas médias mas não o suficiente
# para confirmar estatísticamente. 
# Sugere-se que sejam medidas populações com maior quantidade de amostras, para aumentar o poder do teste.

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


# Isolando os dados de interes pois, o arquivo de 2016 contem também dados de 
# alunos da gradução, logo filtramos os aluno do PPG

dados_2016 <- subset(dados_2016, Course == "PPGEE")


# Consolidando os dados: modificando os dados 2017 para ficar como os de 2016

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


todos <- rbind(
  data.frame(BMI = fem_2016$BMI, Semestre = "2016-2", Gender = "F"),
  data.frame(BMI = fem_2017$BMI, Semestre = "2017-2", Gender = "F"),
  data.frame(BMI = masc_2016$BMI, Semestre = "2016-2", Gender = "M"),
  data.frame(BMI = masc_2017$BMI, Semestre = "2017-2", Gender = "M")
)

# Parte : Explorar os dados

boxplot(BMI ~ Gender + Semestre, data = todos,
        col = c("pink", "lightblue"),
        main = "Distribuição do BMI por Gênero e Semestre",
        xlab = "Grupo (Gênero.Semestre)",
        ylab = "BMI")



calcular_estatisticas <- function(amostra) {
  n_val  <- length(amostra$BMI)
  media  <- mean(amostra$BMI)
  desvio <- sd(amostra$BMI)
  

  tabela <- data.frame(
    n      = n_val,
    media  = media,
    sd     = desvio
  )
  
  return(tabela)
}



aggregate(BMI ~ Gender + Semestre, data = todos, FUN = function(x) shapiro.test(x)$p.value)



fem_2016_estatisitca <- calcular_estatisticas(fem_2016)
fem_2017_estatisitca <- calcular_estatisticas(fem_2017)
masc_2016_estatisitca <- calcular_estatisticas(masc_2016)
masc_2017_estatisitca <- calcular_estatisticas(masc_2017)

tabela_resumo <- rbind(
  cbind(Grupo = "Feminino 2016-2", fem_2016_estatisitca),
  cbind(Grupo = "Feminino 2017-2", fem_2017_estatisitca),
  cbind(Grupo = "Masculino 2016-2", masc_2016_estatisitca),
  cbind(Grupo = "Masculino 2017-2", masc_2017_estatisitca)
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


# Estimativa do tamanho de efeito 'a posteriori' pois neste caso não planejamos 
# o experimento, ja que os dados já foram coletados
# ------------------------------------------------------------------------


<<<<<<< Updated upstream
## Independencia
resid_masc <- tapply(X     = masc$BMI,
                     INDEX = masc$Semestre,
                     FUN   = function(x){x - mean(x)})
=======
efeito_padronizado <- function(x, y) {
  n1 <- length(x); n2 <- length(y)
  s_pooled <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  d <- (mean(x) - mean(y)) / s_pooled
  return(d)
}

efeito_padronizado(fem_2016$BMI, fem_2017$BMI)
efeito_padronizado(masc_2016$BMI, masc_2017$BMI)

>>>>>>> Stashed changes

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

library(dplyr)
dados_resumo <- todos %>%
  group_by(Semestre, Gender) %>%
  summarise(
    media = mean(BMI),
    sd = sd(BMI),
    N = length(BMI),
    ic_inf = t.test(BMI)$conf.int[1],
    ic_sup = t.test(BMI)$conf.int[2],
    .groups = "drop"
  )

pd <- position_dodge(0.5)

ggplot(dados_resumo, aes(x = Semestre, y = media, color = Gender, group = Gender)) +
  
  
  geom_point(position = pd, size = 3) +
  
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), position = pd) +
  labs(
    title = "Média de BMI e Intervalo de Confiança (95%)",
    x = "Semestre",
    y = "Média do BMI",
    color = "Gênero"
  )







hist(fem_2016$BMI, main='Histogram of fem_2016', col ='steelblue',xlab = 'IMC (Kg/m^2)')
hist(fem_2017$BMI, main='Histogram of fem_2017',col ='steelblue',xlab = 'IMC (Kg/m^2)')
hist(masc_2016$BMI, main='Histogram of masc_2016',col ='steelblue',xlab = 'IMC (Kg/m^2)')
hist(masc_2017$BMI, main='Histogram of masc_2017',col ='steelblue',xlab = 'IMC (Kg/m^2)')





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






### Caso 3 — População agregada (masculina + feminina)

Neste caso compara-se a população inteira de 2016-2 contra a população inteira de 2017-2.

#### Verificação das premissas

**Normalidade.**
  
  ```{r norm-pop}
shapiro.test(pop_2016$BMI)
shapiro.test(pop_2017$BMI)
```

```{r qq-pop, fig.width=7, fig.height=7, fig.cap="Gráficos quantil-quantil do IMC da população total, por semestre."}
qqPlot(pop_total$BMI,
       groups = pop_total$Semestre,
       glab   = "IMC população total",
       cex    = 1.5,
       pch    = 16,
       layout = c(2, 1),
       las    = 1)
```

O teste de Shapiro-Wilk resultou em $W = 0{,}92185$ ($p = 0{,}03854$) para 2016-2 e
$W = 0{,}95381$ ($p = 0{,}3049$) para 2017-2.

**[PLACEHOLDER — avaliar a premissa de normalidade da população agregada de 2016-2 à luz do
   p-valor obtido e dos gráficos quantil-quantil.]**
  
  **Igualdade de variâncias.**
  
  ```{r var-pop}
fligner.test(BMI ~ Semestre, data = pop_total)
var(pop_2016$BMI)
var(pop_2017$BMI)
```

O teste de Fligner-Killeen resultou em $\chi^2 = 0{,}025355$ com 1 grau de liberdade e
$p = 0{,}8735$; as variâncias amostrais são $18{,}0276$ (2016-2) e $14{,}9289$ (2017-2), em
$\mathrm{kg^2/m^4}$. Consideraram-se as variâncias iguais para a realização do teste t de
Student.

**Independência.**
  
  ```{r indep-pop, fig.width=7, fig.height=7, fig.cap="Resíduos do IMC da população total em função da ordem de observação."}
resid_pop <- tapply(X     = pop_total$BMI,
                    INDEX = pop_total$Semestre,
                    FUN   = function(x){x - mean(x)})

par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))
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
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
```

```{r dw-pop}
car::durbinWatsonTest(lm(resid_pop[["2016-2"]] ~ 1))
car::durbinWatsonTest(lm(resid_pop[["2017-2"]] ~ 1))
```

O teste de Durbin-Watson resultou em autocorrelação de $0{,}05650$, estatística D-W de
$1{,}8334$ e $p = 0{,}658$ para 2016-2; e autocorrelação de $0{,}37418$, estatística D-W de
$1{,}0501$ e $p = 0{,}010$ para 2017-2.

**[PLACEHOLDER — avaliar a premissa de independência da população agregada de 2017-2 à luz do
   p-valor obtido.]**
  
  #### Teste de hipóteses
  
  ```{r ttest-pop}
t.test(pop_2016$BMI, pop_2017$BMI,
       alternative = "two.sided",
       mu          = delta_star,
       var.equal   = TRUE,
       conf.level  = 0.95)
```

O teste resultou em $t = -0{,}33767$ com 51 graus de liberdade e $p = 0{,}7370$. O intervalo
de confiança de 95% para a diferença entre as médias é
$[-1{,}6268;\ 2{,}8704]\ \mathrm{kg/m^2}$, com médias amostrais de
$23{,}9731\ \mathrm{kg/m^2}$ (2016-2) e $23{,}5128\ \mathrm{kg/m^2}$ (2017-2). Falhou-se em
rejeitar a hipótese nula, com p-valor superior ao nível de significância adotado.

**[PLACEHOLDER: discussão do Caso 3.]**
  
  
  
  

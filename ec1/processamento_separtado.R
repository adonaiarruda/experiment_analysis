# clean workspace
rm(list=ls())

# Leitura dos dados

dados_2016 <- read.csv("imc_20162.csv", header = TRUE)
dados_2017 <- read.csv("CS01_20172.csv", header = TRUE, sep = ";")


# Filtrando os dados do PPG do arquivo  2016

dados_2016 <- subset(dados_2016,Course == "PPGEE")


# Consolidando os dados - modificando 2017 para ficar como 2016

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




# Caso 1: Comparar se o BMI medio do mesmo genero é igual nos dois semestre
# -------------------------------------------------------------------------

shapiro.test(fem_2016$BMI)
shapiro.test(fem_2017$BMI)

#Como o p-valor do teste shapiro deu abaixo de 5% verificamos graficamente

library(car)
qqPlot(fem_2017$BMI)

#mesmo com o p_valor abaxo de 5%. como se trata de valores de calda, podemos assumir normalidade


shapiro.test(masc_2016$BMI)
shapiro.test(masc_2017$BMI)



fligner.test(BMI~ Semestre, data = fem)
t.test(fem_2016$BMI, fem_2017$BMI)


fligner.test(BMI~ Semestre, data = masc)
t.test(masc_2016$BMI, masc_2017$BMI)





# Caso 2: A media BMI dos homes e mulheres de cada semestre é a mesma
# -------------------------------------------------------------------------


shapiro.test(pop_2016$BMI)
shapiro.test(pop_2017$BMI)





t.test(pop_2016$BMI, pop_2017$BMI)

library(car)
qqPlot(masc_2016$BMI)

library(car)
qqPlot(pop_2017$BMI)



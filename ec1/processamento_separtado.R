# clean workspace
rm(list=ls())

# Leitura dos dados

dados_2016 <- read.csv("imc_20162.csv", header = TRUE)


dados_2017 <- read.csv("CS01_20172.csv", header = TRUE, sep = ";")



# Consolidadnod os dados - modificar 2017 para ficar como 2016

names(dados_2017)[2] = "Height.m"
names(dados_2017)[3] = "Gender"




# Função para add a columna BMI nas tabelas

calcular_imc_tabela <- function(tabela) {
  imc <- tabela$Weight.kg / (tabela$Height.m^2)
  tabela$BMI <- imc
  
  return(tabela)
}

dados_2016 <- calcular_imc_tabela(dados_2016)
dados_2017 <- calcular_imc_tabela(dados_2017)


fem_2016 <- subset(dados_2016,Gender == "F")
fem_2017 <- subset(dados_2017,Gender == "F")


masc_2016 <-subset(dados_2016, Gender == "M")
masc_2017 <-subset(dados_2017, Gender == "M")


fem <- rbind(
  data.frame(BMI = fem_2016["BMI"], Semestre = "2016-2" ), 
  data.frame(BMI = fem_2017["BMI"], Semestre = "2017-2" )  
)

masc <- rbind(
  data.frame(BMI = masc_2016["BMI"], Semestre = "2016-2" ), 
  data.frame(BMI = masc_2017["BMI"], Semestre = "2017-2" )  
)

# Caso 1 ------------------------------
#---------------------------------------

t.test(fem_2016["BMI"],fem_2017["BMI"])

fligner.test(BMI~ Semestre, data = fem)


t.test(masc_2016["BMI"],masc_2017["BMI"])

fligner.test(BMI~ Semestre, data = masc)





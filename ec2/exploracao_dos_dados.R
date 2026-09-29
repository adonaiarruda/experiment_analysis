# clean workspace
rm(list=ls())

calcular_retorno <- function(acao) {
  n <- length(acao)
  retorno_list <- numeric(n - 1)
  
  for (i in 1:(n - 1)) {
    retorno_list[i] <- (acao[i] - acao[i + 1]) / acao[i]
  }
  
  return(retorno_list)
}

acoes <- read.csv("dados/DadosAcoesGrupoE.csv",
  header = FALSE
)


retornos <- as.data.frame(sapply(acoes, calcular_retorno))



boxplot(retornos,
        main = "Distribuição dos Retornos por Ação",
        xlab = "Ação",
        ylab = "Retorno",
        col = c("darkorchid3", "steelblue3","darkorange2","darkolivegreen3","tomato2"))




desc <- data.frame(
  Media   = sapply(ret, mean),
  DP      = sapply(ret, sd),
  Mediana = sapply(ret, median),
  Minimo  = sapply(ret, min),
  Maximo  = sapply(ret, max)
)
knitr::kable(round(desc * 100, 3),
             caption = "Estatísticas descritivas dos retornos mensais (em %).")


# converte os dados em formato longo pois é a formatação que usa aov

retornos_long <- stack(retornos)
names(retornos_long) <- c("retorno", "acao")

modelo <- aov(retorno ~ acao, data = retornos_long)
summary(modelo)
# ha evidencias de que não todas as medias são iguais


### verificar nas notas pois tinmha algo relacionado ao alpha real pq quando vão aumentando os testes 
### aumentam tambem as chances de erra pelo 1 vez

shapiro.test(modelo$residuals)

#com p_value 0.16 não rejeita normalidade

library(car)
qqPlot(modelo$residuals)


fligner.test(retorno ~ acao, data = retornos_long)
plot(x = modelo$fitted.values,
     y = modelo$residuals)

library(multcomp)



mc <- glht(modelo, linfct = mcp(acao = "Tukey"))
mc_CI  <- confint(mc, level= 0.95)

plot(mc_CI)

#juntando o boxplot e o mc_IC descartamos v2 e V4. Agora não tem evidencia
#de que v1,v3 e v4 sejam diferntes


# Assumindo que V1 é referência

retornos_long$acao <- relevel(retornos_long$acao, ref = "V1")
modelo2 <- aov(retorno ~ acao, data = retornos_long)

summary(modelo2)

mc2 <- glht(modelo2, linfct = mcp(acao = "Dunnett"))
mc2_CI <- confint(mc2, level = 0.95)

plot(mc2_CI)

#Aumentamos a sensibilidade e conseguimos descartar V5 mas ainda não é ppossivel
#dizer que a diferença de há diferença entre v1 e v3


# o que pode me dar evidencias para escolher apenas 1 entre essas 2
subset_v1_v3 <- subset(retornos_long, acao %in% c("V1", "V3"))
subset_v1_v3$acao <- droplevels(subset_v1_v3$acao)  # remove níveis de fator não usados

teste_t <- t.test(retorno ~ acao, data = subset_v1_v3)
teste_t


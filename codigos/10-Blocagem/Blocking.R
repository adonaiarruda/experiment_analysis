# Planejamento e Análise de Experimentos — Aula 09: Blocagem (RCBD)
# Objetivo: agregar os resultados por algoritmo e bloco, ajustar os modelos,
#          examinar seus diagnósticos, calcular a eficiência da blocagem e
#          comparar cada modificação com o algoritmo original.
# Entrada: algo.csv, arquivo de texto delimitado por espaços com as colunas
#          Replicate, Algorithm, Instance, Group e Result.
# Execução: usar Aula-09 como diretório de trabalho para localizar algo.csv.
# Dependências: ggplot2 (gráfico exploratório), car (qqPlot) e multcomp (Dunnett).
# As seções numeradas indicam os slides correspondentes no PDF da Aula 09.

# Limpa o ambiente de trabalho
rm(list=ls())

# Instala os pacotes usados quando não estiverem disponíveis.
packages_needed <- c("ggplot2", "multcomp", "car")
for (package_name in packages_needed) {      
      if (!(package_name %in% rownames(installed.packages()))){
            install.packages(package_name)
      }
}
# 1) Leitura e agregação [Aula 09 - slide 15]
# Resume os resultados por algoritmo e grupo de instâncias para formar uma
# observação por algoritmo-bloco, conforme o exemplo apresentado nos slides.

data <- read.table("algo.csv",
                   header = TRUE)


# Inspeciona uma amostra dos dados dos dois primeiros grupos.
data[which(data$Group=='1'),][1:12,]
data[which(data$Group=='2'),][1:12,]

# Resume as variáveis da tabela original.
summary(data)


# Calcula a média de Result para cada combinação de algoritmo e grupo.
aggdata <- with(data,
                aggregate(x   = Result,
                          by  = list(Algorithm, Group),
                          FUN = mean))

# Dá nomes explícitos às colunas da tabela agregada.
names(aggdata) <- c("Algorithm", 
                    "Instance_Group",
                    "Y")

# Trata algoritmo e grupo como fatores no modelo estatístico.
for (i in 1:2){
      aggdata[, i] <- as.factor(aggdata[, i])
}

# Substitui os códigos numéricos por nomes legíveis dos algoritmos.
levels(aggdata$Algorithm) <- c("Original",
                               unlist(lapply("Mod",
                                             paste0,
                                             1:6)))

summary(aggdata)

# 2) Análise exploratória [Aula 09 - slide 16]
# Compara visualmente as médias dos algoritmos entre os grupos de instâncias.

library(ggplot2)

# png(filename = "../figs/algo_lineplot.png",
#     width = 1000, height = 400, 
#     bg = "transparent")
p <- ggplot(aggdata, aes(x = Instance_Group, 
                         y = Y, 
                         group = Algorithm, 
                         colour = Algorithm))
p + geom_line(linetype=2) + geom_point(size=5)
# dev.off()

# 3) Modelos e diagnóstico [Aula 09 - slides 23-26]
# Ajusta os modelos RCBD na escala original e na escala logarítmica, e examina
# graficamente os resíduos.

# 3.1) Modelo com resposta na escala original
# Separa os efeitos de algoritmo (tratamento) e grupo de instâncias (bloco).

model <- aov(Y~Algorithm+Instance_Group,
             data = aggdata)

summary(model)
summary.lm(model)$r.squared

# Diagnóstico gráfico dos resíduos do modelo na escala original.
# png(filename = "../figs/algo_res1.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)
# dev.off()


# 3.2) Modelo com resposta log-transformada
# Avalia o ajuste após transformar a resposta para reduzir a heterogeneidade
# visual da variância residual.

model2 <- aov(log(Y)~Algorithm+Instance_Group,
              data = aggdata)
summary(model2)
summary.lm(model2)$r.squared

# Diagnóstico gráfico dos resíduos do modelo com resposta log-transformada.
# png(filename = "../figs/algo_res2.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mfrow = c(2, 2))
plot(model2, pch = 20, las = 1)
# dev.off()

library(car)
# png(filename = "../figs/algo_qq.png",
#     width = 600, height = 600, 
#     bg = "transparent")

# Gráfico Q-Q dos resíduos do modelo com resposta log-transformada.
par(mfrow = c(1, 1))
qqPlot(model2$residuals, pch = 20, las = 1)
# dev.off()

# 4) Eficiência relativa da blocagem [Aula 09 - slide 31]
# Usa os quadrados médios do modelo com resposta log-transformada.

mydf        <- as.data.frame(summary(model2)[[1]])
MSblocks    <- mydf["Instance_Group","Mean Sq"]
MSe         <- mydf["Residuals","Mean Sq"]
a           <- length(unique(aggdata$Algorithm))
b           <- length(unique(aggdata$Instance_Group))
((b - 1) * MSblocks + b * (a - 1) * MSe) / ((a * b - 1) * MSe)

# 5) Comparações múltiplas de Dunnett [Aula 09 - slide 38]
# Compara cada modificação com o algoritmo original, mantendo o original como
# controle.

# 5.1) Comparações no modelo com resposta log-transformada.
library(multcomp)
duntest     <- glht(model2,
                    linfct = mcp(Algorithm = "Dunnett"))

summary(duntest)


duntestCI   <- confint(duntest)
# png(filename = "../figs/algo_mcp.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI,
     xlab = "Mean difference (log scale)")
# dev.off()

# 5.2) Comparações no modelo com resposta na escala original.
# Esta comparação adicional não aparece no slide 38.
duntest1     <- glht(model,
                    linfct = mcp(Algorithm = "Dunnett"))

summary(duntest1)

duntestCI1   <- confint(duntest1)

# png(filename = "../figs/algo_mcp.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI1,
     xlab = "Mean difference")
# dev.off()

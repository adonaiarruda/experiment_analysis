# Aula 10 — Experimento fatorial (11_ExpFatorial.pdf).
# Executar os blocos em ordem, no RStudio, com Aula-10 como diretório de trabalho.
# Os arquivos motors.txt e lio.txt devem estar nesse diretório.
# O script não instala pacotes nem apaga objetos da sessão.

# ===== 0) Dependências =====
pacotes_necessarios <- c("ggplot2", "multcomp", "effects", "car")
pacotes_ausentes <- pacotes_necessarios[
  !vapply(pacotes_necessarios, requireNamespace, logical(1), quietly = TRUE)
]
if (length(pacotes_ausentes) > 0) {
  stop("Instale previamente os pacotes: ",
       paste(pacotes_ausentes, collapse = ", "), call. = FALSE)
}
library(ggplot2)
library(multcomp)
library(effects)
library(car)

# ===== 1) Motores: leitura e preparação [slides 5–6] =====
if (!file.exists("motors.txt")) {
  stop("Arquivo motors.txt não encontrado. Use Aula-10 como diretório de trabalho.",
       call. = FALSE)
}
motores <- read.table("motors.txt", header = TRUE)
summary(motores)
motores$State <- factor(motores$State, levels = c("Original", "Rewinded"))
motores$Manufacturer <- factor(motores$Manufacturer, levels = c("A", "B", "C"))
summary(motores)

# ===== 2) Motores: análise exploratória [slides 6–7] =====
# Exportação opcional; permanece desativada para não sobrescrever figuras.
# png(filename = "../figs/motors_box1.png",
#     width = 1000, height = 400, bg = "transparent")
grafico_motores <- ggplot(motores,
                         aes(x = Manufacturer, y = Current.Amperes,
                             fill = Manufacturer)) +
  geom_boxplot() +
  facet_grid(.~State) +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18))
print(grafico_motores)
# dev.off()

# ===== 3) Motores: ANOVA fatorial [slides 9–17] =====
modelo_motores <- aov(Current.Amperes ~ State*Manufacturer, data = motores)
summary(modelo_motores)
summary.lm(modelo_motores)$r.squared

# ===== 4) Motores: diagnóstico das premissas [slide 18] =====
# png(filename = "../figs/motors_res.png",
#     width = 1000, height = 300, bg = "transparent")
par_motores_res <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(modelo_motores, pch = 16, cex = 2)
par(par_motores_res)
# dev.off()

par_motores_qq <- par(no.readonly = TRUE)
par(mfrow = c(1, 1))
qqPlot(modelo_motores$residuals, pch = 20, las = 1)
par(par_motores_qq)

# Não rejeitar estes testes não comprova normalidade ou homoscedasticidade.
# A independência deve ser sustentada pelo delineamento e pela coleta.
shapiro.test(modelo_motores$residuals)
fligner.test(Current.Amperes ~ interaction(State, Manufacturer), data = motores)

# ===== 5) Motores: comparações entre fabricantes [slides 20–23] =====
# Quantidade de observações por nível e por combinação, para os erros-padrão.
replications(Current.Amperes ~ State*Manufacturer, data = motores)

# Mantemos os contrastes dos slides: com State = Original como referência,
# estes comandos comparam fabricantes no estado Original, não médias marginais.
# O aviso sobre interação é esperado e não deve ser ocultado.
motores_tukey <- glht(modelo_motores, linfct = mcp(Manufacturer = "Tukey"))
summary(motores_tukey)

# B é a referência previamente definida para a comparação de fabricantes.
# A cópia evita alterar a ordem dos níveis nos blocos posteriores.
motores_ref_b <- motores
motores_ref_b$Manufacturer <- relevel(motores_ref_b$Manufacturer, ref = "B")
modelo_motores_ref_b <- aov(Current.Amperes ~ State*Manufacturer,
                            data = motores_ref_b)
motores_dunnett_fabricante <- glht(modelo_motores_ref_b,
                                  linfct = mcp(Manufacturer = "Dunnett"))
summary(motores_dunnett_fabricante)

# png(filename = "../figs/motors_mcpManuf.png",
#     width = 500, height = 500, bg = "transparent")
par_motores_tukey <- par(no.readonly = TRUE)
par(mfrow = c(1, 1))
plot(confint(motores_tukey), cex.axis = 1.2, cex = 2)
par(par_motores_tukey)
# dev.off()

# ===== 6) Motores: médias das combinações de níveis [slides 24–25] =====
motores$interfac <- with(motores, interaction(State, Manufacturer))
# Original.B é o controle definido previamente por decisão experimental/operacional,
# e não escolhido pela menor média observada. Comparam-se médias de células,
# não contrastes de interação (diferenças de diferenças).
motores$interfac <- relevel(motores$interfac, ref = "Original.B")
modelo_motores_grupos <- aov(Current.Amperes ~ interfac, data = motores)
summary(modelo_motores_grupos)
motores_dunnett_grupos <- glht(modelo_motores_grupos,
                               linfct = mcp(interfac = "Dunnett"))
summary(motores_dunnett_grupos)

# png(filename = "../figs/motors_mcpDun.png",
#     width = 1000, height = 500, bg = "transparent")
par_motores_dunnett <- par(no.readonly = TRUE)
par(mfrow = c(1, 1), mar = c(5, 12, 4, 2))
plot(confint(motores_dunnett_grupos), xlim = c(-0.2, 1),
     cex.axis = 1.2, cex = 2)
par(par_motores_dunnett)
# dev.off()

# ===== 7) Lentes intraoculares: leitura e preparação [slides 35–38] =====
if (!file.exists("lio.txt")) {
  stop("Arquivo lio.txt não encontrado. Use Aula-10 como diretório de trabalho.",
       call. = FALSE)
}
lentes <- read.table("lio.txt", header = TRUE)
summary(lentes)
fatores_lentes <- c("CNCTime", "PolTime", "ToolAge")
lentes[fatores_lentes] <- lapply(lentes[fatores_lentes], as.factor)
summary(lentes)
# O lote é a unidade experimental; as 30 lentes não são 30 réplicas de tratamento.

# ===== 8) Lentes: modelo saturado e gráfico de Daniel [slides 38–41] =====
modelo_lentes_saturado <- aov(Conf.rate ~ .^3, data = lentes)
summary(modelo_lentes_saturado)

# Removemos o componente QR associado ao intercepto, não a média global.
# Neste fatorial balanceado 2^3, os demais componentes QR são proporcionais
# aos efeitos fatoriais em magnitude, mas não estão na escala da resposta.
# Preservamos a construção do gráfico utilizada nos slides.
lentes_componentes_qr <- as.numeric(modelo_lentes_saturado$effects[-1])
lentes_rotulos <- trimws(rownames(summary.aov(modelo_lentes_saturado)[[1]]))
lentes_rotulos <- gsub("CNCTime", "Torno", lentes_rotulos, fixed = TRUE)
lentes_rotulos <- gsub("PolTime", "Pol", lentes_rotulos, fixed = TRUE)
lentes_rotulos <- gsub("ToolAge", "Idade", lentes_rotulos, fixed = TRUE)

# png(filename = "../figs/lio_Daniel.png",
#     width = 600, height = 600, bg = "transparent")
par_lentes_daniel <- par(no.readonly = TRUE)
par(mfrow = c(1, 1), mar = c(5, 6, 4, 2))
# Com datax = TRUE, qqnorm troca também os argumentos xlab e ylab ao desenhar.
lentes_qq <- qqnorm(lentes_componentes_qr,
                    datax = TRUE, pch = 16, cex = 2,
                    cex.axis = 1.2, cex.main = 1.3, cex.lab = 1.2,
                    ylim = c(-0.8, 0.8), las = 1,
                    main = "Gráfico de efeitos de Daniel",
                    xlab = "Quantis teóricos normais",
                    ylab = "Componentes QR dos efeitos")
qqline(lentes_componentes_qr, datax = TRUE)
# Rótulos abreviados: Torno = CNCTime; Pol = PolTime; Idade = ToolAge.
text(lentes_qq$x, lentes_qq$y, labels = lentes_rotulos,
     pos = 4, offset = 0.5, cex = 1)
par(par_lentes_daniel)
# dev.off()

# ===== 9) Lentes: modelo reduzido exploratório [slides 39–42] =====
# A inferência pressupõe que as interações excluídas sejam desprezíveis;
# o resíduo não é uma estimativa independente de erro puro.
# A ANOVA de proporções é uma aproximação didática: o modelo normal pode
# prever valores fora de [0, 1]. Não acrescentamos uma análise alternativa.
modelo_lentes_reduzido <- aov(Conf.rate ~ ., data = lentes)
summary(modelo_lentes_reduzido)
summary.lm(modelo_lentes_reduzido)$r.squared
# A não rejeição não comprova normalidade, especialmente com poucos lotes.
shapiro.test(modelo_lentes_reduzido$residuals)

par_lentes_res <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(modelo_lentes_reduzido, pch = 16, cex = 2)
par(par_lentes_res)

# ===== 10) Lentes: comparações e efeitos estimados [slides 43–45] =====
# Tempo do torno: curto menos longo, conforme a ordem dos níveis do fator.
lentes_tukey_torno <- glht(modelo_lentes_reduzido,
                          linfct = mcp(CNCTime = "Tukey"))
summary(lentes_tukey_torno)
# Tempo de polimento: curto menos longo.
lentes_tukey_polimento <- glht(modelo_lentes_reduzido,
                              linfct = mcp(PolTime = "Tukey"))
summary(lentes_tukey_polimento)
lentes_efeitos <- allEffects(modelo_lentes_reduzido)

# png(filename = "../figs/lio_effs.png",
#     width = 500, height = 500, bg = "transparent")
plot(lentes_efeitos)
# dev.off()

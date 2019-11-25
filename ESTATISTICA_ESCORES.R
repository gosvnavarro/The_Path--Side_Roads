################################
# ESTATISTICA PARA ESCORE PERS #
################################


############################
# ESTATISTICA - DESCRITIVA #
summary(dados_w0$CAPE)

summary(comparacao_escores$PERS_W1)

summary(comparacao_escores$PERS_ANY)


##############################
# ESTATISTICA - DISTRIBUICAO #
library(fitdistrplus)
library(logspline)

## We will use the function 'descdist' to gain some ideas about possible candidate distributions.
descdist(dados_w0_v2$CAPE, discrete = FALSE)

fit.weibull <- fitdist(dados_w0$PRS, "weibull")
plot(fit.weibull)

fit.norm <- fitdist(dados_w0$PRS, "norm")
plot(fit.norm)

# shapiro.test - função que testa a normalidade dos dados
shapiro.test(dados_w0_v2$CAPE)
hist(dados_w0_v2$CAPE, freq=FALSE, col="cornflowerblue", xlab="", main="")
curve(dnorm(x, mean=mean(dados_w0_v2$CAPE), sd=sd(dados_w0_v2$CAPE)), add = T, col = "red")

shapiro.test(comparacao_escores$PERS_W1)
hist(comparacao_escores$PERS_W1, freq=FALSE, col="cornflowerblue", xlab="Escores", main="Distribuição PERS-W1")
curve(dnorm(x, mean=mean(comparacao_escores$PERS_W1), sd=sd(comparacao_escores$PERS_W1)), add=TRUE, col="red")

shapiro.test(comparacao_escores$PERS_ANY)
hist(comparacao_escores$PERS_ANY, freq=FALSE, col="cornflowerblue", xlab="Escores", main="Distribuição PERS-ANY_PHASE")
curve(dnorm(x, mean=mean(comparacao_escores$PERS_ANY), sd=sd(comparacao_escores$PERS_ANY)), add=TRUE, col="red")

# ks.test - função que testa a normalidade dos dados
ks.test(dados_w0_v2$CAPE, 'pnorm', mean(dados_w0_v2$CAPE), sd(dados_w0_v2$CAPE))
qqnorm(dados_w0_v2$CAPE)
qqline(dados_w0_v2$CAPE, col="red")

ks.test(comparacao_escores$PERS_W1, 'pnorm', mean(comparacao_escores$PERS_W1), sd(comparacao_escores$PERS_W1))
qqnorm(comparacao_escores$PERS_W1)
qqline(comparacao_escores$PERS_W1)

ks.test(comparacao_escores$PERS_ANY, 'pnorm', mean(comparacao_escores$PERS_ANY), sd(comparacao_escores$PERS_ANY))
qqnorm(comparacao_escores$PERS_ANY)
qqline(comparacao_escores$PERS_ANY)

###################################
# ESTATISTICA - HOMOCEDASTICIDADE #
homocedasticidade_prs_w0 <- aov(dados$PP~dados$Grupo)
summary(mod)

#########################
# ESTATISTICA - TESTE T #

# Para uma amostra
t.test(comparacao_escores$PERS_W0, mu = 0.18820647)

t.test(comparacao_escores$PERS_W1)

t.test(comparacao_escores$PERS_ANY)

# Para amostras pareadas
t.test(comparacao_escores$PERS_W0, comparacao_escores$PERS_W1, paired = TRUE)


##############
# CORRELACAO #
library(corrplot)
library(stats)

resultado_cor1 <- cor(correlacao_1_c, method = "spearman")
corrplot.mixed(resultado_cor1, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor2 <- cor(correlacao_2_c, method = "spearman")
corrplot.mixed(resultado_cor2, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor3 <- cor(correlacao_3_c, method = "spearman")
corrplot.mixed(resultado_cor3, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor4 <- cor(correlacao_4_c, method = "spearman")
cor.test(correlacao_4$PERS, correlacao_4$CAPE, method = "spearman")$p.value
corrplot.mixed(resultado_cor4, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor5 <- cor(correlacao_5_c, method = "spearman")
cor.test(correlacao_5$PRS_c10, correlacao_5$CAPE, method = "spearman")$p.value
corrplot.mixed(resultado_cor5, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor6 <- cor(correlacao_6_c, method = "spearman")
cor.test(correlacao_6$PRS_c10, correlacao_6$PERS, method = "spearman")$p.value
corrplot.mixed(resultado_cor6, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

#############################
# REGRESSAO MULTIPLA LINEAR #
regressao_w0 <- lm(dados_w0_v2$CAPE ~ dados_w0_v2$PERS + dados_w0_v2$PRS, data = dados_w0_v2)
summary(regressao_w0) # show results
write.csv (regressao_w0$residuals, "residuals_w0.csv") # salvar os resultados
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_w0) # gráficos

regressao_w1 <- lm(dados_w1_v2$CAPE ~ dados_w1_v2$PERS + dados_w1_v2$PRS, data = dados_w1_v2)
summary(regressao_w1) # show results
write.csv (regressao_w1$residuals, "residuals_w1.csv") # salvar os resultados

##################
# REGRESSAO BETA #
library(betareg)
regressaoBETA_w0 <- betareg(formula = CAPE ~ PERS + PRS, data = dados_w0_v2_SID)
summary(regressaoBETA_w0) # show results

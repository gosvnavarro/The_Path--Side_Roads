#######################################
# ESTATISTICA - DESCRITIVA E APLICADA #
#######################################

############################
# ESTATISTICA - DESCRITIVA #
summary(ARQUIVO_DE_INTERESSE)
summary(ARQUIVO_DE_INTERESSE)
summary(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE)

########################################
# ESTATISTICA - DISTRIBUICAO (FORMA 1) #
library(fitdistrplus)
library(logspline)

# "descdist" - Funcao usada para obter algumas ideias sobre possíveis distribuicoes de candidatos
descdist(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE, discrete = FALSE)

fit.weibull <- fitdist(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE, "weibull")
plot(fit.weibull)

fit.norm <- fitdist(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE, "norm")
plot(fit.norm)

# "shapiro.test" - Função que testa a normalidade dos dados
shapiro.test(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE)
hist(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE, freq=FALSE, col="cornflowerblue", xlab="", main="")
curve(dnorm(x, mean=mean(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE), 
            sd=sd(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE)), add = T, col = "red")

# "ks.test" - Funcao que testa a normalidade dos dados
ks.test(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE, 'pnorm', mean(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE), 
        sd(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE))
qqnorm(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE)
qqline(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE, col="red")

#########################
# ESTATISTICA - TESTE T #

# Para uma amostra
t.test(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE)

# Para amostras pareadas
t.test(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE_1, ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE_2, paired = TRUE)

##############
# CORRELACAO #
library(corrplot)
library(stats)

resultado_cor1 <- cor(ARQUIVO_DE_INTERESSE, method = "spearman")
cor.test(ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE_1, ARQUIVO_DE_INTERESSE$COLUNA_DE_INTERESSE_2, 
         method = "spearman")$p.value
corrplot.mixed(resultado_cor1, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

#############################
# REGRESSAO MULTIPLA LINEAR #
regressao_w0 <- lm(ARQUIVO_DE_INTERESSE$VARIAVEL_DEPENDENTE ~ ARQUIVO_DE_INTERESSE$VARIAVEL_INDEPENDENTE
                   + ARQUIVO_DE_INTERESSE$VARIAVEL_INDEPENDENTE, data = ARQUIVO_DE_INTERESSE)
summary(regressao_w0) # Mostrar os resultados
write.csv (regressao_w0$residuals, "residuals_w0.csv") # Salvar os resultados

layout(matrix(c(1,2,3,4),2,2)) # Pagina com 4 gráficos, OPCIONAL!!
plot(regressao_w0) # Graficos

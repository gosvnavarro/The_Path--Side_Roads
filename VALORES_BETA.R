#############################################################
# PEGAR OS VALORES DE BETA DE UMA REGRESSAO LINEAR MULTIPLA #
#############################################################

# Pelo pacote lm.beta
library(lm.beta)
lm.final.beta <- lm.beta(ARQUIVO_REGRESSAO)
print(lm.final.beta)
summary(lm.final.beta)
coef(lm.final.beta)

# Pelo pacote QuantPsyc
library(QuantPsyc)
lm.beta(ARQUIVO_REGRESSAO)

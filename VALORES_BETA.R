#############################################################
# PEGAR OS VALORES DE BETA DE UMA REGRESSAO LINEAR MULTIPLA #
#############################################################

# Pelo pacote lm.beta
library(lm.beta)
lm.final.beta <- lm.beta(regressao_w1_corrigida_10pcs)
print(lm.final.beta)
summary(lm.final.beta)
coef(lm.final.beta)

# Pelo pacote QuantPsyc
library(QuantPsyc)
lm.beta(nova_regressao_1704)

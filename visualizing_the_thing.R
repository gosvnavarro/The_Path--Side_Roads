## Diferentes gráficos
## Author: Gabrielle Navarro
## Updated in: 24/02/2020

######################
# PLOTs - CORRELACAO #
######################
# USANDO "ggpubr"
library("ggpubr")
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

# USANDO "plot"
plot(correlacao_4$PERS, correlacao_4$CAPE, main = "CAPE X PERS",
     xlab = "Escore PERS", ylab = "Escore CAPE",
     pch = 19, frame = FALSE, xlim = c(0, 0.5), ylim = c(0, 2.5))
abline(lm(correlacao_4$CAPE ~ correlacao_4$PERS, data = correlacao_4), col = "red")

# USANDO 'ggplot'
library(ggplot2)
ggplot(correlacao_6, aes(x = PRS_c10, y = PERS)) + 
  geom_point() +
  labs(title = "PRS_c10 x PERS") +
  geom_smooth(method = lm)

#######################################################
## PLOTAGEM - REGRESSAO MULTIPLA, MODELO INTEGRATIVO ##
#################### A MAIS LEGAL ####################
library("ggplot2")
ggplot(DADOS_W0_SCZ_APOS_2, 
       aes(y = DADOS_W0_SCZ_APOS_2$CAPE, x = DADOS_W0_SCZ_APOS_2$PRS, 
           color = DADOS_W0_SCZ_APOS_2$PERS)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = F, colour = "red")

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fiti)

# Simple Scatterplot
plot(DADOS_W0_SCZ_APOS_2$CAPE, DADOS_W0_SCZ_APOS_2$PERS, 
     main = "Scatterplot Example",
     xlab = "cape escore ", ylab = "pers escore ", 
     pch = 19)

abline(lm(DADOS_W0_SCZ_APOS_2$CAPE ~ DADOS_W0_SCZ_APOS_2$PERS), col="red") # regression line (y~x)
lines(lowess(DADOS_W0_SCZ_APOS_2$CAPE, DADOS_W0_SCZ_APOS_2$PERS), col="blue") # lowess line (x,y)

# 3D Scatterplot with Coloring and Vertical Drop Lines
library(scatterplot3d)
library(ggrepel)

rbPal <- colorRampPalette(c('red','blue'))
DADOS_W0_SCZ_APOS_2$Col <- rbPal(10)[as.numeric(cut(DADOS_W0_SCZ_APOS_2$PERS, breaks = 10))]

scatterplot3d(DADOS_W0_SCZ_APOS_2$CAPE, DADOS_W0_SCZ_APOS_2$PERS, DADOS_W0_SCZ_APOS_2$PRS, 
              pch = 17, highlight.3d = F,
              type = "p", main = "3D Scatterplot", 
              color = DADOS_W0_SCZ_APOS_2$Col)

ggplot(DADOS_W0_SCZ_APOS_2, aes(x = DADOS_W0_SCZ_APOS_2$CAPE, y = DADOS_W0_SCZ_APOS_2$PERS)) +
  geom_point(color = DADOS_W0_SCZ_APOS_2$Col) +
  labs(title = ' ', x = 'Escore CAPE', y = 'Escore PERS-4')

# original
scatterplot3d(DADOS_W0_SCZ_APOS_2$CAPE, DADOS_W0_SCZ_APOS_2$PERS, DADOS_W0_SCZ_APOS_2$PRS, 
              pch = 16, highlight.3d = T,
              type = "h", main = "3D Scatterplot")

#############################################
## PLOTAGEM - SUBSET DE UM DADO ESPECIFICO ##
#############################################
# Criar o subset
poa_ind_w0 <- subset(comparacao_escores_BF, comparacao_escores_BF$Subject_ID < 12000)
poa_ind_w0_out <- subset(poa_ind_w0, poa_ind_w0$PERS_W0 > 0.2567)

#Plotar usando o ggplot
ggplot(poa_ind_w0, aes(x = poa_ind_w0$Grupo, y = poa_ind_w0$PERS_W0)) + 
  geom_boxplot() + 
  expand_limits(x = poa_ind_w0$Grupo, y = c(0,0.6)) +
  labs(title = 'Comparação PERS POA (W0)', x = 'Individuos', y = 'Escore') +
  geom_text_repel(aes(label = ifelse(poa_ind_w0$PERS_W0 > 0.38, as.character(poa_ind_w0$Subject_ID), '')), 
                  size = 2.5)

###################################
# VISUALIZANDO INFORMACOES GERAIS #
###################################
# Baseline (W0)
divisao.g <- PERS_W0_V3$Subject_ID < 12000
PERS_W0_V3$Grupo[divisao.g] <- 'POA'
PERS_W0_V3$Grupo[!divisao.g] <- 'SP'
PERS_W0_V3$Grupo <- factor(PERS_W0_V3$Grupo, labels = c('POA', 'SP'))

ggplot(PERS_W0_V3, aes(x = PERS_W0_V3$Grupo, y = PERS_W0_V3$PERS, color = Grupo)) + 
  geom_boxplot() + 
  expand_limits(x = PERS_W0_V3$Grupo, y = c(0,0.5)) +
  labs(title = 'Comparação PERS POA - SP (W0)', x = 'Individuos', y = 'Escore')

# Follow-up 3 anos (W1)
divisao.g <- PERS_W1_V3$Subject_ID < 12000
PERS_W1_V3$Grupo[divisao.g] <- 'POA'
PERS_W1_V3$Grupo[!divisao.g] <- 'SP'
PERS_W1_V3$Grupo <- factor(PERS_W0_V3$Grupo, labels = c('POA', 'SP'))

ggplot(PERS_W1_V3, aes(x = PERS_W1_V3$Grupo, y = PERS_W1_V3$PERS, color = Grupo)) + 
  geom_boxplot() + 
  expand_limits(x = PERS_W1_V3$Grupo, y = c(0,0.5)) +
  labs(title = 'Comparação PERS POA - SP (W1)', x = 'Individuos', y = 'Escore')

# W0 X W1
boxplot(comparacao_escores[, c("PERS_W0", "PERS_W1")])

#########################
# FAZENDO GRAFICOS NO R #
#########################
# Make sure to have installed ggrepel with install.packages("ggrepel")
library(ggrepel)
library(ggplot2)

################
# SCATTER PLOT #
## - EXEMPLO: PERS_W0 - Bruto

rbPal <- colorRampPalette(c('red','blue'))
VARAMBS_CALCULO_W1_V3$Col <- rbPal(10)[as.numeric(cut(VARAMBS_CALCULO_W1_V3$PERS,breaks = 10))] # This adds a column of color values, based on the y values

plot(VARAMBS_CALCULO_W0$PERS, main = 'Distribuição do escore PERS da W0', 
     ylab = 'Escore PERS', xlab = 'Indivíduos', 
     ylim = c(0,0.6), xlim = c(0,2520), 
     type = 'p', col=VARAMBS_CALCULO_W0$Col, pch = 20)

plot(VARAMBS_CALCULO_W0$Subject_ID, VARAMBS_CALCULO_W0$PERS, pch = 20, ylim = c(0,0.6))
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")


rbPal <- colorRampPalette(c('red','blue'))
VARAMBS_CALCULO_W0_V3$Col <- rbPal(10)[as.numeric(cut(VARAMBS_CALCULO_W0_V3$PERS,breaks = 10))]
VARAMBS_CALCULO_W1_V3$Col <- rbPal(10)[as.numeric(cut(VARAMBS_CALCULO_W1_V3$PERS,breaks = 10))]

ggplot(VARAMBS_CALCULO_W1_V3, aes(x = row.names(PERS_W0_V3), y = VARAMBS_CALCULO_W1_V3$PERS)) +
  geom_point(color=PERS_W0_V3$Col) +
  labs(title = 'Distribuição do PERS - W0', x = 'Nº de individuos', y = 'Escore') +
  expand_limits(x=c(0,2530), y=c(0,0.5)) +
  geom_text(aes(label = ifelse(PERS_W0_V3$PERS > 0.38, as.character(above_0.38$Subject_ID), ''),
                hjust = -0.1, vjust = 0.4, angle = 45))

# COMANDO QUE DEU MAIS CERTO!!
ggplot(VARAMBS_CALCULO_W0_V3, aes(x = VARAMBS_CALCULO_W0_V3$Subject_ID, y = VARAMBS_CALCULO_W0_V3$PERS)) +
  geom_point(color = VARAMBS_CALCULO_W0_V3$Col) +
  labs(title = 'Distribuição do PERS - W0', x = 'Nº de individuos', y = 'Escore') +
  geom_text(aes(label = VARAMBS_CALCULO_W0_V3$Subject_ID), size = 3, hjust = -0.1, vjust = 0.4)

## POA (COM LABEL)
poa_ind_w0 <- subset(VARAMBS_CALCULO_W0_V3, VARAMBS_CALCULO_W0_V3$Subject_ID < 12000)
poa_ind_w1 <- subset(VARAMBS_CALCULO_W1_V3, VARAMBS_CALCULO_W1_V3$Subject_ID < 12000)

ggplot(poa_ind_w0, aes(x = as.integer(poa_ind_w0$Subject_ID), y = poa_ind_w0$PERS)) +
  geom_point(color = poa_ind_w0$Col) +
  expand_limits(x = poa_ind_w0$Subject_ID, y = c(0,0.5)) +
  labs(title = 'Distribuição do PERS_POA - W1', x = 'Nº de individuos', y = 'Escore') +
  geom_text_repel(aes(label = ifelse(poa_ind_w0$PERS > 0.38, as.character(POA_ind_w1$Subject_ID), '')), 
                  size = 2.5)
## POA (SEM LABEL)
ggplot(poa_ind_w1, aes(x = as.integer(poa_ind_w1$Subject_ID), y = poa_ind_w1$PERS)) +
  geom_point(color = poa_ind_w1$Col) +
  expand_limits(x = poa_ind_w1$Subject_ID, y = c(0,0.5)) +
  labs(title = 'Distribuição do PERS_POA - W1', x = 'Nº de individuos', y = 'Escore')


## SP (COM LABEL)
sp_ind_w0 <- subset(VARAMBS_CALCULO_W0_V3, VARAMBS_CALCULO_W0_V3$Subject_ID > 12000)
sp_ind_w1 <- subset(VARAMBS_CALCULO_W1_V3, VARAMBS_CALCULO_W1_V3$Subject_ID > 12000)

ggplot(SP_ind, aes(x = as.integer(SP_ind$Subject_ID), y = SP_ind$PERS)) +
  geom_point(color = SP_ind$Col) +
  expand_limits(x = SP_ind$Subject_ID, y = c(0,0.5)) +
  labs(title = 'Distribuição do PERS_SP - W0', x = 'Nº de individuos', y = 'Escore') +
  geom_text_repel(aes(label = ifelse(SP_ind$PERS > 0.38, as.character(SP_ind$Subject_ID), '')), 
                  size = 2.5)

## SP (SEM LABEL)
ggplot(sp_ind_w1, aes(x = as.integer(sp_ind_w1$Subject_ID), y = sp_ind_w1$PERS)) +
  geom_point(color = sp_ind_w1$Col) +
  expand_limits(x = sp_ind_w1$Subject_ID, y = c(0,0.5)) +
  labs(title = 'Distribuição do PERS_SP - W0', x = 'Nº de individuos', y = 'Escore')



###############
# BOXPLOT BOX #
below.doze <- VARAMBS_CALCULO_W0_V3$Subject_ID < 12000
VARAMBS_CALCULO_W0_V3$Grupos[below.doze] <- 'POA'
VARAMBS_CALCULO_W0_V3$Grupos[!below.doze] <- 'SP'

VARAMBS_CALCULO_W0_V3$Grupos <- factor(VARAMBS_CALCULO_W0_V3$Grupos, labels = 'POA', 'SP')

ggplot(VARAMBS_CALCULO_W0_V3, aes(x = PERS_W0_V3$Subject_ID, y = PERS_W0_V3$PERS, color = Grupos)) + 
  geom_boxplot() + 
  expand_limits(x = PERS_W0_V3$Subject_ID, y = c(0,0.5)) +
  labs(title = 'Comparação PERS POA - SP (W0)', x = 'Individuos', y = 'Escore') 

ggplot(VARAMBS_CALCULO_W1_V3, aes(x = VARAMBS_CALCULO_W1_V3$Grupos, y = VARAMBS_CALCULO_W1_V3$PERS, color = Grupos)) + 
  geom_boxplot() + 
  expand_limits(x = VARAMBS_CALCULO_W1_V3$Grupos, y = c(0,0.5)) +
  labs(title = 'Comparação PERS POA - SP (W1)', x = 'Individuos', y = 'Escore')

boxplot(POA_ind$PERS, SP_ind$PERS,
        main = "PERS - POA e SP", xlab = c('Cidades'), ylab = "Escore",
        horizontal = F, notch = F)

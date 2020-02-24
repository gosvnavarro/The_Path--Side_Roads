## Comandos interessantes em R
## Author: Gabrielle Navarro
## Updated in: 24/02/2020

# Substituir termos
as.data.frame(teste_510_2 <- sub('.[_W0]$', '', w0_maior_w1$Subject_ID))
as.data.frame(teste_510_2 <- sub('.[_]$', '', w0_maior_w1$Subject_ID))

teste_510_2 <- NULL
as.data.frame(teste_510_2)

write(teste_510_2, file = 'ids_510.csv')

# Copiar uma coluna de uma data.frame para outra data.frame
data1$one <- data$x

# Juntar duas data.frame por um coluna em comum, exemplo:
library(plyr)
correcao <- join_all(list(residuos_prs, regressao_10pcs_w1), by = "ID")

# Adicionar uma nova coluna em um data frame com um valor especifico e depois reordenar
main$newcol <- rep(1,nrow(main))
main[,c(1,5,2,3,4)]

# Deletar linhas
DADOS_W0_SCZ_ancEUR <- DADOS_W0_SCZ_ancEUR[-c(457:506), ]

# Separar uma coluna em varias
library(tidyr)
X20544_3_gwas_imputed_v3_both_sexes_v2 <- separate(X20544_3_gwas_imputed_v3_both_sexes_v2, 
                                                   col = variant, into = c('CHR', 'BP', 'A1', 'A2'), 
                                                   sep = '\\:')

# Criar uma nova coluna a partir de duas outras
X20544_3_gwas_imputed_v3_both_sexes_v2$SNP <- paste(X20544_3_gwas_imputed_v3_both_sexes_v2$CHR, 
                                                    X20544_3_gwas_imputed_v3_both_sexes_v2$BP,  sep = ':')

# Renomear colunas
colnames(X20544_3_gwas_imputed_v3_both_sexes_v2)[12] = "BETA"

# Reordenar as colunas de uma data.frame
col_order <- c('CHR', 'BP', 'A1', 'A2', 'SNP', 'BETA', 'SE', 'P', 'minor_allele', 'minor_AF', 
               'expected_min_category_minor_AC', 'low_confidence_variant', 'n_complete_samples', 'AC', 'ytx', 'tstat')
X20544_3_gwas_imputed_v3_both_sexes_v2 <- X20544_3_gwas_imputed_v3_both_sexes_v2[, col_order]

#################
# PRS CORRIGIDO #
#################
# O PRS corrigido nada mais e que os resíduos da regressão 
#   que leva em consideracao o prs e os pcs de interesse
PRScorrigido <- lm(prs_pcs$PRS ~ prs_pcs$V3 + prs_pcs$V4 + prs_pcs$V5 + prs_pcs$V6 +
                     prs_pcs$V7 + prs_pcs$V8 + prs_pcs$V9 + prs_pcs$V10 + prs_pcs$V11 + prs_pcs$V12)


PRScorrigido_v2 <- lm(prs_pcs$CAPE ~ prs_pcs$V3 + prs_pcs$V4 + prs_pcs$V5 + prs_pcs$V6 +
                        prs_pcs$V7 + prs_pcs$V8 + prs_pcs$V9 + prs_pcs$V10 + prs_pcs$V11 + prs_pcs$V12)

PRScorrigido_v3 <- lm(prs_pcs$PRS ~ prs_pcs$PRS + prs_pcs$V3 + prs_pcs$V4 + prs_pcs$V5 + prs_pcs$V6 +
                        prs_pcs$V7 + prs_pcs$V8 + prs_pcs$V9 + prs_pcs$V10 + prs_pcs$V11 + prs_pcs$V12)

residuos_prs <- residuals(PRScorrigido)
residuos_prs_v2 <- residuals(PRScorrigido_v2)
residuos_prs_v3 <- residuals(PRScorrigido_v3)

head(residuos_prs)
head(residuos_prs_v2)
head(residuos_prs_v3)

##############################
# IDENTIFICACAO DOS OUTLIERS #
##############################
# Identificar a quantidade de outliers nos dados
outVals_W0 <- boxplot(comparacao_escores_BF$PERS_W0, plot = F)$out
which(comparacao_escores_BF$PERS_W0 %in% outVals_W0) # fazer esse comando caso queira saber o rowIndex
individuos_out_W0 <- comparacao_escores_BF[match(outVals_W0, comparacao_escores_BF$PERS_W0),]

outVals_W1 <- boxplot(comparacao_escores_BF$PERS_W1, plot = F)$out
which(comparacao_escores_BF$PERS_W1 %in% outVals_W1) # fazer esse comando caso queira saber o rowIndex
individuos_out_W1 <- comparacao_escores_BF[match(outVals_W1, comparacao_escores_BF$PERS_W1),]

# Salvar a tabela dos outliers
write.table(individuos_out_W0, file = 'individuos_out_W0', sep = '\t', row.names = F)
write.table(individuos_out_W1, file = 'individuos_out_W1', sep = '\t', row.names = F)

# Gráfico
divisao.g <- comparacao_escores_BF$Subject_ID < 12000
comparacao_escores_BF$Grupo[divisao.g] <- 'POA'
comparacao_escores_BF$Grupo[!divisao.g] <- 'SP'
comparacao_escores_BF$Grupo <- factor(comparacao_escores_BF$Grupo, labels = c('POA', 'SP'))

ggplot(comparacao_escores_BF, aes(x = comparacao_escores_BF$Grupo, y = comparacao_escores_BF$PERS_W0, color = Grupo)) + 
  geom_boxplot() + 
  expand_limits(x = comparacao_escores_BF$Grupo, y = c(0,0.5)) +
  labs(title = 'Comparação PERS POA - SP (W0)', x = 'Individuos', y = 'Escore') +
  geom_text_repel(aes(label = ifelse(comparacao_escores_BF$PERS_W0 > 0.38, as.character(comparacao_escores_BF$Subject_ID), '')), 
                  size = 2.5)

## versao 2
summary(comparacao_escores_BF$PERS_W0)
pregnancy.df.IQR <- 0.1720 - 0.1644
pregnancy.df.IQR <-IQR(comparacao_escores_BF$PERS_W0)
pregnancy.df.IQR

LowerInnerFence <- 0.1644 - 1.5 * pregnancy.df.IQR
UpperInnerFence <- 0.1720 + 1.5 * pregnancy.df.IQR
print(LowerInnerFence); print(UpperInnerFence)

print(which(comparacao_escores_BF$PERS_W0 > UpperInnerFence))
print(which(comparacao_escores_BF$PERS_W0 < LowerInnerFence))
print(comparacao_escores_BF$PERS_W0[which(comparacao_escores_BF$PERS_W0 > 1.5), ])

## versao 3
source("https://goo.gl/4mthoF")
outlierKD(PERS_W0_V3, PERS)
boxplot.stats(comparacao_escores_BF$PERS_W0)$out

####################################
# ABRIR E MANIPULAR ARQUIVO '.rds' #
####################################
# Ler um arquivo '.rds' no R
dados <- readRDS('C:/Users/gabio/Desktop/Navarro_HRC_20190119.rds', refhook = NULL)

# Salvar esse arquivo em outro formato para facilitar a manipulação do db;
# Neste caso usaremos o formato de saíde '.csv', uma vez que o mesmo poderá ser aberto no excel
write.csv2(dados, "C:/Users/gabio/Desktop/new_db_inpd_t2.csv")

######################
# FILTRAGEM DE DADOS #
######################
# Chamar a tabela e biblioteca
library(dplyr)

# Checar se quais linhas ('individuos') não tem nenhuma complicacao
results <- filter(any_complication, any_complication$fat9_W0 == 0 & any_complication$fat12_W0 == 0 & 
                    any_complication$fat13_W0 == 0 & any_complication$fat15_W0 == 0 &
                    any_complication$fat16a_W0 == 0 & any_complication$fat16b_W0 == 0 &
                    any_complication$fat16c_W0 == 0 & any_complication$fat16d_W0 == 0 &
                    any_complication$fat17_W0 == 0 & any_complication$fat18_W0 == 0)

# Checar na tabela final quais são os indivuos
any_complication$FINAL <- any_complication$Subject_ID %in% results$Subject_ID

# Salvar a tabela
write.csv(any_complication, file = 'any_complications_RESULTS.csv', sep = '/t', row.names = F)
write.csv(results, file = 'ind_sem_complicacoes.csv', sep = '/t', row.names = F)

#########################################################
# ANALISE FATORIAL DE VARIAVEIS COM MAIS DE 3 VARIAVEIS #
#########################################################

# Chamar para o ambiente a tabela com as variaveis de cada fase e os pacotes
library(readr)
library(corrplot)
library(RColorBrewer)
varambs_W0 <- read_delim("Documents/Gabe/project/database_ambiental/varambs_W0.csv", 
                         +     "\t", escape_double = FALSE, trim_ws = TRUE)

# Excluir a coluna com os IDS
varamb3_filtragem_semB_ <- varamb3_filtragem_semB_[, -1]

# Aplica??o do PCA
varamb3_filtragem_semB_pca <- prcomp(na.omit(varamb3_filtragem_semB_), scale=TRUE)
summary(varamb3_filtragem_semB_pca)
plot(varamb3_filtragem_semB_pca)

# Fatorial
varamb3_filtragem_semB_AF_v2 <- factanal(na.omit(varamb3_filtragem_semB_), factors = 4, rotation = 'varimax', scores = 'regression') 
varamb3_filtragem_semB_AF_v2
head(varamb3_filtragem_semB_AF_v2, n = 9L)

# Plotagem dos gr?ficos
corrplot(varamb3_filtragem_semB_AF_v2$correlation, method="color",  
         type="full", addCoef.col = "black", addshade = 'all',
         tl.col="black", tl.srt=45, col=brewer.pal(n=8, name="RdBu"))

corrplot(varamb3_filtragem_semB_AF_v2$loadings, method="color",  
         type="full", addCoef.col = "black", addshade = 'all',
         tl.col="black", tl.srt=45, col=brewer.pal(n=8, name="RdBu"))
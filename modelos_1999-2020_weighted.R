########################################################################
#Universidade do Estado de São Paulo - USP
#Faculdade de Saúde Pública - FSP
#Programa de Pós Graduação em Nutrição em Saúde Pública - PPGNSP
#Laboratório de Avaliação do Estado Nutricional de Populações - LANPOP
#Variação o peso corporal ao longo da fase adulta: uma análise da NHANES 1999-2019
#Versão Final - 10/20
#Autora: Débora Borges dos Santos Pereira
#Co-Autora: Claudia Cristina Vieira Pastorello
#Co-Autora: Mariane Helen de Oliveria
#Co-Autora: Iolanda Karla Santana dos Santos
#Co-Autora: Camila Medeiros da Silva Mazzeti
#Orientação: Wolney Lisboa Conde
########################################################################

#pacotes utilizados
library(foreign)
library(tidyverse)
library(anchors)
library(readxl)
library(openxlsx)
library(dplyr)
library(mice)
library(VIM)
library(lattice)
library(ggplot2)
library(hrbrthemes)
library(Rcpp)
library(roxygen2)
library(devtools)
library(digest)
library(lme4)
library(sjPlot)
library(reshape2)
library(glmmTMB)
library(nlme)
library(rlist)
library(psych)
library(summarytools)
library(glue)
library(stats)



# _________________________________________
# _________________________________________
# Analise Multinivel
# _________________________________________
# _________________________________________

setwd("C:/Users/Débora/Desktop/contra prova")

# Peso da Ponderação
PESO <- read.xlsx("peso_ponderacao.xlsx")
PESO <- rename(PESO, c("id" = "SEQN"))

# GERAL
NH_GERAL <- read.csv('NHS_molted_GERAL_FINAL.csv')
NH_GERAL <- merge(NH_GERAL, PESO, by = "id")

# Mulheres
NH_FEM <- read.csv('NHS_molted_FEM_FINAL.csv')
NH_FEM <- merge(NH_FEM, PESO, by = "id")

write.xlsx(NH_FEM, file = "NH_FEM_pond.xlsx", sheetName = "NH_FEM_pond", append = FALSE)


# Homens
NH_MASC <- read.csv('NHS_molted_MASC_FINAL.csv')
NH_MASC <- merge(NH_MASC, PESO, by = "id")


write.xlsx(NH_MASC, file = "NH_MASC_pond.xlsx", sheetName = "NH_MASC_pond", append = FALSE)

# Gerações
NH_G1 <- read.csv('NHS_molted_G1_FINAL.csv')
NH_G1 <- merge(NH_G1, PESO, by = "id")
NH_G2 <- read.csv('NHS_molted_G2_FINAL.csv')
NH_G2 <- merge(NH_G2, PESO, by = "id")

NH_G3 <- read.csv('NHS_molted_G3_FINAL.csv')
NH_G3 <- merge(NH_G3, PESO, by = "id")

NH_G4 <- read.csv('NHS_molted_G4_FINAL.csv')
NH_G4 <- merge(NH_G4, PESO, by = "id")

# Estado Nutricional
NH_EN1 <- read.csv('NHS_molted_EN1_FINAL.csv')
NH_EN1 <- merge(NH_EN1, PESO, by = "id")

NH_EN2 <- read.csv('NHS_molted_EN2_FINAL.csv')
NH_EN2 <- merge(NH_EN2, PESO, by = "id")

NH_EN3 <- read.csv('NHS_molted_EN3_FINAL.csv')
NH_EN3 <- merge(NH_EN3, PESO, by = "id")

NH_EN4 <- read.csv('NHS_molted_EN4_FINAL.csv')
NH_EN4 <- merge(NH_EN4, PESO, by = "id")

# Multinivel Geral
NH_GERAL$idadedoimc <- as.factor(as.character(NH_GERAL$idadedoimc))

mod_geral <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_GERAL, weights = MEC72Y)
summary(mod_geral)
tab_model(mod_geral)


summary(modelo_final_hlm2)
tab_model(modelo_final_hlm2)


sjPlot::plot_model(mod_geral,
                   colors = "Accent",
                   show.values = TRUE,
                   value.offset = .4,
                   value.size = 4,
                   dot.size = 1,
                   line.size = 1,
                   vline.color = "blue",
                   width = .5)

mod_geral.fix <- fixed.effects(mod_geral)
mod_geral.fix
mod_geral.blup <- random.effects(mod_geral)
NH_GERAL$mod_geral.blup <- mod_geral.blup
NH_GERAL$imcpredito <- predict(mod_geral)
NH_GERAL$modelMAE <- (NH_GERAL$imc - NH_GERAL$imcpredito)
MAE_GERAL <-mean(NH_GERAL$modelMAE)
MAE_GERAL
NH_GERAL$modelMSE <- (NH_GERAL$imc - NH_GERAL$imcpredito)^2
MSE_GERAL <- mean(NH_GERAL$modelMSE)
MSE_GERAL

# Multinivel Femino
NH_FEM$idadedoimc <- as.factor(as.character(NH_FEM$idadedoimc))
mod_fem <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_FEM, weights = MEC72Y)
mod_fem
summary(mod_fem)
tab_model(mod_fem)
# sjPlot::plot_model(mod_fem,
                   # colors = "Accent", 
                   # show.values = TRUE,
                   # value.offset = .4,
                   # value.size = 4,
                   # dot.size = 1,
                   # line.size = 1,
                   # vline.color = "blue",
                   # width = .5)

# mod_fem.fix <- fixed.effects(mod_fem)
# mod_fem.fix
# mod_fem.blup <- random.effects(mod_fem)
# NH_FEM$mod_fem.blup <- mod_fem.blup
NH_FEM$imcpredito <- predict(mod_fem)
NH_FEM$modelMAE <- (NH_FEM$imc - NH_FEM$imcpredito)
MAE_FEM <-mean(NH_FEM$modelMAE)
MAE_FEM
NH_FEM$modelMSE <- (NH_FEM$imc - NH_FEM$imcpredito)^2
MSE_FEM <- mean(NH_FEM$modelMSE)
MSE_FEM
LogLikelyhood_FEM <- logLik(mod_fem, REML = FALSE)
LogLikelyhood_FEM

# Multinivel Masculino
NH_MASC$idadedoimc <- as.factor(as.character(NH_MASC$idadedoimc))
mod_masc <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_MASC, weights = MEC72Y)
mod_masc
summary(mod_masc)
tab_model(mod_masc)
# sjPlot::plot_model(mod_masc,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_masc.fix <- fixed.effects(mod_masc)
# mod_masc.fix
# mod_masc.blup <- random.effects(mod_masc)
# NH_MASC$mod_masc.blup <- mod_masc.blup
NH_MASC$imcpredito <- predict(mod_masc)
NH_MASC$modelMAE <- (NH_MASC$imc - NH_MASC$imcpredito)
MAE_MASC <-mean(NH_MASC$modelMAE)
MAE_MASC
NH_MASC$modelMSE <- (NH_MASC$imc - NH_MASC$imcpredito)^2
MSE_MASC <- mean(NH_MASC$modelMSE)
MSE_MASC
LogLikelyhood_MASC <- logLik(mod_masc, REML = FALSE)
LogLikelyhood_MASC

# plot por sexo do modelo de efeitos fixos
idadedoimc_masc <- 18:65
sexo_masc <- rep('Male',48)

mod_masc_use <- summary(mod_masc)
mod_masc_use$coefficients
media_imc_masc <- mod_masc_use$coefficients[,1]
media_imc_masc # 25.056  intercept
media_imc_masc <- media_imc_masc + 25.056
media_imc_masc[1] <- media_imc_masc[1] - 25.056
media_imc_masc
desvio_padrao_imc_masc <- mod_masc_use$coefficients[,2]

MOD_NH_MASC <- as.data.frame(cbind(idadedoimc_masc, media_imc_masc, 
                                   desvio_padrao_imc_masc,
                                   sexo_masc))
colnames(MOD_NH_MASC) <- c('idadedoimc', 'media_imc', 
                           'desvio_padrao_imc',
                           'sexo')

idadedoimc_fem <- 18:65
sexo_fem <- rep('Female',48)

mod_fem_use <- summary(mod_fem)
media_imc_fem <- mod_fem_use$coefficients[,1]
media_imc_fem # 23.769 intercept
media_imc_fem <- media_imc_fem + 23.769
media_imc_fem[1] <- media_imc_fem[1] - 23.769
media_imc_fem
desvio_padrao_imc_fem <- mod_fem_use$coefficients[,2]

MOD_NH_FEM <- as.data.frame(cbind(idadedoimc_fem, media_imc_fem, 
                                  desvio_padrao_imc_fem,
                                  sexo_fem))
colnames(MOD_NH_FEM) <- c('idadedoimc', 'media_imc', 
                          'desvio_padrao_imc',
                          'sexo')

NH_SEX <-  as.data.frame(rbind(MOD_NH_FEM, MOD_NH_MASC))

NH_SEX$idadedoimc <- as.numeric(NH_SEX$idadedoimc)
NH_SEX$media_imc <- as.numeric(NH_SEX$media_imc)
NH_SEX$desvio_padrao_imc <- as.numeric(NH_SEX$desvio_padrao_imc)
NH_SEX$sexo <- as.factor(as.character(NH_SEX$sexo))


ggplot(NH_SEX, aes(x=idadedoimc, y=media_imc, group=sexo, color=sexo)) + 
  geom_errorbar(aes(ymin=media_imc-desvio_padrao_imc, 
                    ymax=media_imc+desvio_padrao_imc), width=.005, 
                position=position_dodge(0.005)) +
  scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5))  +
  scale_y_continuous(limits = c(20,35), 
                     breaks = seq(20,35, by=5))   +
  geom_line() + geom_point()+ geom_smooth()+
  scale_color_brewer(palette="Paired")+theme_minimal()+ 
  labs(colour = "Sexo", x = 'Age (years)', y='Body Mass Index (kg/m²)',
       tag = '',
       caption = "Source: NHANES 1999-2020 Adults (18 to 65 years old)") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        plot.caption = element_text(hjust=0, face = "italic", color = "darkgray"),
        plot.caption.position = "plot",
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="black"),
        panel.grid.major.x = element_line(color="darkgrey", size = 0.1),
        panel.grid.major.y = element_line(color="darkgrey", size = 0.1, linetype = "dotted"),                                          plot.tag.position = 'top',
        plot.tag = element_text(vjust = 1, hjust = 1),
        legend.position="bottom") +
  scale_color_manual(name=NULL,
                     values = c("#909090",
                                "#606060"))+
  geom_hline(yintercept=25, color="black", size=0.5, linetype = 'dashed') + 
  geom_hline(yintercept=30, color="black", size=0.5, linetype = 'dashed') 

# Multinivel Geração 1
NH_G1$idadedoimc <- as.factor(as.character(NH_G1$idadedoimc))
mod_G1 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G1, weights = MEC72Y)
mod_G1
summary(mod_G1)
tab_model(mod_G1)
# sjPlot::plot_model(mod_G1,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_G1.fix <- fixed.effects(mod_G1)
# mod_G1.fix
# mod_G1.blup <- random.effects(mod_G1)
# NH_G1$mod_G1.blup <- mod_G1.blup
NH_G1$imcpredito <- predict(mod_G1)
NH_G1$modelMAE <- (NH_G1$imc - NH_G1$imcpredito)
MAE_G1 <-mean(NH_G1$modelMAE)
MAE_G1
NH_G1$modelMSE <- (NH_G1$imc - NH_G1$imcpredito)^2
MSE_G1 <- mean(NH_G1$modelMSE)
MSE_G1
LogLikelyhood_G1 <- logLik(mod_G1, REML = FALSE)
LogLikelyhood_G1

# Multinivel Geração 2
NH_G2$idadedoimc <- as.factor(as.character(NH_G2$idadedoimc))
mod_G2 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G2, weights = MEC72Y)
mod_G2
summary(mod_G2)
# sjPlot::plot_model(mod_G2,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_G2.fix <- fixed.effects(mod_G2)
# mod_G2.fix
# mod_G2.blup <- random.effects(mod_G2)
# NH_G2$mod_G2.blup <- mod_G2.blup
NH_G2$imcpredito <- predict(mod_G2)
NH_G2$modelMAE <- (NH_G2$imc - NH_G2$imcpredito)
MAE_G2 <-mean(NH_G2$modelMAE)
MAE_G2
NH_G2$modelMSE <- (NH_G2$imc - NH_G2$imcpredito)^2
MSE_G2 <- mean(NH_G2$modelMSE)
MSE_G2
LogLikelyhood_G2 <- logLik(mod_G2, REML = FALSE)
LogLikelyhood_G2


# Multinivel Geração 3
NH_G3$idadedoimc<- as.factor(as.character(NH_G3$idadedoimc))
mod_G3 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G3, weights = MEC72Y)
mod_G3
summary(mod_G3)
# sjPlot::plot_model(mod_G3,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_G3.fix <- fixed.effects(mod_G3)
# mod_G3.fix
# mod_G3.blup <- random.effects(mod_G3)
# NH_G3$mod_G3.blup <- mod_G3.blup
NH_G3$imcpredito <- predict(mod_G3)
NH_G3$modelMAE <- (NH_G3$imc - NH_G3$imcpredito)
MAE_G3 <-mean(NH_G3$modelMAE)
MAE_G3
NH_G3$modelMSE <- (NH_G3$imc - NH_G3$imcpredito)^2
MSE_G3 <- mean(NH_G3$modelMSE)
MSE_G3
LogLikelyhood_G3 <- logLik(mod_G3, REML = FALSE)
LogLikelyhood_G3

# Multinivel Geração 4
NH_G4$idadedoimc <- as.factor(as.character(NH_G4$idadedoimc))
mod_G4 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G4, weights = MEC72Y)
mod_G4
summary(mod_G4)
# sjPlot::plot_model(mod_G4,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_G4.fix <- fixed.effects(mod_G4)
# mod_G4.fix
# mod_G4.blup <- random.effects(mod_G4)
# NH_G4$mod_G4.blup <- mod_G4.blup
NH_G4$imcpredito <- predict(mod_G4)
NH_G4$modelMAE <- (NH_G4$imc - NH_G4$imcpredito)
MAE_G4 <-mean(NH_G4$modelMAE)
MAE_G4
NH_G4$modelMSE <- (NH_G4$imc - NH_G4$imcpredito)^2
MSE_G4 <- mean(NH_G4$modelMSE)
MSE_G4
LogLikelyhood_G4 <- logLik(mod_G4, REML = FALSE)
LogLikelyhood_G4


# plot por Geração do modelo de efeitos fixos
idadedoimc_G1 <- 40:65
G1 <- rep('Baixo peso',25)

mod_GI_use <- summary(mod_G1)
media_imc_G1 <- mod_GI_use$coefficients[,1]
media_imc_G1 # 23.277216 intercept
media_imc_G1 <- media_imc_G1 + 23.277216
media_imc_G1[1] <- media_imc_G1[1] - 23.277216
media_imc_G1 <- head(media_imc_G1,-1)
media_imc_G1
desvio_padrao_imc_G1 <- mod_GI_use$coefficients[,2]

MOD_NH_G1 <- as.data.frame(cbind(idadedoimc_G1, media_imc_G1, 
                                 desvio_padrao_imc_G1,
                                 G1))
colnames(MOD_NH_G1) <- c('idadedoimc', 'media_imc', 
                         'desvio_padrao_imc',
                         'geracao')

idadedoimc_G2 <- 30:65
G2 <- rep('Born from 1950 to 1960',25)

mod_G2_use <- summary(mod_G2)
media_imc_G2 <- mod_G2_use$coefficients[,1]
media_imc_G2 # 23.73 intercept
media_imc_G2 <- media_imc_G2 + 23.73
media_imc_G2[1] <- media_imc_G2[1] - 23.73
media_imc_G2 <- head(media_imc_G2,-1)
media_imc_G2
desvio_padrao_imc_G2 <- mod_G2_use$coefficients[,2]

MOD_NH_G2 <- as.data.frame(cbind(idadedoimc_G2, media_imc_G2, 
                                 desvio_padrao_imc_G2,
                                 G2))
colnames(MOD_NH_G2) <- c('idadedoimc', 'media_imc', 
                         'desvio_padrao_imc',
                         'geracao')

idadedoimc_G3 <- 19:58
G3 <- rep('Born from 1960 to 1970',39)

mod_G3_use <- summary(mod_G3)
media_imc_G3 <- mod_G3_use$coefficients[,1]
media_imc_G3 # 23.13 intercept
media_imc_G3 <- media_imc_G3 + 23.13
media_imc_G3[1] <- media_imc_G3[1] - 23.13
media_imc_G3 <- head(media_imc_G3,-1)
media_imc_G3
desvio_padrao_imc_G3 <- mod_G3_use$coefficients[,2]

MOD_NH_G3 <- as.data.frame(cbind(idadedoimc_G3, media_imc_G3, 
                                 desvio_padrao_imc_G3,
                                 G3))
colnames(MOD_NH_G3) <- c('idadedoimc', 'media_imc', 
                         'desvio_padrao_imc',
                         'geracao')

idadedoimc_G4 <- 18:48
G4 <- rep('Born in 1970 or later',29)

mod_G4_use <- summary(mod_G4)
media_imc_G4 <- mod_G4_use$coefficients[,1]
media_imc_G4 # 24.33 intercept
media_imc_G4 <- media_imc_G4 + 24.33
media_imc_G4[1] <- media_imc_G4[1] - 24.33
media_imc_G4
desvio_padrao_imc_G4 <- mod_G4_use$coefficients[,2]

MOD_NH_G4 <- as.data.frame(cbind(idadedoimc_G4, media_imc_G4, 
                                 desvio_padrao_imc_G4,
                                 G4))
colnames(MOD_NH_G4) <- c('idadedoimc', 'media_imc', 
                         'desvio_padrao_imc',
                         'geracao')

#remoção de ruído gerado no loop geracional
MOD_NH_G1 <- MOD_NH_G1[-27,]
MOD_NH_G2 <- MOD_NH_G2[-37,]
MOD_NH_G3 <- MOD_NH_G3[-c(41,42,43),]
MOD_NH_G4 <- MOD_NH_G4[-c(32,33),]

NH_G <-  as.data.frame(rbind(MOD_NH_G1, MOD_NH_G2,MOD_NH_G3, MOD_NH_G4))

NH_G$idadedoimc <- as.numeric(NH_G$idadedoimc)
NH_G$media_imc <- as.numeric(NH_G$media_imc)
NH_G$desvio_padrao_imc <- as.numeric(NH_G$desvio_padrao_imc)
NH_G$geracao <- as.factor(as.character(NH_G$geracao))

NH_G$geracao = with(NH_G, reorder(geracao, idadedoimc, median))

ggplot(NH_G, aes(x=idadedoimc, y=media_imc, group=geracao, colour=geracao)) + 
  geom_errorbar(aes(ymin=media_imc-desvio_padrao_imc, 
                    ymax=media_imc+desvio_padrao_imc), width=.005, 
                position=position_dodge(0.005)) +
  geom_line() + geom_point()+geom_smooth()+
    scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5))  +
  scale_y_continuous(limits = c(20,35), 
                     breaks = seq(20,35, by=5))   +
  scale_color_brewer(palette="Paired",)+theme_minimal()+
  labs(colour = "Generation", x = 'age (years)', y='Body Mass Index (Kg/m²)',
       tag = 'Fixed-effects Multilevel Analysis by Generation Stratification',
       caption = "Source: NHANES 1999-2018 Adults (18 to 65 years old)") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        plot.caption = element_text(hjust=0, face = "italic", color = "darkgray"),
        plot.caption.position = "plot",
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="black"),
        panel.grid.major.x = element_line(color="darkgrey", size = 0.1),
        panel.grid.major.y = element_line(color="darkgrey", size = 0.1, linetype = "dotted"),                                          plot.tag.position = 'top',
        plot.tag = element_text(vjust = 1, hjust = 1),
        legend.position="bottom") +
  scale_color_manual(name=NULL,
                     values = c("#202020", "#606060", "#909090","#D0D0D0")) +
  geom_hline(yintercept=25, color="black", size=0.5, linetype = 'dashed') + 
  geom_hline(yintercept=30, color="black", size=0.5, linetype = 'dashed')




# Multinivel ESTNUT1 Baixo Peso
NH_EN1$idadedoimc <- as.factor(as.character(NH_EN1$idadedoimc))
mod_EN1 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN1, weights = MEC72Y)
mod_EN1
summary(mod_EN1)
# sjPlot::plot_model(mod_EN1,
#                    colors = "Accent",
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_EN1.fix <- fixed.effects(mod_EN1)
# mod_EN1.fix
# mod_EN1.blup <- random.effects(mod_EN1)
# NH_EN1$mod_EN1.blup <- mod_EN1.blup
NH_EN1$imcpredito <- predict(mod_EN1)
NH_EN1$modelMAE <- (NH_EN1$imc - NH_EN1$imcpredito)
MAE_EN1 <-mean(NH_EN1$modelMAE)
MAE_EN1
NH_EN1$modelMSE <- (NH_EN1$imc - NH_EN1$imcpredito)^2
MSE_EN1 <- mean(NH_EN1$modelMSE)
MSE_EN1
LogLikelyhood_EN1 <- logLik(mod_EN1, REML = FALSE)
LogLikelyhood_EN1


# Multinivel ESTNUT2 Peso Normal
NH_EN2$idadedoimc <- as.factor(as.character(NH_EN2$idadedoimc))
mod_EN2 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN2, weights = MEC72Y)
mod_EN2
summary(mod_EN2)
# sjPlot::plot_model(mod_EN2,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_EN2.fix <- fixed.effects(mod_EN2)
# mod_EN2.fix
# mod_EN2.blup <- random.effects(mod_EN2)
# NH_EN2$mod_EN2.blup <- mod_EN2.blup
NH_EN2$imcpredito <- predict(mod_EN2)
NH_EN2$modelMAE <- (NH_EN2$imc - NH_EN2$imcpredito)
MAE_EN2 <-mean(NH_EN2$modelMAE)
MAE_EN2
NH_EN2$modelMSE <- (NH_EN2$imc - NH_EN2$imcpredito)^2
MSE_EN2 <- mean(NH_EN2$modelMSE)
MSE_EN2
LogLikelyhood_EN2 <- logLik(mod_EN2, REML = FALSE)
LogLikelyhood_EN2


# Multinivel ESTNUT3 Sobrepeso
NH_EN3$idadedoimc <- as.factor(as.character(NH_EN3$idadedoimc))
mod_EN3 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN3, weights = MEC72Y)
mod_EN3
summary(mod_EN3)
# sjPlot::plot_model(mod_EN3,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_EN3.fix <- fixed.effects(mod_EN3)
# mod_EN3.fix
# mod_EN3.blup <- random.effects(mod_EN3)
# NH_EN3$mod_EN3.blup <- mod_EN3.blup
NH_EN3$imcpredito <- predict(mod_EN3)
NH_EN3$modelMAE <- (NH_EN3$imc - NH_EN3$imcpredito)
MAE_EN3 <-mean(NH_EN3$modelMAE)
MAE_EN3
NH_EN3$modelMSE <- (NH_EN3$imc - NH_EN3$imcpredito)^2
MSE_EN3 <- mean(NH_EN3$modelMSE)
MSE_EN3
LogLikelyhood_EN3 <- logLik(mod_EN3, REML = FALSE)
LogLikelyhood_EN3


# Multinivel ESTNUT4 - Obesidade
NH_EN4$idadedoimc <- as.factor(as.character(NH_EN4$idadedoimc))
mod_EN4 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN4, weights = MEC72Y)
mod_EN4
summary(mod_EN4)
# sjPlot::plot_model(mod_EN4,
#                    colors = "Accent", 
#                    show.values = TRUE,
#                    value.offset = .4,
#                    value.size = 4,
#                    dot.size = 1,
#                    line.size = 1,
#                    vline.color = "blue",
#                    width = .5)
# 
# mod_EN4.fix <- fixed.effects(mod_EN4)
# mod_EN4.fix
# mod_EN4.blup <- random.effects(mod_EN4)
# NH_EN4$mod_EN4.blup <- mod_EN4.blup
NH_EN4$imcpredito <- predict(mod_EN4)
NH_EN4$modelMAE <- (NH_EN4$imc - NH_EN4$imcpredito)
MAE_EN4 <-mean(NH_EN4$modelMAE)
MAE_EN4
NH_EN4$modelMSE <- (NH_EN4$imc - NH_EN4$imcpredito)^2
MSE_EN4 <- mean(NH_EN4$modelMSE)
MSE_EN4
LogLikelyhood_EN4 <- logLik(mod_EN4, REML = FALSE)
LogLikelyhood_EN4


# plot por Estado Nutricional do modelo de efeitos fixos
idadedoimc_EN1 <- 18:65
EN1_rep <- rep('Baixo peso',48)

mod_EN1_use <- summary(mod_EN1)
mod_EN1_use$coefficients
media_imc_EN1 <- mod_EN1_use$coefficients[,1]
media_imc_EN1 # 16.5993022  intercept
media_imc_EN1 <- media_imc_EN1 + 16.5993022 
media_imc_EN1[1] <- media_imc_EN1[1] - 16.5993022 
desvio_padrao_imc_EN1 <- mod_EN1_use$coefficients[,2]

MOD_NH_EN1 <- as.data.frame(cbind(idadedoimc_EN1, media_imc_EN1, 
                                   desvio_padrao_imc_EN1,
                                   EN1_rep))
colnames(MOD_NH_EN1) <- c('idadedoimc', 'media_imc', 
                           'desvio_padrao_imc',
                           'estado_nutricional')

idadedoimc_EN2 <- 18:65
EN2_rep <- rep('Peso Normal',48)

mod_EN2_use <- summary(mod_EN2)
media_imc_EN2 <- mod_EN2_use$coefficients[,1]
media_imc_EN2 # 20.8470499
media_imc_EN2 <- media_imc_EN2 + 20.8470499
media_imc_EN2[1] <- media_imc_EN2[1] - 20.8470499
media_imc_EN2
desvio_padrao_imc_EN2 <- mod_EN2_use$coefficients[,2]

MOD_NH_EN2 <- as.data.frame(cbind(idadedoimc_EN2, media_imc_EN2, 
                                  desvio_padrao_imc_EN2,
                                  EN2_rep))
colnames(MOD_NH_EN2) <- c('idadedoimc', 'media_imc', 
                          'desvio_padrao_imc',
                          'estado_nutricional')

idadedoimc_EN3 <- 18:65
EN3_rep <- rep('Sobrepeso',48)

mod_EN3_use <- summary(mod_EN3)
media_imc_EN3 <- mod_EN3_use$coefficients[,1]
media_imc_EN3 # 25.5184096 intercept
media_imc_EN3 <- media_imc_EN3 + 25.5184096
media_imc_EN3[1] <- media_imc_EN3[1] - 25.5184096
media_imc_EN3
desvio_padrao_imc_EN3 <- mod_EN3_use$coefficients[,2]

MOD_NH_EN3 <- as.data.frame(cbind(idadedoimc_EN3, media_imc_EN3, 
                                 desvio_padrao_imc_EN3,
                                 EN3_rep))
colnames(MOD_NH_EN3) <- c('idadedoimc', 'media_imc', 
                         'desvio_padrao_imc',
                         'estado_nutricional')

idadedoimc_EN4 <- 18:65
EN4_rep <- rep('Obesidade',48)

mod_EN4_use <- summary(mod_EN4)
media_imc_EN4 <- mod_EN4_use$coefficients[,1]
media_imc_EN4 # 33.95421 intercept
media_imc_EN4 <- media_imc_EN4 + 33.95421 
media_imc_EN4[1] <- media_imc_EN4[1] - 33.95421 
media_imc_EN4
desvio_padrao_imc_EN4 <- mod_EN4_use$coefficients[,2]

MOD_NH_EN4 <- as.data.frame(cbind(idadedoimc_EN4, media_imc_EN4, 
                                 desvio_padrao_imc_EN4,
                                 EN4_rep))
colnames(MOD_NH_EN4) <- c('idadedoimc', 'media_imc', 
                         'desvio_padrao_imc',
                         'estado_nutricional')

NH_EN <-  as.data.frame(rbind(MOD_NH_EN2, MOD_NH_EN1,MOD_NH_EN3,MOD_NH_EN4))

NH_EN$idadedoimc <- as.numeric(NH_EN$idadedoimc)
NH_EN$media_imc <- as.numeric(NH_EN$media_imc)
NH_EN$desvio_padrao_imc <- as.numeric(NH_EN$desvio_padrao_imc)
NH_EN$estado_nutricional <- as.factor(as.character(NH_EN$estado_nutricional))


NH_EN$estado_nutricional = with(NH_EN, reorder(estado_nutricional, media_imc, median))

ggplot(NH_EN, aes(x=idadedoimc, y=media_imc, group=estado_nutricional, color=estado_nutricional)) + 
  geom_errorbar(aes(ymin=media_imc-desvio_padrao_imc, 
                    ymax=media_imc+desvio_padrao_imc), width=.005, 
                position=position_dodge(0.005)) +
  scale_fill_grey() +theme_minimal()+ 
  geom_line() + geom_point()+ geom_smooth()+
  scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5),
                     labels=glue("{seq(18,65,5)} years"))  +
  scale_y_continuous(limits = c(15,45), 
                     breaks = seq(15,45, by=5))   +
  labs(colour = "Income",x = '', y='Body Mass Index (Kg/m²)',
       tag = 'Fixed-effects Multilevel Analysis by Nutritional Status at 25 years old Stratification',
caption = "Source: NHANES 1999-2020 Adults (18 to 65 years old)") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        plot.caption = element_text(hjust=0, face = "italic", color = "darkgray"),
        plot.caption.position = "plot",
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="black"),
        panel.grid.major.x = element_line(color="darkgrey", size = 0.1),
        panel.grid.major.y = element_line(color="darkgrey", size = 0.1, linetype = "dotted"),                                          plot.tag.position = 'top',
        plot.tag = element_text(vjust = 1, hjust = 1),
        legend.position="bottom") +
  scale_color_manual(name=NULL,
                     values = c("#A0A0A0", "#909090",
                                "#606060","#202020"),
                     labels = c("Underweight","Normal Weight", "Overweight","Obesity"))+
  geom_hline(yintercept=25, color="black", size=0.5, linetype = 'dashed') + 
  geom_hline(yintercept=30, color="black", size=0.5, linetype = 'dashed') 




# linear Estado Nutricional

lin_NHEN1 <- NH_EN1 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))
lin_NHEN2 <- NH_EN2 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))
lin_NHEN3 <- NH_EN3 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))
lin_NHEN4 <- NH_EN4 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))

lin_NHEN1 <- data.frame(lapply(lin_NHEN1, function(x) as.numeric(as.character(x))-1))
lin_NHEN2 <- data.frame(lapply(lin_NHEN2, function(x) as.numeric(as.character(x))-1))
lin_NHEN3 <- data.frame(lapply(lin_NHEN3, function(x) as.numeric(as.character(x))-1))
lin_NHEN4 <- data.frame(lapply(lin_NHEN4, function(x) as.numeric(as.character(x))-1))

ggplot() +
  geom_smooth(data=lin_NHEN1,aes(x=idadedoimc,y=imc, color = 'red'),method=glm)+
  geom_smooth(data=lin_NHEN2,aes(x=idadedoimc,y=imc, color = 'blue'),method=glm)+
  geom_smooth(data=lin_NHEN3,aes(x=idadedoimc,y=imc, color = 'green'),method=glm)+
  geom_smooth(data=lin_NHEN4,aes(x=idadedoimc,y=imc, color = 'pink'),method=glm)+
  theme_minimal()+
  labs(x="Age",y="IMC mean") + 
  scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5),
                     labels=glue("{seq(18,65,5)} years"))  +
  scale_y_continuous(limits = c(15,45),
                     breaks = seq(15,45, by=5)) +
  scale_color_manual(name = "Nutritional State at 25", 
                     labels = c("Underweight", "Normal",
                                "Overweight", "Obesity"), values = c("red","blue",
                                                                     "green","pink"))


ggplot() +
  geom_smooth(data=lin_NHEN1,aes(x=idadedoimc,y=imc, color = 'red'),method=loess)+
  geom_smooth(data=lin_NHEN2,aes(x=idadedoimc,y=imc, color = 'blue'),method=loess)+
  geom_smooth(data=lin_NHEN3,aes(x=idadedoimc,y=imc, color = 'green'),method=loess)+
  geom_smooth(data=lin_NHEN4,aes(x=idadedoimc,y=imc, color = 'pink'),method=loess)+
  theme_minimal()+
  labs(x="Age",y="IMC mean") + 
  scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5),
                     labels=glue("{seq(18,65,5)} years"))  +
  scale_y_continuous(limits = c(15,45),
                     breaks = seq(15,45, by=5)) +
  scale_color_manual(name = "Nutritional State at 25", 
                     labels = c("Underweight", "Normal",
                                "Overweight", "Obesity"), values = c("red","blue",
                                                                     "green","pink"))
                                                                     

# linear Geração

lin_NHG1 <- NH_G1 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))
lin_NHG2 <- NH_G2 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))
lin_NHG3 <- NH_G3 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))
lin_NHG4 <- NH_G4 %>% group_by(idadedoimc) %>%
  summarise_at(vars(imc), funs(mean(., na.rm=TRUE)))

lin_NHG1 <- data.frame(lapply(lin_NHG1, function(x) as.numeric(as.character(x))-1))
lin_NHG2 <- data.frame(lapply(lin_NHG2, function(x) as.numeric(as.character(x))-1))
lin_NHG3 <- data.frame(lapply(lin_NHG3, function(x) as.numeric(as.character(x))-1))
lin_NHG4 <- data.frame(lapply(lin_NHG4, function(x) as.numeric(as.character(x))-1))


ggplot() +
  geom_smooth(data=lin_NHG1,aes(x=idadedoimc,y=imc, color = 'red'),method=glm)+
  geom_smooth(data=lin_NHG2,aes(x=idadedoimc,y=imc, color = 'blue'),method=glm)+
  geom_smooth(data=lin_NHG3,aes(x=idadedoimc,y=imc, color = 'green'),method=glm)+
  geom_smooth(data=lin_NHG4,aes(x=idadedoimc,y=imc, color = 'pink'),method=glm)+
  theme_minimal()+
  labs(x="Age",y="IMC mean") + 
  scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5),
                     labels=glue("{seq(18,65,5)} years"))  +
  scale_y_continuous(limits = c(22,30),
                     breaks = seq(22,30, by=5)) +
  scale_color_manual(name = "Birth Decade", 
                     labels = c("1950 or earlier", "1950 - 1960",
                                "1960 - 1970", "1970 or latter"), values = c("red","blue",
                                                                     "green","pink"))



ggplot() +
  geom_smooth(data=lin_NHG1,aes(x=idadedoimc,y=imc, color = 'red'),method=loess)+
  geom_smooth(data=lin_NHG2,aes(x=idadedoimc,y=imc, color = 'blue'),method=loess)+
  geom_smooth(data=lin_NHG3,aes(x=idadedoimc,y=imc, color = 'green'),method=loess)+
  geom_smooth(data=lin_NHG4,aes(x=idadedoimc,y=imc, color = 'pink'),method=loess)+
  theme_minimal()+
  labs(x="Age",y="IMC mean") + 
  scale_x_continuous(limits = c(18,65), 
                     breaks = seq(18,65, by=5),
                     labels=glue("{seq(18,65,5)} years"))  +
  scale_y_continuous(limits = c(22,30),
                     breaks = seq(22,30, by=5)) +
  scale_color_manual(name = "Birth Decade", 
                     labels = c("1950 or earlier", "1950 - 1960",
                                "1960 - 1970", "1970 or latter"), values = c("red","blue",
                                                                     "green","pink"))



########################################################################
#Universidade do Estado de São Paulo - USP
#Faculdade de Saúde Pública - FSP
#Programa de Pós Graduação em Nutrição em Saúde Pública - PPGNSP
#Laboratório de Avaliação do Estado Nutricional de Populações - LANPOP
#Variação o peso corporal ao longo da fase adulta: uma análise da NHANES 1999-2018
#Autora: Debora dos Santos Pereira
#Co-Autora: Claudia Cristina Vieira Pastorello
#Co-Autora: Mariane Helen de Oliveria
#Co-Autora: Iolanda Karla Santana dos Santos
#Co-Autora: Camila Medeiros da Silva Mazzeti
#Orientação: Wolney Lisboa Conde
########################################################################

########################################################################
#Separação das populações de interesse
########################################################################

# pacotes utilizados
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


setwd("C:\\Users\\Débora\\Desktop\\contra prova")

# População geral 
NHTOT <- read.csv('NHTOT.csv')

# Visualização e adequação da base geral

summary(NHTOT)

# Checar duplicados por SEQN 
n_occur <- data.frame(table(NHTOT$SEQN))
table(n_occur$Freq)
# Não há duplicados
                      
# Removendo participantes que Estão grávidas ou talvez estejam
# RIDEXPRG  : 1 ou 3 serão removidos, valores 2 ou NA serão mantidos
NHTOT1 <- NHTOT
NHTOT1$RIDEXPRG[is.na(NHTOT1$RIDEXPRG)] <- 2
NH_SG1 <- subset(NHTOT1, NHTOT1$RIDEXPRG >1)
NH_SG2 <- subset(NH_SG1, NH_SG1$RIDEXPRG <3)

# Recorte etário (28 a 65 anos)
# RIDEXPRG  : 1 ou 3 serão removidos, valores 2 ou NA serão mantidos
NH_SG3 <- subset(NH_SG2, NH_SG2$RIDAGEYR <= 65)
NH_SG <- subset(NH_SG3, NH_SG3$RIDAGEYR >= '28')

# Transformando medidas em variaveis de tipo numerica 
NH_SG$WHD010 <- as.numeric(NH_SG$WHD010)
NH_SG$WHD020 <- as.numeric(NH_SG$WHD020)
NH_SG$WHD050 <- as.numeric(NH_SG$WHD050)
NH_SG$WHD110 <- as.numeric(NH_SG$WHD110)
NH_SG$WHD120 <- as.numeric(NH_SG$WHD120)
NH_SG$WHD130 <- as.numeric(NH_SG$WHD130)
NH_SG$WHD140 <- as.numeric(NH_SG$WHD140)
NH_SG$WHQ150 <- as.numeric(NH_SG$WHQ150)

summary(NH_SG)
str(NH_SG)

# Transformação de valores listados como faltantes para NA

NH_SG$WHD010[NH_SG$WHD010 == 9999] <- NA
NH_SG$WHD010[NH_SG$WHD010 == 99999] <- NA
NH_SG$WHD010[NH_SG$WHD010 == 77777] <- NA
NH_SG$WHD010[NH_SG$WHD010 == 7777] <- NA

NH_SG$WHD020[NH_SG$WHD020 == 9999] <- NA
NH_SG$WHD020[NH_SG$WHD020 == 7777] <- NA
NH_SG$WHD020[NH_SG$WHD020 == 99999] <- NA
NH_SG$WHD020[NH_SG$WHD020 == 77777] <- NA

NH_SG$WHD050[NH_SG$WHD050 == 99999] <- NA
NH_SG$WHD050[NH_SG$WHD050 == 9999] <- NA
NH_SG$WHD050[NH_SG$WHD050 == 7777] <- NA
NH_SG$WHD050[NH_SG$WHD050 == 77777] <- NA

NH_SG$WHD110[NH_SG$WHD110 == 9999] <- NA
NH_SG$WHD110[NH_SG$WHD110 == 7777] <- NA
NH_SG$WHD110[NH_SG$WHD110 == 99999] <- NA
NH_SG$WHD110[NH_SG$WHD110 == 77777] <- NA

NH_SG$WHD120[NH_SG$WHD120 == 7777] <- NA
NH_SG$WHD120[NH_SG$WHD120 == 9999] <- NA
NH_SG$WHD120[NH_SG$WHD120 == 77777] <- NA
NH_SG$WHD120[NH_SG$WHD120 == 99999] <- NA

NH_SG$WHD130[NH_SG$WHD130 == 9999] <- NA
NH_SG$WHD130[NH_SG$WHD130 == 7777] <- NA
NH_SG$WHD130[NH_SG$WHD130 == 99999] <- NA
NH_SG$WHD130[NH_SG$WHD130 == 77777] <- NA


NH_SG$WHD140[NH_SG$WHD140 == 77777] <- NA
NH_SG$WHD140[NH_SG$WHD140 == 99999] <- NA
NH_SG$WHD140[NH_SG$WHD140 == 9999] <- NA
NH_SG$WHD140[NH_SG$WHD140 == 7777] <- NA

NH_SG$WHQ150[NH_SG$WHQ150 == 77777] <- NA
NH_SG$WHQ150[NH_SG$WHQ150 == 7777] <- NA
NH_SG$WHQ150[NH_SG$WHQ150 == 99999] <- NA
NH_SG$WHQ150[NH_SG$WHQ150 == 9999] <- NA

NH_SG$DMDMARTL[NH_SG$DMDMARTL == 99] <- NA
NH_SG$DMDMARTL[NH_SG$DMDMARTL == 77] <- NA

summary(NH_SG)
str(NH_SG)
round(mean(is.na(NH_SG$WHD130)),digits=3)# 58,7%




# altura de 10 anos(WHD130) atrás faltante receberá altura atual referida(WHD010) 
NH_base_altura10anos <- NH_SG
NH_base_altura10anos$WHD130 <- ifelse(is.na(NH_SG$WHD130), 
                                         NH_SG$WHD010, NH_SG$WHD130)
summary(NH_SG$WHD130)
summary(NH_base_altura10anos$WHD130)

# porcentagem de valores faltantes 

round(mean(is.na(NH_base_altura10anos$DMDMARTL)),digits=3)# 1% - imputação
round(mean(is.na(NH_base_altura10anos$INDFMPIR)),digits=3)# 8.7% - imputação
round(mean(is.na(NH_base_altura10anos$WHD010)),digits=3)# 2.2% - imputação
round(mean(is.na(NH_base_altura10anos$WHD020)),digits=3)# 1.7% - imputação
round(mean(is.na(NH_base_altura10anos$WHD050)),digits=3)# 1.9% - imputação
round(mean(is.na(NH_base_altura10anos$WHD120)),digits=3)# 4.4% - imputação
round(mean(is.na(NH_base_altura10anos$WHD130)),digits=3)# 2.1% - imputação
round(mean(is.na(NH_base_altura10anos$WHQ150)),digits=3)# 9.7% - imputação
round(mean(is.na(NH_base_altura10anos$WHD140)),digits=3)# 1.8% - imputação
round(mean(is.na(NH_base_altura10anos$WHD110)),digits=3)# 22.7% - Acima

# A partir daqui serão feitas duas leituras das regressões multinivel.
# Uma dela excluindo NAs de variaveis que apresentaram mais do que 10% de NA
# Outra imputando todos os faltantes na NH_base_altura10anos

# Limpando os NAs das variaveis WHD120, WHQ150, WHD110 
NH_filtro_narel <- NH_base_altura10anos[!is.na(NH_base_altura10anos$WHD110),]
summary(NH_filtro_narel)
str(NH_filtro_narel)

# porcentagem de valores faltantes após remoção


round(mean(is.na(NH_filtro_narel$DMDMARTL)),digits=3)# 1% - imputação
round(mean(is.na(NH_filtro_narel$INDFMPIR)),digits=3)# 9.3% - imputação
round(mean(is.na(NH_filtro_narel$WHD010)),digits=3)# 1.5% - imputação
round(mean(is.na(NH_filtro_narel$WHD020)),digits=3)# 1% - imputação
round(mean(is.na(NH_filtro_narel$WHD050)),digits=3)# 1% - imputação
round(mean(is.na(NH_filtro_narel$WHD120)),digits=3)# 2.8% - imputação
round(mean(is.na(NH_filtro_narel$WHD130)),digits=3)# 1.4% - imputação
round(mean(is.na(NH_filtro_narel$WHQ150)),digits=3)# 7.7% - imputação
round(mean(is.na(NH_filtro_narel$WHD140)),digits=3)# 1% - imputação

# Criando base de dados para imputação PMM|RF

##### Utilizei para imputar valores categoricos
##### o classificador randon forest do mice 
##### (PMM não aceita range baixo)

# IMPUTAÇÃO APÓS TIRAR FALTANTES COM >10%
NH_filtro_narel$SDDSRVYR <- as.factor(NH_filtro_narel$SDDSRVYR)
NH_filtro_narel$RIAGENDR <- as.factor(NH_filtro_narel$RIAGENDR)
NH_filtro_narel$RIDRETH1 <- as.factor(NH_filtro_narel$RIDRETH1)
NH_filtro_narel$DMDEDUC2 <- as.factor(NH_filtro_narel$DMDEDUC2)
NH_filtro_narel$DMDMARTL <- as.factor(NH_filtro_narel$DMDMARTL)
NH_filtro_narel$INDFMPIR <- as.factor(NH_filtro_narel$INDFMPIR)
NH_filtro_narel$RIDEXPRG <- as.factor(NH_filtro_narel$RIDEXPRG)
str(NH_filtro_narel)

# imputação PMM|RF

# considera-se que o valores faltantes nestas proporções seguem
# o comportamento Missing At Random(MAR) por serem
# valores baixos e por não apresentarem padrões de falta

#retirando faltantes da 110

# Para reimputar e avaliar as imputações,
# descomente as linhas 206-260


#imp_pmm <- NH_filtro_narel
#summary(imp_pmm)

#my_imp <- mice(imp_pmm, m = 5,
                method = c("","","","pmm","",
                           "","","","pmm","pmm",
                           "","pmm","pmm","pmm","",
                           "pmm","pmm","pmm","pmm"),
                maxit = 5, seed = 42)

#nhanes.comp <- complete(my_imp, "long", include = TRUE)

# # Diagnóstico do melhor M por distribuição de dados imputados
# #
# # diagnostico de imputações de WHD010 - MELHORES: TODOS (5 melhor)
# summary(NH_filtro_narel$WHD010)
# with(my_imp, mean(WHD010))
# nhanes.comp$WHD010.NA <- cci(NH_filtro_narel$WHD010)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHD010 , color = WHD010.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
# #
# #diagnostico das imputações multiplas de WHD020 - MELHORES: TODOS
# summary(NH_filtro_narel$WHD020)
# with(my_imp, mean(WHD020))
# nhanes.comp$WHD020.NA <- cci(NH_filtro_narel$WHD020)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHD020 , color = WHD020.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
# #
# #diagnostico das imputações multiplas de WHD050 - MELHORES: 3,4,5
# summary(NH_filtro_narel$WHD050)
# with(my_imp, mean(WHD050))
# nhanes.comp$WHD050.NA <- cci(NH_filtro_narel$WHD050)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHD050 , color = WHD050.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHD120 - MELHORES: 1,3,4,5
# summary(NH_filtro_narel$WHD120)
# with(my_imp, mean(WHD120)) #consistentes: 1,2 e 5
# nhanes.comp$WHD120.NA <- cci(NH_filtro_narel$WHD120)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHD120 , color = WHD120.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #
# #diagnostico das imputações multiplas de WHD130 - MELHORES: 1,3,4,5
# summary(NH_filtro_narel$WHD130)
# with(my_imp, mean(WHD130)) #consistentes: 1,2 e 5
# nhanes.comp$WHD130.NA <- cci(NH_filtro_narel$WHD130)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHD130 , color = WHD130.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
# #
# #diagnostico das imputações multiplas de WHD140 - MELHORES: 1,2,5
# summary(NH_filtro_narel$WHD140)
# with(my_imp, mean(WHD140))
# nhanes.comp$WHD140.NA <- cci(NH_filtro_narel$WHD140)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHD140 , color = WHD140.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #
# #diagnostico das imputações multiplas de WHQ150 - MELHORES: TODOS
# summary(NH_filtro_narel$WHQ150)
# with(my_imp, mean(WHQ150))
# nhanes.comp$WHQ150.NA <- cci(NH_filtro_narel$WHQ150)
# ggplot(nhanes.comp,
#        aes(x = .imp, y = WHQ150 , color = WHQ150.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# # Melhor m aparente = 5
# imp_pmm_final <- complete(my_imp, 1) #usei a 1
# write.csv(imp_pmm_final, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/imp_pmm_final.csv")

#imp_pmm_final <- read.csv("imp_pmm_final.csv")

# IMPUTAÇÃO COM FALTANTES PLENOS
NH_base_altura10anos$SDDSRVYR <- as.factor(NH_base_altura10anos$SDDSRVYR)
NH_base_altura10anos$RIAGENDR <- as.factor(NH_base_altura10anos$RIAGENDR)
NH_base_altura10anos$RIDRETH1 <- as.factor(NH_base_altura10anos$RIDRETH1)
NH_base_altura10anos$DMDEDUC2 <- as.factor(NH_base_altura10anos$DMDEDUC2)
NH_base_altura10anos$DMDMARTL <- as.factor(NH_base_altura10anos$DMDMARTL)
NH_base_altura10anos$INDFMPIR <- as.factor(NH_base_altura10anos$INDFMPIR)
NH_base_altura10anos$RIDEXPRG <- as.factor(NH_base_altura10anos$RIDEXPRG)
str(NH_base_altura10anos)

# considera-se que o valores faltantes nestas proporções seguem
# o comportamento Missing At Random(MAR) por serem
# valores baixos e por não apresentarem padrões de falta

#não retirando faltantes da 110
imp_pmm_MA <- NH_base_altura10anos
summary(imp_pmm_MA)

my_imp_MA <- mice(imp_pmm_MA, m = 5,
                  method 
                  = c("","","","pmm","",
                             "","","","pmm","pmm",
                             "","pmm","pmm","pmm","",
                             "pmm","pmm","pmm","pmm"),
                  maxit = 5, seed = 42)

nhanes.comp_MA <- complete(my_imp_MA, "long", include = TRUE)
#
# # Diagnóstico do melhor M por distribuição de dados imputados
#
# # diagnostico de imputações de WHD010 - MELHORES: 1,2,4
# summary(NH_base_altura10anos$WHD010)
# with(my_imp_MA, mean(WHD010))
# nhanes.comp_MA$WHD010.NA <- cci(NH_base_altura10anos$WHD010)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD010 , color = WHD010.NA)) +
#   geom_jitter(show.legend = FALSE, width = .1)
#
# #diagnostico das imputações multiplas de WHD020 - MELHORES: TODOS
# summary(NH_base_altura10anos$WHD020)
# with(my_imp_MA, mean(WHD020))
# nhanes.comp_MA$WHD020.NA <- cci(NH_base_altura10anos$WHD020)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD020 , color = WHD020.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHD050 - MELHORES: TODAS
# summary(NH_base_altura10anos$WHD050)
# with(my_imp_MA, mean(WHD050))
# nhanes.comp_MA$WHD050.NA <- cci(NH_base_altura10anos$WHD050)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD050 , color = WHD050.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHD110 - MELHORES: 1,3,4
# summary(NH_base_altura10anos$WHD110)
# with(my_imp_MA, mean(WHD110)) #consistentes: 1,2 e 5
# nhanes.comp_MA$WHD110.NA <- cci(NH_base_altura10anos$WHD110)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD110 , color = WHD110.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHD120 - MELHORES: 1,3,4,5
# summary(NH_base_altura10anos$WHD120)
# with(my_imp_MA, mean(WHD120)) #consistentes: 1,2 e 5
# nhanes.comp_MA$WHD120.NA <- cci(NH_base_altura10anos$WHD120)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD120 , color = WHD120.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHD130 - MELHORES: 1,2,4
# summary(NH_base_altura10anos$WHD130)
# with(my_imp_MA, mean(WHD130)) #consistentes: 1,2 e 5
# nhanes.comp_MA$WHD130.NA <- cci(NH_base_altura10anos$WHD130)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD130 , color = WHD130.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHD140 - MELHORES: TODAS
# summary(NH_base_altura10anos$WHD140)
# with(my_imp_MA, mean(WHD140))
# nhanes.comp_MA$WHD140.NA <- cci(NH_base_altura10anos$WHD140)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHD140 , color = WHD140.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# #diagnostico das imputações multiplas de WHQ150 - MELHORES: TODAS
# summary(NH_base_altura10anos$WHQ150)
# with(my_imp_MA, mean(WHQ150))
# nhanes.comp_MA$WHQ150.NA <- cci(NH_base_altura10anos$WHQ150)
# ggplot(nhanes.comp_MA,
#        aes(x = .imp, y = WHQ150 , color = WHQ150.NA)) +
#   geom_jitter(show.legend = FALSE,
#               width = .1)
#
# # Melhor m aparente = 4
imp_pmm_final_MA <- complete(my_imp_MA, 4)
write.csv(imp_pmm_final_MA, "C:/Users/Débora/Desktop/contra prova/imp_pmm_final_MA.csv")

imp_pmm_final_MA <- read.csv("imp_pmm_final_MA.csv")
View(imp_pmm_final_MA)
#Criando variaveis de interesse em kg, cm e IMCs

#peso atual
imp_pmm_final$WHD020 = as.numeric(imp_pmm_final$WHD020)
imp_pmm_final$WHD020 <- imp_pmm_final$WHD020*0.453592
imp_pmm_final$WHD020 <- round(imp_pmm_final$WHD020, digits = 2)


#peso 1 ano atrás
imp_pmm_final$WHD050 = as.numeric(imp_pmm_final$WHD050)
imp_pmm_final$WHD050 <- imp_pmm_final$WHD050*0.453592
imp_pmm_final$WHD050 <- round(imp_pmm_final$WHD050, digits = 2)


#peso 10 anos atrás
imp_pmm_final$WHD110 = as.numeric(imp_pmm_final$WHD110)
imp_pmm_final$WHD110 <- imp_pmm_final$WHD110*0.453592
imp_pmm_final$WHD110 <- round(imp_pmm_final$WHD110, digits = 2)


#peso aos 25 anos
imp_pmm_final$WHD120 = as.numeric(imp_pmm_final$WHD120)
imp_pmm_final$WHD120 <- imp_pmm_final$WHD120*0.453592
imp_pmm_final$WHD120 <- round(imp_pmm_final$WHD120, digits = 2)


#maior peso obtido na vida 
imp_pmm_final$WHD140 = as.numeric(imp_pmm_final$WHD140)
imp_pmm_final$WHD140 <- imp_pmm_final$WHD140*0.453592
imp_pmm_final$WHD140 <- round(imp_pmm_final$WHD140, digits = 2)



#altura atual

imp_pmm_final$WHD010 = as.numeric(imp_pmm_final$WHD010)
imp_pmm_final$WHD010 <- imp_pmm_final$WHD010*0.0254
imp_pmm_final$WHD010 <- round(imp_pmm_final$WHD010, digits = 2)


#altura aos 25 anos

imp_pmm_final$WHD130 = as.numeric(imp_pmm_final$WHD130)
imp_pmm_final$WHD130 <- imp_pmm_final$WHD130*0.0254
imp_pmm_final$WHD130 <- round(imp_pmm_final$WHD130, digits = 2)


#-----

#imc atual
imp_pmm_final$imcatual <- (imp_pmm_final$WHD020/(imp_pmm_final$WHD010*imp_pmm_final$WHD010))
imp_pmm_final$imcatual <- round(imp_pmm_final$imcatual, digits = 2)

#-
#imc um ano
imp_pmm_final$imc1ano <- (imp_pmm_final$WHD050/(imp_pmm_final$WHD010*imp_pmm_final$WHD010))
imp_pmm_final$imc1ano <- round(imp_pmm_final$imc1ano, digits = 2)


#imc 10 anos

imp_pmm_final$imc10ano <- (imp_pmm_final$WHD110/(imp_pmm_final$WHD010*imp_pmm_final$WHD010))
imp_pmm_final$imc10ano <- round(imp_pmm_final$imc10ano, digits = 2)


#imc 25 anos
imp_pmm_final$imc25ano <- (imp_pmm_final$WHD120/(imp_pmm_final$WHD010*imp_pmm_final$WHD010))
imp_pmm_final$imc25ano <- round(imp_pmm_final$imc25ano, digits = 2)

#maior IMC obtido

imp_pmm_final$imcmaior <- (imp_pmm_final$WHD140/(imp_pmm_final$WHD010*imp_pmm_final$WHD010))
imp_pmm_final$imcmaior <- round(imp_pmm_final$imcmaior, digits = 2)

#--criando colunas DO IMC

imp_pmm_final$cimcatual = cut(imp_pmm_final$imcatual, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final$cimc1ano = cut(imp_pmm_final$imc1ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final$cimc10ano = cut(imp_pmm_final$imc10ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final$cimc25ano = cut(imp_pmm_final$imc25ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final$cimcmaior = cut(imp_pmm_final$imcmaior, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)

# sem remoção de faltantes com imputação completa

#peso atual
imp_pmm_final_MA$WHD020 = as.numeric(imp_pmm_final_MA$WHD020)
imp_pmm_final_MA$WHD020 <- imp_pmm_final_MA$WHD020*0.453592
imp_pmm_final_MA$WHD020 <- round(imp_pmm_final_MA$WHD020, digits = 2)


#peso 1 ano atrás
imp_pmm_final_MA$WHD050 = as.numeric(imp_pmm_final_MA$WHD050)
imp_pmm_final_MA$WHD050 <- imp_pmm_final_MA$WHD050*0.453592
imp_pmm_final_MA$WHD050 <- round(imp_pmm_final_MA$WHD050, digits = 2)


#peso 10 anos atrás
imp_pmm_final_MA$WHD110 = as.numeric(imp_pmm_final_MA$WHD110)
imp_pmm_final_MA$WHD110 <- imp_pmm_final_MA$WHD110*0.453592
imp_pmm_final_MA$WHD110 <- round(imp_pmm_final_MA$WHD110, digits = 2)


#peso aos 25 anos
imp_pmm_final_MA$WHD120 = as.numeric(imp_pmm_final_MA$WHD120)
imp_pmm_final_MA$WHD120 <- imp_pmm_final_MA$WHD120*0.453592
imp_pmm_final_MA$WHD120 <- round(imp_pmm_final_MA$WHD120, digits = 2)


#maior peso obtido na vida 
imp_pmm_final_MA$WHD140 = as.numeric(imp_pmm_final_MA$WHD140)
imp_pmm_final_MA$WHD140 <- imp_pmm_final_MA$WHD140*0.453592
imp_pmm_final_MA$WHD140 <- round(imp_pmm_final_MA$WHD140, digits = 2)



#altura atual

imp_pmm_final_MA$WHD010 = as.numeric(imp_pmm_final_MA$WHD010)
imp_pmm_final_MA$WHD010 <- imp_pmm_final_MA$WHD010*0.0254
imp_pmm_final_MA$WHD010 <- round(imp_pmm_final_MA$WHD010, digits = 2)


#altura aos 25 anos

imp_pmm_final_MA$WHD130 = as.numeric(imp_pmm_final_MA$WHD130)
imp_pmm_final_MA$WHD130 <- imp_pmm_final_MA$WHD130*0.0254
imp_pmm_final_MA$WHD130 <- round(imp_pmm_final_MA$WHD130, digits = 2)


#-----

#imc atual
imp_pmm_final_MA$imcatual <- (imp_pmm_final_MA$WHD020/(imp_pmm_final_MA$WHD010*imp_pmm_final_MA$WHD010))
imp_pmm_final_MA$imcatual <- round(imp_pmm_final_MA$imcatual, digits = 2)

#-
#imc um ano
imp_pmm_final_MA$imc1ano <- (imp_pmm_final_MA$WHD050/(imp_pmm_final_MA$WHD010*imp_pmm_final_MA$WHD010))
imp_pmm_final_MA$imc1ano <- round(imp_pmm_final_MA$imc1ano, digits = 2)


#imc 10 anos

imp_pmm_final_MA$imc10ano <- (imp_pmm_final_MA$WHD110/(imp_pmm_final_MA$WHD010*imp_pmm_final_MA$WHD010))
imp_pmm_final_MA$imc10ano <- round(imp_pmm_final_MA$imc10ano, digits = 2)


#imc 25 anos
imp_pmm_final_MA$imc25ano <- (imp_pmm_final_MA$WHD120/(imp_pmm_final_MA$WHD010*imp_pmm_final_MA$WHD010))
imp_pmm_final_MA$imc25ano <- round(imp_pmm_final_MA$imc25ano, digits = 2)

#maior IMC obtido

imp_pmm_final_MA$imcmaior <- (imp_pmm_final_MA$WHD140/(imp_pmm_final_MA$WHD010*imp_pmm_final_MA$WHD010))
imp_pmm_final_MA$imcmaior <- round(imp_pmm_final_MA$imcmaior, digits = 2)

#--criando colunas DO IMC

imp_pmm_final_MA$cimcatual = cut(imp_pmm_final_MA$imcatual, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final_MA$cimc1ano = cut(imp_pmm_final_MA$imc1ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final_MA$cimc10ano = cut(imp_pmm_final_MA$imc10ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final_MA$cimc25ano = cut(imp_pmm_final_MA$imc25ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
imp_pmm_final_MA$cimcmaior = cut(imp_pmm_final_MA$imcmaior, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)


# Separação da Base Geral por Gênero
summary(imp_pmm_final)
write.csv(imp_pmm_final, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_GERAL.csv")

summary(imp_pmm_final_MA)
write.csv(imp_pmm_final_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_GERAL_MA.csv")


NH_FEM_MA <- imp_pmm_final_MA[imp_pmm_final_MA$RIAGENDR == 2,]
write.csv(NH_FEM_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_FEM_MA.csv")

NH_FEM <- imp_pmm_final[imp_pmm_final$RIAGENDR == 2,]
write.csv(NH_FEM, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_FEM.csv")


NH_MASC_MA <- imp_pmm_final_MA[imp_pmm_final_MA$RIAGENDR == 1,]
write.csv(NH_MASC_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_MASC_MA.csv")

NH_MASC <- imp_pmm_final[imp_pmm_final$RIAGENDR == 1,]
write.csv(NH_MASC, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_MASC.csv")


# Separação da Base Geral por Geração

# SDDSRVYR
# 1	NHANES 1999-2000 Public Release
# 2	NHANES 2001-2002 Public Release
# 3	NHANES 2003-2004 Public Release
# 4	NHANES 2005-2006 Public Release
# 5	NHANES 2007-2008 Public Release
# 6	NHANES 2009-2010 Public Release
# 7	NHANES 2011-2012 Public Release
# 8	NHANES 2013-2014 Public Release
# 9	NHANES 2015-2016 Public Release
# 10NHANES 2017-2018 Public Release

# Trocando valores por anos
imp_pmm_final$SDDSRVYR2 <- '0'
imp_pmm_final$SDDSRVYR3 <- '0'
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==1]  <- "1999"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==1]  <- "2000"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==2]  <- "2001"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==2]  <- "2002"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==3]  <- "2003"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==3]  <- "2004"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==4]  <- "2005"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==4]  <- "2006"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==5]  <- "2007"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==5]  <- "2008"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==6]  <- "2009"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==6]  <- "2010"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==7]  <- "2011"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==7]  <- "2012"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==8]  <- "2013"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==8]  <- "2014"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==9]  <- "2015"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==9]  <- "2016"
imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==10]  <- "2017"
imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==10]  <- "2018"
#imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==11]  <- "2017"
#imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==11]  <- "2018"
#imp_pmm_final$SDDSRVYR2[imp_pmm_final$SDDSRVYR==11]  <- "2019"
#imp_pmm_final$SDDSRVYR3[imp_pmm_final$SDDSRVYR==11]  <- "2020"

imp_pmm_final$SDDSRVYR2 <- as.numeric(imp_pmm_final$SDDSRVYR2)
imp_pmm_final$SDDSRVYR3 <- as.numeric(imp_pmm_final$SDDSRVYR3)
imp_pmm_final$RIDAGEYR <- as.numeric(imp_pmm_final$RIDAGEYR)

imp_pmm_final$ANONASCMIN <- imp_pmm_final$SDDSRVYR2 - imp_pmm_final$RIDAGEYR
imp_pmm_final$ANONASCMAX <- imp_pmm_final$SDDSRVYR3 - imp_pmm_final$RIDAGEYR
hist(imp_pmm_final$ANONASCMIN)
hist(imp_pmm_final$ANONASCMAX)

# Divisões geracionais

# G1 : ANONASCMAX <= 1950 
imp_pmm_final$G1 <- 0
imp_pmm_final$G1[(imp_pmm_final$ANONASCMAX <= 1950)] <- "1"
NH_G1 <- subset(imp_pmm_final, imp_pmm_final$G1 == 1)
hist(NH_G1$RIDAGEYR)
write.csv(NH_G1, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G1.csv")

# G2 : ANONASCMIN >= 1950, ANONASCMAX <= 1960
imp_pmm_final$G2 <- 0
imp_pmm_final$G2[(imp_pmm_final$ANONASCMAX >= 1950) & (imp_pmm_final$ANONASCMAX <= 1960) ] <- "1"
NH_G2 <- subset(imp_pmm_final, imp_pmm_final$G2 == 1)
hist(NH_G2$RIDAGEYR)
write.csv(NH_G2, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G2.csv")

# G3 : ANONASCMIN >= 1960, ANONASCMAX <= 1970
imp_pmm_final$G3 <- 0
imp_pmm_final$G3[(imp_pmm_final$ANONASCMAX >= 1960) & (imp_pmm_final$ANONASCMAX <= 1980) ] <- "1"
NH_G3 <- subset(imp_pmm_final, imp_pmm_final$G3 == 1)
hist(NH_G3$RIDAGEYR)
write.csv(NH_G3, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G3.csv")

# G4 : ANONASCMIN >= 1970
imp_pmm_final$G4 <- 0
imp_pmm_final$G4[(imp_pmm_final$ANONASCMAX >= 1970)] <- "1"
NH_G4 <- subset(imp_pmm_final, imp_pmm_final$G4 == 1)
hist(NH_G4$RIDAGEYR)
write.csv(NH_G4, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G4.csv")


# Separação da Base Geral por Geração MA

# SDDSRVYR
# 1	NHANES 1999-2000 Public Release
# 2	NHANES 2001-2002 Public Release
# 3	NHANES 2003-2004 Public Release
# 4	NHANES 2005-2006 Public Release
# 5	NHANES 2007-2008 Public Release
# 6	NHANES 2009-2010 Public Release
# 7	NHANES 2011-2012 Public Release
# 8	NHANES 2013-2014 Public Release
# 9	NHANES 2015-2016 Public Release
# 10NHANES 2017-2018 Public Release

# Trocando valores por anos
imp_pmm_final_MA$SDDSRVYR2 <- '0'
imp_pmm_final_MA$SDDSRVYR3 <- '0'
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==1]  <- "1999"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==1]  <- "2000"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==2]  <- "2001"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==2]  <- "2002"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==3]  <- "2003"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==3]  <- "2004"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==4]  <- "2005"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==4]  <- "2006"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==5]  <- "2007"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==5]  <- "2008"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==6]  <- "2009"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==6]  <- "2010"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==7]  <- "2011"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==7]  <- "2012"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==8]  <- "2013"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==8]  <- "2014"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==9]  <- "2015"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==9]  <- "2016"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==10]  <- "2017"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==10]  <- "2018"
imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==11]  <- "2017"
imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==11]  <- "2018"
#imp_pmm_final_MA$SDDSRVYR2[imp_pmm_final_MA$SDDSRVYR==11]  <- "2019"
#imp_pmm_final_MA$SDDSRVYR3[imp_pmm_final_MA$SDDSRVYR==11]  <- "2020"

imp_pmm_final_MA$SDDSRVYR2 <- as.numeric(imp_pmm_final_MA$SDDSRVYR2)
imp_pmm_final_MA$SDDSRVYR3 <- as.numeric(imp_pmm_final_MA$SDDSRVYR3)
imp_pmm_final_MA$RIDAGEYR <- as.numeric(imp_pmm_final_MA$RIDAGEYR)

imp_pmm_final_MA$ANONASCMIN <- imp_pmm_final_MA$SDDSRVYR2 - imp_pmm_final_MA$RIDAGEYR
imp_pmm_final_MA$ANONASCMAX <- imp_pmm_final_MA$SDDSRVYR3 - imp_pmm_final_MA$RIDAGEYR
hist(imp_pmm_final_MA$ANONASCMIN)
hist(imp_pmm_final_MA$ANONASCMAX)

# Divisões geracionais

# G1 : ANONASCMAX <= 1950 
imp_pmm_final_MA$G1 <- 0
imp_pmm_final_MA$G1[(imp_pmm_final_MA$ANONASCMAX <= 1950)] <- "1"
NH_G1_MA <- subset(imp_pmm_final_MA, imp_pmm_final_MA$G1 == 1)
hist(NH_G1_MA$RIDAGEYR)
write.csv(NH_G1_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G1_MA.csv")

# G2 : ANONASCMIN >= 1950, ANONASCMAX <= 1960
imp_pmm_final_MA$G2 <- 0
imp_pmm_final_MA$G2[(imp_pmm_final_MA$ANONASCMAX >= 1950) & (imp_pmm_final_MA$ANONASCMAX <= 1960) ] <- "1"
NH_G2_MA <- subset(imp_pmm_final_MA, imp_pmm_final_MA$G2 == 1)
hist(NH_G2_MA$RIDAGEYR)
write.csv(NH_G2_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G2_MA.csv")

# G3 : ANONASCMIN >= 1960, ANONASCMAX <= 1970
imp_pmm_final_MA$G3 <- 0
imp_pmm_final_MA$G3[(imp_pmm_final_MA$ANONASCMAX >= 1960) & (imp_pmm_final_MA$ANONASCMAX <= 1980) ] <- "1"
NH_G3_MA <- subset(imp_pmm_final_MA, imp_pmm_final_MA$G3 == 1)
hist(NH_G3_MA$RIDAGEYR)
write.csv(NH_G3_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G3_MA.csv")

# G4 : ANONASCMIN >= 1970
imp_pmm_final_MA$G4 <- 0
imp_pmm_final_MA$G4[(imp_pmm_final_MA$ANONASCMAX >= 1970)] <- "1"
NH_G4_MA <- subset(imp_pmm_final_MA, imp_pmm_final_MA$G4 == 1)
hist(NH_G4_MA$RIDAGEYR)
write.csv(NH_G4_MA, "C:\\Users\\Débora\\Desktop\\contra prova\\NH_G4_MA.csv")


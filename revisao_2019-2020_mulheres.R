########################################################################
#Universidade do Estado de São Paulo - USP
#Faculdade de Saúde Pública - FSP
#Programa de Pós Graduação em Nutrição em Saúde Pública - PPGNSP
#Laboratório de Avaliação do Estado Nutricional de Populações - LANPOP
#Variação o peso corporal ao longo da fase adulta: uma análise da NHANES 1999-2019
#Versão Final - 10/20
#Autora: Debora dos Santos Pereira
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

# ATENÇÃO: 
# mude o caminho para a pasta onde os dados estão na sua máquina

setwd("C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA")

# População geral 
NH_GERAL <- read.csv('NH_GERAL_MA.csv')

# Mulheres
NH_FEM <- read.csv('NH_FEM_MA.csv')

# Homens
NH_MA_MASC <- read.csv('NH_MASC_MA.csv')

# Gerações
NH_G1 <- read.csv('NH_G1_MA.csv')
NH_G2 <- read.csv('NH_G2_MA.csv')
NH_G3 <- read.csv('NH_G3_MA.csv')
NH_G4 <- read.csv('NH_G4_MA.csv')

# SÓ HOMENS
NH_G1 <- NH_G1 %>% filter(RIAGENDR==2)
NH_G2 <- NH_G2 %>% filter(RIAGENDR==2)
NH_G3 <- NH_G3 %>% filter(RIAGENDR==2)
NH_G4 <- NH_G4 %>% filter(RIAGENDR==2)

# Estado Nutricional
NH_EN1 <- read.csv('NH_ESTNUT1_MA.csv')
NH_EN2 <-  read.csv('NH_ESTNUT2_MA.csv')
NH_EN3 <- read.csv('NH_ESTNUT3_MA.csv')
NH_EN4 <-  read.csv('NH_ESTNUT4_MA.csv')

# SÓ HOMENS
NH_EN1 <- NH_EN1 %>% filter(RIAGENDR==2)
NH_EN2 <- NH_EN2 %>% filter(RIAGENDR==2)
NH_EN3 <- NH_EN3 %>% filter(RIAGENDR==2)
NH_EN4 <- NH_EN4 %>% filter(RIAGENDR==2)


#Criando base de dados de interesse (ABT) GERAL

NH_ABT <- cbind(NH_GERAL$imcatual, 
                NH_GERAL$imc1ano, 
                NH_GERAL$imc10ano,
                NH_GERAL$imc25ano,
                NH_GERAL$RIDAGEYR,
                NH_GERAL$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_GERAL <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_GERAL$tempo <- as.numeric(as.character(NHS_molted_GERAL$tempo))
NHS_molted_GERAL$idadedoimc <- ifelse(NHS_molted_GERAL$tempo == 25, 25, 
                                      NHS_molted_GERAL$idade - NHS_molted_GERAL$tempo)
NHS_molted_GERAL$idadedoimc <- as.factor(NHS_molted_GERAL$idadedoimc)
write.csv(NHS_molted_GERAL, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_GERAL_FINAL.csv")

#Criando base de dados de interesse (ABT) SEXO FEMININO

NH_ABT <- cbind(NH_FEM$imcatual, 
                NH_FEM$imc1ano, 
                NH_FEM$imc10ano,
                NH_FEM$imc25ano,
                NH_FEM$RIDAGEYR,
                NH_FEM$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_FEM <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_FEM$tempo <- as.numeric(as.character(NHS_molted_FEM$tempo))
NHS_molted_FEM$idadedoimc <- ifelse(NHS_molted_FEM$tempo == 25, 25, 
                                    NHS_molted_FEM$idade - NHS_molted_FEM$tempo)
NHS_molted_FEM$idadedoimc <- as.factor(NHS_molted_FEM$idadedoimc)
write.csv(NHS_molted_FEM, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_FEM_FINAL.csv")

#Criando base de dados de interesse (ABT) SEXO MASCULINO

NH_ABT <- cbind(NH_MA_MASC$imcatual, 
                NH_MA_MASC$imc1ano, 
                NH_MA_MASC$imc10ano,
                NH_MA_MASC$imc25ano,
                NH_MA_MASC$RIDAGEYR,
                NH_MA_MASC$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_MA_MASC <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_MA_MASC$tempo <- as.numeric(as.character(NHS_molted_MA_MASC$tempo))
NHS_molted_MA_MASC$idadedoimc <- ifelse(NHS_molted_MA_MASC$tempo == 25, 25, 
                                        NHS_molted_MA_MASC$idade - NHS_molted_MA_MASC$tempo)
NHS_molted_MA_MASC$idadedoimc <- as.factor(NHS_molted_MA_MASC$idadedoimc)
NHS_molted_MA_MASC <- subset(NHS_molted_MA_MASC, NHS_molted_MA_MASC$idade > 20)
write.csv(NHS_molted_MA_MASC, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_MASC_FINAL.csv")

#Criando base de dados de interesse (ABT) GERAÇÃO 1 (Nasc <= 1950)

NH_ABT <- cbind(NH_G1$imcatual, 
                NH_G1$imc1ano, 
                NH_G1$imc10ano,
                NH_G1$imc25ano,
                NH_G1$RIDAGEYR,
                NH_G1$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_G1 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_G1$tempo <- as.numeric(as.character(NHS_molted_G1$tempo))
NHS_molted_G1$idadedoimc <- ifelse(NHS_molted_G1$tempo == 25, 25, 
                                   NHS_molted_G1$idade - NHS_molted_G1$tempo)
NHS_molted_G1$idadedoimc <- as.factor(NHS_molted_G1$idadedoimc)
write.csv(NHS_molted_G1, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_G1_FINAL.csv")

#Criando base de dados de interesse (ABT) GERAÇÃO 2 (Nasc entre 1950 e 1960)

NH_ABT <- cbind(NH_G2$imcatual, 
                NH_G2$imc1ano, 
                NH_G2$imc10ano,
                NH_G2$imc25ano,
                NH_G2$RIDAGEYR,
                NH_G2$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_G2 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_G2$tempo <- as.numeric(as.character(NHS_molted_G2$tempo))
NHS_molted_G2$idadedoimc <- ifelse(NHS_molted_G2$tempo == 25, 25, 
                                   NHS_molted_G2$idade - NHS_molted_G2$tempo)
NHS_molted_G2$idadedoimc <- as.factor(NHS_molted_G2$idadedoimc)
write.csv(NHS_molted_G2, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_G2_FINAL.csv")

#Criando base de dados de interesse (ABT) GERAÇÃO 3 (Nasc entre 1960 e 1970)

NH_ABT <- cbind(NH_G3$imcatual, 
                NH_G3$imc1ano, 
                NH_G3$imc10ano,
                NH_G3$imc25ano,
                NH_G3$RIDAGEYR,
                NH_G3$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_G3 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_G3$tempo <- as.numeric(as.character(NHS_molted_G3$tempo))
NHS_molted_G3$idadedoimc <- ifelse(NHS_molted_G3$tempo == 25, 25, 
                                   NHS_molted_G3$idade - NHS_molted_G3$tempo)
NHS_molted_G3$idadedoimc <- as.factor(NHS_molted_G3$idadedoimc)
write.csv(NHS_molted_G3, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_G3_FINAL.csv")

#Criando base de dados de interesse (ABT) GERAÇÃO 4 (Nasc acima de 1970)

NH_ABT <- cbind(NH_G4$imcatual, 
                NH_G4$imc1ano, 
                NH_G4$imc10ano,
                NH_G4$imc25ano,
                NH_G4$RIDAGEYR,
                NH_G4$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_G4 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_G4$tempo <- as.numeric(as.character(NHS_molted_G4$tempo))
NHS_molted_G4$idadedoimc <- ifelse(NHS_molted_G4$tempo == 25, 25, 
                                   NHS_molted_G4$idade - NHS_molted_G4$tempo)
NHS_molted_G4$idadedoimc <- as.factor(NHS_molted_G4$idadedoimc)
write.csv(NHS_molted_G4, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_G4_FINAL.csv")

#Criando base de dados de interesse (ABT) Estado Nutricional 0 Baixa

NH_ABT <- cbind(NH_EN1$imcatual, 
                NH_EN1$imc1ano, 
                NH_EN1$imc10ano,
                NH_EN1$imc25ano,
                NH_EN1$RIDAGEYR,
                NH_EN1$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_EN1 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_EN1$tempo <- as.numeric(as.character(NHS_molted_EN1$tempo))
NHS_molted_EN1$idadedoimc <- ifelse(NHS_molted_EN1$tempo == 25, 25, 
                                    NHS_molted_EN1$idade - NHS_molted_EN1$tempo)
NHS_molted_EN1$idadedoimc <- as.factor(NHS_molted_EN1$idadedoimc)
write.csv(NHS_molted_EN1, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_EN1_FINAL.csv")

#Criando base de dados de interesse (ABT) Estado Nutricional 1 Media

NH_ABT <- cbind(NH_EN2$imcatual, 
                NH_EN2$imc1ano, 
                NH_EN2$imc10ano,
                NH_EN2$imc25ano,
                NH_EN2$RIDAGEYR,
                NH_EN2$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_EN2 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_EN2$tempo <- as.numeric(as.character(NHS_molted_EN2$tempo))
NHS_molted_EN2$idadedoimc <- ifelse(NHS_molted_EN2$tempo == 25, 25, 
                                    NHS_molted_EN2$idade - NHS_molted_EN2$tempo)
NHS_molted_EN2$idadedoimc <- as.factor(NHS_molted_EN2$idadedoimc)
write.csv(NHS_molted_EN2, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_EN2_FINAL.csv")

#Criando base de dados de interesse (ABT) Estado Nutricional 2 Alta 

NH_ABT <- cbind(NH_EN3$imcatual, 
                NH_EN3$imc1ano, 
                NH_EN3$imc10ano,
                NH_EN3$imc25ano,
                NH_EN3$RIDAGEYR,
                NH_EN3$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_EN3 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_EN3$tempo <- as.numeric(as.character(NHS_molted_EN3$tempo))
NHS_molted_EN3$idadedoimc <- ifelse(NHS_molted_EN3$tempo == 25, 25, 
                                    NHS_molted_EN3$idade - NHS_molted_EN3$tempo)
NHS_molted_EN3$idadedoimc <- as.factor(NHS_molted_EN3$idadedoimc)
write.csv(NHS_molted_EN3, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_EN3_FINAL.csv")

#Criando base de dados de interesse (ABT) Estado Nutricional 3 Muito Alta

NH_ABT <- cbind(NH_EN4$imcatual, 
                NH_EN4$imc1ano, 
                NH_EN4$imc10ano,
                NH_EN4$imc25ano,
                NH_EN4$RIDAGEYR,
                NH_EN4$SEQN)
colnames(NH_ABT) <- c('0','1','10','25','idade','SEQN')

# Empilhando
NHS_molted <- melt(NH_ABT,id='SEQN', value.name = 'value')
colnames(NHS_molted) <- c('id','tempo','imc')
NHS_molted

# Recuperando o SEQN e a idade

NHS_molted$idade = 0
NHS_molted$SEQN = 0

listaDeIdades <- subset(NHS_molted, NHS_molted$tempo == "idade")

for (row in 1:nrow(listaDeIdades)) {
  id <- listaDeIdades[row, "id"]
  idade <- listaDeIdades[row, "imc"]
  NHS_molted$idade[NHS_molted$id == id] <- idade
}

listaDeSequences <- subset(NHS_molted, NHS_molted$tempo == "SEQN")
for (row in 1:nrow(listaDeSequences)) {
  id <- listaDeSequences[row, "id"]
  sqn <- listaDeSequences[row, "imc"]
  NHS_molted$SEQN[NHS_molted$id == id] <- sqn
}

# Retirando idade e SEQN do empilhamento
no_idade <- subset(NHS_molted, NHS_molted$tempo != "idade")
NHS_molted_EN4 <- subset(no_idade, no_idade$tempo != "SEQN")

# Transformando as idades

NHS_molted_EN4$tempo <- as.numeric(as.character(NHS_molted_EN4$tempo))
NHS_molted_EN4$idadedoimc <- ifelse(NHS_molted_EN4$tempo == 25, 25, 
                                    NHS_molted_EN4$idade - NHS_molted_EN4$tempo)
NHS_molted_EN4$idadedoimc <- as.factor(NHS_molted_EN4$idadedoimc)
write.csv(NHS_molted_EN4, "C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA/NHS_molted_EN4_FINAL.csv")

# ATENÇÃO: .
# mude o caminho para a pasta onde os dados estão na sua máquina

setwd("C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA")

# População geral 
NH_GERAL_NTOTAL <- read.csv('NH_GERAL_MA.csv')
summary(NH_GERAL_NTOTAL)
menosdetrintaecinco <- subset(NH_GERAL_NTOTAL, NH_GERAL_NTOTAL$RIDAGEYR < 35)
aatrintaquarentaecinco <- subset(NH_GERAL_NTOTAL, NH_GERAL_NTOTAL$RIDAGEYR < 45)
aatrintaquarentaecinco <- subset(aatrintaquarentaecinco, aatrintaquarentaecinco$RIDAGEYR >= 35)
aaquarentacinquentaecinco <- subset(NH_GERAL_NTOTAL, NH_GERAL_NTOTAL$RIDAGEYR < 55)
aaquarentacinquentaecinco <- subset(aaquarentacinquentaecinco, aaquarentacinquentaecinco$RIDAGEYR >= 45)
aamaisdecinco <-  subset(NH_GERAL_NTOTAL, NH_GERAL_NTOTAL$RIDAGEYR >= 55)

NH_GERAL_NTOTAL$cimcatual = cut(NH_GERAL_NTOTAL$imcatual, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
summarytools::freq(NH_GERAL_NTOTAL$cimcatual) # nbruto e %


# Analise Multinivel
setwd("C:/Users/Cacau Pastorello/Desktop/NHANES_DEBORA")
summary(NH_GERAL_NTOTAL)
NH_GERAL <- read.csv('NHS_molted_GERAL_FINAL.csv')

# Mulheres
NH_FEM <- read.csv('NHS_molted_FEM_FINAL.csv')

# Homens
NH_MASC <- read.csv('NHS_molted_MASC_FINAL.csv')

# Gerações
NH_G1 <- read.csv('NHS_molted_G1_FINAL.csv')

NH_G2 <- read.csv('NHS_molted_G2_FINAL.csv')

NH_G3 <- read.csv('NHS_molted_G3_FINAL.csv')

NH_G4 <- read.csv('NHS_molted_G4_FINAL.csv')

# Estado Nutricional
NH_EN2 <- read.csv('NHS_molted_EN2_FINAL.csv')

NH_EN1 <- read.csv('NHS_molted_EN1_FINAL.csv')

NH_EN3 <- read.csv('NHS_molted_EN3_FINAL.csv')

NH_EN4 <- read.csv('NHS_molted_EN4_FINAL.csv')

# Multinivel Geral
NH_GERAL$idadedoimc <- as.factor(as.character(NH_GERAL$idadedoimc))
mod_geral <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_GERAL)
mod_geral
summary(mod_geral)
# tab_model(mod_gerral)

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
mod_fem <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_FEM)
mod_fem
summary(mod_fem)
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
mod_masc <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_MASC)
mod_masc
summary(mod_masc)
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
sexo_masc <- rep('male',48)

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
sexo_fem <- rep('female',48)

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
  labs(colour = "Sexo", x = 'age (years)', y='Body Mass Index (Kg/m²)',
       tag = 'Fixed-effects Multilevel Analysis by Sex Stratification',
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
mod_G1 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G1)
mod_G1
summary(mod_G1)
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
mod_G2 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G2)
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
mod_G3 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G3)
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
mod_G4 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_G4)
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
G1 <- rep('Born 1950 or earlier',25)

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
       tag = 'Fixed-effects Multilevel Analysis by Generation Stratification - Females',
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
mod_EN1 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN1)
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
mod_EN2 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN2)
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
mod_EN3 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN3)
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
mod_EN4 <- lmer(imc ~ idadedoimc +( 1 | id), data=NH_EN4)
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
       tag = 'Fixed-effects Multilevel Analysis by Nutritional Status at 25 years old Stratification - Females',
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


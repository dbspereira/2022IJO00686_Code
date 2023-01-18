########################################################################
#Universidade do Estado de São Paulo - USP
#Faculdade de Saúde Pública - FSP
#Programa de Pós Graduação em Nutrição em Saúde Pública - PPGNSP
#Laboratório de Avaliação do Estado Nutricional de Populações - LANPOP
#Variação o peso corporal ao longo da fase adulta: uma análise da NHANES 1999-2019
#Autora: Debora dos Santos Pereira
#Co-Autora: Claudia Cristina Vieira Pastorello
#Co-Autora: Mariane Helen de Oliveria
#Co-Autora: Iolanda Karla Santana dos Santos
#Co-Autora: Camila Medeiros da Silva Mazzeti
#Orientação: Wolney Lisboa Conde
########################################################################

########################################################################
#UNIÃO DAS BASES DE DADOS CONTINUAS NHANES COM VARIAVEIS DE INTERESSE
########################################################################

# Pacotes utilizados nesta analise
library(foreign)
library(tidyverse)
library(anchors)
library(haven)


# ATENÇÃO: 
# mude o caminho para a pasta onde os dados estão na sua máquina

setwd("C:\\Users\\Débora\\Desktop\\contra prova\\Bancos ano a ano")

# chamando os dados Demográficos

DM9900 <- read.xport("DEMO.XPT")
DM0102 <- read.xport("DEMO_B.XPT")
DM0304 <- read.xport("DEMO_C.XPT")
DM0506 <- read.xport("DEMO_D.XPT")
DM0708 <- read.xport("DEMO_E.XPT")
DM0910 <- read.xport("DEMO_F.XPT")
DM1112 <- read.xport("DEMO_G.XPT")
DM1314 <- read.xport("DEMO_H.XPT")
DM1516 <- read.xport("DEMO_I.XPT")
DM1718 <- read.xport("DEMO_J.XPT")
DM1920 <- read_dta("C:/Users/Débora/Desktop/contra prova/Bancos ano a ano/demog1720.dta")

# Separando variaveis de interesse

# SEQN     - Respondent sequence number
# SDDSRVYR - Data Release Number
# RIDEXMON - Six month time period
# RIAGENDR - Gender
# RIDAGEYR - Age at Screening
# RIDRETH1 - Race/Ethnicity - Recode
# DMDEDUC2 - Education Level - Adults 20+
# DMDMARTL - Marital Status
# DMDHHSIZ - Total people in the Household
# RIDEXPRG - Pregnancy Status at Exam - Recode
# INDFMPIR - poverty Index

# Filtrando bases ano a ano

DM9900s <- subset(DM9900, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM0102s <- subset(DM0102, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM0304s <- subset(DM0304, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM0506s <- subset(DM0506, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM0708s <- subset(DM0708, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM0910s <- subset(DM0910, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM1112s <- subset(DM1112, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))


DM1314s <- subset(DM1314, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM1516s <- subset(DM1516, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))

DM1718s <- subset(DM1718, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                            'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                            'DMDEDUC2', 'DMDMARTL', 
                            'INDFMPIR', 'RIDEXPRG'))
DM1920s <- subset(DM1920, select = c('SEQN', 'SDDSRVYR', 'RIDEXMON', 
                                     'RIAGENDR', 'RIDAGEYR', 'RIDRETH1',
                                     'DMDEDUC2', 'DMDMARTL', 
                                     'INDFMPIR', 'RIDEXPRG'))

DMTOT <- rbind(DM9900s, DM0102s, DM0304s, DM0506s, DM0708s, 
               DM0910s, DM1112s, DM1314s, DM1516s, DM1718s, DM1920s)

table(DMTOT$SDDSRVYR)

# mudando ref de ano 1920 de 66 para 11
DMTOT$SDDSRVYR <- ifelse(DMTOT$SDDSRVYR==66,11,DMTOT$SDDSRVYR)
table(DMTOT$SDDSRVYR)

# chamando os dados de Questionário Histórico de Peso

WH9900 <- read.xport("WHQ.XPT")
WH0102 <- read.xport("WHQ_B.XPT")
WH0304 <- read.xport("WHQ_C.XPT")
WH0506 <- read.xport("WHQ_D.XPT")
WH0708 <- read.xport("WHQ_E.XPT")
WH0910 <- read.xport("WHQ_F.XPT")
WH1112 <- read.xport("WHQ_G.XPT")
WH1314 <- read.xport("WHQ_H.XPT")
WH1516 <- read.xport("WHQ_I.XPT")
WH1718 <- read.xport("WHQ_J.XPT")
WH1920 <- read.xport("P_WHQ.XPT")

# Separando variáveis de interesse

# SEQN - Respondent sequence number
# WHD010 - Current self-reported height (inches)
# WHD020 - Current self-reported weight (pounds)
# WHD050 - Self-reported weight-1 yr ago (pounds)
# WHD110 - Self-reported weight-10 yrs ago (pounds)
# WHD120 - Self-reported weight - age 25 (pounds)
# WHD130 - Self-reported height - age 25 (inches)
# WHD140 - Self-reported greatest weight (pounds)
# WHQ150 - Age when heaviest weight (Não tem em 1999-2000)


# Filtrando bases ano a ano

# criando variável inexistente em 1999-2000 com dados faltantes
WH9900$WHQ150 <- 'NA'

WH9900s <- subset(WH9900, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH0102s <- subset(WH0102, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH0304s <- subset(WH0304, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH0506s <- subset(WH0506, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH0708s <- subset(WH0708, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH0910s <- subset(WH0910, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH1112s <- subset(WH1112, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH1314s <- subset(WH1314, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH1516s <- subset(WH1516, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH1718s <- subset(WH1718, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WH1920s <- subset(WH1920, select = c('SEQN', 'WHD010', 'WHD020', 
                                     'WHD050', 'WHD110', 'WHD120', 
                                     'WHD130','WHD140', 'WHQ150'))

WHTOT <- rbind(WH9900s, WH0102s, WH0304s, WH0506s, WH0708s, 
               WH0910s, WH1112s, WH1314s, WH1516s, WH1718s, WH1920s)

# Unificando pelo SEQN

NHTOT <- merge(DMTOT,WHTOT,by='SEQN')
table(NHTOT$SDDSRVYR)
# Salvando a base em .csv

write.csv(NHTOT, "C:\\Users\\Débora\\Desktop\\contra prova\\NHTOT.csv")


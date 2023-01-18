#calculando as prevalencias do banco

setwd("C:/Users/Débora/Desktop/contra prova")

# População geral 
NH_GERAL <- read.csv('NH_GERAL_MA.csv')

NH_GERAL$X.1 <- NULL
NH_GERAL$X <- NULL

# Banco somente de pesos

setwd("C:/Users/Débora/Desktop/contra prova/Arquivos Modelo Pós Ponderação")

ponderacao <- read.xlsx("peso_ponderacao.xlsx")


# Unindo os bancos

banco_descritiva <- merge(NH_GERAL, ponderacao, by="SEQN")


# Analisando variável por variável

colnames(banco_descritiva)

######### 
# sexo
# 1 = homem, 2 = mulher
table(banco_descritiva$RIAGENDR)
str(banco_descritiva$RIAGENDR)
banco_descritiva$RIAGENDR <- as.factor(banco_descritiva$RIAGENDR)
freq(banco_descritiva$RIAGENDR)

banco_descritiva$sexoCat <- factor(banco_descritiva$RIAGENDR,
                                    levels = c("2", "1"),
                                    labels = c("Female", "Male"))
table(banco_descritiva$sexoCat)

# criando base ponderada
banco_descritiva %>% 
  as_survey_design(., weights=MEC72Y) -> banco_descritiva_2

#segundo passo
banco_descritiva_2 %>% group_by(sexoCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

a <- svymean(~sexoCat, design = banco_descritiva_2)
confint(a, level = 0.95)

# idade categórica

banco_descritiva$idadeCat = cut(banco_descritiva$RIDAGEYR,
                                 breaks = c(0, 39, 59, Inf),
                                 labels = c('28-39','40-59','60+')) 

table(banco_descritiva$idadeCat)
str(banco_descritiva$idadeCat)

# criando base ponderada
banco_descritiva %>% 
  as_survey_design(., weights=MEC72Y) -> banco_descritiva_2

banco_descritiva_2 %>% group_by(idadeCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

b <- svymean(~idadeCat, design = banco_descritiva_2)
confint(b, level = 0.95)

#etnia categorica

table(banco_descritiva$RIDRETH1)

banco_descritiva$etniaCat <- factor(banco_descritiva$RIDRETH1,
                                     levels = c("1", "2", "3", "4", "5"),
                                     labels = c("Mexican American",
                                                "Other Hispanic",
                                                "Non-Hispanic White",
                                                "Non-Hispanic Black",
                                                "Other Race - Including Multi-Racial"))
freq(banco_descritiva$etniaCat)

#imc categorias

table(banco_descritiva$cimcatual)
banco_descritiva$cimc2 <- factor(banco_descritiva$cimcatual,
                                    levels = c("baixo peso", "peso normal", "sobrepeso", "obesidade"),
                                    labels = c("baixo peso",
                                               "peso normal",
                                               "sobrepeso",
                                               "sobrepeso"))


# criando base ponderada
banco_descritiva %>% 
  as_survey_design(., weights=MEC72Y) -> banco_descritiva_2

#segundo passo
banco_descritiva_2 %>% group_by(etniaCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

c <- svymean(~etniaCat, design = banco_descritiva_2)
confint(c, level = 0.95)

# índice de pobreza

table(banco_descritiva$INDFMPIR)

banco_descritiva$indpobrezaCat = cut(banco_descritiva$INDFMPIR,
                                      breaks = c(-Inf, 1.25, 3.50, Inf),
                                      labels = c('<1.25','1.25-3.49','>=3.50')) 

table(banco_descritiva$indpobrezaCat)

# criando base ponderada
banco_descritiva %>% 
  as_survey_design(., weights=MEC72Y) -> banco_descritiva_2
#segundo passo

banco_descritiva_2 %>% group_by(indpobrezaCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

d <- svymean(~indpobrezaCat, design = banco_descritiva_2)
confint(d, level = 0.95)

# escolaridade categórica

table(banco_descritiva$DMDEDUC2)
banco_descritiva$escolaridadeCat <- factor(banco_descritiva$DMDEDUC2,
                                            levels = c("1", "2", "3", "4", "5", "7", "9"),
                                            labels = c("Incomplete High School/Elementary School",
                                                       "Incomplete High School/Elementary School",
                                                       "Complete High School",
                                                       "Complete Undergraduate or Graduate Education",
                                                       "Complete Undergraduate or Graduate Education",
                                                       "Refusals/they were unable to inform",
                                                       "Refusals/they were unable to inform"))


table(banco_descritiva$escolaridadeCat)
# criando base ponderada
banco_descritiva %>% 
  as_survey_design(., weights=MEC72Y) -> banco_descritiva_2
#segundo passo

banco_descritiva_2 %>% group_by(escolaridadeCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

e <- svymean(~escolaridadeCat, design = banco_descritiva_2)
confint(e, level = 0.95)


#medias de imc por ciclo
colnames(banco_descritiva)
table(banco_descritiva$imcatual)

svyby(~cimcatual, by=~SDDSRVYR, design = banco_descritiva_2, FUN = svymean)

xx <- banco_descritiva_2 %>% group_by(cimcatual, SDDSRVYR) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))
print(xx, n=34)

xy <- banco_descritiva_2 %>% group_by(imcatual, RIDAGEYR) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))
print(xy, n=34)

svyby(~imcatual, by=~RIDAGEYR, design = banco_descritiva_2, FUN = svymean)
svyby(~imcatual, by=~RIDAGEYR, design = banco_descritiva_2, FUN = svymean)


#categorizando por IMC


svyby(~sexoCat, by=~cimcatual, design = banco_descritiva_2, FUN = svymean)

#Verificando o balanceamento dos dados:

banco_descritiva %>% 
  group_by(cimcatual, SDDSRVYR) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


#Exploração visual do imc médio
ggplotly(
  banco_descritiva %>%
    group_by(RIDAGEYR) %>%
    mutate(desempenho_medio = mean(imcatual, na.rm = TRUE)) %>% 
    ggplot() +
    geom_point(aes(x = RIDAGEYR, y = imcatual),color = "orange", alpha = 0.1) +
    geom_line(aes(x = RIDAGEYR, y = desempenho_medio, 
                  group = 1, color = "Média de IMC atual"), size = 1.2) +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    labs(x = "Idade",
         y = "Média de IMC (kg/m²)") +
    theme(legend.title = element_blank(),
          panel.border = element_rect(NA),
          panel.grid = element_line("grey"),
          panel.background = element_rect("white"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90))
)


svyby(~imcatual, by=~ciclo==5, design = banco_analise_ponderado, FUN = svymean)
svyby(~imc1ano, by=~ciclo==5, design = banco_analise_ponderado, FUN = svymean)
svyby(~imc10ano, by=~ciclo, design = banco_analise_ponderado, FUN = svymean)
svyby(~imc25ano, by=~ciclo, design = banco_analise_ponderado, FUN = svymean)

svyby(~imcatual, by=~idade, design = banco_analise_ponderado, FUN = svymean)

svyby(~cimcatual, by=~sexoCat, design = banco_descritiva_2, FUN = svymean)

mean(NH_GERAL_NTOTAL$RIDAGEYR)

#ESTRATIFICANDO POR IMC AS VARIAVEIS
#SEXO
propimcEsexo <- svyby(~cimc2, by=~sexoCat, design = banco_descritiva_2, FUN = svymean)
ftable(propimcEsexo)
confint(propimcEsexo, level = 0.95)

#IDADE
propimcEidade <- svyby(~cimc2, by=~idadeCat, design = banco_descritiva_2, FUN = svymean)
ftable(propimcEidade)
confint(propimcEidade, level = 0.95)

#etnia
propimcEetnia <- svyby(~cimc2, by=~etniaCat, design = banco_descritiva_2, FUN = svymean)
ftable(propimcEetnia)
confint(propimcEetnia, level = 0.95)

#indice de pobreza

#etnia
propimcEip <- svyby(~cimc2, by=~indpobrezaCat, design = banco_descritiva_2, FUN = svymean)
ftable(propimcEip)
confint(propimcEip, level = 0.95)

#escolaridade

propimcEesc <- svyby(~cimc2, by=~escolaridadeCat, design = banco_descritiva_2, FUN = svymean)
ftable(propimcEesc)
confint(propimcEesc, level = 0.95)

#estimando totais
totalsexo <- svytotal(x=~sexoCat, design=banco_descritiva_2, na.rm=TRUE)
totalsexo

#MÉDIA DE IDADE PONDERADA


svymean(banco_descritiva$RIDAGEYR, design = banco_descritiva_2 )

#FILTRANDO POR SEXO
sexoF <- banco_descritiva %>% filter(., sexoCat == "Female")
sexoM <- banco_descritiva %>% filter(., sexoCat == "Male")


setwd("C:/Users/Débora/Desktop/contra prova")

ponderacao <- read.xlsx("peso_ponderacao.xlsx")

# Peso da Ponderação
PESO <- read.xlsx("peso_ponderacao.xlsx")
PESO <- rename(PESO, c("id" = "SEQN"))

# GERAL
NH_GERAL <- read.csv('NHS_molted_GERAL_FINAL.csv')
NH_GERAL <- merge(NH_GERAL, PESO, by = "id")

# Mulheres
NH_FEM <- read.csv('NHS_molted_FEM_FINAL.csv')
NH_FEM <- merge(NH_FEM, PESO, by = "id")

# Homens
NH_MASC <- read.csv('NHS_molted_MASC_FINAL.csv')
NH_MASC <- merge(NH_MASC, PESO, by = "id")


# criando base ponderada
NH_FEM %>% 
  as_survey_design(., weights=MEC72Y) -> sexoF_ponderado


NH_MASC %>% 
  as_survey_design(., weights=MEC72Y) -> sexoM_ponderado


#padronizando as médias de IMC por idade

svyby(~imc, by=~idadedoimc, design = sexoF_ponderado, FUN = svymean)
svyby(~imc, by=~idadedoimc, design = sexoM_ponderado, FUN = svymean)

#Exploração visual do imc médio
ggplotly(
  NH_MASC %>%
    group_by(idadedoimc) %>%
    mutate(desempenho_medio = mean(imc, na.rm = TRUE)) %>% 
    ggplot() +
    geom_point(aes(x = idadedoimc, y = imc),color = "orange", alpha = 0.1) +
    geom_line(aes(x = idadedoimc, y = desempenho_medio, 
                  group = 1, color = "Média de IMC atual- Homens"), size = 1.2) +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    scale_x_continuous(limits = c(18,65), 
                       breaks = seq(18,65, by=5))  +
    scale_y_continuous(limits = c(20,35), 
                       breaks = seq(20,35, by=5))   +
    labs(x = "Idade",
         y = "Média de IMC (kg/m²)") +
    theme(legend.title = element_blank(),
          panel.border = element_rect(NA),
          panel.grid = element_line("grey"),
          panel.background = element_rect("white"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90))
)

ggplotly(
  NH_FEM %>%
    group_by(idadedoimc) %>%
    mutate(desempenho_medio = mean(imc, na.rm = TRUE)) %>% 
    ggplot() +
    geom_point(aes(x = idadedoimc, y = imc),color = "lightgrey", alpha = 0.1) +
    geom_line(aes(x = idadedoimc, y = desempenho_medio, 
                  group = 1, color = "Média de IMC atual - Mulheres"), size = 1.2) +
    scale_color_manual("Legenda:",
                       values = "black") +
    scale_x_continuous(limits = c(18,65), 
                       breaks = seq(18,65, by=5))  +
    scale_y_continuous(limits = c(20,35), 
                       breaks = seq(20,35, by=5))   +
    labs(x = "Idade",
         y = "Média de IMC (kg/m²)") +
    theme(legend.title = element_blank(),
          panel.border = element_rect(NA),
          panel.grid = element_line("grey"),
          panel.background = element_rect("white"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90))
)

# criando base ponderada do banco inteiro
NH_GERAL %>% 
  as_survey_design(., weights=MEC72Y) -> NH_GERAL_PONDERADO

ggplotly(
  NH_GERAL %>%
    group_by(idadedoimc) %>%
    mutate(desempenho_medio = mean(imc, na.rm = TRUE)) %>% 
    ggplot() +
    geom_point(aes(x = idadedoimc, y = imc),color = "lightgrey", alpha = 0.1) +
    geom_line(aes(x = idadedoimc, y = desempenho_medio, 
                  group = 1, color = "Média de IMC atual - Geral"), size = 1.2) +
    scale_color_manual("Legenda:",
                       values = "black") +
    scale_x_continuous(limits = c(18,65), 
                       breaks = seq(18,65, by=5))  +
    scale_y_continuous(limits = c(20,35), 
                       breaks = seq(20,35, by=5))   +
    labs(x = "Idade",
         y = "Média de IMC (kg/m²)") +
    theme(legend.title = element_blank(),
          panel.border = element_rect(NA),
          panel.grid = element_line("grey"),
          panel.background = element_rect("white"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90))
)

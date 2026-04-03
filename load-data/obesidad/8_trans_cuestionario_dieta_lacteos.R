#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de dieta - lácteos          ##
##                             ##
#################################

# Nota: Puede ser una buena idea crear una variable de leche con
# poca grasa. Para eso dejaré comentado el código de esta variable
# nueva. También puede ser bueno pasar la varaibles binarias a 
# factores

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset de obesidad
df_obesidad <- read_csv(
  "eda-obesidad/data/nhanes/obesidad_nhanes.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del cuestionario para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    consumo_lacteos = DBQ197,
    bebe_leche_entera = DBQ223A,
    bebe_leche_2_grasa = DBQ223B,
    bebe_leche_1_grasa = DBQ223C,
    bebe_leche_sin_grasa = DBQ223D,
    bebe_leche_soja = DBQ223E,
    bebe_otras_leches = DBQ223U,
    bebedor_de_leche = DBQ229
  )

#####################################
##
## Consumo de productos lácteos 
## en los últimos 30 días
##
#####################################

summary(df_obesidad$consumo_lacteos) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   1.809   3.000   4.000 

unique(df_obesidad$consumo_lacteos) # produce: [1] 1 3 0 2 4

table(df_obesidad$consumo_lacteos)
#   0    1    2    3    4 
#1077 1012 1649 2025   19 

# Transformar los valores numéricos a factores 
df_obesidad <- df_obesidad %>%
  mutate(
    consumo_lacteos = case_when(
      consumo_lacteos == 0 ~ "nunca",
      consumo_lacteos == 1 ~ "rara_vez",
      consumo_lacteos == 2 ~ "frecuente",
      consumo_lacteos == 3 ~ "diario",
      TRUE ~ NA_character_
    )
  )

# Transformar la variable de uso de sal en la mesa a factor ordenado
df_obesidad <- df_obesidad %>%
  mutate(
    consumo_lacteos = factor(
      consumo_lacteos,
      levels = c("nunca", "rara_vez", "frecuente", "diario"),
      ordered = TRUE
    )
  )

class(df_obesidad$consumo_lacteos) # produce: [1] "ordered" "factor"

unique(df_obesidad$consumo_lacteos) # produce:
# Levels: nunca < rara_vez < frecuente < diario

table(df_obesidad$consumo_lacteos) # produce:
#nunca  rara_vez frecuente    diario 
# 1077      1012      1649      2025 

#####################################
##
## Consumo de leche entera
##
#####################################

class(df_obesidad$bebe_leche_entera) # produce: numeric

summary(df_obesidad$bebe_leche_entera) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   10.00   10.00   10.74   10.00   99.00    4366

unique(df_obesidad$bebe_leche_entera) # produce: [1] 10 NA 99 77

# transformar consumo de leche entera a binario
df_obesidad <- df_obesidad %>%
  mutate(
    bebe_leche_entera = ifelse(!is.na(bebe_leche_entera), 1, 0)
  )
class(df_obesidad$bebe_leche_entera) # produce: numeric
summary(df_obesidad$bebe_leche_entera) # produce:
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.2449  0.0000  1.0000 
table(df_obesidad$bebe_leche_entera) # produce: 
#   0    1 
#4366 1416

#######################################
##
## Consumo de leche con un 2% de grasa
##
#######################################

class(df_obesidad$bebe_leche_2_grasa) # produce: numeric

summary(df_obesidad$bebe_leche_2_grasa) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11      11      11      11      11      11    3766 

unique(df_obesidad$bebe_leche_2_grasa) # [1] NA 11

# transformar consumo de leche con 2% de grasa a binario
df_obesidad <- df_obesidad %>%
  mutate(
    bebe_leche_2_grasa = ifelse(!is.na(bebe_leche_2_grasa), 1, 0)
  )
class(df_obesidad$bebe_leche_2_grasa) # produce: numeric
unique(df_obesidad$bebe_leche_2_grasa) # produce:
#[1] 0 1
table(df_obesidad$bebe_leche_2_grasa) # produce: 
#   0    1 
#3766 2016 

#######################################
##
## Consumo de leche con un 1% de grasa
##
#######################################

class(df_obesidad$bebe_leche_1_grasa) # produce: numeric

summary(df_obesidad$bebe_leche_1_grasa) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11      11      11      11      11      11    3766 

unique(df_obesidad$bebe_leche_1_grasa) # [1] NA 12

# transformar consumo de leche con 1% de grasa a binario
df_obesidad <- df_obesidad %>%
  mutate(
    bebe_leche_1_grasa = ifelse(!is.na(bebe_leche_1_grasa), 1, 0)
  )
class(df_obesidad$bebe_leche_1_grasa) # produce: numeric
unique(df_obesidad$bebe_leche_1_grasa) # produce:
#[1] 0 1
table(df_obesidad$bebe_leche_1_grasa) # produce: 
#   0    1 
#5289  493 

#######################################
##
## Consumo de leche sin grasa 
##
#######################################

class(df_obesidad$bebe_leche_sin_grasa) # produce: numeric

summary(df_obesidad$bebe_leche_sin_grasa) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13      13      13      13      13      13    5266 

unique(df_obesidad$bebe_leche_sin_grasa) # [1] NA 13

# transformar consumo de leche sin grasa a binario
df_obesidad <- df_obesidad %>%
  mutate(
    bebe_leche_sin_grasa = ifelse(!is.na(bebe_leche_sin_grasa), 1, 0)
  )
class(df_obesidad$bebe_leche_sin_grasa) # produce: numeric
unique(df_obesidad$bebe_leche_sin_grasa) # produce:
#[1] 0 1
table(df_obesidad$bebe_leche_sin_grasa) # produce: 
#   0    1 
#5266  516 

###############################
##
## Consumo de leche de soja
##
###############################

class(df_obesidad$bebe_leche_soja) # produce: numeric

summary(df_obesidad$bebe_leche_soja) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  14      14      14      14      14      14    5618  

unique(df_obesidad$bebe_leche_soja) # [1] NA 14

# transformar consumo de leche de soja a binario
df_obesidad <- df_obesidad %>%
  mutate(
    bebe_leche_soja = ifelse(!is.na(bebe_leche_soja), 1, 0)
  )
class(df_obesidad$bebe_leche_soja) # produce: numeric
unique(df_obesidad$bebe_leche_soja) # produce:
#[1] 0 1
table(df_obesidad$bebe_leche_soja) # produce: 
#   0    1 
#5618  164 

###############################
##
## Consumo de otras leches
##
###############################

class(df_obesidad$bebe_otras_leches) # produce: numeric

summary(df_obesidad$bebe_otras_leches) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  30      30      30      30      30      30    5401  

unique(df_obesidad$bebe_otras_leches) # [1] NA 30

# transformar consumo de otros tipos de leche a binario
df_obesidad <- df_obesidad %>%
  mutate(
    bebe_otras_leches = ifelse(!is.na(bebe_otras_leches), 1, 0)
  )
class(df_obesidad$bebe_otras_leches) # produce: numeric
unique(df_obesidad$bebe_otras_leches) # produce:
#[1] 0 1
table(df_obesidad$bebe_otras_leches) # produce: 
#   0    1 
#5401  381 

###################################
##
## Es un bebedor regular de leche
## al menos 5 veces por semana?
##
###################################

unique(df_obesidad$bebedor_de_leche) # produce: [1]  2  1  3 NA  9

table(df_obesidad$bebedor_de_leche)
#   1    2    3    9 
#2154 1392 1907    2 

# Transformar los valores numéricos a factores 
df_obesidad <- df_obesidad %>%
  mutate(
    bebedor_de_leche = case_when(
      bebedor_de_leche == 1 ~ "si",
      bebedor_de_leche == 3 ~ "variado",
      bebedor_de_leche == 2 ~ "no",
      TRUE ~ NA_character_
    )
  )

# Transformar la variable de uso de bebedor regular de leche a factor
# ordenado
df_obesidad <- df_obesidad %>%
  mutate(
    bebedor_de_leche = factor(
      bebedor_de_leche,
      levels = c("si", "variado", "no"),
      ordered = TRUE
    )
  )

class(df_obesidad$bebedor_de_leche) # produce: [1] "ordered" "factor"

unique(df_obesidad$bebedor_de_leche) # produce:
#[1] variado si      no      <NA>   
# Levels: si < variado < no

table(df_obesidad$bebedor_de_leche) # produce:
#  si variado      no 
#2154    1907    1392 

###############################
##
## Consumo de leches con poca
## grasa
##
###############################

# Agrupar a los bebedores de leche al 1%, 2% y sin grasa en una 
# sola variable de leche baja en grasa
#df_obesidad <- df_obesidad %>%
#  mutate(
#    leche_baja_grasa = ifelse(bebe_leche_2_grasa == 1 | 
#                                bebe_leche_1_grasa == 1 | 
#                                bebe_leche_sin_grasa == 1, 
#                              1, 0),
#  )
#class(df_obesidad$leche_baja_grasa) # produce: numeric

#unique(df_obesidad$leche_baja_grasa) # produce: [1] 0 1 

#table(df_obesidad$leche_baja_grasa) # produce:
#   0    1 
#2841 2941 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# lácteos en la dieta
obesidad_cuestionario_dieta_lacteos <- df_obesidad %>%
  select(SEQN, consumo_lacteos, bebe_leche_entera, bebe_leche_2_grasa,
         bebe_leche_1_grasa, bebe_leche_sin_grasa, bebe_leche_soja,
         bebe_otras_leches, bebedor_de_leche)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_cuestionario_dieta_lacteos,
          "eda-obesidad/data/obesidad/8_obesidad_cuestionario_dieta_lacteos.csv",
          row.names = FALSE)
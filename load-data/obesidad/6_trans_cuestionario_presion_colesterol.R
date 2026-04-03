#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de presión y colesterol     ##
##                             ##
#################################

# Nota: no olvidar crear las variables de homa-ir y nos/mos

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
    tiene_presion_alta = BPQ030,
    toma_medicamentos_presion = BPQ050A,
    tiene_colesterol_alto = BPQ080,
    toma_medicamentos_colesterol = BPQ100D
  )

##########################
##
## Le dijeron que tenía
## presión alta ? 2+ veces
##
##########################

summary(df_obesidad$tiene_presion_alta) # produce:

unique(df_obesidad$tiene_presion_alta) # produce: [1]  1  2 NA  9

# Pasar los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    tiene_presion_alta = case_when(
      tiene_presion_alta == 1 ~ "si",
      tiene_presion_alta == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    tiene_presion_alta = as.factor(tiene_presion_alta) # tranformar a 
    # factor
  )
class(df_obesidad$tiene_presion_alta) # produce: factor

unique(df_obesidad$tiene_presion_alta) # produce:
#[1] si   no   <NA>
#Levels: no si

summary(df_obesidad$tiene_presion_alta) # produce:
# no   si NA's 
#397 1653 3732 

##########################
##
## Toma medicamentos para
## la presión alta ?
##
##########################

summary(df_obesidad$toma_medicamentos_presion) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.129   1.000   9.000    4056 

unique(df_obesidad$toma_medicamentos_presion) # produce:
#[1]  2 NA  1  9

# Pasar los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    toma_medicamentos_presion = case_when(
      toma_medicamentos_presion == 1 ~ "si",
      toma_medicamentos_presion == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    toma_medicamentos_presion = as.factor(toma_medicamentos_presion)
  )
class(df_obesidad$toma_medicamentos_presion) # produce: factor
unique(df_obesidad$toma_medicamentos_presion) # produce:
#[1] si   no   <NA>
#Levels: no si
summary(df_obesidad$toma_medicamentos_presion) # produce:
# no   si NA's 
#215 1510 40572 

##########################
##
## Médico dijo que tenía 
## colesterol alto ?
##
##########################

unique(df_obesidad$tiene_colesterol_alto) # produce: [1] 1 2 9

# Pasar los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    tiene_colesterol_alto = case_when(
      tiene_colesterol_alto == 1 ~ "si",
      tiene_colesterol_alto == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    tiene_colesterol_alto = as.factor(tiene_colesterol_alto)
  )
class(df_obesidad$tiene_colesterol_alto) # produce: factor
unique(df_obesidad$tiene_colesterol_alto) # produce:
#[1] si   no   <NA>
#Levels: no si
summary(df_obesidad$tiene_colesterol_alto) # produce:
#  no   si NA's 
#3810 1937   35 

#################################
##
## Actualmente toma 
## medicación para colesterol?
##
#################################

unique(df_obesidad$toma_medicamentos_colesterol) # produce:
#[1]  1 NA  2  9

# Pasar los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    toma_medicamentos_colesterol = case_when(
      toma_medicamentos_colesterol == 1 ~ "si",
      toma_medicamentos_colesterol == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    toma_medicamentos_colesterol = as.factor(toma_medicamentos_colesterol)
  )
class(df_obesidad$toma_medicamentos_colesterol) # produce: factor
unique(df_obesidad$toma_medicamentos_colesterol) # produce:
#[1] si   no   <NA>
#Levels: no si
summary(df_obesidad$toma_medicamentos_colesterol) # produce:
# no   si NA's 
#300 1079 4403  

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# presión y colesterol
obesidad_cuestionario_presion_colesterol <- df_obesidad %>%
  select(SEQN, tiene_presion_alta, toma_medicamentos_presion,
         tiene_colesterol_alto, toma_medicamentos_colesterol)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_cuestionario_presion_colesterol,
          "eda-obesidad/data/obesidad/6_obesidad_cuestionario_presion_colesterol.csv",
          row.names = FALSE)

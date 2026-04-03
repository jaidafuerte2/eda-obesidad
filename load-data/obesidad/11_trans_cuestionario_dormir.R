#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de sueño                    ##
##                             ##
#################################

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
    horas_suenio = SLD010H,
    trastorno_suenio = SLQ060
  )

############################
##
## Cuántas horas duermes ?
##
############################

unique(df_obesidad$horas_suenio) # produce:
#  [1]  7  9  8  5  6 10  4  3 12  2 99 11 NA

summary(df_obesidad$horas_suenio) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.000   6.000   7.000   6.994   8.000  99.000       3 

# Transformar los valores raros 77 y 99 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    horas_suenio = 
      ifelse(horas_suenio %in% c(99, 77), NA, horas_suenio)
  )
unique(df_obesidad$horas_suenio) # produce:
# [1]  7  9  8  5  6 10  4  3 12  2 NA 11

summary(df_obesidad$horas_suenio) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.000   6.000   7.000   6.914   8.000  12.000       8 

############################
##
## Médico dijo que tienes
## trastornos del sueño
##
############################

unique(df_obesidad$trastorno_suenio) # produce: [1] 2 1 9

table(df_obesidad$trastorno_suenio) # produce:
#  1    2    9 
#549 5223   10 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    trastorno_suenio = case_when(
      trastorno_suenio == 1 ~ "si",
      trastorno_suenio == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    trastorno_suenio = as.factor(trastorno_suenio)
  )
unique(df_obesidad$trastorno_suenio) # produce: [1] 2 1 9
#[1] no   si   <NA>
#Levels: no si
table(df_obesidad$trastorno_suenio) # produce:
#  no   si 
#5223  549

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# actividad física
obesidad_cuestionario_dormir <- df_obesidad %>%
  select(SEQN, horas_suenio, trastorno_suenio)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_cuestionario_dormir,
          "eda-obesidad/data/obesidad/11_obesidad_cuestionario_dormir.csv",
          row.names = FALSE)

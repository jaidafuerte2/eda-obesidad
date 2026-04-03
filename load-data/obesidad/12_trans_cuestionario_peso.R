#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de peso                     ##
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
    estatura_auto = WHD010,
    peso_auto = WHD020,
    peso_hace_un_anio = WHD050,
    cambio_peso = WHQ060,
    intento_adelgazar = WHQ070,
    comio_menos = WHD080A,
    cambio_alimentos_menos_calorias = WHD080B,
    comio_menos_grasa = WHD080C,
    hizo_ejercicio = WHD080D,
    omitio_comidas = WHD080E,
    comio_dieteticos = WHD080F,
    unio_programa_adelgazar = WHD080H,
    bebio_agua = WHD080M,
    comio_menos_carbs = WHD080O,
    fumo_adelgazar = WHD080P,
    comio_vegetales = WHD080Q,
    comio_menos_azucar = WHD080S,
    comio_menos_chatarra = WHD080T,
    tiene_sobrepeso = MCQ080
  )

############################
##
## Cuántas horas duermes ?
##
############################
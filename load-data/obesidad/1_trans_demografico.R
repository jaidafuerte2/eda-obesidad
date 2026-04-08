#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset demográfico     ##
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

# Seleccionar sólo las variables que voy a usar
obesidad_demografico <- df_obesidad |>
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG) 

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables de demografico para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad |>
  rename(
    genero = RIAGENDR,
    edad = RIDAGEYR,
    embarazada = RIDEXPRG
  )

###################
##
## Género
##
###################

class(df_obesidad$genero) # produce: "numeric"

unique(df_obesidad$genero) # produce: [1] 1 2

table(df_obesidad$genero) # produce:
#hombre  mujer 
#  2791   2991

df_obesidad <- df_obesidad %>%
  mutate(
    genero = factor(
      genero,
      levels = c(1, 2),
      labels = c("hombre", "mujer"))
  )
unique(df_obesidad$genero) # produce:
#[1] hombre mujer 
#Levels: hombre mujer

###################
##
## Edad
##
###################

class(df_obesidad$edad) # produce: numeric

unique(df_obesidad$edad)[1:20] # produce:
#  [1] 69 54 72 73 56 61 65 26 76 33 32 18 38 50 28 35 29 23 58 57

###################
##
## Emabarazada ?
##
###################

class(df_obesidad$embarazada) # produce: [1] "numeric"

unique(df_obesidad$embarazada) # produce: [1] NA  2  3
# Donde 2 es no embarazada y 3 es desconocido 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn, el género, la edad y estado de embarazo en el
# examen
obesidad_demografico <- df_obesidad |>
  select(SEQN, genero, edad, embarazada) 
head(obesidad_demografico) # produce:
# A tibble: 6 × 4
#   SEQN genero  edad embarazada
#  <dbl> <fct>  <dbl>      <dbl>
#1 73557 hombre    69         NA
#2 73558 hombre    54         NA
#3 73559 hombre    72         NA
#4 73561 mujer     73         NA

# Crear un archivo .cvs con la dataset de demográfico
write.csv(obesidad_demografico, 
          "eda-obesidad/data/obesidad/1_obesidad_demografico.csv", 
          row.names = FALSE)

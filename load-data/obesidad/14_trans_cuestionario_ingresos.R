# Puede ser conveniente crear una entrada de blog con ingresos para
# relacionar pobreza con obesidad. Para esto se usará las variables 
# de nivel de pobreza que están en el cuestionario de ingresos:

# - INDFMMPI - Índice mensual de nivel de pobreza familiar y 

# - INDFMMPC - Categoría de nivel de pobreza mensual familiar

# - INDFMMPI : es una variable numérica

# - INDFMMPC : es una variable categórica donde 1 significa <= 1,30 
# (pobreza), 2 significa <= 1.85 (bajo ingreso) y 3 significa > 1,85
# (ingreso medio alto). El código para categorizar puede quedar así:

#df_obesidad <- df_obesidad %>%
#  mutate(
#    pir_cat = case_when(
#      INDFMMPC == 1 ~ "pobreza",
#      INDFMMPC == 2 ~ "bajo_ingreso",
#      INDFMMPC == 3 ~ "medio_alto",
#      TRUE ~ NA_character_
#    ),
#    pir_cat = factor(pir_cat,
#                     levels = c("pobreza", "bajo_ingreso", "medio_alto"),
#                     ordered = TRUE)
#  )

# Estas dos variables son pir (poverty income ratio) o índice de 
# pobreza

#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de ingresos                 ##
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

# Seleccionar las variables más importantes
df_obesidad <- df_obesidad %>%
  select(SEQN, INDFMMPI, INDFMMPC)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del cuestionario para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    indice_pobreza = INDFMMPI,
    indice_pobreza_cat = INDFMMPC,
  )

#########################
##
## índice de pobreza
##
#########################

unique(df_obesidad$indice_pobreza)[1:20] # produce:
#[1] 0.86 0.92 4.37 5.00 5.00 4.18 0.51 1.14 3.03 5.00 5.00 0.30   NA
#[14]   NA 5.00 4.64 1.46 2.06 0.41 2.32

summary(df_obesidad$indice_pobreza) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    0.97    1.82    2.29    3.57    5.00     627 

#########################
##
## índice de pobreza
## (categórico)
##
#########################

unique(df_obesidad$indice_pobreza_cat) # produce: [1]  1  3  2 NA  7  9

table(df_obesidad$indice_pobreza_cat) # produce:
#   1    2    3    7    9 
#2011  765 2679   40   75 

df_obesidad <- df_obesidad %>%
  mutate(
    indice_pobreza_cat = case_when(
      indice_pobreza_cat == 1 ~ "pobreza",
      indice_pobreza_cat == 2 ~ "bajo_ingreso",
      indice_pobreza_cat == 3 ~ "medio_alto",
      TRUE ~ NA_character_
    ),
    indice_pobreza_cat = factor(
      indice_pobreza_cat,
      levels = c("pobreza", "bajo_ingreso", "medio_alto"),
      ordered = TRUE
    )
  )
unique(df_obesidad$indice_pobreza_cat) # produce:
#[1] pobreza      medio_alto   bajo_ingreso <NA>        
#Levels: pobreza < bajo_ingreso < medio_alto

table(df_obesidad$indice_pobreza_cat) # produce:
#pobreza bajo_ingreso   medio_alto 
#   2011          765         2679 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# ingreso
obesidad_cuestionario_ingresos <- df_obesidad %>%
  select(SEQN, indice_pobreza, indice_pobreza_cat)

# Crear un archivo .cvs con la dataset de cuestionario de ingresos
write.csv(obesidad_cuestionario_ingresos,
          "eda-obesidad/data/obesidad/14_obesidad_cuestionario_ingresos.csv",
          row.names = FALSE)
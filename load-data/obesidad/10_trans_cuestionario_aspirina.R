#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de dieta - lácteos          ##
##                             ##
#################################

# Nota: Podría ser bueno sacar el total de quienes usan aspirina
# pero más importante podría ser dividir a los que toman aspirina
# antiinflamatoria como 100 o menos mg y los que usan como analgésico
# más de 100 mg

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
    toma_aspirina_prescrita = RXQ515,
    toma_aspirina_auto = RXQ520,
    dosis_aspirina = RXD530
  )

############################
##
## Aspirina prescrita
##
############################

unique(df_obesidad$toma_aspirina_prescrita) # produce:
#[1]  1  3 NA  2  4

table(df_obesidad$toma_aspirina_prescrita) # produce:
#  1   2   3   4 
#901 217  57  46 

# Pasar los valores numéricos a cadenas y la variable a factor
df_obesidad <- df_obesidad %>%
  mutate(
    toma_aspirina_prescrita = case_when(
      toma_aspirina_prescrita == 1 ~ "si",
      toma_aspirina_prescrita == 2 ~ "no",
      toma_aspirina_prescrita == 3 ~ "no",
      toma_aspirina_prescrita == 4 ~ "no",
      TRUE ~ NA_character_
    ),
    toma_aspirina_prescrita = as.factor(toma_aspirina_prescrita)
  )
unique(df_obesidad$toma_aspirina_prescrita) # produce:
#[1] si   no   <NA>
#Levels: no si

table(df_obesidad$toma_aspirina_prescrita) # produce:
# no  si 
#320 901 

############################
##
## Aspirina automedicada
##
############################

unique(df_obesidad$toma_aspirina_auto) # produce:
#[1] NA  2  1  9  7
table(df_obesidad$toma_aspirina_auto) # produce:
#  1    2    7    9 
#130 2289    3    2

# Pasar los valores numéricos a cadenas y la variable a factor
df_obesidad <- df_obesidad %>%
  mutate(
    toma_aspirina_auto = case_when(
      toma_aspirina_auto == 1 ~ "si",
      toma_aspirina_auto == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    toma_aspirina_auto = as.factor(toma_aspirina_auto)
  )
unique(df_obesidad$toma_aspirina_auto) # produce:
#[1] si   no   <NA>
#Levels: no si

table(df_obesidad$toma_aspirina_auto) # produce:
#no   si 
#2289  130 

summary(df_obesidad$toma_aspirina_auto) # produce:
#  no   si NA's 
#2289  130 3363 

############################
##
## Dosis de Aspirina
##
############################

unique(df_obesidad$dosis_aspirina) # produce:
#[1]    81    NA   325    20 99999   400   500   150    90    75   200
#[12]   250   163

summary(df_obesidad$dosis_aspirina) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  20      81      81    1221      81   99999    4699 

# Cambiar los valores raros 99999 y 77777 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    dosis_aspirina = 
      ifelse(dosis_aspirina %in% c(99999, 77777),
             NA, dosis_aspirina)
  )
unique(df_obesidad$dosis_aspirina) # produce:
[1]  81  NA 325  20 400 500 150  90  75 200 250 163

summary(df_obesidad$dosis_aspirina) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#20.0    81.0    81.0   114.5    81.0   500.0    4711 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# actividad física
obesidad_cuestionario_aspirina <- df_obesidad %>%
  select(SEQN, toma_aspirina_prescrita, toma_aspirina_auto,
         dosis_aspirina)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_cuestionario_aspirina,
          "eda-obesidad/data/obesidad/10_obesidad_cuestionario_aspirina.csv",
          row.names = FALSE)

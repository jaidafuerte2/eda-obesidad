#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de diabetes                 ##
##                             ##
#################################

# Nota: no olvidar de filtrar por uso de insulina y hayque pasar a meses
# el tiempo que usa insulina

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
    tiene_diabetes = DIQ010,
    tiene_prediabetes = DIQ160,
    usa_insulina = DIQ050,
    tiempo_usa_insulina = DID060,
    medida_tiempo_uso_insulina = DIQ060U,
    toma_hipoglicemiantes = DIQ070
  )

##########################
##
## Médico dijo que tienes
## diabetes ?
##
##########################

summary(df_obesidad$tiene_diabetes) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   2.000   1.913   2.000   9.000 

unique(df_obesidad$tiene_diabetes) # produce: [1] 1 2 3 9

# Pasar las variables numéricas  a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    tiene_diabetes = case_when(
      tiene_diabetes == 1 ~ "si",
      tiene_diabetes == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    tiene_diabetes = as.factor(tiene_diabetes) # tranformar a factor
  )
class(df_obesidad$tiene_diabetes) # produce: factor

unique(df_obesidad$tiene_diabetes) # produce:
#[1] si   no   <NA>
#Levels: no si

summary(df_obesidad$tiene_diabetes) # produce:
#  no   si NA's 
#4916  694  172 


##########################
##
## Médico dijo que tienes
## prediabetes ?
##
##########################

summary(df_obesidad$tiene_prediabetes) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.954   2.000   9.000     863

unique(df_obesidad$tiene_prediabetes) # produce: [1] NA  2  1  9

# Pasar las variables numéricas  a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    tiene_prediabetes = case_when(
      tiene_prediabetes == 1 ~ "si",
      tiene_prediabetes == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    tiene_prediabetes = as.factor(tiene_prediabetes) # tranformar a 
    # factor
  )
class(df_obesidad$tiene_prediabetes) # produce: factor

unique(df_obesidad$tiene_prediabetes) # produce:
#[1] si   no   <NA>
#Levels: no si

summary(df_obesidad$tiene_prediabetes) # produce:
#  no   si NA's 
#4655  259  868 

##########################
##
## Está usando insulina
## ahora ?
##
##########################

summary(df_obesidad$usa_insulina) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.967   2.000   9.000       1 

unique(df_obesidad$usa_insulina) # produce: [1]  1  2 NA  9

# Pasar las variables numéricas  a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    usa_insulina = case_when(
      usa_insulina == 1 ~ "si",
      usa_insulina == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    usa_insulina = as.factor(usa_insulina) # tranformar a 
    # factor
  )
class(df_obesidad$usa_insulina) # produce: factor

unique(df_obesidad$usa_insulina) # produce:
#[1] si   no   <NA>
#Levels: no si

summary(df_obesidad$usa_insulina) # produce:
#  no   si NA's 
#5583  197    2 

##########################
##
## Tiempo usando insulina?
##
##########################

summary(df_obesidad$tiempo_usa_insulina) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    3.00    8.00   21.37   15.00  999.00    5585 

unique(df_obesidad$tiempo_usa_insulina) # produce:
#[1]   5   1  16  NA  17   6  28   8   3  29 666   9  10  14  15  18   2
#[18]   4  31  12  30  20   7  13  11  23  46  21  43 999  19  26  40  25
#
# Donde: 1-46 : puede ser años o meses, 666 = menos de un mes, 777 =
# rechazado, 999 es no lo sé

# Tranformar los valores 777 y 999 a NA y 666 a cero
df_obesidad <- df_obesidad %>%
  mutate(tiempo_usa_insulina = case_when(
    tiempo_usa_insulina == 666 ~ 0,
    tiempo_usa_insulina %in% c(777, 999) ~ NA_real_,
    TRUE ~ tiempo_usa_insulina
  ))
class(df_obesidad$tiempo_usa_insulina) # produce: numeric

unique(df_obesidad$tiempo_usa_insulina) # produce:
#[1]  5  1 16 NA 17  6 28  8  3 29  0  9 10 14 15 18  2  4 31 12 30 20  7
#[24] 13 11 23 46 21 43 19 26 40 25

summary(df_obesidad$tiempo_usa_insulina) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   3.000   7.000   9.582  14.250  46.000    5586 

#################################
##
## Meses o años usando insulina
##
#################################

class(df_obesidad$medida_tiempo_uso_insulina) # produce: numeric
 
summary(df_obesidad$medida_tiempo_uso_insulina) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.923   2.000   2.000    5588

#str(df_obesidad$medida_tiempo_uso_insulina) # produce:
#  num [1:5782] 2 2 2 NA NA NA NA NA NA NA ...

unique(df_obesidad$medida_tiempo_uso_insulina)[1:20] # produce:
#[1]  2 NA  1 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

table(df_obesidad$medida_tiempo_uso_insulina, useNA = "ifany")
# produce:
# 1    2 <NA> 
#15  179 5588 

# Transformar la variable de medida de tiempo a factor 
df_obesidad <- df_obesidad %>%
  mutate(
    medida_tiempo_uso_insulina = factor(
      medida_tiempo_uso_insulina,
      levels = c(1, 2),
      labels = c("meses", "anios"))
  )
class(df_obesidad$medida_tiempo_uso_insulina) # produce: factor

unique(df_obesidad$medida_tiempo_uso_insulina) # produce:
#[1] anios <NA>  meses
#Levels: meses anios

summary(df_obesidad$medida_tiempo_uso_insulina) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   3.000   7.000   9.582  14.250  46.000    5586 

##########################
##
## Está usando pastillas
## hipoglicemiantes ?
##
##########################

summary(df_obesidad$toma_hipoglicemiantes) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.542   2.000   9.000    4660 

unique(df_obesidad$toma_hipoglicemiantes) # produce: [1]  1  2 NA  9

# Pasar las variables numéricas  a factores no ordenados
df_obesidad <- df_obesidad |>
  mutate(
    toma_hipoglicemiantes = case_when(
      toma_hipoglicemiantes == 1 ~ "si",
      toma_hipoglicemiantes == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    toma_hipoglicemiantes = as.factor(toma_hipoglicemiantes) 
    # transformar a factor
  )
class(df_obesidad$toma_hipoglicemiantes) # produce: factor

unique(df_obesidad$toma_hipoglicemiantes) # produce:
#[1] si   no   <NA>
#Levels: no si

summary(df_obesidad$toma_hipoglicemiantes) # produce:
# no   si NA's 
#592  528 4662 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# presión y colesterol
obesidad_cuestionario_diabetes <- df_obesidad %>%
  select(SEQN, tiene_diabetes, tiene_prediabetes, usa_insulina,
         tiempo_usa_insulina, medida_tiempo_uso_insulina, 
         toma_hipoglicemiantes)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_cuestionario_diabetes,
          "eda-obesidad/data/obesidad/7_obesidad_cuestionario_diabetes.csv",
          row.names = FALSE)

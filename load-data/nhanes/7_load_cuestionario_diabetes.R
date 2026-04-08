###############################
##
## Questionnaire - diabetes 
##
###############################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

# Seleccionar sólo las variables que voy a usar
questionnaire <- questionnaire |>
  select(SEQN, DIQ010, DIQ160, DIQ050, DID060, DIQ060U, DIQ070) 

############ El médico le dijo que tiene diabetes #############

# DIQ010 - El médico le dijo que tiene diabetes

summary(questionnaire$DIQ010) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.000   2.000   1.948   2.000   9.000     406 

unique(questionnaire$DIQ010) # produce: [1]  1  2 NA  3  9  7
# (Nota: 3, 9 y 7 que son borderline, refused y don't know quedan 
# como datos faltantes). 1 es sí, 2 es no.

##### ¿Alguna vez te han dicho que tienes prediabetes? ####

# DIQ160 - ¿Alguna vez te han dicho que tienes prediabetes?

summary(questionnaire$DIQ160) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.965   2.000   9.000    3888

unique(questionnaire$DIQ160) # produce: [1] NA  2  1  9
# Donde 1 es sí, 2 es no , 9 es no sé

################ Usa insulina #######################

# DIQ050 - Usando insulina ahora

summary(questionnaire$DIQ050) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.979   2.000   9.000     407 

unique(questionnaire$DIQ050) # produce: [1]  1  2 NA  7  9
# Donde: 1 es sí, 2 es no, 7 es rechazado y 9 es no lo sé

############## Tiempo usando insulina #################

# DID060 - ¿Cuánto tiempo lleva tomando insulina?

# Nota: Para saber si son meses o años hay que revisar la variable:
# DIQ060U - Unidad de medida (mes/año) que no está disponible en 
# la dataset de kaggle

summary(questionnaire$DID060) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    3.00    8.00   20.59   15.00  999.00    9955 

unique(questionnaire$DID060) # produce:
#[1]   5   1  16  NA  17   6  28   8   3  29 666   9  10  25  14  20  15
#[18]  18   2   4  31  12  30   7  13  11  23  32  46  39  21  43 999  26
#[35]  19  40
# Donde: 1-46 : puede ser años o meses, 666 = menos de un mes, 777 =
# rechazado, 999 es no lo sé

############## Meses o años usando insulina #################

# DIQ060U - Unidad de medida (mes/año)

summary(questionnaire$DIQ060U) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.908   2.000   2.000    9958 

unique(questionnaire$DIQ060U) # produce: [1]  2 NA  1
# Donde 1 es meses y 2 es años

############### Tomar pastillas hipoglicemiantes ###############

# DIQ070 - Tomar pastillas para diabéticos para bajar el nivel de 
# azúcar en la sangre

summary(questionnaire$DIQ070) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.556   2.000   9.000    8975 

unique(questionnaire$DIQ070) # produce: [1]  1  2 NA  9

############### Seleccionar #################

names(questionnaire)

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_diabetes <- questionnaire |>
  select(SEQN, DIQ010, DIQ160, DIQ050, DID060, DIQ060U, DIQ070) 

head(cuestionario_diabetes) # produce:

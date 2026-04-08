###################################
##
## Questionnaire - dieta - lácteos 
##
###################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

# Seleccionar sólo las variables que voy a usar
questionnaire <- questionnaire |>
  select(SEQN, DBQ197, DBQ223A, DBQ223B, DBQ223C, DBQ223D, DBQ223E,
         DBQ223U, DBQ229)

############## Consumo de productos lácteos #################

# DBQ197 - Consumo de productos lácteos en los últimos 30 días

summary(questionnaire$DBQ197) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   1.000   3.000   2.111   3.000   7.000     406 

unique(questionnaire$DBQ197) # produce: [1]  1  3  0 NA  2  4  7
# Donde: 0 es nunca, 1 es ocasional, 2 es medio, 3 frecuente, 4 es
# variado y 7 rechazado

############ Consumo de leche entera ################

# DBQ223A - Usted bebe leche entera o regular

summary(questionnaire$DBQ223A) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   10.00   10.00   10.41   10.00   99.00    7379 

unique(questionnaire$DBQ223A) # produce: [1] 10 NA 99 77
# Donde: 10 es entera, 77 es rechazado y 99 es no lo sé

########### Usted bebe leche con un 2% de grasa ############

# DBQ223B - Usted bebe leche con un 2% de grasa

summary(questionnaire$DBQ223B) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11      11      11      11      11      11    6384

unique(questionnaire$DBQ223B) # produce: [1] NA 11

############# Usted bebe leche con 1% de grasa ##################

# DBQ223C - Usted bebe leche con 1% de grasa

summary(questionnaire$DBQ223C) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  12      12      12      12      12      12    9301 

unique(questionnaire$DBQ223C) # produce: [1] NA 12

########### Consumo de leche desnatada #############

# DBQ223D - Usted bebe leche desnatada/sin grasa

summary(questionnaire$DBQ223D) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13      13      13      13      13      13    9429 

unique(questionnaire$DBQ223D) # produce: [1] NA 13
# Donde: 13 es sí.

################ Usted bebe leche de soja ###################

# DBQ223E - Usted bebe leche de soja

summary(questionnaire$DBQ223E) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  14      14      14      14      14      14    9940

unique(questionnaire$DBQ223E) # produce: [1] NA 14

############## Usted bebe otro tipo de leche #################

# DBQ223U - Usted bebe otro tipo de leche

summary(questionnaire$DBQ223U) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  30      30      30      30      30      30    9648 

unique(questionnaire$DBQ223U) # produce: [1] NA 30

################## Bebedor regular de leche ####################

# DBQ229 - Uso regular de leche 5 veces por semana

# Nota: se refiere a que si ha sido bebedor regular de leche al
# menos 5 veces por semana a lo largo de la vida

summary(questionnaire$DBQ229) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.959   3.000   9.000    4406

unique(questionnaire$DBQ229) # produce: [1]  2  1 NA  3  9
# Donde 1 es sí,  2 es no, 3 es variable, y 9 es no sé 

############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_dieta_lacteos <- questionnaire |>
  select(SEQN, DBQ197, DBQ223A, DBQ223B, DBQ223C, DBQ223D, DBQ223E,
         DBQ223U, DBQ229) 

head(cuestionario_dieta_lacteos) # produce:

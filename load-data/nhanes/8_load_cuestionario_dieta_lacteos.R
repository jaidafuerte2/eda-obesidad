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

########### Consumo de leche desnatada #############

# DBQ223D - Usted bebe leche desnatada/sin grasa

summary(questionnaire$DBQ223D) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13      13      13      13      13      13    9429 

unique(questionnaire$DBQ223D) # produce: [1] NA 13
# Donde: 13 es sí.

############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_dieta_lacteos <- questionnaire |>
  select(SEQN, DBQ197, DBQ223A, DBQ223D) 

head(cuestionario_dieta_lacteos) # produce:

##########################################
##
## Questionnaire - Ingresos
##
##########################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

# Selccionar sólo las variables que voy a usar
questionnaire <- questionnaire |>
  select(SEQN, INDFMMPI, INDFMMPC)

################# índice de Pobreza familiar ##################

# INDFMMPI - Índice mensual de nivel de pobreza familiar

unique(questionnaire$INDFMMPI)[1:20] # produce:
#[1] 0.86 0.92 4.37 2.52 5.00 4.18 0.51 1.14 3.03   NA 0.91 0.55 1.72
#[14] 0.76 0.30 1.54 4.30 2.46 4.64 1.27

summary(questionnaire$INDFMMPI) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   0.830   1.540   2.096   3.090   5.000    1066 

################# Categoría de Pobreza familiar ##################

unique(questionnaire$INDFMMPC) # produce:
# [1]  1  3 NA  2  9  7

table(questionnaire$INDFMMPC) # produce:
#   1    2    3    7    9 
#4107 1329 4189   63  118 

# Dónde 1 es menor o igual a 1,30 (pobreza), 2 es menor o igual 1,85
# (bajo ingreso), 3 es mayor a 1,85, (medio_alto), 7 es rechazado,
# 9 es no lo sé

############### Seleccionar #################

# Seleccionar las variables del cuestionario más 
# importantes
cuestionario_ingresos <- questionnaire |>
  select(SEQN, INDFMMPI, INDFMMPC) 

###################################
##
## Questionnaire - Dormir
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
  select(SEQN, SLD010H, SLQ060) 

############ ¿Cuántas horas duermes? ################

# SLD010H - ¿Cuántas horas duermes?

summary(questionnaire$SLD010H) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.00    6.00    7.00    7.05    8.00   99.00    3714

unique(questionnaire$SLD010H) # produce:
#[1]  7  9  8 NA  5  6 10  4  3 12 11  2 99
# Donde: 12 es 12 o más horas, y 99 es no lo sé

########### Trastorno del sueño por médico ###############

# SLQ060 - ¿Alguna vez le ha dicho su médico que tiene un trastorno del sueño?

summary(questionnaire$SLQ060) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.925   2.000   9.000    3711 

unique(questionnaire$SLQ060) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no, 9 es no lo sé

############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_dormir <- questionnaire |>
  select(SEQN, SLD010H, SLQ060) 

head(cuestionario_dormir) # produce:

###################################
##
## Questionnaire - Aspirina
##
###################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

######## Toma aspirina en dosis baja por prescripción #########

# RXQ515 - ¿Siguió las indicaciones y tomó aspirina en dosis bajas?

summary(questionnaire$RXQ515) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.386   2.000   4.000    8891 

unique(questionnaire$RXQ515) # produce: [1]  1  3 NA  2  4
# Donde 1 es sí, 2 es no , 3 a veces y 4 es dejé de usar por los 
# efectos secundarios

######### Toma aspirina en dosis bajas por su cuenta ############

# RXQ520 - ¿Toma aspirina en dosis bajas por su cuenta?

summary(questionnaire$RXQ520) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.962   2.000   9.000    7644 

unique(questionnaire$RXQ520) # produce: [1] NA  2  1  9  7

########## Dosis de aspirina en miligramos ###############

# RXD530 - Dosis de aspirina en miligramos (mg)

summary(questionnaire$RXD530) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  20      81      81    1172      81   99999    9042 

unique(questionnaire$RXD530) # produce:
#[1]    81    NA   325    20 99999   400   500   150   120    90    75
#[12]   200   250   163
# Donde 99999 es no lo sé

############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_aspirina <- questionnaire |>
  select(SEQN, RXQ515, RXQ520) 

head(cuestionario_aspirina) # produce:

#############################################
##
## Questionnaire - presión - colesterol
##
############################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

####### Le dijeron que tenía presión arterial alta #######

# BPQ030 - Le dijeron que tenía presión arterial alta - 2+ veces

summary(questionnaire$BPQ030) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.226   1.000   9.000    8001

unique(questionnaire$BPQ030) # produce: [1]  1 NA  2  9
# Donde: 1 es sí, 2 es no y 9 no lo sé

######### Medicamentos para la presión #############

# BPQ050A - Actualmente estoy tomando medicamentos para presión

summary(questionnaire$BPQ050A) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    1.00    1.00    1.13    1.00    9.00    8360 

unique(questionnaire$BPQ050A) # produce: [1]  2 NA  1  9
# Donde: 1 es sí, 2 es no, 9 es no lo sé 

########### Colesterol alto ############

# BPQ080 - El médico le dijo que tenía el colesterol alto.

summary(questionnaire$BPQ080) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.724   2.000   9.000    3711 

unique(questionnaire$BPQ080) # produce: [1]  1 NA  2  9
# Donde: 1 es sí,  2 es no, 9 es no lo sé

########### Medicamentos colesterol ################

# BPQ100D - Ahora estoy tomando medicamentos para colesterol

summary(questionnaire$BPQ100D) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.236   1.000   9.000    8725 

unique(questionnaire$BPQ100D) # produce: [1]  1 NA  2  9
# Donde 1 es sí, 2 es no y 9 es no lo sé

############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_presion_colesterol <- questionnaire |>
  select(SEQN, BPQ030, BPQ050A, BPQ080, BPQ100D) 

head(cuestionario_presion_colesterol) # produce:


############################
##
## Unir todas las datasets 
## de NHANES en una sola
##
############################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar todos los archivos que crean las datasets 
source("eda-obesidad/load-data/nhanes/1_load_demografico.R")
source("eda-obesidad/load-data/nhanes/2_load_examina.R")
source("eda-obesidad/load-data/nhanes/3_load_dieta_tipo.R")
source("eda-obesidad/load-data/nhanes/4_load_dieta_nutrientes.R")
source("eda-obesidad/load-data/nhanes/5_load_laboratorio.R")
source("eda-obesidad/load-data/nhanes/6_load_cuestionario_presion_colesterol.R")
source("eda-obesidad/load-data/nhanes/7_load_cuestionario_diabetes.R")
source("eda-obesidad/load-data/nhanes/8_load_cuestionario_dieta_lacteos.R")
source("eda-obesidad/load-data/nhanes/9_load_cuestionario_ejercicio.R")
source("eda-obesidad/load-data/nhanes/10_load_cuestionario_aspirina.R")
source("eda-obesidad/load-data/nhanes/11_load_cuestionario_dormir.R")
source("eda-obesidad/load-data/nhanes/12_load_cuestionario_peso.R")
source("eda-obesidad/load-data/nhanes/13_load_cuestionario_enfermedad.R")
source("eda-obesidad/load-data/nhanes/14_load_cuestionario_ingresos.R")


# Unir todas las datasets en una sola
obesidad_nhanes <- demografico %>%
  left_join(examina, by = "SEQN") %>%
  left_join(dieta_tipo, by = "SEQN") %>%
  left_join(dieta_nutrientes, by = "SEQN") %>%
  left_join(laboratorio, by = "SEQN") %>%
  left_join(cuestionario_presion_colesterol, by = "SEQN") %>%
  left_join(cuestionario_diabetes, by = "SEQN") %>%
  left_join(cuestionario_dieta_lacteos, by = "SEQN") %>%
  left_join(cuestionario_ejercicio, by = "SEQN") %>%
  left_join(cuestionario_aspirina, by = "SEQN") %>%
  left_join(cuestionario_dormir, by = "SEQN") %>%
  left_join(cuestionario_peso, by = "SEQN") %>%
  left_join(cuestionario_enfermedad, by = "SEQN") %>%
  left_join(cuestionario_ingresos, by = "SEQN")
dim(obesidad_nhanes) # produce: [1] 10175   117

########### Crear dataset sin quitar los que no ayunaron #############

# Filtrar a los mayores o iguales a 18 años, las que no están 
# embarazadas y los que no tienen índice de masa corporal
obesidad_nhanes_clean <- obesidad_nhanes %>%
  filter(
    RIDAGEYR >= 18,
    RIDEXPRG != 1 | is.na(RIDEXPRG),
    !is.na(BMXBMI)
  )
dim(obesidad_nhanes) # produce:
# [1] 10175   117
dim(obesidad_nhanes_clean)
#[1] 5782  117


# Crear un archivo .cvs con la dataset incluso con la gente que NO
# ayunó
write.csv(obesidad_nhanes_clean, 
          "eda-obesidad/data/nhanes/obesidad_nhanes.csv", 
          row.names = FALSE)

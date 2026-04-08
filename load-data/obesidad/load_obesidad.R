############################
##
## Unir todas las datasets 
## de obesidad en una sola
##
############################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar todos los archivos que crean las datasets 
source("eda-obesidad/load-data/obesidad/1_trans_demografico.R")
source("eda-obesidad/load-data/obesidad/2_trans_examina.R")
source("eda-obesidad/load-data/obesidad/3_trans_dieta_tipo.R")
source("eda-obesidad/load-data/obesidad/4_trans_dieta_nutrientes.R")
source("eda-obesidad/load-data/obesidad/5_trans_laboratorio.R")
source("eda-obesidad/load-data/obesidad/6_trans_cuestionario_presion_colesterol.R")
source("eda-obesidad/load-data/obesidad/7_trans_cuestionario_diabetes.R")
source("eda-obesidad/load-data/obesidad/8_trans_cuestionario_dieta_lacteos.R")
source("eda-obesidad/load-data/obesidad/9_trans_cuestionario_ejercicio.R")
source("eda-obesidad/load-data/obesidad/10_trans_cuestionario_aspirina.R")
source("eda-obesidad/load-data/obesidad/11_trans_cuestionario_dormir.R")
source("eda-obesidad/load-data/obesidad/12_trans_cuestionario_peso.R")
source("eda-obesidad/load-data/obesidad/13_trans_cuestionario_enfermedad.R")


# Unir todas las datasets en una sola
obesidad <- obesidad_demografico %>%
  left_join(obesidad_examina, by = "SEQN") %>%
  left_join(obesidad_dieta_tipo, by = "SEQN") %>%
  left_join(obesidad_dieta_nutrientes, by = "SEQN") %>%
  left_join(obesidad_laboratorio, by = "SEQN") %>%
  left_join(obesidad_cuestionario_presion_colesterol, by = "SEQN") %>%
  left_join(obesidad_cuestionario_diabetes, by = "SEQN") %>%
  left_join(obesidad_cuestionario_dieta_lacteos, by = "SEQN") %>%
  left_join(obesidad_cuestionario_ejercicio, by = "SEQN") %>%
  left_join(obesidad_cuestionario_aspirina, by = "SEQN") %>%
  left_join(obesidad_cuestionario_dormir, by = "SEQN") %>%
  left_join(obesidad_cuestionario_peso, by = "SEQN") %>%
  left_join(obesidad_cuestionario_enfermedad, by = "SEQN")
dim(obesidad) # produce: [1] 5782  124

# Crear un archivo .cvs con la dataset incluso con la gente que NO
# ayunó
write.csv(obesidad_nhanes_clean, 
          "eda-obesidad/data/obesidad/obesidad.csv", 
          row.names = FALSE)
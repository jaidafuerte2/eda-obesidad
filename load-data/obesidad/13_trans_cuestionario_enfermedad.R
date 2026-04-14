#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de enfermedad               ##
##                             ##
#################################

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

# Seleccionar las variables más importantes
df_obesidad <- df_obesidad %>%
  select(SEQN, MCQ080, MCQ082, MCQ086, MCQ160A, MCQ195, MCQ180A,
         MCQ160B, MCQ180B, MCQ160C, MCQ180C, MCQ160E, MCQ180E,
         MCQ160F, MCQ180F, MCQ160M, MCQ170M, MCQ180M)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del cuestionario para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    dijeron_sobrepeso = MCQ080,
    dijeron_celiaquia = MCQ082,
    sigue_dieta_sin_gluten = MCQ086,
    dijeron_artritis = MCQ160A,
    tipo_artritis = MCQ195,
    edad_diagnostico_artritis = MCQ180A,
    dijeron_icc = MCQ160B,
    edad_diagnostico_icc = MCQ180B,
    dijeron_enf_coronaria = MCQ160C,
    edad_diagnostico_enf_coronaria = MCQ180C,
    dijeron_infarto_cardiaco = MCQ160E,
    edad_infarto_cardiaco = MCQ180E,
    dijeron_derrame_cerebral = MCQ160F,
    edad_derrame_cerebral = MCQ180F,
    dijeron_enf_tiroides = MCQ160M,
    tienes_aun_enf_tiroides = MCQ170M,
    edad_enf_tiroides = MCQ180M
  )

############################
##
## Alguna vez te dijeron 
## que tenías sobrepeso ?
##
############################

unique(df_obesidad$dijeron_sobrepeso) # produce: [1] 1 2 9

table(df_obesidad$dijeron_sobrepeso) # produce:
#   1    2    9 
#1997 3784    1 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_sobrepeso = case_when(
      dijeron_sobrepeso == 1 ~ "si",
      dijeron_sobrepeso == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_sobrepeso = as.factor(dijeron_sobrepeso)
  )
unique(df_obesidad$dijeron_sobrepeso) # produce: [1] 1 2 9
#[1] si   no   <NA>
#Levels: no si

table(df_obesidad$dijeron_sobrepeso) # produce:
#  no   si 
#3784 1997

############################
##
## Alguna vez te dijeron
## que tenías celiaquía
##
############################

unique(df_obesidad$dijeron_celiaquia) # produce:  [1] 2 1 9

table(df_obesidad$dijeron_celiaquia) # produce:
# 1    2    9 
#27 5751    4 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_celiaquia = case_when(
      dijeron_celiaquia == 1 ~ "si",
      dijeron_celiaquia == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_celiaquia = as.factor(dijeron_celiaquia)
  )

unique(df_obesidad$dijeron_celiaquia) # produce: 
#[1] no   si   <NA>
#Levels: no si
table(df_obesidad$dijeron_celiaquia) # produce:
#  no   si 
#5751   27 

############################
##
## Sigues dieta sin
## gluten ?
##
############################

unique(df_obesidad$sigue_dieta_sin_gluten) # produce: [1] 2 1 9

table(df_obesidad$sigue_dieta_sin_gluten) # produce:
#  1    2    9 
#104 5677    1

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    sigue_dieta_sin_gluten = case_when(
      sigue_dieta_sin_gluten == 1 ~ "si",
      sigue_dieta_sin_gluten == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    sigue_dieta_sin_gluten = as.factor(sigue_dieta_sin_gluten)
  )

unique(df_obesidad$sigue_dieta_sin_gluten) # produce:
#[1] no   si   <NA>
#Levels: no si
table(df_obesidad$sigue_dieta_sin_gluten) # produce:
#  no   si 
#5677  104 

############################
##
## Alguna vez te dijeron
## que tenías artritis
##
############################

unique(df_obesidad$dijeron_artritis) # produce: [1]  1  2 NA  9

table(df_obesidad$dijeron_artritis) # produce: 
#   1    2    9 
#1439 4003   13 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_artritis = case_when(
      dijeron_artritis == 1 ~ "si",
      dijeron_artritis == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_artritis = as.factor(dijeron_artritis)
  )

unique(df_obesidad$dijeron_artritis) # produce:
#[1] si   no   <NA>
#Levels: no si
table(df_obesidad$dijeron_artritis) # produce: 
#  no   si 
#4003 1439 

############################
##
## Qué tipo de artritis 
## era ?
##
############################

unique(df_obesidad$tipo_artritis) # produce: 
#[1]  9 NA  2  4  1  3  7

table(df_obesidad$tipo_artritis) # produce:
#  1   2   3   4   7   9 
#667 244  19 132   1 376 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    tipo_artritis = case_when(
      tipo_artritis == 1 ~ "degenerativa",
      tipo_artritis == 2 ~ "reumatoide",
      TRUE ~ NA_character_
    ),
    dijeron_artritis = as.factor(dijeron_artritis)
  )
unique(df_obesidad$tipo_artritis) # produce: 
#[1] NA             "reumatoide"   "degenerativa"

table(df_obesidad$tipo_artritis) # produce:
#degenerativa   reumatoide 
#         667          244 

############################
##
## Edad a la que te dijeron
## que tenías artritis
##
############################

unique(df_obesidad$edad_diagnostico_artritis) # produce:
#[1]    62    NA    70    45    50    42    40    19    59    79    39
#[12]    52    60    38    25    68    22    80    57    48    64    61
summary(df_obesidad$edad_diagnostico_artritis) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  2      39      50    1994      61   99999    4343 

# Pasar los valores raros 99999 y 77777 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    edad_diagnostico_artritis = 
      ifelse(edad_diagnostico_artritis %in% c(77777, 99999),
             NA, edad_diagnostico_artritis)
  )
unique(df_obesidad$edad_diagnostico_artritis) # produce:
#[1] 62 NA 70 45 50 42 40 19 59 79 39 52 60 38 25 68 22 80 57 48 64 61 55
#[24] 34 49 78 54 27 63 51 21 65 46 36 47 74 58 35 66 20 75 24 10 30 28 23
summary(df_obesidad$edad_diagnostico_artritis) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.00   38.00   50.00   48.99   60.00   80.00    4371 

##################################
##
## Alguna vez te dijeron que
## tienes insuficiencia cardíaca
##
##################################

unique(df_obesidad$dijeron_icc) # produce: [1]  2 NA  1  9

table(df_obesidad$dijeron_icc) # produce:
#  1    2    9 
#166 5282    7 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_icc = case_when(
      dijeron_icc == 1 ~ "si",
      dijeron_icc == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_icc = as.factor(dijeron_icc)
  )

unique(df_obesidad$dijeron_icc) # produce:
#[1] no   <NA> si  
#Levels: no si
table(df_obesidad$dijeron_icc) # produce:
#  no   si 
#5282  166 

##################################
##
## Edad a la que te dijeron que
## tienes insuficiencia cardíaca
##
##################################

unique(df_obesidad$edad_diagnostico_icc) # produce:
#[1]    NA    78    57    37    54    55    45    71    30    50    65
#[12]    49    40    64    59    43    56    79    67    70    31    80
#[23]    75    35    58     0    32    46    62    77 99999    48    68
summary(df_obesidad$edad_diagnostico_icc) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    46.0    58.0   658.2    68.0 99999.0    5616 

# Cambiar los valores raros 77777 y 999999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    edad_diagnostico_icc = 
      ifelse(edad_diagnostico_icc %in% c(77777, 99999),
             NA, edad_diagnostico_icc)
  )

unique(df_obesidad$edad_diagnostico_icc) # produce:
#[1] NA 78 57 37 54 55 45 71 30 50 65 49 40 64 59 43 56 79 67 70 31 80 75
#[24] 35 58  0 32 46 62 77 48 68 41 52 69 44 63 39 15 73 38 53 74 60 66 10
#[47] 26 47 29 12 51 72  1 76
summary(df_obesidad$edad_diagnostico_icc) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   46.00   58.00   56.09   68.00   80.00    5617 

##################################
##
## Alguna vez te dijeron que
## tienes insuficiencia cardíaca
##
##################################

unique(df_obesidad$dijeron_enf_coronaria) # produce: [1]  2  1 NA  9

table(df_obesidad$dijeron_enf_coronaria) # produce:
#  1    2    9 
#215 5225   15 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_enf_coronaria = case_when(
      dijeron_enf_coronaria == 1 ~ "si",
      dijeron_enf_coronaria == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_enf_coronaria = as.factor(dijeron_enf_coronaria)
  )
unique(df_obesidad$dijeron_enf_coronaria) # produce:
#[1] no   si   <NA>
#Levels: no si
table(df_obesidad$dijeron_enf_coronaria) # produce:
#no   si 
#5225  215 

##################################
##
## Edad a la que te dijeron que
## tienes enfermedad coronaria
##
##################################

unique(df_obesidad$edad_diagnostico_enf_coronaria) # produce:
#[1]    NA    54    53    51    57    45    80    49    65    46    40
#[12]    59    48    73    64    52    50    47    70    56    55    43
#[23]    35    60     0    12    61    66    63 99999    62    69    74
summary(df_obesidad$edad_diagnostico_enf_coronaria) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    50.0    59.0   522.1    66.0 99999.0    5567 

# Transformar lo valores raros 77777 y 99999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    edad_diagnostico_enf_coronaria = 
      ifelse(edad_diagnostico_enf_coronaria %in% c(77777, 99999),
             NA, edad_diagnostico_enf_coronaria)
  )

unique(df_obesidad$edad_diagnostico_enf_coronaria) # produce:
#[1] NA 54 53 51 57 45 80 49 65 46 40 59 48 73 64 52 50 47 70 56 55 43 35
#[24] 60  0 12 61 66 63 62 69 74 58 25 75 16 68 78 39 33 71 32 41 77 42 37
#[47] 30 79 44 27 38 72 36 11 67 76
summary(df_obesidad$edad_diagnostico_enf_coronaria) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    50.0    58.5    57.3    66.0    80.0    5568 

##################################
##
## Alguna vez te dijeron que
## tuviste un infarto cardíaco
##
##################################

unique(df_obesidad$dijeron_infarto_cardiaco) # produce:
#[1]  2  1 NA  9
table(df_obesidad$dijeron_infarto_cardiaco) # produce:
#  1    2    9 
#214 5238    3 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_infarto_cardiaco = case_when(
      dijeron_infarto_cardiaco == 1 ~ "si",
      dijeron_infarto_cardiaco == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_infarto_cardiaco = as.factor(dijeron_infarto_cardiaco)
  )
unique(df_obesidad$dijeron_infarto_cardiaco) # produce:
#[1] no   si   <NA>
#Levels: no si
table(df_obesidad$dijeron_infarto_cardiaco) # produce:
#  no   si 
#5238  214 

##################################
##
## Edad a la que te dijeron que
## tuviste un infarto cardíaco
##
##################################

unique(df_obesidad$edad_infarto_cardiaco) # produce:
#[1]    NA    55    72    53    51    57    32    45    47    48    40
#[12]    64    70    23    80    49    65    52    56    46    31    43
#[23]    35    62    60    30    63    66    69    42    16    50    58
summary(df_obesidad$edad_infarto_cardiaco) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#16.0    45.0    57.0   524.1    65.0 99999.0    5569 

# Transformar lo valores raros 77777 y 99999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    edad_infarto_cardiaco = 
      ifelse(edad_infarto_cardiaco %in% c(77777, 99999),
             NA, edad_infarto_cardiaco)
  )

unique(df_obesidad$edad_infarto_cardiaco) # produce:
#[1] NA 55 72 53 51 57 32 45 47 48 40 64 70 23 80 49 65 52 56 46 31 43 35
#[24] 62 60 30 63 66 69 42 16 50 58 68 71 54 26 59 67 29 37 75 25 74 39 41
#[47] 61 78 44 38 20 28 76
summary(df_obesidad$edad_infarto_cardiaco) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#16.00   45.00   56.50   54.91   65.00   80.00    5570 

##################################
##
## Alguna vez te dijeron que
## tuviste un derrame cerebral ?
##
##################################

unique(df_obesidad$dijeron_derrame_cerebral) # produce: 
# [1]  1  2 NA  9

table(df_obesidad$dijeron_derrame_cerebral) # produce:
#  1    2    9 
#182 5268    5 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_derrame_cerebral = case_when(
      dijeron_derrame_cerebral == 1 ~ "si",
      dijeron_derrame_cerebral == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_derrame_cerebral = as.factor(dijeron_derrame_cerebral)
  )

unique(df_obesidad$dijeron_derrame_cerebral) # produce: 
#[1] si   no   <NA>
#Levels: no si

table(df_obesidad$dijeron_derrame_cerebral) # produce:
#  no   si 
#5268  182 

##################################
##
## Edad a la que te dijeron que
## tuviste derrame cerebral ?
##
##################################

unique(df_obesidad$edad_derrame_cerebral) # produce:
#[1]    62    NA    41    38    43    61    60    50    66    45    80
#[12]    59    32    64    49    40    79    75    56    51    55    65
#[23]    35    53    71    47 99999    70    48    67    69    25    63

summary(df_obesidad$edad_derrame_cerebral) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   7      50      60    1156      69   99999    5600 

# Transformar lo valores raros 77777 y 99999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    edad_derrame_cerebral = 
      ifelse(edad_derrame_cerebral %in% c(77777, 99999),
             NA, edad_derrame_cerebral)
  )

unique(df_obesidad$edad_derrame_cerebral) # produce:
#[1] 62 NA 41 38 43 61 60 50 66 45 80 59 32 64 49 40 79 75 56 51 55 65 35
#[24] 53 71 47 70 48 67 69 25 63 24 74 78 18 77 73 42 58 52 57 37 19 72  7
#[47] 29 36 44 39 54

summary(df_obesidad$edad_derrame_cerebral) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#7.00   49.75   60.00   58.24   67.50   80.00    5602 

##################################
##
## Alguna vez te dijeron que
## tenías un problema de tiroides?
##
##################################

unique(df_obesidad$dijeron_enf_tiroides) # produce:[1]  2  1 NA  9

table(df_obesidad$dijeron_enf_tiroides) # produce:
#  1    2    9 
#564 4881   10 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    dijeron_enf_tiroides = case_when(
      dijeron_enf_tiroides == 1 ~ "si",
      dijeron_enf_tiroides == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    dijeron_enf_tiroides = as.factor(dijeron_enf_tiroides)
  )

unique(df_obesidad$dijeron_enf_tiroides) # produce:
#[1] no   si   <NA>
#Levels: no si
table(df_obesidad$dijeron_enf_tiroides) # produce:
#  no   si 
#4881  564 

##################################
##
## Edad a la que te dijeron que
## tenías problemas de tiroides ?
##
##################################

unique(df_obesidad$edad_enf_tiroides) # produce:
#[1]    NA    50    53    20    72    46    51    60    65    38    14
#[12]    15    25    55    30    26    24    49    70    59     5    48
#[23]    27    41    43    69    45    47    21    40    39    35    13
summary(df_obesidad$edad_enf_tiroides) # produce:
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#1.00    30.00    43.50  1638.43    56.25 99999.00     5218 

# Transformar lo valores raros 77777 y 99999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    edad_enf_tiroides = 
      ifelse(edad_enf_tiroides %in% c(77777, 99999),
             NA, edad_enf_tiroides)
  )

unique(df_obesidad$edad_enf_tiroides) # produce:
#[1] NA 50 53 20 72 46 51 60 65 38 14 15 25 55 30 26 24 49 70 59  5 48 27
#[24] 41 43 69 45 47 21 40 39 35 13 76 66 33 32 17 34 58 57 56 12 44 18 11
#[47] 16 31 68 54 62 80 64 61 63 23 52 29 28 37 75 71 77 67  7 36 73 74 42
summary(df_obesidad$edad_enf_tiroides) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00   30.00   43.00   43.39   56.00   80.00    5227 

##################################
##
## Sigues teniendo problemas de
## tiroides ?
##
##################################

unique(df_obesidad$tienes_aun_enf_tiroides) # produce: [1] NA  1  2  9

table(df_obesidad$tienes_aun_enf_tiroides) # produce:
#  1   2   9 
#410 137  17 

# Transformar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    tienes_aun_enf_tiroides = case_when(
      tienes_aun_enf_tiroides == 1 ~ "si",
      tienes_aun_enf_tiroides == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    tienes_aun_enf_tiroides = as.factor(tienes_aun_enf_tiroides)
  )

unique(df_obesidad$tienes_aun_enf_tiroides) # produce: 
#[1] <NA> si   no  
#Levels: no si

table(df_obesidad$tienes_aun_enf_tiroides) # produce:
# no  si 
#137 410

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# enfermedad
obesidad_cuestionario_enfermedad <- df_obesidad %>%
  select(SEQN, dijeron_sobrepeso, dijeron_celiaquia, 
         sigue_dieta_sin_gluten, dijeron_artritis, tipo_artritis,
         edad_diagnostico_artritis, dijeron_icc, edad_diagnostico_icc,
         dijeron_enf_coronaria, edad_diagnostico_enf_coronaria,
         dijeron_infarto_cardiaco, edad_infarto_cardiaco, 
         dijeron_derrame_cerebral, edad_derrame_cerebral,
         dijeron_enf_tiroides, tienes_aun_enf_tiroides,
         edad_enf_tiroides
  )

# Crear un archivo .cvs con la dataset de cuestionario de enfermedad
write.csv(obesidad_cuestionario_enfermedad,
          "eda-obesidad/data/obesidad/13_obesidad_cuestionario_enfermedad.csv",
          row.names = FALSE)

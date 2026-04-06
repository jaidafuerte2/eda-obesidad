##########################################
##
## Questionnaire - Historial de peso
##
##########################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

##### ¿Alguna vez el médico le dijo que tenía sobrepeso? #####

# MCQ080 - ¿Alguna vez el médico le dijo que tenía sobrepeso?

summary(questionnaire$MCQ080) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.669   2.000   9.000    3711

unique(questionnaire$MCQ080) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé

##### ¿Alguna vez te han dicho que tienes celiaquía? #######

# MCQ082 - ¿Alguna vez te han dicho que tienes celiaquía?

summary(questionnaire$MCQ082) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1       2       2       2       2       9    1603 

unique(questionnaire$MCQ082) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

######## ¿Sigues una dieta sin gluten? #########

# MCQ086 - ¿Sigues una dieta sin gluten? 

summary(questionnaire$MCQ086) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.988   2.000   9.000    1603 

unique(questionnaire$MCQ086) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

###### ¿Alguna vez el médico le dijo que tenía artritis? ######

# MCQ160A - ¿Alguna vez el médico le dijo que tenía artritis?

summary(questionnaire$MCQ160A) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.754   2.000   9.000    4406 

unique(questionnaire$MCQ160A) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé

######### Tipo de artritis ############

# MCQ195 - ¿Qué tipo de artritis era?

unique(questionnaire$MCQ195) # produce: [1]  9 NA  2  4  1  3  7

table(questionnaire$MCQ195) # produce:
#  1   2   3   4   7   9 
#694 254  21 138   1 400 

# Dónde 1 es degenerativa, 2 es reumatoide, 3 es psoriásica, 4 es otro
# 7 es rechazado y 9 es nó lo sé

########## Edad a la que le diagnosticaron artritis ##########

# MCQ180a - Edad a la que le diagnosticaron artritis

summary(questionnaire$MCQ180A) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1      39      50    2038      62   99999    8667 

unique(questionnaire$MCQ180A)[1:20] # produce:
#  [1] 62 NA 70 45 50 42 40 19 59 79 39 52 60 38  1 78 25 68 22 80

############ Insuficiencia cardíaca ##############

# MCQ160B - ¿Alguna vez le han dicho que tiene insuficiencia cardíaca?

summary(questionnaire$MCQ160B) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.978   2.000   9.000    4406 

unique(questionnaire$MCQ160B) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

###### Edad a la que le diagnosticaron insuficiencia ######

# MCQ180B - Edad a la que le diagnosticaron insuficiencia cardíaca

summary(questionnaire$MCQ180B) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    46.0    58.0   605.4    68.0 99999.0    9993 

unique(questionnaire$MCQ180B)[1:20] # produce:
#[1] NA 78 57 37 54 55 45 71 30 76 50 65 38 49 40 64 59 43 56 79

########### Enfermedad Coronaria #############

# MCQ160C - ¿Alguna vez te han dicho que tienes una enfermedad coronaria?

summary(questionnaire$MCQ160C) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.982   2.000   9.000    4406 

unique(questionnaire$MCQ160C) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

############## Edad Enfermedad Coronaria ################

# MCQ180C - Edad a la que le diagnosticaron enfermedad coronaria

summary(questionnaire$MCQ180C) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    50.0    59.0   488.4    67.0 99999.0    9943 

unique(questionnaire$MCQ180C)[1:20] # produce:
#[1] NA 54 53 51 57 45 66 80 49 71 65 76 46 40 59 48 73 64 52 50

########### Infarto Cardíaco #############

# MCQ160E - ¿Alguna vez te han dicho que tuviste un ataque al corazón?

summary(questionnaire$MCQ160E) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.964   2.000   9.000    4406 

unique(questionnaire$MCQ160E) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

######### Edad Infarto cardíaco ############

# MCQ180E - Edad a la que le dijeron que había sufrido un infarto

summary(questionnaire$MCQ180E) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#16.0    45.0    57.0   491.5    65.0 99999.0    9946 

unique(questionnaire$MCQ180E)[1:20] # produce:
# [1] NA 55 72 53 51 57 32 45 47 48 40 64 70 23 80 49 65 52 56 46

############ Derrame cerebral ###############

# MCQ160F - ¿Alguna vez te han dicho que tuviste un derrame cerebral?

summary(questionnaire$MCQ160F) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.971   2.000   9.000    4406

unique(questionnaire$MCQ160F) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé

############# Edad Derrame cerebral ###############

# MCQ180F - Edad a la que le dijeron que había sufrido un derrame cerebral

summary(questionnaire$MCQ180F) # produce:
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#7.00    50.00    60.50  1048.01    69.75 99999.00     9973 

unique(questionnaire$MCQ180F)[1:20] # produce:
# [1] 62 NA 21 41 38 43 61 60 50 75 66 45 80 59 40 32 64 65 49 79

############## Hipotiroidismo #################

# MCQ160M - ¿Alguna vez te han dicho que tienes un problema de tiroides?

summary(questionnaire$MCQ160M) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.909   2.000   9.000    4406 

unique(questionnaire$MCQ160M) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

######## ¿Sigues teniendo problemas de tiroides? ########

# MCQ170M - ¿Sigues teniendo problemas de tiroides?

summary(questionnaire$MCQ170M) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.514   2.000   9.000    9574 

unique(questionnaire$MCQ170M) # produce: [1] NA  1  2  9
# Donde 1 es sí, 2 es no y 9 es no sé

############ Edad de hipotiroidismo ##############

# MCQ180M - Edad a la que le diagnosticaron un problema de tiroides

summary(questionnaire$MCQ180M) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1      30      43    1706      56   99999    9574 

unique(questionnaire$MCQ180M)[1:20] # produce:
#  [1] NA 50 53 20 72 46 51 60 65 38 14 15 25 55 30 26 24 49 70 59


############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_enfermedad <- questionnaire |>
  select(SEQN, MCQ080, MCQ082, MCQ086, MCQ160A, MCQ180A, MCQ195,
         MCQ160B, MCQ180B, MCQ160C, MCQ180C, MCQ160E, MCQ180E,
         MCQ160F, MCQ180F, MCQ160M, MCQ170M, MCQ180M) 

head(cuestionario_enfermedad) # produce:

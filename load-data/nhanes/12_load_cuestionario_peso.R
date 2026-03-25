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

############# Estatura Autoinformada #################  

# WHD010 - Estatura actual autoinformada (pulgadas)

summary(questionnaire$WHD010) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#48.0    63.0    66.0   163.2    69.0  9999.0    3736 

unique(questionnaire$WHD010) # produce:
#[1]   69   71   70   NA   67   64   61   60   62   68   66   74   63
#[14]   75   72   65   73   76   77 9999   59   58   57   55   79   54

######## Peso actual autoinformado #############

# WHD020 - Peso actual autoinformado (libras)

summary(questionnaire$WHD020) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   143.0   170.0   254.2   200.0  9999.0    3745 

unique(questionnaire$WHD020)[1:20] # produce:
#[1]  180  200  195   NA  120  235  212  137  165  105  224  128  145
#[14] 9999  175  104  205  170  240  230

############  Peso hace un año ##############

# WHD050 - Peso autoinformado - hace 1 año (libras)

summary(questionnaire$WHD050) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   140.0   170.0   312.5   200.0  9999.0    3753

(questionnaire$WHD050)[1:20] # produce:
#[1] 210 160 195  NA 150 240  NA 212 190 137 165 110  NA  NA 222  NA  NA
#[18] 123  NA 165

############### Cambio de peso intencional ###############

# WHQ060 - Cambio de peso intencional

summary(questionnaire$WHQ060) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    1.00    1.00    1.37    2.00    9.00    8936 

unique(questionnaire$WHQ060) # produce: [1]  1 NA  2  9
# Donde 1 es sí, 2 es no y 9 es no sé

############## Intentó perder peso el año pasado ##############

# WHQ070 - Intentó perder peso el año pasado

summary(questionnaire$WHQ070) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.641   2.000   9.000    4501 

unique(questionnaire$WHQ070) # produce: [1] NA  2  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

############# Comí menos para bajar de peso ################

# WHD080A - Comí menos para bajar de peso

summary(questionnaire$WHD080A) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  10      10      10      10      10      10    8482 
unique(questionnaire$WHD080A) # produce: [1] NA 10
# Donde 10 es sí

########## Cambié a alimentos con menos calorías #############

# WHD080B - Cambié a alimentos con menos calorías.

summary(questionnaire$WHD080B) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11      11      11      11      11      11    9292

unique(questionnaire$WHD080B) # produce: [1] NA 11

########### Comí menos grasa para bajar de peso ###############

# WHD080C - Comí menos grasa para bajar de peso

summary(questionnaire$WHD080C) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  12      12      12      12      12      12    9373 

unique(questionnaire$WHD080C) # produce: [1] NA 12

############### Hacer ejercicio para bajar de peso ##############

# WHD080D - Hacer ejercicio para bajar de peso

summary(questionnaire$WHD080D) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13      13      13      13      13      13    8345 

unique(questionnaire$WHD080D) # produce: [1] NA 13
# Donde 13 es sí

############### Comidas Omitidas ##################

# WHD080E - Comidas omitidas

summary(questionnaire$WHD080E) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  14      14      14      14      14      14    9776

unique(questionnaire$WHD080E) # produce: [1] NA 14

####### Consumió alimentos o productos dietéticos. ######## 

# WHD080F - Consumió alimentos o productos dietéticos.

summary(questionnaire$WHD080F) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  15      15      15      15      15      15    9942 

unique(questionnaire$WHD080F) # produce: [1] NA 15

######## Se unió a un programa de pérdida de peso ########

# WHD080H - Se unió a un programa de pérdida de peso.

summary(questionnaire$WHD080H) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  17      17      17      17      17      17   10061 

unique(questionnaire$WHD080H) # produce: [1] NA 17

######## Tomar agua para adelgazar ########## 

# WHD080M - Bebió mucha agua 

summary(questionnaire$WHD080M) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  34      34      34      34      34      34    9142 

unique(questionnaire$WHD080M) # produce: [1] NA 34

######## Comer menos carbohidratos para adelgazar ##########

# WHD080O - Consumió menos carbohidratos

summary(questionnaire$WHD080O) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  41      41      41      41      41      41    9486 

unique(questionnaire$WHD080O) # produce: [1] NA 41

########## Fumar para adelgazar ###########

# WHD080P - Empezó a echar humo o volvió a echar humo

summary(questionnaire$WHD080P) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  42      42      42      42      42      42   10156 

unique(questionnaire$WHD080P) # produce: [1] NA 42

######### Comer vegetales para adelgazar #############

# WHD080Q - Comió más frutas, verduras y ensaladas.

summary(questionnaire$WHD080Q) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#43      43      43      43      43      43    9054 

unique(questionnaire$WHD080Q) # produce: [1] NA 43

########### Comer menos azúcar para adelgazar #############

# WHD080S - Comió menos azúcar, caramelos y dulces.

summary(questionnaire$WHD080S) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  45      45      45      45      45      45    9232 

unique(questionnaire$WHD080S) # produce: [1] NA 45

######## Comer menos comida chatarra para adelgazar ############

# WHD080T - Comió menos comida chatarra o comida rápida.

summary(questionnaire$WHD080T) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  46      46      46      46      46      46    9179 

unique(questionnaire$WHD080T) # produce: [1] NA 46

##### ¿Alguna vez el médico le dijo que tenía sobrepeso? #####

# MCQ080 - ¿Alguna vez el médico le dijo que tenía sobrepeso?

summary(questionnaire$MCQ080) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.669   2.000   9.000    3711

unique(questionnaire$MCQ080) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé


############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_peso <- questionnaire |>
  select(SEQN, WHD010, WHD020, WHD050, WHQ060, WHQ070, WHD080A,
         WHD080B, WHD080C, WHD080D, WHD080E, WHD080F, WHD080H,
         WHD080M, WHD080O, WHD080P, WHD080Q, WHD080S, WHD080T) 

head(cuestionario_peso) # produce:

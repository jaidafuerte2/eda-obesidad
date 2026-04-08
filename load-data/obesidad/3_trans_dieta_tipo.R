#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset dieta - tipo    ##
## de dieta                    ##
##                             ##
#################################

# NOTA: No olvidar que estas variables numéricas binarias deben tener
# una versión factor pues la versión numérica será para modelos y
# la versión factor para gráficos.

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

###################################
##
## Mutar algunas variables para
## usarlas como categóricas
##
###################################
df_obesidad <- df_obesidad %>%
  mutate(
    sigues_dieta_cat = DRQSDIET,
    dieta_baja_calorias_cat = DRQSDT1,
    dieta_baja_grasas_colesterol_cat = DRQSDT2,
    dieta_baja_sal_cat = DRQSDT3,
    dieta_baja_azucar_cat = DRQSDT4,
    dieta_alta_fibra_cat = DRQSDT6,
    dieta_diabeticos_cat = DRQSDT7,
    dieta_baja_carbos_cat = DRQSDT9,
    dieta_alta_proteina_cat = DRQSDT10,
    dieta_sin_gluten_cat = DRQSDT11
  )

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables de tipo de dieta para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    sigues_dieta = DRQSDIET,
    uso_sal_mesa = DBD100,
    dieta_baja_calorias = DRQSDT1,
    dieta_baja_grasas_colesterol = DRQSDT2,
    dieta_baja_sal = DRQSDT3,
    dieta_baja_azucar = DRQSDT4,
    dieta_alta_fibra = DRQSDT6,
    dieta_diabeticos = DRQSDT7,
    dieta_baja_carbos = DRQSDT9,
    dieta_alta_proteina = DRQSDT10,
    dieta_sin_gluten = DRQSDT11
  )

#####################                          ###################
#####################    Versiones Binarias    ###################
#####################                          ################### 

########################
##
## Sigues alguna dieta?
##
########################

class(df_obesidad$sigues_dieta) # produce: numeric

unique(df_obesidad$sigues_dieta) # produce: [1]  2  1 NA  9

summary(df_obesidad$sigues_dieta) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.883   2.000   9.000     495 

# Pasar a los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad %>%
  mutate(
    sigues_dieta = case_when(
      sigues_dieta == 1 ~ "si",
      sigues_dieta == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    sigues_dieta = as.factor(sigues_dieta) # transformar en categorías
  )

class(df_obesidad$sigues_dieta) # produce:factor

unique(df_obesidad$sigues_dieta) # produce: 
#[1] no   si   <NA>
#Levels: no si

###############################
##
## Frecuencia de uso de sal
## en la mesa
##
###############################

class(df_obesidad$uso_sal_mesa) # produce: numeric

unique(df_obesidad$uso_sal_mesa) # produce: [1]  2  1 NA  3  9

# Transformar los valores numéricos a factores 
df_obesidad <- df_obesidad %>%
  mutate(
    uso_sal_mesa = case_when(
      uso_sal_mesa == 1 ~ "casi_nunca",
      uso_sal_mesa == 2 ~ "ocasional",
      uso_sal_mesa == 3 ~ "frecuente",
      TRUE ~ NA_character_
    )
  )

# Transformar la variable de uso de sal en la mesa a factor ordenado
df_obesidad <- df_obesidad %>%
  mutate(
    uso_sal_mesa = factor(
      uso_sal_mesa,
      levels = c("casi_nunca", "ocasional", "frecuente"),
      ordered = TRUE
    )
  )

class(df_obesidad$uso_sal_mesa) # produce: [1] "ordered" "factor" 

unique(df_obesidad$uso_sal_mesa) # produce:
#[1] ocasional  casi_nunca <NA>       frecuente 
#Levels: casi_nunca < ocasional < frecuente

table(df_obesidad$uso_sal_mesa) # produce:
#casi_nunca  ocasional  frecuente 
#      1777       1139        775 

###############################
##
## Hace dieta para bajar de 
## peso / baja en calorías?
##
###############################

class(df_obesidad$dieta_baja_calorias) # produce: numeric

summary(df_obesidad$dieta_baja_calorias) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1       1       1       1       1       1    5350 

unique(df_obesidad$dieta_baja_calorias) # produce: [1] NA  1

# transformar dieta baja en calorías a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_calorias = ifelse(!is.na(dieta_baja_calorias), 1, 0)
  )
class(df_obesidad$dieta_baja_calorias) # produce: numeric
summary(df_obesidad$dieta_baja_calorias) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.07471 0.00000 1.00000 
unique(df_obesidad$dieta_baja_calorias) # produce: 0 1

table(df_obesidad$dieta_baja_calorias) # produce:
#   0    1 
#5350  432 

###############################
##
## Hace dieta baja en grasas 
## y colesterol ?
##
###############################

class(df_obesidad$dieta_baja_grasas_colesterol) # produce: numeric

summary(df_obesidad$dieta_baja_grasas_colesterol) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2       2       2       2       2       2    5660

unique(df_obesidad$dieta_baja_grasas_colesterol) 
# produce: [1] NA  2

# transformar dieta baja en grasas y colesterol
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_grasas_colesterol = 
      ifelse(!is.na(dieta_baja_grasas_colesterol), 
             1, 0)
  )
class(df_obesidad$dieta_baja_grasas_colesterol) # produce: numeric
summary(df_obesidad$dieta_baja_grasas_colesterol) # produce:
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.0211  0.0000  1.0000 
unique(df_obesidad$dieta_baja_grasas_colesterol) # produce: 0 1

table(df_obesidad$dieta_baja_grasas_colesterol) # produce:
#   0    1 
#5660  122 

###############################
##
## Hace dieta baja en sal ?
##
###############################

class(df_obesidad$dieta_baja_sal) # produce: numeric

summary(df_obesidad$dieta_baja_sal) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3       3       3       3       3       3    5665 

unique(df_obesidad$dieta_baja_sal) # produce: [1] NA  3

# transformar dieta baja en sal a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_sal = ifelse(!is.na(dieta_baja_sal), 1, 0)
  )
class(df_obesidad$dieta_baja_sal) # produce: numeric
summary(df_obesidad$dieta_baja_sal) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.02024 0.00000 1.00000 
table(df_obesidad$dieta_baja_sal) # produce: 
#   0    1 
#5665  117 

###############################
##
## Hace dieta baja en azucar ?
##
###############################

class(df_obesidad$dieta_baja_azucar) # produce: numeric

summary(df_obesidad$dieta_baja_azucar) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   4       4       4       4       4       4    5749 

unique(df_obesidad$dieta_baja_azucar) # produce: [1] NA  4

# transformar dieta baja en azúcar a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_azucar = ifelse(!is.na(dieta_baja_azucar), 1, 0)
  )
class(df_obesidad$dieta_baja_azucar) # produce: numeric
summary(df_obesidad$dieta_baja_azucar) # produce:
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.005707 0.000000 1.000000 
table(df_obesidad$dieta_baja_azucar) # produce: 
#   0    1 
#5749   33  

###############################
##
## Hace dieta alta en fibra ?
##
###############################

class(df_obesidad$dieta_alta_fibra) # produce: numeric

summary(df_obesidad$dieta_alta_fibra) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   6       6       6       6       6       6    5780 

unique(df_obesidad$dieta_alta_fibra) # produce: [1] NA  6

# transformar dieta baja en sal a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_alta_fibra = ifelse(!is.na(dieta_alta_fibra), 1, 0)
  )
class(df_obesidad$dieta_alta_fibra) # produce: numeric
summary(df_obesidad$dieta_alta_fibra) # produce:
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0000000 0.0000000 0.0003459 0.0000000 1.0000000  
table(df_obesidad$dieta_alta_fibra) # produce: 
#   0    1 
#5780    2 

###############################
##
## Hace dieta para diabéticos ?
##
###############################

class(df_obesidad$dieta_diabeticos) # produce: numeric

summary(df_obesidad$dieta_diabeticos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   7       7       7       7       7       7    5643 

unique(df_obesidad$dieta_diabeticos) # produce: [1] NA  7

# transformar dieta baja en sal a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_diabeticos = ifelse(!is.na(dieta_diabeticos), 1, 0)
  )
class(df_obesidad$dieta_diabeticos) # produce: numeric
summary(df_obesidad$dieta_diabeticos) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.02024 0.00000 1.00000 
table(df_obesidad$dieta_diabeticos) # produce: 
#   0    1 
#5643  139 

#######################################
##
## Hace dieta baja en carbohidratos ?
##
#######################################

class(df_obesidad$dieta_baja_carbos) # produce: numeric

summary(df_obesidad$dieta_baja_carbos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   9       9       9       9       9       9    5720  

unique(df_obesidad$dieta_baja_carbos) # produce: [1] NA  9

# transformar dieta baja en sal a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_carbos = ifelse(!is.na(dieta_baja_carbos), 1, 0)
  )
class(df_obesidad$dieta_baja_carbos) # produce: numeric
summary(df_obesidad$dieta_baja_carbos) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.01072 0.00000 1.00000 
table(df_obesidad$dieta_baja_carbos) # produce: 
#   0    1 
#5720   62  



###############################
##
## Hace dieta alta en proteína ?
##
###############################

class(df_obesidad$dieta_alta_proteina) # produce: numeric

summary(df_obesidad$dieta_alta_proteina) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  10      10      10      10      10      10    5761 

unique(df_obesidad$dieta_alta_proteina) # produce: [1] NA  10

# transformar dieta alta en proteína a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_alta_proteina = ifelse(!is.na(dieta_alta_proteina), 1, 0)
  )
class(df_obesidad$dieta_alta_proteina) # produce: numeric
summary(df_obesidad$dieta_alta_proteina) # produce:
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.003632 0.000000 1.000000 
table(df_obesidad$dieta_alta_proteina) # produce: 
#   0    1 
#5761   21 

###############################
##
## Hace dieta sin gluten ?
##
###############################

class(df_obesidad$dieta_sin_gluten) # produce: numeric

summary(df_obesidad$dieta_sin_gluten) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3       3       3       3       3       3    5665 

unique(df_obesidad$dieta_sin_gluten) # produce: [1] NA  3

# transformar dieta sin gluten a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_sin_gluten = ifelse(!is.na(dieta_sin_gluten), 1, 0)
  )
class(df_obesidad$dieta_sin_gluten) # produce: numeric
summary(df_obesidad$dieta_sin_gluten) # produce:
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.004151 0.000000 1.000000  
table(df_obesidad$dieta_sin_gluten) # produce: 
#   0    1 
#5758   24  

#####################                          ###################
#####################  Versiones Categóricas   ###################
#####################                          ################### 

###############################
##
## ¿Sigues una dieta especial?
##
###############################

unique(df_obesidad$sigues_dieta_cat) # produce: [1]  2  1 NA  9

table(df_obesidad$sigues_dieta_cat) # produce:
#  1    2    9 
#865 4387   35

# Cambiar la variable numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    sigues_dieta_cat = case_when(
      sigues_dieta_cat == 1 ~ "si",
      sigues_dieta_cat == 2 ~ "no",
      sigues_dieta_cat %in% c(7, 9) ~ "desconocido",
      TRUE ~ NA_character_
    ),
    
    sigues_dieta_cat = factor(
      sigues_dieta_cat,
      levels = c("no", "si", "desconocido")
    )
  )

unique(df_obesidad$sigues_dieta_cat) # produce: 
#[1] no          si          <NA>        desconocido
#Levels: no si desconocido

table(df_obesidad$sigues_dieta_cat) # produce:
#  no          si desconocido 
#4387         865          35 

###############################
##
## ¿Sigues dieta baja en 
## calorías (categorica)
##
###############################

unique(df_obesidad$dieta_baja_calorias_cat) # produce:[1] NA  1

table(df_obesidad$dieta_baja_calorias_cat) # produce:
#  1 
#432 

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_calorias_cat = if_else(
      dieta_baja_calorias_cat == 1, "si", "no"
    ),
    
    dieta_baja_calorias_cat = factor(
      dieta_baja_calorias_cat,
      levels = c("no", "si")
    )
  )

unique(df_obesidad$dieta_baja_calorias_cat) # produce:
#[1] <NA> si  
#Levels: no si

table(df_obesidad$dieta_baja_calorias_cat) # produce:
#no  si 
# 0 432 

###############################
##
## ¿Sigues dieta baja en 
## grasas y colesterol 
## (categorica) ?
##
###############################

unique(df_obesidad$dieta_baja_grasas_colesterol_cat) # produce:
# [1] NA  2
table(df_obesidad$dieta_baja_grasas_colesterol_cat) # produce:
#   2 
# 122 

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_grasas_colesterol_cat = if_else(
      dieta_baja_grasas_colesterol_cat == 2, "si", "no"
    ),
    
    dieta_baja_grasas_colesterol_cat = factor(
      dieta_baja_grasas_colesterol_cat,
      levels = c("no", "si")
    )
  )


unique(df_obesidad$dieta_baja_grasas_colesterol_cat) # produce:
#[1] <NA> si  
#Levels: no si

table(df_obesidad$dieta_baja_grasas_colesterol_cat) # produce:
#no  si 
# 0 122

###############################
##
## ¿Sigues dieta baja en 
## sal (categorica) ?
##
###############################

unique(df_obesidad$dieta_baja_sal_cat) # produce: [1] NA  3

table(df_obesidad$dieta_baja_sal_cat) # produce: 
#  3 
#117 

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    dieta_baja_sal_cat = if_else(
      dieta_baja_sal_cat == 3, "si", "no"
    ),
    
    dieta_baja_sal_catt = factor(
      dieta_baja_sal_cat,
      levels = c("no", "si")
    )
  )



unique(df_obesidad$dieta_baja_sal_cat) # produce: [1] NA   "si" 

table(df_obesidad$dieta_baja_sal_cat) # produce: 
# si 
#117 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y el tipo de dieta
obesidad_dieta_tipo <- df_obesidad %>%
  select(SEQN, sigues_dieta, uso_sal_mesa, dieta_baja_calorias,
         dieta_baja_grasas_colesterol, dieta_baja_sal, 
         dieta_baja_azucar, dieta_alta_fibra, dieta_diabeticos,
         dieta_baja_carbos, dieta_alta_proteina, dieta_sin_gluten)
#glimpse(obesidad_dieta_tipo[1:4,]) # produce:
#Rows: 4
#Columns: 12
#$ SEQN                         <dbl> 73557, 73558, 73559, 73561
#$ sigues_dieta                 <fct> no, no, si, si
#$ uso_sal_mesa                 <ord> ocasional, ocasional, casi_nunca,…
#$ dieta_baja_calorias          <dbl> 0, 0, 0, 0
#$ dieta_baja_grasas_colesterol <dbl> 0, 0, 0, 0
#$ dieta_baja_sal               <dbl> 0, 0, 0, 0
#$ dieta_baja_azucar            <dbl> 0, 0, 0, 0
#$ dieta_alta_fibra             <dbl> 0, 0, 0, 0
#$ dieta_diabeticos             <dbl> 0, 0, 1, 0

# Crear un archivo .cvs con la dataset de dieta - tipo
write.csv(obesidad_dieta_tipo,
          "eda-obesidad/data/obesidad/3_obesidad_dieta_tipo.csv",
          row.names = FALSE)

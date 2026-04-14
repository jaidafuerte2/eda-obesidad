#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de peso                     ##
##                             ##
#################################

# NOTA: Se deben cambiar los pesos autoinformados a kilos y centrímetros
# por razones de consistencia. También hay que sacar imc autoinformado y
# cambio de peso con el peso de hace un año. También es importante 
# que las variables numéricas binarias tengan su correspondiente 
#  versión factor para gráficos.

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
  select(SEQN, WHD010, WHD020, WHD050, WHQ060, WHQ070, WHD080A,
         WHD080B, WHD080C, WHD080D, WHD080E, WHD080F, WHD080H,
         WHD080M, WHD080O, WHD080P, WHD080Q, WHD080S, WHD080T)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del cuestionario para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    estatura_auto = WHD010,
    peso_auto = WHD020,
    peso_hace_un_anio = WHD050,
    cambio_peso_intencional = WHQ060,
    intento_adelgazar = WHQ070,
    comio_menos = WHD080A,
    cambio_alimentos_menos_calorias = WHD080B,
    comio_menos_grasa = WHD080C,
    hizo_ejercicio = WHD080D,
    omitio_comidas = WHD080E,
    comio_dieteticos = WHD080F,
    unio_programa_adelgazar = WHD080H,
    bebio_agua = WHD080M,
    comio_menos_carbs = WHD080O,
    fumo_adelgazar = WHD080P,
    comio_vegetales = WHD080Q,
    comio_menos_azucar = WHD080S,
    comio_menos_chatarra = WHD080T
  )

############################
##
## Estatura autoinformada
## (pulgadas)
##
############################

unique(df_obesidad$estatura_auto) # produce:
#[1]   69   71   70   67   64   61   60   62   66   74   63   75   72
#[14]   68   65   NA   73   76   77 9999   59   58   57   55   54   56
#[27]   79   78   81   80   53

summary(df_obesidad$estatura_auto) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#53.0    63.0    66.0   164.6    69.0  9999.0      21 

# Cambiar los valores raros 7777 y 9999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    estatura_auto = 
      ifelse(estatura_auto %in% c(9999, 7777), NA, estatura_auto)
  )
unique(df_obesidad$estatura_auto) # produce:
#[1] 69 71 70 67 64 61 60 62 66 74 63 75 72 68 65 NA 73 76 77 59 58 57 55
#[24] 54 56 79 78 81 80 53

summary(df_obesidad$estatura_auto) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#53.00   63.00   66.00   66.37   69.00   81.00      78 

############################
##
## Peso autoinformado
## (libras)
##
############################

unique(df_obesidad$peso_auto) # produce:
#[1]  180  200  195  120  235  212  137  165  105  224  128 9999  175
#[14]  205  170  240  230  226  280  215  130  245  233  135  167  156
#[27]  185  171  220  169  168  160  133  184  106  217  161  238  250
summary(df_obesidad$peso_auto) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   145.0   170.0   263.7   200.0  9999.0      28 

# Cambiar los valores raros 7777 y 9999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    peso_auto = 
      ifelse(peso_auto %in% c(9999, 7777), NA, peso_auto)
  )
unique(df_obesidad$peso_auto) # produce:
#[1] 180 200 195 120 235 212 137 165 105 224 128  NA 175 205 170 240 230
#[18] 226 280 215 130 245 233 135 167 156 185 171 220 169 168 160 133 184
#[35] 106 217 161 238 250 263 163 140 275 202 166 190 155 154 150 143 126

summary(df_obesidad$peso_auto) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   145.0   170.0   177.2   200.0   493.0      80 

############################
##
## Peso hace un anio 
## autoinformado (libras)
##
############################

unique(df_obesidad$peso_hace_un_anio) # produce:
#[1]  210  160  195  150  240  212  137  165  110  222  123 9999  168
#[14]  220  175  225  230  215  226  170  270  205  130  245  200  250
#[27]  147  156  155  185  120  103  159  161  290  163   NA  285  180
summary(df_obesidad$peso_hace_un_anio) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   143.0   170.0   311.6   202.0  9999.0      31

df_obesidad <- df_obesidad %>%
  mutate(
    peso_hace_un_anio =
      ifelse(peso_hace_un_anio %in% c(7777, 9999), 
             NA, peso_hace_un_anio)
  )


unique(df_obesidad$peso_hace_un_anio) # produce:
#[1] 210 160 195 150 240 212 137 165 110 222 123  NA 168 220 175 225 230
#[18] 215 226 170 270 205 130 245 200 250 147 156 155 185 120 103 159 161
#[35] 290 163 285 180 190 145 152 183 115 149 232 117 133 127 166 258 144
summary(df_obesidad$peso_hace_un_anio) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   142.0   170.0   177.1   200.0   559.0     112 


############################
##
## Fue un cambio de peso 
## intencional?
##
############################

unique(df_obesidad$cambio_peso_intencional) # produce: [1]  1 NA  2  9

table(df_obesidad$cambio_peso_intencional) # produce:
#  1   2   9 
#724 406   1 

df_obesidad <- df_obesidad %>%
  mutate(
    cambio_peso_intencional = case_when(
      cambio_peso_intencional == 1 ~ "si",
      cambio_peso_intencional == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    cambio_peso_intencional = as.factor(cambio_peso_intencional)
  )
unique(df_obesidad$cambio_peso_intencional) # produce:
#[1] si   <NA> no  
#Levels: no si

class(df_obesidad$cambio_peso_intencional) # produce: [1] "factor"

table(df_obesidad$cambio_peso_intencional)
# no  si 
#406 724 

############################
##
## Intentó adelgazar el 
## año pasado ?
##
############################

unique(df_obesidad$intento_adelgazar) # produce: [1] NA  2  1  9

table(df_obesidad$intento_adelgazar) # produce:
#   1    2    9 
#1848 3206    2 

# Pasar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    intento_adelgazar = case_when(
      intento_adelgazar == 1 ~ "si",
      intento_adelgazar == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    intento_adelgazar = as.factor(intento_adelgazar)
  )

unique(df_obesidad$intento_adelgazar) # produce:
# [1] NA   "no" "si"

table(df_obesidad$intento_adelgazar) # produce:
#  no   si 
#3206 1848 

class(df_obesidad$intento_adelgazar) # produce: factor

############################
##
## Comió menos para  
##  adelgazar ?
##
############################

unique(df_obesidad$comio_menos) # produce: [1] NA 10

table(df_obesidad$comio_menos) # produce:
#  10 
#1540 

# Pasar el valor númerico a binario
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos = ifelse(!is.na(comio_menos), 1, 0)
  )

unique(df_obesidad$comio_menos) # produce: [1] 0 1

table(df_obesidad$comio_menos) # produce:
#   0    1 
#4242 1540 

###############################
##
## Cambió alimentos con menos
## calorías para adelgazar ?
##
###############################

unique(df_obesidad$cambio_alimentos_menos_calorias) # produce:
# [1] NA 11

table(df_obesidad$cambio_alimentos_menos_calorias) # produce:
# 11 
#808 

# Cambiar los valores numéricos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    cambio_alimentos_menos_calorias =
      ifelse(!is.na(cambio_alimentos_menos_calorias), 1, 0)
  )

unique(df_obesidad$cambio_alimentos_menos_calorias) # produce:
#[1] 0 1

table(df_obesidad$cambio_alimentos_menos_calorias) # produce:
#   0    1 
#4974  808 

###############################
##
## Comió menos grasa para bajar 
## de peso ?
##
###############################

unique(df_obesidad$comio_menos_grasa) # produce: [1] NA 12

table(df_obesidad$comio_menos_grasa) # produce:
# 12 
#738 


df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_grasa =
      ifelse(!is.na(comio_menos_grasa), 1, 0)
  )

unique(df_obesidad$comio_menos_grasa) # produce: [1] 0 1

table(df_obesidad$comio_menos_grasa) # produce:
#   0    1 
#5044  738 

###############################
##
## Hizo ejercicio para  
## adelgazar ?
##
###############################

unique(df_obesidad$hizo_ejercicio) # produce: [1] NA 13

table(df_obesidad$hizo_ejercicio) # produce:
#  13 
#1633 

# Cambiar la variable numérica a binaria
df_obesidad <- df_obesidad %>%
  mutate(
    hizo_ejercicio = ifelse(!is.na(hizo_ejercicio), 1, 0)
  )
unique(df_obesidad$hizo_ejercicio) # produce: [1] 0 1
table(df_obesidad$hizo_ejercicio) # produce:
#   0    1 
#4149 1633 

###############################
##
## Omitió comidas para  
## adelgazar ?
##
###############################

unique(df_obesidad$omitio_comidas) # produce: [1] NA 14

table(df_obesidad$omitio_comidas) # produce:
# 14 
#349

# Vambiar los valores numéricos a factor
df_obesidad <- df_obesidad %>%
  mutate(
    omitio_comidas = ifelse(!is.na(omitio_comidas), 1, 0)
  )

unique(df_obesidad$omitio_comidas) # produce: [1] 0 1

table(df_obesidad$omitio_comidas) # produce:
#   0    1 
#5433  349 

###############################
##
## Comió alimento dietéticos  
## para adelgazar ?
##
###############################

unique(df_obesidad$comio_dieteticos) # produce: [1] NA 15

table(df_obesidad$comio_dieteticos) # produce:
# 15 
#213 

# Cambiar los valores numéricos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    comio_dieteticos = ifelse(!is.na(comio_dieteticos), 1, 0)
  )

unique(df_obesidad$comio_dieteticos) # produce: [1] 0 1

table(df_obesidad$comio_dieteticos) # produce:
#   0    1 
#5569  213 

###############################
##
## Comió alimento dietéticos  
## para adelgazar ?
##
###############################

unique(df_obesidad$unio_programa_adelgazar) # produce: [1] NA 17

table(df_obesidad$unio_programa_adelgazar) # produce:
# 17 
#105 

# Cambiar los valores numericos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    unio_programa_adelgazar =
      ifelse(!is.na(unio_programa_adelgazar), 1, 0)
  )

unique(df_obesidad$unio_programa_adelgazar) # produce: [1] 0 1

table(df_obesidad$unio_programa_adelgazar) # produce:
#   0    1 
#5677  105 

#######################
##
## Tomó agua  
## para adelgazar ?
##
#######################

unique(df_obesidad$bebio_agua) # produce: [1] NA 34

table(df_obesidad$bebio_agua) # produce:
# 34 
#929 

# Cambiar los valores numéricos a valores binarios
df_obesidad <- df_obesidad %>%
  mutate(
    bebio_agua = ifelse(!is.na(bebio_agua), 1, 0)
  )

unique(df_obesidad$bebio_agua) # produce: [1] 0 1

table(df_obesidad$bebio_agua) # produce:
#   0    1 
#4853  929 

################################
##
## Tomó menos carbohidratos  
## para adelgazar ?
##
################################

unique(df_obesidad$comio_menos_carbs) # produce: [1] NA 41

table(df_obesidad$comio_menos_carbs) # produce:
# 41 
#636

# Cambiar los valores numéricos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_carbs = ifelse(!is.na(comio_menos_carbs), 1, 0)
  )

unique(df_obesidad$comio_menos_carbs) # produce: [1] 0 1

table(df_obesidad$comio_menos_carbs) # produce:
#   0    1 
#5146  636 

#######################
##
## Fumó  
## para adelgazar ?
##
#######################

unique(df_obesidad$fumo_adelgazar) # produce: [1] NA 42

table(df_obesidad$fumo_adelgazar) # produce:
#42 
#17 

# Cambiar los valores numéricos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    fumo_adelgazar = ifelse(!is.na(fumo_adelgazar), 1, 0)
  )

unique(df_obesidad$fumo_adelgazar) # produce: [1] 0 1

table(df_obesidad$fumo_adelgazar) # produce:
#   0    1 
#5765   17

#######################
##
## Comió vegetales 
## para adelgazar ?
##
#######################

unique(df_obesidad$comio_vegetales) # produce: [1] NA 43

table(df_obesidad$comio_vegetales) # produce: 
#  43 
#1014 

# Cambiar los valores numpericos a bianrios
df_obesidad <- df_obesidad %>%
  mutate(
    comio_vegetales = ifelse(!is.na(comio_vegetales), 1, 0)
  )

unique(df_obesidad$comio_vegetales) # produce: [1] 0 1

table(df_obesidad$comio_vegetales) # produce:
#   0    1 
#4768 1014 

#######################
##
## Comió menos azúcar 
## para adelgazar ?
##
#######################

unique(df_obesidad$comio_menos_azucar) # produce: [1] NA 45

table(df_obesidad$comio_menos_azucar) # produce:
# 45 
#852

# Cambiar los valores numéricos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_azucar = ifelse(!is.na(comio_menos_azucar), 1, 0)
  )
unique(df_obesidad$comio_menos_azucar) # produce: [1] 0 1

table(df_obesidad$comio_menos_azucar) # produce:
#   0    1 
#4930  852 

#######################
##
## Comió menos azúcar 
## para adelgazar ?
##
#######################

unique(df_obesidad$comio_menos_chatarra) # produce:

table(df_obesidad$comio_menos_chatarra) # produce:

# Cambiar los valores numéricos a binarios
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_chatarra = ifelse(!is.na(comio_menos_chatarra), 1, 0)
  )


unique(df_obesidad$comio_menos_chatarra) # produce: [1] 0 1

table(df_obesidad$comio_menos_chatarra) # produce:
#   0    1 
#4880  902 

####################                             ##################
#################### Transformar y crear algunas ##################
#################### variables importantes       ##################
####################                             ##################

############################
##
## Estatura y peso actual
## y peso hace un año en 
## kilos y metros
##
#############################

# Calcular el peso actual y hace un año en kilos y la estatura 
# en metros (autoinformados)
df_obesidad <- df_obesidad %>%
  mutate(
    peso_kg_auto = peso_auto * 0.453592,
    estatura_m_auto = estatura_auto * 0.0254,
    peso_kg_hace_un_anio = peso_hace_un_anio * 0.453592
  )

# Peso
unique(df_obesidad$peso_kg_auto)[1:20] # produce:
#[1]  81.64656  90.71840  88.45044  54.43104 106.59412  96.16150
#[7]  62.14210  74.84268  47.62716 101.60461  58.05978        NA
#[13]  79.37860  92.98636  77.11064 108.86208 104.32616 102.51179

summary(df_obesidad$peso_kg_auto) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#34.02   65.77   77.11   80.39   90.72  223.62      80 

# Estatura
unique(df_obesidad$estatura_m_auto)[1:20] # produce:
#[1] 1.7526 1.8034 1.7780 1.7018 1.6256 1.5494 1.5240 1.5748 1.6764
#[10] 1.8796 1.6002 1.9050 1.8288 1.7272 1.6510     NA 1.8542 1.9304
#[19] 1.9558 1.4986

summary(df_obesidad$estatura_m_auto) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.346   1.600   1.676   1.686   1.753   2.057      78 

# Peso hace un año
unique(df_obesidad$peso_kg_hace_un_anio)[1:20] # produce:
#[1]  95.25432  72.57472  88.45044  68.03880 108.86208  96.16150
#[7]  62.14210  74.84268  49.89512 100.69742  55.79182        NA
#[13]  76.20346  99.79024  79.37860 102.05820 104.32616  97.52228

summary(df_obesidad$peso_kg_hace_un_anio) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#34.02   64.41   77.11   80.33   90.72  253.56     112 

#############################
##
## IMC actual y hace un año
## (autoinformados)
##
#############################

# Calcular el imc actual y hace un año
df_obesidad <- df_obesidad %>%
  mutate(
    imc_auto = peso_kg_auto / (estatura_m_auto ^ 2),
    imc_hace_un_anio = peso_kg_hace_un_anio / (estatura_m_auto ^ 2)
  )

# índice de masa corporal autoinformado 
unique(df_obesidad$imc_auto)[1:20] # produce:
#[1] 26.58106 27.89403 27.97928 18.79445 40.33721 36.38931 25.88564
#[8] 23.01257 20.50618 33.07865 23.41125       NA 25.82702 32.28049
#[15] 22.46842 30.27287 25.84237 25.10433 29.22811 34.43603

summary(df_obesidad$imc_auto) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#14.91   23.62   27.12   28.19   31.46   75.90     140 

# índice de masa corporal de hace un año autoinformado
unique(df_obesidad$imc_hace_un_anio)[1:20] # produce:
#[1] 31.01124 22.31522 27.97928 23.49306 41.19545 36.38931 25.88564
#[8] 23.01257 21.48266 32.78331 22.49674       NA 24.10522 35.50854
#[15] 22.46842 33.22632 29.22811 33.00120 34.70152 28.24775

summary(df_obesidad$imc_hace_un_anio) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#15.00   23.40   26.78   28.17   31.32   82.55     173

########################
##
## Cambio de peso
##
########################

# Cambio de peso actual vs año anterior
df_obesidad <- df_obesidad %>%
  mutate(
    cambio_peso_kg = peso_kg_auto - peso_kg_hace_un_anio
  )
unique(df_obesidad$cambio_peso_kg)[1:20] # produce:
#[1] -13.607760  18.143680   0.000000  -2.267960  -2.267960   0.907184
#[7]   2.267960         NA   5.443104  -9.071840   6.803880 -18.143680
#[13]   4.535920  -7.711064  11.339800   9.071840   6.803880   0.453592

summary(df_obesidad$cambio_peso_kg) # produce:
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -161.47875   -2.26796    0.00000    0.05027    2.72155   87.99685 
# NA's 
#  134 

########################
##
## Cambio de peso
## (categórico)
##
########################

# Categorizar el cambio de peso
df_obesidad <- df_obesidad %>%
  mutate(
    cambio_peso_cat = case_when(
      cambio_peso_kg < -2.3 ~ "perdio", # Se usa 2.3 para evitar
      cambio_peso_kg > 2.3 ~ "gano", # ruido
      TRUE ~ "estable"
    )
  )

# Cambiar la variable a factor ordenado
df_obesidad <- df_obesidad %>%
  mutate(
    cambio_peso_cat = factor(
      cambio_peso_cat,
      levels = c("perdio", "estable", "gano"),
      ordered = TRUE
    )
  )
unique(df_obesidad$cambio_peso_cat) # produce:
#[1] perdio  gano    estable
#Levels: perdio < estable < gano

table(df_obesidad$cambio_peso_cat) # produce:
#perdio estable    gano 
#  1325    3029    1428 

##########################
##
## Perdió peso
## binario
##
##########################

# Cambiar la variable de cambio de peso a valores binarios
df_obesidad <- df_obesidad %>%
  mutate(
    perdio_peso = if_else(cambio_peso_kg < -2.3, 1, 0)
  )
unique(df_obesidad$perdio_peso) # produce: [1]  1  0 NA

table(df_obesidad$perdio_peso) # produce:
#   0    1 
#4323 1325

#########################
##
## Pérdida de peso
## intencional
##
#########################

# Conocer si la pérdida de peso fue intencional
df_obesidad <- df_obesidad %>%
  mutate(
    perdida_intencional = case_when(
      perdio_peso == 1 & cambio_peso_intencional == "si" ~ "intencional",
      perdio_peso == 1 & cambio_peso_intencional == "no" ~ "no_intencional",
      perdio_peso == 1 ~ "desconocido",
      TRUE ~ NA_character_
    )
  )
unique(df_obesidad$perdida_intencional) # produce:
#[1] "intencional"    NA               "no_intencional" "desconocido"  
table(df_obesidad$perdida_intencional) # produce:
#desconocido    intencional no_intencional 
#        201            721            403

##########################
##
## Ganó peso
## binario
##
##########################

# Cambiar la variable de cambio de peso a valores binarios
df_obesidad <- df_obesidad %>%
  mutate(
    gano_peso = if_else(cambio_peso_kg > 2.3, 1, 0)
  )
unique(df_obesidad$gano_peso) # produce: [1]  0  1 NA

table(df_obesidad$gano_peso) # produce:
#   0    1 
#4220 1428 

#########################
##
## Ganó peso
## intencional
##
#########################

# Conocer si la ganancia de peso fue intencional
df_obesidad <- df_obesidad %>%
  mutate(
    ganancia_intencional = case_when(
      gano_peso == 1 & cambio_peso_intencional == "si" ~ 
        "intencional",
      gano_peso == 1 & cambio_peso_intencional == "no" ~ 
        "no_intencional",
      gano_peso == 1 ~ "desconocido",
      TRUE ~ NA_character_
    )
  )
unique(df_obesidad$ganancia_intencional) # produce:
# [1] NA            "desconocido" "intencional"
table(df_obesidad$ganancia_intencional) # produce:
#desconocido intencional 
#       1427           1 

####################                             ##################
#################### Pasar a categóricas algunas ##################
#################### variables binarias          ##################
####################                             ##################

#############################
##
## Comió menos para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_cat = if_else(
      comio_menos == 1, "si", "no"
    ),
    
    comio_menos_cat = factor(
      comio_menos_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_menos_cat) # produce:
#  no   si 
#4242 1540  

unique(df_obesidad$comio_menos_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## Cambió a alimentos con 
## menos calorías para
## adelgazar? (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    cambio_alimentos_menos_calorias_cat = if_else(
      cambio_alimentos_menos_calorias == 1, "si", "no"
    ),
    
    cambio_alimentos_menos_calorias_cat = factor(
      cambio_alimentos_menos_calorias_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$cambio_alimentos_menos_calorias_cat) # produce:
#  no   si 
#4974  808  

unique(df_obesidad$cambio_alimentos_menos_calorias_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## Comió menos grasas para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_grasa_cat = if_else(
      comio_menos_grasa == 1, "si", "no"
    ),
    
    comio_menos_grasa_cat = factor(
      comio_menos_grasa_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_menos_grasa_cat) # produce:
#  no   si 
#5044  738 

unique(df_obesidad$comio_menos_grasa_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## hizo ejercicio para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    hizo_ejercicio_cat = if_else(
      hizo_ejercicio == 1, "si", "no"
    ),
    
    hizo_ejercicio_cat = factor(
      hizo_ejercicio_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$hizo_ejercicio_cat) # produce:
#  no   si 
#4149 1633 

unique(df_obesidad$hizo_ejercicio_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## Omitió comidas para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    omitio_comidas_cat = if_else(
      omitio_comidas == 1, "si", "no"
    ),
    
    omitio_comidas_cat = factor(
      omitio_comidas_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$omitio_comidas_cat) # produce:
#  no   si 
#5433  349 

unique(df_obesidad$omitio_comidas_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## Comió comida de dieta para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_dieteticos_cat = if_else(
      comio_dieteticos == 1, "si", "no"
    ),
    
    comio_dieteticos_cat = factor(
      comio_dieteticos_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_dieteticos_cat) # produce:
#  no   si 
#5569  213 

unique(df_obesidad$comio_dieteticos_cat) # produce:
#[1] no si
#Levels: no si

##################################
##
## Se unió a un programa para
## adelgazar (categórica)
##
##################################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    unio_programa_adelgazar_cat = if_else(
      unio_programa_adelgazar == 1, "si", "no"
    ),
    
    unio_programa_adelgazar_cat = factor(
      unio_programa_adelgazar_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$unio_programa_adelgazar_cat) # produce:
#  no   si 
#5677  105 

unique(df_obesidad$unio_programa_adelgazar_cat) # produce:
#[1] no si
#Levels: no si

############################
##
## Bebió más agua para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    bebio_agua_cat = if_else(
      bebio_agua == 1, "si", "no"
    ),
    
    bebio_agua_cat = factor(
      bebio_agua_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$bebio_agua_cat) # produce:
#  no   si 
#5677  105 

unique(df_obesidad$bebio_agua_cat) # produce:
#[1] no si
#Levels: no si

############################
##
## Comió menos carbs para
## adelgazar (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_carbs_cat = if_else(
      comio_menos_carbs == 1, "si", "no"
    ),
    
    comio_menos_carbs_cat = factor(
      comio_menos_carbs_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_menos_carbs_cat) # produce:
#  no   si 
#5146  636 

unique(df_obesidad$comio_menos_carbs_cat) # produce:
#[1] no si
#Levels: no si

#########################
##
## Fumó para adelgazar
##  (categórica)
##
#########################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    fumo_adelgazar_cat = if_else(
      fumo_adelgazar == 1, "si", "no"
    ),
    
    fumo_adelgazar_cat = factor(
      fumo_adelgazar_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$fumo_adelgazar_cat) # produce:
#  no   si 
#5765   17

unique(df_obesidad$fumo_adelgazar_cat) # produce:
#[1] no si
#Levels: no si

#########################
##
## Comió vegetales para 
## adelgazar? (categórica)
##
#########################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_vegetales_cat = if_else(
      comio_vegetales == 1, "si", "no"
    ),
    
    comio_vegetales_cat = factor(
      comio_vegetales_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_vegetales_cat) # produce:
#  no   si 
#4768 1014 

unique(df_obesidad$comio_vegetales_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## Comió menos azúcar para 
## adelgazar? (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_azucar_cat = if_else(
      comio_menos_azucar == 1, "si", "no"
    ),
    
    comio_menos_azucar_cat = factor(
      comio_menos_azucar_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_menos_azucar_cat) # produce:
#  no   si 
#4930  852  

unique(df_obesidad$comio_menos_azucar_cat) # produce:
#[1] no si
#Levels: no si

#############################
##
## Comió menos chatarra para 
## adelgazar? (categórica)
##
#############################

# Cambiar la variable de numérica a categórica
df_obesidad <- df_obesidad %>%
  mutate(
    comio_menos_chatarra_cat = if_else(
      comio_menos_chatarra == 1, "si", "no"
    ),
    
    comio_menos_chatarra_cat = factor(
      comio_menos_chatarra_cat,
      levels = c("no", "si")
    )
  )
table(df_obesidad$comio_menos_chatarra_cat) # produce:
#  no   si 
#4880  902   

unique(df_obesidad$comio_menos_chatarra_cat) # produce:
#[1] no si
#Levels: no si


###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# peso
obesidad_cuestionario_peso <- df_obesidad %>%
  select(SEQN, estatura_auto, peso_auto, peso_hace_un_anio, 
         cambio_peso_intencional,intento_adelgazar, comio_menos,
         cambio_alimentos_menos_calorias, comio_menos_grasa,
         hizo_ejercicio, omitio_comidas, comio_dieteticos,
         unio_programa_adelgazar, bebio_agua, comio_menos_carbs,
         fumo_adelgazar, comio_vegetales, comio_menos_azucar,
         comio_menos_chatarra,
         # Nuevas variables
         peso_kg_auto, estatura_m_auto, peso_kg_hace_un_anio,
         imc_auto, imc_hace_un_anio, cambio_peso_kg, cambio_peso_cat,
         perdio_peso, perdida_intencional, gano_peso, ganancia_intencional,
         comio_menos_cat, cambio_alimentos_menos_calorias_cat, 
         comio_menos_grasa_cat, hizo_ejercicio_cat, omitio_comidas_cat,
         comio_dieteticos_cat, comio_dieteticos_cat, 
         unio_programa_adelgazar_cat, bebio_agua_cat, 
         comio_menos_carbs_cat, fumo_adelgazar_cat, comio_vegetales_cat, 
         comio_menos_azucar_cat, comio_menos_chatarra_cat
         )

# Crear un archivo .cvs con la dataset de cuestionario de peso
write.csv(obesidad_cuestionario_peso,
          "eda-obesidad/data/obesidad/12_obesidad_cuestionario_peso.csv",
          row.names = FALSE)
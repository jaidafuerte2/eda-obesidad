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
    cambio_peso = WHQ060,
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
    comio_menos_chatarra = WHD080T,
    tiene_sobrepeso = MCQ080
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

unique(df_obesidad$cambio_peso) # produce: [1]  1 NA  2  9

table(df_obesidad$cambio_peso) # produce:
#  1   2   9 
#724 406   1 

df_obesidad <- df_obesidad %>%
  mutate(
    cambio_peso = case_when(
      cambio_peso == 1 ~ "si",
      cambio_peso == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    cambio_peso = as.factor(cambio_peso)
  )
unique(df_obesidad$cambio_peso) # produce:
#[1] si   <NA> no  
#Levels: no si

class(df_obesidad$cambio_peso) # produce: [1] "factor"

table(df_obesidad$cambio_peso)
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
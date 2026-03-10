#################################
##                             ##
## Transformaciones Básicas    ##
## del obesity risk            ##
##                             ##
#################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Importar desde el IDE la tabla obesity latin y cuidar de llamarla 
# df_obesity

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del obesity risk para que su nombre se
# entienda mejor en español

df_obesidad <- df_obesidad |>
  rename(
    genero = Gender,
    edad = Age,
    talla = Height,
    peso = Weight,
    tiene_familiares_obesos = family_history_with_overweight,
    come_densidad_alta = FAVC,
    consumo_vegetales = FCVC,
    comidas_principales = NCP,
    come_refrigerios = CAEC,
    fuma = SMOKE,
    consumo_agua_diaria = CH2O,
    toma_bebidas_caloricas = SCC,
    actividad_fisica = FAF,
    uso_tecnologia = TUE,
    consume_alcohol = CALC,
    medio_transporte = MTRANS
  )
glimpse(df_obesidad[1:4,])
#Rows: 4
#Columns: 20
#$ id                      <int> 0, 1, 2, 3
#$ Gender                  <chr> "Male", "Female", "Female", "Female"
#$ Age                     <dbl> 24.44301, 18.00000, 18.00000, 20.95274
#$ Height                  <dbl> 1.699998, 1.560000, 1.711460, 1.710730
#$ Weight                  <dbl> 81.66995, 57.00000, 50.16575, 131.27485
#$ tiene_familiares_obesos <int> 1, 1, 1, 1
#$ come_densidad_alta      <int> 1, 1, 1, 1
#$ consumo_vegetales       <dbl> 2.000000, 2.000000, 1.880534, 3.000000
#$ comidas_principales     <dbl> 2.983297, 3.000000, 1.411685, 3.000000
#$ come_refrigerios        <chr> "Sometimes", "Frequently", "Sometimes",…
#$ fuma                    <int> 0, 0, 0, 0
#$ consumo_agua_diaria     <dbl> 2.763573, 2.000000, 1.910378, 1.674061
#$ toma_bebidas_caloricas  <int> 0, 0, 0, 0
#$ actividad_fisica        <dbl> 0.000000, 1.000000, 0.866045, 1.467863
#$ uso_tecnologia          <dbl> 0.976473, 1.000000, 1.673584, 0.780199
#$ consume_alcohol         <chr> "Sometimes", "0", "0", "Sometimes"
#$ medio_transporte        <chr> "Public_Transportation", "Automobile", …
#$ X0be1dad                <chr> "Overweight_Level_II", "0rmal_Weight", …
#$ imc                     <dbl> 28.25956, 23.42209, 17.12671, 44.85580
#$ tipo_obesidad           <ord> sobrepeso, peso_normal, desnutricion, o…

################################################
##
## Crear las variables imc y tipo de obesidad 
##
################################################

# Crear la variable imc y calcular el índice de masa corporal.
df_obesidad <- df_obesidad |>
  mutate(
    imc = peso / (talla ^ 2)
  )
unique(df_obesidad$imc) # produce:
#[1] 28.25956 23.42209 17.12671 44.85580 25.59915 16.86193 36.61079
#[8] 38.59145 24.22145 41.43135 23.87511 32.99929 43.99566 19.87910
#[15] 35.22807 27.68166 20.54569 36.46328 28.68514 23.50781 40.69075
#[22] 34.51503 16.93431 27.97504 43.11063 39.29935 18.73049 33.29865
# Conocer el valor mínimo de la variable imc
min(df_obesidad$imc) # produce: [1] 12.86854
# Conocer el valor máximo
max(df_obesidad$imc) # produce: [1] 54.99799


# Reclasificar el tipo de obesidad porque no coincide con la 
# clasificación de la OMS
df_obesidad <- df_obesidad |>
  mutate(
    tipo_obesidad = case_when(
      imc < 18.5 ~ "desnutricion",
      imc < 25 ~ "peso_normal",
      imc < 30 ~ "sobrepeso",
      imc < 35 ~ "obesidad_1",
      imc < 40 ~ "obesidad_2",
      imc >= 40 ~ "obesidad_3",
      TRUE ~ NA_character_
    )
  )
unique(df_obesidad$tipo_obesidad) # produce:
#[1] "sobrepeso"    "peso_normal"  "desnutricion" "obesidad_3"  
#[5] "obesidad_2"   "obesidad_1" 

# Transformar en un factor ordenado a la variable de tipo de obesidad
df_obesidad <- df_obesidad |>
  mutate(
    tipo_obesidad = factor(
      tipo_obesidad,
      levels = c(
        "desnutricion",
        "peso_normal",
        "sobrepeso",
        "obesidad_1",
        "obesidad_2",
        "obesidad_3"
      ),
      ordered = TRUE
    )
  )
unique(df_obesidad$tipo_obesidad) # produce:
#[1] sobrepeso    peso_normal  desnutricion obesidad_3   obesidad_2  
#[6] obesidad_1  
#6 Levels: desnutricion < peso_normal < sobrepeso < ... < obesidad_3

#################################
##
## Transformaciones de Género
##
#################################

# Conocer el tipo de valores de la variable genero
unique(df_obesidad$genero) # produce: [1] "Male"   "Female"
# Conocer el tipo de la varaible género
class(df_obesidad$genero) # produce: [1] "character"

# Recodificar la variable género para que male sea masculino y female 
# sea femenino
df_obesidad <- df_obesidad |>
  mutate(
    genero = recode(
      genero,
      "Male" = "masculino",
      "Female" = "femenino"
    )
  )
unique(df_obesidad$genero) # produce: [1] "masculino" "femenino" 

# Cambiar el tipo de la variable de género de character a factor 
df_obesidad <- df_obesidad |>
  mutate(
    genero = factor(
      genero,
      levels = c("masculino", "femenino")
    )
  )
class(df_obesidad$genero) # produce: "factor"

######################################################
##
## Transformaciones de Historia Familiar de Obesidad
##
######################################################

# Conocer los valores de la variable historia familiar de obesidad
unique(df_obesidad$tiene_familiares_obesos) # produce: [1] 1 0

# Conocer el tipo de la varaiable de historia familiar de obesidad
class(df_obesidad$tiene_familiares_obesos) # produce: [1] "integer"

# Cambiar el tipo de la variable de historia familiar de obesidad
df_obesidad <- df_obesidad |>
  mutate(
    tiene_familiares_obesos_cat = factor(
      tiene_familiares_obesos,
      levels = c(0, 1)
    )
  )
class(df_obesidad$tiene_familiares_obesos_cat) # produce: "factor"
unique(df_obesidad$tiene_familiares_obesos_cat) # produce:
#[1] 1 0
#Levels: 0 1

# Cambiar los valores de la variable de historia familiar de obesidad
# para que "0" sea "no" y "1" sea "si" 
df_obesidad <- df_obesidad |>
  mutate(
    tiene_familiares_obesos_cat = recode(
      tiene_familiares_obesos_cat,
      "0" = "no",
      "1" = "si"
    )
  )
unique(df_obesidad$tiene_familiares_obesos_cat) # produce:
#[1] si no
#Levels: no si

class(df_obesidad$tiene_familiares_obesos_cat) # produce:
class(df_obesidad$tiene_familiares_obesos) # produce:

# Reposicionar la variable categórica de tiene familiares obesos
# junto a la varaible integer
df_obesidad <- df_obesidad |>
  relocate(tiene_familiares_obesos_cat, 
           .after = tiene_familiares_obesos)

############################################
##
## Transformaciones de Comida de alta 
## densidad calórica
##
############################################

# Conocer los valores de la variable de consumo de comida de alta
# densidad calórica
unique(df_obesidad$come_densidad_alta) # produce:[1] 1 0
# Conocer el tipo de la varaible de consumo de alimentos de alta 
# densidad calórica
class(df_obesidad$come_densidad_alta) # produce: "integer"

# Cambiar el tipo de la varaible de consumo de comida de alta densidad
# calórica de integer a factor
df_obesidad <- df_obesidad |>
  mutate(
    come_densidad_alta_cat = factor(
      come_densidad_alta,
      levels = c(0, 1)
    )
  )
class(df_obesidad$come_densidad_alta_cat) # produce: "factor"

# Cambiar los valores de la variable de consumo de alimentos de alta
# densidad calórica para que 0 sea no y 1 sea si
df_obesidad <- df_obesidad |>
  mutate(
    come_densidad_alta_cat = recode(
      come_densidad_alta_cat,
      "0" = "no",
      "1" = "si"
    )
  )
unique(df_obesidad$come_densidad_alta_cat) # produce: [1] si no

class(df_obesidad$come_densidad_alta_cat) # produce: factor
class(df_obesidad$come_densidad_alta) # produce: "integer"

# Reposicionar la variable categórica de consumo de comida de alta 
# densidad junto a la varaible integer
df_obesidad <- df_obesidad |>
  relocate(come_densidad_alta_cat, 
           .after = come_densidad_alta)

##############################################
##
## Transformaciones de Consumo de Vegetales
##
##############################################

# Conocer el tipo de la variable de consumo de vegetales 
class(df_obesidad$consumo_vegetales) # produce: "numeric"
# Conocer los valores de la variable de consumo de vegetales
unique(df_obesidad$consumo_vegetales)[1:20] # produce:
#[1] 2.000000 1.880534 3.000000 2.679664 2.919751 1.991240 1.397468
#[8] 2.636719 1.000000 1.392665 2.203962 2.971588 2.668949 1.989899
#[15] 2.417635 2.219186 2.919526 2.263245 2.649406 1.754401
# Valor mínimo de la varaible de consumo de vegetales
min(df_obesidad$consumo_vegetales) # produce: 1
# Valor máximo de la varaible de consumo de vegetales
max(df_obesidad$consumo_vegetales) # produce: 3

# NOTA: Es posible que sean valores diarios, el manual no lo 
# indica. "numeric" es double o de doble precisión es decir que puede
# tener decimales

# Cambiar la variable de consumo de vegetales de numérica a categórica
df_obesidad <- df_obesidad |>
  mutate(
    consumo_vegetales_cat = case_when(
      consumo_vegetales <= 1.5 ~ "nunca",
      consumo_vegetales <= 2.5 ~ "ocasional",
      consumo_vegetales > 2.5 ~ "siempre",
      TRUE ~ NA_character_
    )
  )
class(df_obesidad$consumo_vegetales_cat) # produce: character

# Cambiar la varaible de consumo de vegetales a factor
df_obesidad <- df_obesidad |>
  mutate(
    consumo_vegetales_cat = factor(
      consumo_vegetales_cat,
      levels = c("nunca", "ocasional", "siempre"),
      ordered = TRUE
    )
  )
unique(df_obesidad$consumo_vegetales_cat) # produce:
#[1] ocasional siempre   nunca    
#Levels: nunca < ocasional < siempre

# Reposicionar la variable categórica de consumo de vegetales de
# para juntarla a su correpondiente numérica
df_obesidad <- df_obesidad |>
  relocate(consumo_vegetales_cat, 
           .after = consumo_vegetales)

#####################################################
##
## Transformaciones Cantidad de Comidas Principales
##
#####################################################

# Conocer el tipo de la variable de cantidad de comidas principales
class(df_obesidad$comidas_principales) # produce: "numeric"
# Conocer los valores de la variable cantidad de comidas principales
unique(df_obesidad$comidas_principales)[1:20] # produce:
#[1] 2.983297 3.000000 1.411685 1.971472 2.164839 1.000000 2.954446
#[8] 1.893811 3.998618 1.703299 2.937989 2.996444 2.581015 2.473913
#[15] 1.437959 2.989791 4.000000 2.853676 1.104642 3.362758
# Valor mínimo de la varaible de número de comidas principales
min(df_obesidad$comidas_principales) # produce: 1
#Valor máximo de la variable de número de comidas principales
max(df_obesidad$comidas_principales) # produce: 4

# Cambiar la variable de comidas principales de numérica a 
# categórica
df_obesidad <- df_obesidad |>
  mutate(
    comidas_principales_cat = case_when(
      comidas_principales <= 2 ~ "1-2",
      comidas_principales <= 3 ~ "3",
      comidas_principales > 3 ~ ">3",
      TRUE ~ NA_character_
    )
  )
class(df_obesidad$comidas_principales_cat) # produce: "character"

# Cambiar la varaible de comidas principales de character a factor
df_obesidad <- df_obesidad |>
  mutate(
    comidas_principales_cat = factor(
      comidas_principales_cat,
      levels = c("1-2", "3", ">3"),
      ordered = TRUE
    )
  )
unique(df_obesidad$comidas_principales_cat) # produce:
#[1] 3   1-2 >3 
#Levels: 1-2 < 3 < >3

# Reposicionar la variable categórica de comidas principales junto
# con su correspondiente variable numérica.
df_obesidad <- df_obesidad |>
  relocate(comidas_principales_cat, .after = comidas_principales)

#################################################
##
## Transformaciones de Consumo de Refrigerios
##
#################################################

# Conocer el tipo de la variable de consumo de refrigerios
class(df_obesidad$come_refrigerios) # produce: "character"
# Conocer los valores de la variable de consumo de refrigerios
unique(df_obesidad$come_refrigerios) # produce:
#[1] "Sometimes"  "Frequently" "0"          "Always" 

# Cambiar a español los valores de la variable de consumo de 
# refigerios
df_obesidad <- df_obesidad |>
  mutate(
    come_refrigerios = recode(
      come_refrigerios,
      "0" = "nunca",
      "Sometimes" = "ocasional",
      "Frequently" = "frecuente",
      "Always" = "siempre"
    )
  )
unique(df_obesidad$come_refrigerios) # produce:
#[1] "ocasional" "frecuente" "nunca"     "siempre" 

# Cambiar de tipo a la variable de consumo de refriegerios de 
# character a factor
df_obesidad <- df_obesidad |>
  mutate(
    come_refrigerios = factor(
      come_refrigerios,
      levels = c("nunca", "ocasional", "frecuente", "siempre"),
      ordered = TRUE
    )
  )
class(df_obesidad$come_refrigerios) # produce: [1] "ordered" "factor" 

######################################################
##
## Transformaciones de Hábito de Fumar
##
######################################################

# Conocer los valores de la variable de hábito de fumar
unique(df_obesidad$fuma) # produce: [1] 0 1
# Conocer el tipo de la variable hábito de fumar
class(df_obesidad$fuma) # produce: "intenger"

# Cambiar el tipo de la variable de hábito de fumar de integer a 
# factor
df_obesidad <- df_obesidad |>
  mutate(
    fuma_cat = factor(
      fuma,
      levels = c(0, 1)
    )
  )
class(df_obesidad$fuma_cat) # produce: "factor"

# Cambiar los valores de la variable de hábito de fumar para que
# cero sea "no" y uno sea "si"
df_obesidad <- df_obesidad |>
  mutate(
    fuma_cat = recode(
      fuma_cat,
      "0" = "no",
      "1" = "si"
    )
  )
unique(df_obesidad$fuma) # produce: [1] 0 1
class(df_obesidad$fuma) # produce: integer
unique(df_obesidad$fuma_cat) # produce:
#[1] no si
#Levels: no si

# Reposicionar la variable categórica de hábito de fumar junto a su
# correspondiente numérica
df_obesidad <- df_obesidad |>
  relocate(fuma_cat, .after = fuma) 

###############################################
##
## Transformaciones Consumo diario de Agua
##
###############################################

# Conocer el tipo de la variable consumo diario de agua
class(df_obesidad$consumo_agua_diaria) # produce: "numeric"
# Conocer los valores de la variable consumo diario de agua  
unique(df_obesidad$consumo_agua_diaria)[1:20] # produce:
#[1] 2.763573 2.000000 1.910378 1.674061 1.979848 2.137550 3.000000
#[8] 2.632253 2.530157 1.959531 1.000000 1.238057 2.724099 2.072194
#[15] 2.609052 2.487070 2.854161 2.632224 1.726109 2.939492
# Valor mínimo de la varaible de consumo diario de agua:
min(df_obesidad$consumo_agua_diaria) # produce: 1
# Valor máximo de la varaible de consumo diario de agua
max(df_obesidad$consumo_agua_diaria) # produce: 3
# NOTA: Puede referirse al consumo en litros diarios

# Cambiar la varaible de consumo de agua diaria de numérica a categórica
df_obesidad <- df_obesidad |>
  mutate(
    consumo_agua_diaria_cat = case_when(
      consumo_agua_diaria <= 1.5 ~ "<1L",
      consumo_agua_diaria <= 2.5 ~ "1-2L",
      consumo_agua_diaria > 2.5 ~ ">2L",
      TRUE ~ NA_character_
    )
  )
class(df_obesidad$consumo_agua_diaria_cat) # produce: "character"

# Cambiar el tipo de la variable de consumo de agua diaria  de
# character a factor
df_obesidad <- df_obesidad |>
  mutate(
    consumo_agua_diaria_cat = factor(
      consumo_agua_diaria_cat,
      levels = c("<1L", "1-2L", ">2L"),
      ordered = TRUE
    )
  )
class(df_obesidad$consumo_agua_diaria_cat) # produce:
#[1] "ordered" "factor"
unique(df_obesidad$consumo_agua_diaria_cat) # produce:
#[1] >2L  1-2L <1L 
#Levels: <1L < 1-2L < >2L
class(df_obesidad$consumo_agua_diaria) # produce: "numeric"

# Reposicionar la variable categórica de consumo de agua diaria
# para que esté junto a su correspondiente numérica
df_obesidad <- df_obesidad |>
  relocate(consumo_agua_diaria_cat, .after = consumo_agua_diaria)

#######################################################
##
## Transformaciones de Consumo de Bebidas Calóricas
##
#######################################################

# Conocer el tipo de la variable de consumo de bebidas calóricas 
class(df_obesidad$toma_bebidas_caloricas) # produce: integer
# Conocer los valores de la varaible de consumo de bebidas calóricas
unique(df_obesidad$toma_bebidas_caloricas) # produce: [1] 0 1

# Cambiar el tipo de la variable de consumo de bebidas calóricas
# de integer a factor
df_obesidad <- df_obesidad |>
  mutate(
    toma_bebidas_caloricas_cat = factor(
      toma_bebidas_caloricas,
      levels = c(0, 1)
    )
  )
class(df_obesidad$toma_bebidas_caloricas_cat) # produce: "factor"

# Cambiar los valores de la variable de consumo de bebidas calóricas
# para que cero sea "0" y uno sea "si"
df_obesidad <- df_obesidad |>
  mutate(
    toma_bebidas_caloricas_cat = recode(
      toma_bebidas_caloricas_cat,
      "0" = "no",
      "1" = "si"
    )
  )
unique(df_obesidad$toma_bebidas_caloricas_cat) # produce: 
#[1] no si
#Levels: no si
class(df_obesidad$toma_bebidas_caloricas) # produce: "integer"

# Reposicionar la variable categórica de consumo de bebidas azucaradas
# para que esté junto a su correspondiente numérica
df_obesidad <- df_obesidad |>
  relocate(toma_bebidas_caloricas_cat, 
           .after = toma_bebidas_caloricas)

############################################
##
## Transformaciones de Actividad Física
##
############################################

# Conocer el tipo de la variable de actividad física
class(df_obesidad$actividad_fisica) # produce: "numeric"
# Conocer los valores de la variable de actividad física 
unique(df_obesidad$actividad_fisica)[1:20] # produce: 
#[1] 0.000000 1.000000 0.866045 1.467863 1.967973 1.930033 0.598655
#[8] 2.000000 1.425712 3.000000 1.995582 1.097905 0.680464 1.191020
#[15] 1.999836 1.465931 0.826660 0.035928 1.628637 1.427413

# Valor mínimo de la variable de actividad física
min(df_obesidad$actividad_fisica) # produce: 0
# Valor máximo de la variable de actividad física
max(df_obesidad$actividad_fisica) # produce: 3

# Cambiar la variable de actividad física de numérica a categórica
df_obesidad <- df_obesidad |>
  mutate(
    actividad_fisica_cat = case_when(
      actividad_fisica <= 0.5 ~ "0_dias",
      actividad_fisica <= 1.5 ~ "1-2_dias",
      actividad_fisica <= 2.5 ~ "3-4_dias",
      actividad_fisica > 2.5 ~ "5+_dias",
      TRUE ~ NA_character_
    )
  )
class(df_obesidad$actividad_fisica_cat) # produce: character

# Cambiar la varaible actividad física de character a factor
df_obesidad <- df_obesidad |>
  mutate(
    actividad_fisica_cat = factor(
      actividad_fisica_cat,
      levels = c("0_dias", "1-2_dias", "3-4_dias", "5+_dias")
    )
  )
unique(df_obesidad$actividad_fisica_cat) # produce:
#[1] 0_dias   1-2_dias 3-4_dias 5+_dias 
#Levels: 0_dias 1-2_dias 3-4_dias 5+_dias

class(df_obesidad$actividad_fisica) # produce: "numeric"

# Reposicionar la variable categórica de actividad física para que
# esté junto a su correpondiente numérica
df_obesidad <- df_obesidad |>
  relocate(actividad_fisica_cat, .after = actividad_fisica)

############################################
##
## Transformaciones de Uso de Tecnología
##
############################################

# Conocer el tipo de la variable de uso de tecnología
class(df_obesidad$uso_tecnologia) # produce: "numeric"
# Conocer los valores de la variablede uso de tecnología
unique(df_obesidad$uso_tecnologia)[1:20] # produce: 
#[1] 0.976473 1.000000 1.673584 0.780199 0.931721 0.696948 0.000000
#[8] 0.218645 0.553311 0.947884 2.000000 0.930836 0.619012 0.081156
#[15] 1.258881 0.079334 0.250502 0.232858 0.453649 0.831412

# Valor mínimo de la variable de uso de tecnología 
min(df_obesidad$uso_tecnologia) # produce: [1] 0
# Valor máximo de la variable de uso de tecnología 
max(df_obesidad$uso_tecnologia) # produce: [1] 2

#  Pasar la varaible de uso de tecnología de numerica a categórica
df_obesidad <- df_obesidad |>
  mutate(
    uso_tecnologia_cat = case_when(
      uso_tecnologia <= 0.5 ~ "0-2h",
      uso_tecnologia <= 1.5 ~ "3-5h",
      uso_tecnologia > 1.5 ~ ">5h"
    )
  )
class(df_obesidad$uso_tecnologia_cat) # produce: character

# Cambiar de tipo a la varaible de uso de tecnología de character
# a factor
df_obesidad <- df_obesidad |>
  mutate(
    uso_tecnologia_cat = factor(
      uso_tecnologia_cat,
      levels = c("0-2h", "3-5h", ">5h"),
      ordered = TRUE
    )
  )
unique(df_obesidad$uso_tecnologia_cat) # produce:
#[1] "3-5h" ">5h"  "0-2h"

# Reposicionar la variable categórica de uso de tecnología para que
# esté junto a su correspondiente numérica
df_obesidad <- df_obesidad |>
  relocate(uso_tecnologia_cat, .after = uso_tecnologia) 

##############################################
##
## Transformaciones de Consumo de Alcohol
##
##############################################

# Conocer el tipo de la varaible de cosumo de alcohol
class(df_obesidad$consume_alcohol) # produce: character
# Conocer los valores de la varaible de consumo de alcohol
unique(df_obesidad$consume_alcohol) # produce: 
# [1] "Sometimes"  "0"          "Frequently"

# Cambiar los vaores de la variable de consumo de alcohol para que
# cero sea "nunca", sometime sea "ocasional" y frequently sea 
# "frecuente"
df_obesidad <- df_obesidad |>
  mutate(
    consume_alcohol = recode(
      consume_alcohol,
      "0" = "nunca",
      "Sometimes" = "ocasional",
      "Frequently" = "frecuente"
    )
  )
unique(df_obesidad$consume_alcohol) # produce: 
#[1] "ocasional" "nunca"     "frecuente"

df_obesidad <- df_obesidad |>
  mutate(
    consume_alcohol = factor(
      consume_alcohol,
      levels = c("nunca", "ocasional", "frecuente"),
      ordered = TRUE
    )
  )
unique(df_obesidad$consume_alcohol) # produce:
#[1] ocasional nunca     frecuente
#Levels: nunca < ocasional < frecuente

######################################################
##
## Transformaciones de Tipo de Medio de Transporte
##
######################################################

# Conocer el tipo de la variable de medio de transporte
class(df_obesidad$medio_transporte) # produce: "character"
# Conocer los valores de la varaible de medio de transporte
unique(df_obesidad$medio_transporte) # produce:
#[1] "Public_Transportation" "Automobile"           
#[3] "Walking"               "Motorbike"            
#[5] "Bike"           

# Cambiar los valores de la variable de medio de transporte de inglés
# a español
df_obesidad <- df_obesidad |>
  mutate(
    medio_transporte = recode(
      medio_transporte,
      "Public_Transportation" = "publico",
      "Automobile" = "auto",
      "Motorbike" = "moto",
      "Bike" = "bicicleta",
      "Walking" = "camina"
    )
  )
unique(df_obesidad$medio_transporte) # produce:
#[1] "publico"   "auto"      "camina"    "moto"      "bicicleta"

# Cambiar el tipo de la variable de medio de transporte de character
# a factor
df_obesidad <- df_obesidad |>
  mutate(
    medio_transporte = factor(
      medio_transporte,
      levels = c("publico", "auto", "moto", "bicicleta", "camina")
    )
  )
class(df_obesidad$medio_transporte) # produce: "factor"

###############################################
##
## Retirar desnutridos y crear 3 categorías: 
## normal, sobrepero y obesidad
##
###############################################

# Excluir de la tabla a los desnutridos (imc < 18.5) pues este análisis
# trata sobre obesidad y, la desnutrición y el bajo peso tienen 
# características psico-socio-económicas distintas al exceso de peso
df_sobre_obesidad <- df_obesidad |>
  filter(tipo_obesidad != "desnutricion")
min(df_sobre_obesidad$imc) # produce: [1] 18.50301

# Reclasificar por peso normal, sobrepeso y obesidad
# imc_grupos3
df_sobre_obesidad <- df_sobre_obesidad |>
  mutate(
    imc_grupos3 = case_when(
      imc < 25 ~ "peso_normal",
      imc < 30 ~ "sobrepeso",
      imc >= 30 ~ "obesidad"#,
      #TRUE ~ NA_character_  # Ya no es necesario porque ya está 
      # filtrado
    )
  )
unique(df_sobre_obesidad$imc_grupos3) # produce:
#[1] "sobrepeso"   "peso_normal" "obesidad" 

# Ordenar a las 3 categorías de peso:
df_sobre_obesidad <- df_sobre_obesidad |>
  mutate(
    imc_grupos3 = factor(
      imc_grupos3,
      levels = c("peso_normal", "sobrepeso", "obesidad"),
      ordered = TRUE # importante si en algún momento se va a 
      # modelar (por ejemplo: regresión lineal)
    )
  )
unique(df_sobre_obesidad$imc_grupos3) # produce:
#[1] sobrepeso   peso_normal obesidad   
#Levels: peso_normal < sobrepeso < obesidad
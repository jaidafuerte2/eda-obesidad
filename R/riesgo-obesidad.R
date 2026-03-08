##########################################################
##                                                      ##  
##   Análisis explortorio de la dataset obesity risk    ##
##                                                      ##
##########################################################

library(tidyverse)
source("eda-obesidad/load-data/load-obesity.R")

##################################
##
## 0.- Índice de Masa Corporal
##
##################################

# Conocer el tipo de la variable de índice de masa corporal 
class(df_obesidad$imc) # produce: "numeric"
# Conocer el tipo de la varaible de índice de masa corporal
unique(df_obesidad$imc)[1:20] # produce:
#[1] 28.25956 23.42209 17.12671 44.85580 25.59915 16.86193 36.61079
#[8] 38.59145 24.22145 41.43135 23.87511 32.99929 43.99566 19.87910
#[15] 35.22807 27.68166 20.54569 36.46328 28.68514 23.50781

# Conocer el valor mínimo del índice de masa corporal
min(df_obesidad$imc) # produce: [1] 12.86854
# Conocer el valor máximo del índice de masa corporal
max(df_obesidad$imc) # produce: [1] 54.99799

# Histograma del índice de masa corporal
ggplot(df_obesidad, aes(x = imc)) +
  geom_histogram(binwidth = 1)

# Gráfico de densidad del índice de masa corporal
ggplot(df_obesidad, aes(x = imc)) +
  geom_density()

#############################
##
## 0.- Tipo de Obesidad
##
#############################

# Conocer el tipo de la varaible de tipo de obesidad
class(df_obesidad$tipo_obesidad) # produce:[1] "ordered" "factor" 
# Conocer los valores de la variable de tipo de obesidad
unique(df_obesidad$tipo_obesidad) # produce:

#[1] sobrepeso    peso_normal  desnutricion obesidad_3   obesidad_2  
#[6] obesidad_1  
#6 Levels: desnutricion < peso_normal < sobrepeso < ... < obesidad_3

# Gráfico de barras de la variable de tipo de obesidad
ggplot(df_obesidad, aes(x = tipo_obesidad)) +
  geom_bar()

#############################
##
## 1.- Género
##
#############################

# Conocer el tipo de la varaible genero
class(df_obesidad$genero) # produce: "factor"
# Conocer los valores de la variable género
unique(df_obesidad$genero) # produce:
#[1] masculino femenino 
#Levels: masculino femenino

# Gráfico de barras de la varaible género
ggplot(df_obesidad, aes(x = genero)) +
  geom_bar()

# Diagrama de caja que relaciona el género con el índice de masa
# corporal  y está facetado por tipo de obesidad
ggplot(df_obesidad, aes(x = genero, y = imc)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Gráfico de barras que relaciona el género con el tipo de obesidad
ggplot(df_obesidad, aes(x = genero, fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupar la tabla de obesidad y resumir por imc y cantidad
obesidad_genero <- df_obesidad |>
  group_by(tipo_obesidad, genero) |>
  summarize(
    med_imc = median(imc, na.rm = TRUE),
    n_cantidad = n(),
    .groups = "keep"
  ) 
obesidad_genero # produce:
# A tibble: 12 × 4
# Groups:   tipo_obesidad, genero [12]
#  tipo_obesidad genero    med_imc n_cantidad
#  <ord>         <fct>       <dbl> <int>
#1 desnutricion  masculino    17.4   876
#2 desnutricion  femenino     17.4  1532
#3 peso_normal   masculino    22.5  1640
#4 peso_normal   femenino     21.8  1886
#5 sobrepeso     masculino    27.3  2991
#6 sobrepeso     femenino     26.9  1749
#7 obesidad_1    masculino    32.9  1923
#8 obesidad_1    femenino     31.9  1195
#9 obesidad_2    masculino    36.6  2799
#10 obesidad_2    femenino     38.9   914
#11 obesidad_3    masculino    41.2   107
#12 obesidad_3    femenino     42.2  3146

# Diagrama de caja que relaciona el género con la cantidad de 
# individuos (agrupados por tipo de obesidad y género), facetado
# por tipo de obesidad
ggplot(obesidad_genero, aes(x = genero, y = n_cantidad)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Diagrama de dispersión que relaciona el índice de masa corporal
# con la cantidad de individuos. Facetado por tipo de obesidad
ggplot(obesidad_genero, aes(x = med_imc, y = n_cantidad)) +
  geom_point(aes(color = genero)) +
  facet_wrap(~tipo_obesidad)

######################
##
## 2.- Edad
##
######################

# Conocer el tipo de la varaible de edad
class(df_obesidad$edad) # produce: "numeric"
# Conocer los valores de la variable de edad
unique(df_obesidad$edad)[1:20] # produce:
#[1] 24.44301 18.00000 20.95274 31.64108 18.12825 29.88302 29.89147
#[8] 17.00000 26.00000 20.00000 22.00000 21.41254 28.37796 34.00000
#[15] 25.49285 19.00000 25.91852 29.74050 41.31830 23.00000
# Conocer el valor mínimo de la varaible edad
min(df_obesidad$edad) # produce:  14
# Conocer el valor máximo de la varaible edad
max(df_obesidad$edad) # produce:  61

# Histograma de la variable de edad 
ggplot(df_obesidad, aes(x = edad)) +
  geom_histogram(binwidth = 1)

# Gráfico de densidad de la variable de edad
ggplot(df_obesidad, aes(x = edad)) +
  geom_density()

# Diagrama de dispersión que relacione la edad con el índice de masa
# corporal
ggplot(df_obesidad, aes(x = imc, y = edad)) +
  geom_point(alpha = 1/20)

# Diagrama de caja que relaciona el tipo de obesidad con la edad
ggplot(df_obesidad, aes(x = tipo_obesidad, y = edad)) +
  geom_boxplot()

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por
# índice de masa corporal y edad
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    med_edad = median(edad, na.rm = TRUE),
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  )
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad med_edad med_imc
#  <ord>            <dbl>   <dbl>
#1 desnutricion      19      17.4
#2 peso_normal       21      22.1
#3 sobrepeso         22      27.1
#4 obesidad_1        23      32.5
#5 obesidad_2        26.0    37.1
#6 obesidad_3        26.0    42.1

#############################################
##
## 3.- Historia familiar de exceso de peso 
##
#############################################

# Conocer el tipo de la variable de historia familiar de obesidad 
class(df_obesidad$tiene_familiares_obesos_cat) # produce: "factor"
# Conocer el tipo de valores de la variable de historia familiar
# de obesidad
unique(df_obesidad$tiene_familiares_obesos_cat) # produce:
#[1] si no
#Levels: no si

# Diagrama de caja que relaciona la historia familiar de obesidad con
# el índice de masa corporal
ggplot(df_obesidad, aes(x = tiene_familiares_obesos_cat, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona la historia familiar de exceso de
# peso con el tipo de obesida
ggplot(df_obesidad, aes(x = tiene_familiares_obesos_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por
# porcentaje de personas que sí tienen familiares obesos
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_familiares = sum(tiene_familiares_obesos_cat == "si", 
                         na.rm = TRUE),
    porc_si_familiares = mean(tiene_familiares_obesos_cat == "si",
                           na.rm = TRUE) * 100,
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad sum_familiares porc_si_familiares
#  <ord>                  <int>              <dbl>
#1 desnutricion            1005               41.7
#2 peso_normal             2078               58.9
#3 sobrepeso               3910               82.5
#4 obesidad_1              3060               98.1
#5 obesidad_2              3710               99.9
#6 obesidad_3              3251               99.9

# Agrupar la tabla de obesidad (con 3 grupos de tipo según) por
# tipo de obesidad y resumir por porcentaje de personas que sí tienen
# historia familiar de exceso de peso
df_sobre_obesidad |>
  group_by(imc_grupos3) |>
  summarise(
    sum_familiares = sum(tiene_familiares_obesos_cat == "si", 
                         na.rm = TRUE),
    porc_si_familiares = mean(tiene_familiares_obesos_cat == "si",
                              na.rm = TRUE) * 100,
    .groups = "keep"
  ) # produce:
# A tibble: 3 × 3
# Groups:   imc_grupos3 [3]
#  imc_grupos3 sum_familiares porc_si_familiares
#  <ord>                <int>              <dbl>
#1 peso_normal           2078               58.9
#2 sobrepeso             3910               82.5
#3 obesidad             10021               99.4

###############################
##
## 4.- Consumo de comida de 
## alta densidad calórica
##
###############################

# Conocer el tipo de la variable de consumo de comida de alta densidad 
class(df_obesidad$come_densidad_alta_cat) # produce: "factor"
# Conocer los valores de la variable de consumo de comida de alta 
# densidad
unique(df_obesidad$come_densidad_alta_cat) # produce:
#[1] si no
#Levels: no si

# Diagrama de caja que relaciona el consumo de comida de alta densidad
# con el índice de masa corporal
ggplot(df_obesidad, aes(x = come_densidad_alta_cat, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona el consumo de comida de alta densidad
# con el tipo de obesidad
ggplot(df_obesidad, aes(x = come_densidad_alta_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por
# los que sí comen comida de alta densidad calórica
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    porc_si_densidad_alta = mean(come_densidad_alta_cat == "si",
                                 na.rm = TRUE) * 100,
    .groups = "keep"
  )
# A tibble: 6 × 2
# Groups:   tipo_obesidad [6]
#  tipo_obesidad porc_si_densidad_alta
#  <ord>                         <dbl>
#1 desnutricion                   84.9
#2 peso_normal                    84.1
#3 sobrepeso                      85.8
#4 obesidad_1                     96.2
#5 obesidad_2                     98.7
#6 obesidad_3                     99.8

# Agrupar la tabla de obesidad por 3 tipos de peso según índice
# de masa corporal y resumir por los que sí comen comida de alta 
# densidad calórica
df_sobre_obesidad |>
  group_by(imc_grupos3) |>
  summarise(
    porc_si_densidad_alta = mean(come_densidad_alta_cat == "si",
                                 na.rm = TRUE) * 100,
    .groups = "keep"
  ) # produce:
# A tibble: 3 × 2
# Groups:   imc_grupos3 [3]
#  imc_grupos3 porc_si_densidad_alta
#  <ord>                       <dbl>
#1 peso_normal                  84.1
#2 sobrepeso                    85.8
#3 obesidad                     98.2

##################################
##
## 5.- Consumo de vegetales
##
##################################

# Conocer el tipo de la variable de consumo de vegetales
class(df_obesidad$consumo_vegetales) # produce: "numeric"
# Conocer los valores de la variable de consumo de vegetales
unique(df_obesidad$consumo_vegetales)[1:20] # produce:
#[1] 2.000000 1.880534 3.000000 2.679664 2.919751 1.991240 1.397468
#[8] 2.636719 1.000000 1.392665 2.203962 2.971588 2.668949 1.989899
#[15] 2.417635 2.219186 2.919526 2.263245 2.649406 1.754401
# Conocer el valor mínimo de la variable de consumo de vegetales
min(df_obesidad$consumo_vegetales) # produce: 1
# Conocer el valor máximo de la variable de consumo de vegetales
max(df_obesidad$consumo_vegetales) # produce: 3

# Diagrama de dispersión que relaciona el consumo de vegetales 
# con e índice de masa corporal
ggplot(df_obesidad, aes(x = consumo_vegetales, y = imc)) +
  geom_point(alpha = 1/20)

# Diagrama de caja que realaciona el tipo de obesidad con el
# consumo de vagetales
ggplot(df_obesidad, aes(x = tipo_obesidad, y = consumo_vegetales)) +
  geom_boxplot()

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por 
# consumo de vegetales
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    med_vegetales = median(consumo_vegetales, na.rm = TRUE),
    .groups = "keep"
  )
# A tibble: 6 × 2
# Groups:   tipo_obesidad [6]
#  tipo_obesidad med_vegetales
#  <ord>                 <dbl>
#1 desnutricion           2.75
#2 peso_normal            2   
#3 sobrepeso              2   
#4 obesidad_1             2   
#5 obesidad_2             2.72
#6 obesidad_3             3 

# Agrupar la tabla de obesidad por tipo de peso (acon 3 categorías) 
# y resumir por  consumo de vegetales
df_sobre_obesidad |>
  group_by(imc_grupos3) |>
  summarise(
    med_vegetales = median(consumo_vegetales, na.rm = TRUE),
    .groups = "keep"
  )
# A tibble: 3 × 2
# Groups:   imc_grupos3 [3]
#  imc_grupos3 med_vegetales
#  <ord>               <dbl>
#1 peso_normal          2   
#2 sobrepeso            2   
#3 obesidad             2.93

######### Consumo de vegetales categórica ###########

# Diagrama de caja que relaciona el consumo de vegetales con el 
# índice de masa corporal
ggplot(df_obesidad, aes(x = consumo_vegetales_cat, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona el consumo de vegetales con el
# tipo de obesidad
ggplot(df_obesidad,aes(x = consumo_vegetales_cat, 
                       fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupar por consumo de vegetales y resumir por índice de masa 
# corporal
df_obesidad |>
  group_by(consumo_vegetales_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 3 × 2
# Groups:   consumo_vegetales_cat [3]
#  consumo_vegetales_cat med_imc
#  <ord>                   <dbl>
#1 nunca                    27.7
#2 ocasional                27.8
#3 siempre                  35.6

#############################
##
## 6.- Comidas principales
##
#############################

# Conocer el tipo de la variable de comidas principales
class(df_obesidad$comidas_principales) # produce: "numeric"
# Conocer los valores de la varaible de comidas principales
unique(df_obesidad$comidas_principales)[1:20] # produce:
#[1] 2.983297 3.000000 1.411685 1.971472 2.164839 1.000000 2.954446
#[8] 1.893811 3.998618 1.703299 2.937989 2.996444 2.581015 2.473913
#[15] 1.437959 2.989791 4.000000 2.853676 1.104642 3.362758

# Conocer el valor mínimo de la variable de comidas principales
min(df_obesidad$comidas_principales) # produce: 1
# Conocer el valor máximo de la variable de comidas principales
max(df_obesidad$comidas_principales) # produce: 4

# Diagrama de dispersión que relaciona las comidas principales
# con el índice de masa corporal
ggplot(df_obesidad, aes(x = comidas_principales, y = imc)) +
  geom_point(alpha = 1/50)

# Diagrama de caja que relaciona el tipo de obesidad con las comidas
# principales
ggplot(df_obesidad, aes(x = tipo_obesidad, y = comidas_principales)) +
  geom_boxplot()

# Agrupar la tabla obesidad por tipo de obesidad y resumir por
# comidas principales
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    med_principales = median(comidas_principales, na.rm = TRUE),
    avg_principales = mean(comidas_principales, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
#A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad med_principales avg_principales
#  <ord>                   <dbl>           <dbl>
#1 desnutricion                3            2.89
#2 peso_normal                 3            2.86
#3 sobrepeso                   3            2.56
#4 obesidad_1                  3            2.50
#5 obesidad_2                  3            2.85
#6 obesidad_3                  3            2.99

# Agrupar la tabla obesidad por 3 tipos de peso y resumir por
# comidas principales
df_sobre_obesidad |>
  group_by(imc_grupos3) |>
  summarise(
    med_principales = median(comidas_principales, na.rm = TRUE),
    avg_principales = mean(comidas_principales, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 3 × 3
# Groups:   imc_grupos3 [3]
#  imc_grupos3 med_principales avg_principales
#  <ord>                 <dbl>           <dbl>
#1 peso_normal               3            2.86
#2 sobrepeso                 3            2.56
#3 obesidad                  3            2.79

########## Comidas principales categórica ##########

# Diagrama de caja que relaciona las comidas principales con el
# índice de masa corporal
ggplot(df_obesidad, aes(x = comidas_principales_cat, y = imc)) +
  geom_boxplot()

# Grafico de barras que relaciona las comidas principales con el 
# tipo de obesidad
ggplot(df_obesidad, aes(x = comidas_principales_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill")

df_obesidad |>
  group_by(comidas_principales_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# Groups:   comidas_principales_cat [3]
#  comidas_principales_cat med_imc
#  <ord>                     <dbl>
#1 1-2                        27.4
#2 3                          31.9
#3 >3                         18.6

##################################
##
## 7.- Consumo de refrigerios
##
##################################

# Conocer el tipo de la varaible de consumo de refrigerios
class(df_obesidad$come_refrigerios) # produce:
#[1] "ordered" "factor" 
# Conocer los valores de la varaible de consumo de refrigerios
unique(df_obesidad$come_refrigerios) # produce:
# [1] ocasional frecuente nunca     siempre  
# Levels: nunca < ocasional < frecuente < siempre

# Diagrama de caja que relaciona el consumo de refrigerios con el
# índice de masa corporal
ggplot(df_obesidad, aes(x = come_refrigerios, y = imc)) + 
  geom_boxplot()

# Gráfico de barras que relaciona el consumo de refrigerios con el
# tipo de obesidad
ggplot(df_obesidad, aes(x = come_refrigerios, fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# 
df_obesidad |>
  group_by(come_refrigerios) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  )

###########################
##
## 8.- Hábito de fumar
##
###########################

# Conocer el tipo  de la variable de hábito de fumar 
class(df_obesidad$fuma_cat) # produce: "factor"
# Conocer los valores de la variable de hábito de fumar
unique(df_obesidad$fuma_cat) # produce:
#[1] no si
#Levels: no si

# Diagrama de caja que relaciona el hábito de fumar con el índice de
# masa corporal
ggplot(df_obesidad, aes(x = fuma_cat, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona el hábito de fumar con el tipo
# de obesidad
ggplot(df_obesidad, aes(x = fuma_cat, fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupar tabla de obesidad por hábito de fumar y resumir por índice
# de masa corporal
df_obesidad |>
  group_by(fuma_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 2 × 2
# Groups:   fuma_cat [2]
#  fuma_cat med_imc
#  <fct>      <dbl>
#1 no          29.4
#2 si          32.9
  
#################################
##
## 9.- Consumo diario de agua
##
#################################

class(df_obesidad$consumo_agua_diaria) # produce: "numeric"
unique(df_obesidad$consumo_agua_diaria)[1:20] # produce:
#[1] 2.763573 2.000000 1.910378 1.674061 1.979848 2.137550 3.000000
#[8] 2.632253 2.530157 1.959531 1.000000 1.238057 2.724099 2.072194
#[15] 2.609052 2.487070 2.854161 2.632224 1.726109 2.939492

# Conocer el valor mínimo de consumo diario de agua
min(df_obesidad$consumo_agua_diaria) # produce: 1
# Conocer el valor máximo de consumo diario de agua
max(df_obesidad$consumo_agua_diaria) # produce: 3

# Diagrama de dispersión que relaciona el consumo diario de agua 
# con el índice de masa corporal
ggplot(df_obesidad, aes(x = consumo_agua_diaria, y = imc)) +
  geom_point(alpha = 1/10)

# Diagrama  de caja que relaciona el tipo de obesidad con el
# consumo diario de agua
ggplot(df_obesidad, aes(x = tipo_obesidad, y = consumo_agua_diaria)) +
  geom_boxplot()

# Agrupar la tabla de obesidad por tipo de obesidad y consumo 
# de agua
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    med_agua = median(consumo_agua_diaria, na.rm = TRUE),
    avg_agua = mean(consumo_agua_diaria, na.rm = TRUE),
    .groups = "keep"
  )
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad med_agua avg_agua
#  <ord>            <dbl>    <dbl>
#1 desnutricion      2        1.73
#2 peso_normal       2        1.85
#3 sobrepeso         2        2.03
#4 obesidad_1        2        2.08
#5 obesidad_2        2.02     2.00
#6 obesidad_3        2.63     2.43

########################################
##
## 10.- Consumo de bebidas azucaradas
##
########################################

# Conocer el tipo de la variable de consumo de bebidas calóricas
class(df_obesidad$toma_bebidas_caloricas) # produce: "factor"
# Conocer los valores de la variable de consumo de bebidas calóricas
unique(df_obesidad$toma_bebidas_caloricas) # produce:
#[1] no si
#Levels: no si

#  Diagrama de caja que relacionoa el consumo de bebidas azucaradas
# con el índice de masa corporal
ggplot(df_obesidad, aes(x = toma_bebidas_caloricas, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona el consumo de bebidas azucaradas
# con el tipo de obesidad
ggplot(df_obesidad, aes(x = toma_bebidas_caloricas, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill")

df_obesidad |>
  group_by(toma_bebidas_caloricas) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 2 × 2
# Groups:   toma_bebidas_caloricas [2]
#  toma_bebidas_caloricas med_imc
#  <fct>                    <dbl>
#1 no                        30.1
#2 si                        22.5

df_obesidad |>
  group_by(tipo_obesidad, toma_bebidas_caloricas) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  )

#################################
##
## 11.- Actividad Física
##
#################################

# Conocer el tipo de la variable de actividad física 
class(df_obesidad$actividad_fisica) # produce: "numeric"
# Conocer los valores de la variable de actividad física
unique(df_obesidad$actividad_fisica)[1:20] # produce:
#[1] 0.000000 1.000000 0.866045 1.467863 1.967973 1.930033 0.598655
#[8] 2.000000 1.425712 3.000000 1.995582 1.097905 0.680464 1.191020
#[15] 1.999836 1.465931 0.826660 0.035928 1.628637 1.427413

# Conocer el valor mínimo de la varaible de actividad física
min(df_obesidad$actividad_fisica) # produce: 0
# Conocer el valor máximo de la varaible de actividad física
max(df_obesidad$actividad_fisica) # produce: 3

ggplot(df_obesidad, aes(x = actividad_fisica, y = imc)) +
  geom_point(alpha = 1/10)

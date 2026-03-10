##########################################################
##                                                      ##  
##   Análisis explortorio de la dataset obesity risk    ##
##                                                      ##
##########################################################

library(tidyverse)
library(scales)
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

# Diagrama de caja que relaciona el género con el índice de masa
# corporal  y está facetado por tipo de obesidad
ggplot(df_obesidad, aes(x = genero, y = imc)) +
  geom_boxplot() 

# Gráfico de barras que relaciona el género con el tipo de obesidad
ggplot(df_obesidad, aes(x = genero, fill = tipo_obesidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    color = "Tipo de obesidad",
    title = "El porcentaje de obesidad grado 3 en mujeres es mayor",
    subtitle = "Pero el porcentaje de obesidad grado 2 es mayor en hombres",
    caption = "Datos del dataset de riesgo de obesidad de kaggle"
  )

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

# Agrupar la tabla obesidad-género y crear una nueva columna de
# porcentajes
obesidad_genero |>
  group_by(genero) |>
  mutate(
    porcentaje = n_cantidad / sum(n_cantidad) * 100
  ) # produce:
# A tibble: 12 × 5
# Groups:   genero [2]
#  tipo_obesidad genero    med_imc n_cantidad porcentaje
#  <ord>         <fct>       <dbl>      <int>      <dbl>
#1 desnutricion  masculino    17.4        876       8.48
#2 desnutricion  femenino     17.4       1532      14.7 
#3 peso_normal   masculino    22.5       1640      15.9 
#4 peso_normal   femenino     21.8       1886      18.1 
#5 sobrepeso     masculino    27.3       2991      28.9 
#6 sobrepeso     femenino     26.9       1749      16.8 
#7 obesidad_1    masculino    32.9       1923      18.6 
#8 obesidad_1    femenino     31.9       1195      11.5 
#9 obesidad_2    masculino    36.6       2799      27.1 
#10 obesidad_2    femenino     38.9        914       8.77
#11 obesidad_3    masculino    41.2        107       1.04
#12 obesidad_3    femenino     42.2       3146      30.2 

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
ggplot(df_obesidad, aes(x = edad, y = imc)) +
  geom_point(alpha = 1/50) 

# Diagrama de caja que relaciona el tipo de obesidad con la edad
ggplot(df_obesidad, aes(x = tipo_obesidad, y = edad)) +
  geom_boxplot(alpha = 1/20) +
  labs(
    y = "Edad (años)",
    x = "Tipo de obesidad (según OMS)",
    title = "A mayor edad es mayor la obesidad",
    subtitle = "Pero no es una diferencia mayor",
    caption = "Datos del dataset de riesgo de obesidad de kaggle"
  )


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
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    x = "Historia familiar de obesidad",
    color = "Tipo de obesidad",
    title = "Sin historia familiar de obesidad casi no hay obesidad",
    subtitle = "Con historia familiar de obesidad hay mucha obesidad",
    caption = "Datos del dataset de riesho de obesidad de Kaggle"
  )

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

df_obesidad |>
  group_by(tiene_familiares_obesos_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 2 × 2
# Groups:   tiene_familiares_obesos_cat [2]
#  tiene_familiares_obesos_cat med_imc
#  <fct>                         <dbl>
#1 no                             20.8
#2 si                             32.4

# Agrupar la tabla de obesidad por historia familiar de oebsidad y
# Tipo de obesidad y resumir por cantidad
obesidad_familiar <- df_obesidad |>
  group_by(tiene_familiares_obesos_cat, tipo_obesidad) |>
  summarise(
    n_cantidad = n(),
    .groups = "keep"
  )
obesidad_familiar  # produce:
# A tibble: 12 × 4
# Groups:   tiene_familiares_obesos_cat, tipo_obesidad [12]
#  tiene_familiares_obesos_cat tipo_obesidad med_imc n_cantidad
#  <fct>                       <ord>           <dbl>      <int>
#1 no                          desnutricion     17.3       1403
#2 no                          peso_normal      21.9       1448
#3 no                          sobrepeso        26.6        830
#4 no                          obesidad_1       32.0         58
#5 no                          obesidad_2       35.6          3
#6 no                          obesidad_3       41.2          2
#7 si                          desnutricion     17.5       1005
#8 si                          peso_normal      22.2       2078
#9 si                          sobrepeso        27.4       3910
#10 si                          obesidad_1       32.5       3060
#11 si                          obesidad_2       37.1       3710
#12 si                          obesidad_3       42.1       3251

# Volver a agrupar la tabla y crear una variable de porcentaje 
obesidad_familiar |>
  group_by(tiene_familiares_obesos_cat) |>
  mutate(
    porcentaje = n_cantidad / sum(n_cantidad) * 100
  ) # produce:
# A tibble: 12 × 4
# Groups:   tiene_familiares_obesos_cat [2]
#  tiene_familiares_obesos_cat tipo_obesidad n_cantidad porcentaje
#  <fct>                       <ord>              <int>      <dbl>
#1 no                          desnutricion        1403    37.5   
#2 no                          peso_normal         1448    38.7   
#3 no                          sobrepeso            830    22.2   
#4 no                          obesidad_1            58     1.55  
#5 no                          obesidad_2             3     0.0801
#6 no                          obesidad_3             2     0.0534
#7 si                          desnutricion        1005     5.91  
#8 si                          peso_normal         2078    12.2   
#9 si                          sobrepeso           3910    23.0   
#10 si                          obesidad_1          3060    18.0   
#11 si                          obesidad_2          3710    21.8   
#12 si                          obesidad_3          3251    19.1 


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
  geom_boxplot(alpha = 1/5) +
  labs(
    x = "Consumo de comida de alta densidad calórica",
    y = "Indice de Masa Corporal",
    title = "Los que no comen alimentos de alta densidad suelen ser delgados",
    subtitle = "Los que sí comen estos alimentos suelen ser obesos",
    caption = "datos del dataset de riesgo de obesidad de Kaggle"
  )

# Gráfico de barras que relaciona el consumo de comida de alta densidad
# con el tipo de obesidad
ggplot(df_obesidad, aes(x = come_densidad_alta_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    x = "Consumo de comida de alta densidad",
    title = "Los que no comen comida densa tienen poca obesidad",
    subtitle = "Los que sí comen tienen más obesidad",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  )

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por
# los que sí comen comida de alta densidad calórica
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    porcentaje = mean(come_densidad_alta_cat == "si",
                                 na.rm = TRUE) * 100,
    .groups = "keep"
  )
# A tibble: 6 × 2
# Groups:   tipo_obesidad [6]
#  tipo_obesidad            porcentaje
#  <ord>                         <dbl>
#1 desnutricion                   84.9
#2 peso_normal                    84.1
#3 sobrepeso                      85.8
#4 obesidad_1                     96.2
#5 obesidad_2                     98.7
#6 obesidad_3                     99.8


# Agrupar la tabla de obesidad por comida de alta densidad calórica
# y resumir por imc
df_obesidad |>
  group_by(come_densidad_alta_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 2 × 2
# Groups:   come_densidad_alta_cat [2]
#  come_densidad_alta_cat med_imc
#  <fct>                    <dbl>
#1 no                        24.7
#2 si                        31.0

# Agrupar la tabla de obesidad por consumo de comida de alta densidad
# calórica y tipo de obesidad y resumir por cantidad
obesidad_densidad <- df_obesidad |>
  group_by(come_densidad_alta_cat, tipo_obesidad) |>
  summarise(
    n_cantidad = n(),
    .groups = "keep"
  ) 
obesidad_densidad # produce:
# A tibble: 12 × 4
# Groups:   come_densidad_alta_cat, tipo_obesidad [12]
#  come_densidad_alta_cat tipo_obesidad med_imc n_cantidad
#  <fct>                  <ord>           <dbl>      <int>
#1 no                     desnutricion     17.5        364
#2 no                     peso_normal      22.4        562
#3 no                     sobrepeso        27.2        673
#4 no                     obesidad_1       32.1        119
#5 no                     obesidad_2       36.7         50
#6 no                     obesidad_3       40.8          8
#7 si                     desnutricion     17.4       2044
#8 si                     peso_normal      22.0       2964
#9 si                     sobrepeso        27.1       4067
#10 si                     obesidad_1       32.5       2999
#11 si                     obesidad_2       37.1       3663
#12 si                     obesidad_3       42.1       3245

# Agrupar la tabla por consumo de alimentos de alta densidad y crear
# una varaible para conocer los porcentajes
obesidad_densidad |>
  group_by(come_densidad_alta_cat) |>
  mutate(
    porcentaje = n_cantidad / sum(n_cantidad) * 100
  ) # produce:
# A tibble: 12 × 4
# Groups:   come_densidad_alta_cat [2]
#  come_densidad_alta_cat tipo_obesidad n_cantidad porcentaje
#  <fct>                  <ord>              <int>      <dbl>
#1 no                     desnutricion         364     20.5  
#2 no                     peso_normal          562     31.6  
#3 no                     sobrepeso            673     37.9  
#4 no                     obesidad_1           119      6.70 
#5 no                     obesidad_2            50      2.82 
#6 no                     obesidad_3             8      0.450
#7 si                     desnutricion        2044     10.8  
#8 si                     peso_normal         2964     15.6  
#9 si                     sobrepeso           4067     21.4  
#10 si                     obesidad_1          2999     15.8  
#11 si                     obesidad_2          3663     19.3  
#12 si                     obesidad_3          3245     17.1  

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

######### Consumo de vegetales categórica ###########

# Diagrama de caja que relaciona el consumo de vegetales con el 
# índice de masa corporal
ggplot(df_obesidad, aes(x = consumo_vegetales_cat, y = imc)) +
  geom_boxplot() + 
  labs(
    x = "Consumo de vegetales",
    y = "Índice de Masa Corporal",
    title = "Quienes siempre comen vegetales tienen un peso mayor",
    subtitle = "Los que siempre comen vegetales tienen un imc mayor a 35",
    caption= "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()

# Gráfico de barras que relaciona el consumo de vegetales con el
# tipo de obesidad
ggplot(df_obesidad,aes(x = consumo_vegetales_cat, 
                       fill = tipo_obesidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    x = "Consumo de Vegetales",
    color = "Tipo de Obesidad",
    title = "Comer siempre vegetales muestra alto porcentaje de obesidad grado 3",
    subtitle = "Comer vegetales nunca y ocasional muestra poca obesidad grado 3",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()

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

# Contar por tipo de obesidad y consumo de vegetales, agrupar 
# por tipo de obesidad y elegir el máximo. Sacar la moda de 
# consumo de vegetales por tipo de obesidad.
df_obesidad |>
  count(tipo_obesidad, consumo_vegetales_cat) |>
  group_by(tipo_obesidad) |>
  slice_max(n, n = 1) #  produce:
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad consumo_vegetales_cat     n
#  <ord>         <ord>                 <int>
#1 desnutricion  siempre                1369
#2 peso_normal   ocasional              1991
#3 sobrepeso     ocasional              3351
#4 obesidad_1    ocasional              2257
#5 obesidad_2    siempre                2126
#6 obesidad_3    siempre                3190

# Moda del consumo de vegetales con 3 tipos de peso 
df_sobre_obesidad |>
  count(imc_grupos3, consumo_vegetales_cat) |>
  group_by(imc_grupos3) |>
  slice_max(n, n = 1) # produce:
# A tibble: 3 × 3
# Groups:   imc_grupos3 [3]
#  imc_grupos3 consumo_vegetales_cat     n
#  <ord>       <ord>                 <int>
#1 peso_normal ocasional              1991
#2 sobrepeso   ocasional              3351
#3 obesidad    siempre                6052

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

# Agrupar la tabla de obesidad por consumo de comidas principales y
# resumir por índice de masa corporal
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

# Agrupar la tabla obesidad por consumo de refrigerios y rusumir
# por índice de masa corporal
df_obesidad |>
  group_by(come_refrigerios) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 4 × 3
# Groups:   come_refrigerios [4]
#  come_refrigerios med_imc avg_imc
#  <ord>              <dbl>   <dbl>
#1 nunca               26.3    25.6
#2 ocasional           32.0    31.8
#3 frecuente           19.1    21.0
#4 siempre             23.5    24.3

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

# Conocer el tipo de la variable consumo diario de agua
class(df_obesidad$consumo_agua_diaria) # produce: "numeric"
# Conocer los valores de la varaibles de consumo de agua
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

######### Agua diaria categórica ###########

# Diagrama de caja que relaciona el consumo de agua con el índice
# de masa corporal
ggplot(df_obesidad, aes(x = consumo_agua_diaria_cat, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona el consumo de agua con el tipo de
# obesidad
ggplot(df_obesidad, aes(x = consumo_agua_diaria_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupa la tabla de obsidad por consumo diario de agua y resumir 
# por índice de masa corporal
df_obesidad |>
  group_by(consumo_agua_diaria_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 3 × 2
# Groups:   consumo_agua_diaria_cat [3]
#  consumo_agua_diaria_cat med_imc
#  <ord>                     <dbl>
#1 <1L                        27.0
#2 1-2L                       27.8
#3 >2L                        36.6

########################################
##
## 10.- Consumo de bebidas azucaradas
##
########################################

# Conocer el tipo de la variable de consumo de bebidas calóricas
class(df_obesidad$toma_bebidas_caloricas_cat) # produce: "factor"
# Conocer los valores de la variable de consumo de bebidas calóricas
unique(df_obesidad$toma_bebidas_caloricas_cat) # produce:
#[1] no si
#Levels: no si

#  Diagrama de caja que relaciona el consumo de bebidas azucaradas
# con el índice de masa corporal
ggplot(df_obesidad, aes(x = toma_bebidas_caloricas_cat, y = imc)) +
  geom_boxplot() +
  labs(
    x = "Consumo de bebidas calóricas",
    y = "índice de Masa Corporal",
    title = "Quienes toman bebidas calóricas son más delgados",
    subtitle = "Son más delgados y tienen un imc menor a 25",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()

# Gráfico de barras que relaciona el consumo de bebidas azucaradas
# con el tipo de obesidad
ggplot(df_obesidad, aes(x = toma_bebidas_caloricas_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    x = "Consumo de bebidas calóricas",
    title = "Tomar bebidas azucaradas tiene menos obesidad",
    subtitle = "Mientras no tomar tiene mucha obesidad"
  ) +
  theme_linedraw()

# Agrupar por consumo de bebidas calóricas y resumir por índice de 
# masa corporal
df_obesidad |>
  group_by(toma_bebidas_caloricas_cat) |>
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


obesidad_bebidas <- df_obesidad |>
  group_by(toma_bebidas_caloricas_cat, tipo_obesidad) |>
  summarise(
    n_cantidad = n(),
    .groups = "keep"
  )
obesidad_bebidas  # produce:
# A tibble: 12 × 3
# Groups:   toma_bebidas_caloricas_cat, tipo_obesidad [12]
#  toma_bebidas_caloricas_cat tipo_obesidad n_cantidad
#  <fct>                      <ord>              <int>
#1 no                         desnutricion        2183
#2 no                         peso_normal         3307
#3 no                         sobrepeso           4516
#4 no                         obesidad_1          3106
#5 no                         obesidad_2          3708
#6 no                         obesidad_3          3251
#7 si                         desnutricion         225
#8 si                         peso_normal          219
#9 si                         sobrepeso            224
#10 si                         obesidad_1            12
#11 si                         obesidad_2             5
#12 si                         obesidad_3             2


obesidad_bebidas |>
  group_by(toma_bebidas_caloricas_cat) |>
  mutate(
    porcentaje = n_cantidad / sum(n_cantidad) * 100
  ) |>
  relocate(porcentaje, .before = n_cantidad) # produce:
# A tibble: 12 × 4
# Groups:   toma_bebidas_caloricas_cat [2]
#  toma_bebidas_caloricas_cat tipo_obesidad porcentaje n_cantidad
#  <fct>                      <ord>              <dbl>      <int>
#1 no                         desnutricion      10.9         2183
#2 no                         peso_normal       16.5         3307
#3 no                         sobrepeso         22.5         4516
#4 no                         obesidad_1        15.5         3106
#5 no                         obesidad_2        18.5         3708
#6 no                         obesidad_3        16.2         3251
#7 si                         desnutricion      32.8          225
#8 si                         peso_normal       31.9          219
#9 si                         sobrepeso         32.6          224
#10 si                         obesidad_1         1.75          12
#11 si                         obesidad_2         0.728          5
#12 si                         obesidad_3         0.291          2

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

# Diagrama de dispersión que relaciona la actividad física con el 
# índice de masa corporal
ggplot(df_obesidad, aes(x = actividad_fisica, y = imc)) +
  geom_point(alpha = 1/10)

# Diagrama de caja que relaciona el tipo de obesidad con la actividad
# física
ggplot(df_obesidad, aes(x = tipo_obesidad, y = actividad_fisica)) +
  geom_boxplot()

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por
# actividad física
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    med_imc = median(actividad_fisica, na.rm = TRUE),
    avg_imc = mean(actividad_fisica, na.rm = TRUE),
    .groups = "keep"
  )

######## Análisis de Actividad Física Categórica #########

# Diagrama de caja que relaciona la actividad física con el índice
# de masa corporal
ggplot(df_obesidad, aes(x = actividad_fisica_cat, 
                        y = imc)) +
  geom_boxplot() +
  labs(
    x = "Actividad Física",
    y = "índice de Masa Corporal",
    title = "A menos actividad física mayor peso",
    subtitle = "Incluso haciendo 5 días ejercicio el imc es mayor a 25",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()

# Gráfico de barras que relaciona la actividad física con el
# índice de masa corporal
ggplot(df_obesidad, aes(x = actividad_fisica_cat, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill")

# Agrupar la tabla obesidad por actividad física y resumir por índice
# de masa corporal
df_obesidad |>
  group_by(actividad_fisica_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    #avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 4 × 2
# Groups:   actividad_fisica_cat [4]
#  actividad_fisica_cat med_imc
#  <fct>                  <dbl>
#1 0_dias                  33.0
#2 1-2_dias                28.7
#3 3-4_dias                27.3
#4 5+_dias                 26.0

# Contar por tipo de obesidad y actividad física, agrupar por tipo
# de obesidad y elegir el máximo. Sacar la moda de actividad física
# por tipo de obesidad
df_obesidad |>
  count(tipo_obesidad, actividad_fisica_cat) |>
  group_by(tipo_obesidad) |>
  slice_max(n, n = 1) #  produce:
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad actividad_fisica_cat     n
#  <ord>         <fct>                <int>
#1 desnutricion  3-4_dias               890
#2 peso_normal   1-2_dias              1481
#3 sobrepeso     1-2_dias              2219
#4 obesidad_1    1-2_dias              1279
#5 obesidad_2    1-2_dias              1804
#6 obesidad_3    0_dias                1840

# Moda con 3 tipos de peso 
df_sobre_obesidad |>
  count(imc_grupos3, actividad_fisica_cat) |>
  group_by(imc_grupos3) |>
  slice_max(n, n = 1) # produce:
# A tibble: 3 × 3
# Groups:   imc_grupos3 [3]
#  imc_grupos3 actividad_fisica_cat     n
#  <ord>       <fct>                <int>
#1 peso_normal 1-2_dias              1481
#2 sobrepeso   1-2_dias              2219
#3 obesidad    0_dias                4307

###################################
##
## 12.- Uso de tecnología
##
###################################

# Conocer el tipo de la varaible de uso de tecnología
class(df_obesidad$uso_tecnologia) # produce: "numeric"
# Conocer los valores de la varaible de uso de tecnología
unique(df_obesidad$uso_tecnologia)[1:20] # produce:
#[1] 0.976473 1.000000 1.673584 0.780199 0.931721 0.696948 0.000000
#[8] 0.218645 0.553311 0.947884 2.000000 0.930836 0.619012 0.081156
#[15] 1.258881 0.079334 0.250502 0.232858 0.453649 0.831412
# Conocer el valor mínimo de la variable de uso de tecnología
min(df_obesidad$uso_tecnologia) # produce: 0
# Conocer el valor mpaximo de la variable de uso de tecnología 
max(df_obesidad$uso_tecnologia) # produce:2

# Diagrama de dispersión que muestra
ggplot(df_obesidad, aes(x = uso_tecnologia, y = imc)) +
  geom_point(alpha = 1/10)

# Diagrama de caja que relaciona el tipo de obesidad con el uso de
# tecnología
ggplot(df_obesidad, aes(x = tipo_obesidad, y = uso_tecnologia)) +
  geom_boxplot()

# Agrupar la tabla obesidad por tipo de obesidad y resumir por
# uso de tecnología
df_obesidad |>
  group_by(tipo_obesidad) |>
  summarise(
    med_tecnologia = median(uso_tecnologia, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 2
# Groups:   tipo_obesidad [6]
#  tipo_obesidad med_tecnologia
#  <ord>                  <dbl>
#1 desnutricion           1    
#2 peso_normal            1    
#3 sobrepeso              0.659
#4 obesidad_1             0.504
#5 obesidad_2             0.414
#6 obesidad_3             0.552

########## Análisis de uso de tecnología categórica ###########

# Diagrama de caja que relaciona el uso de tecnología con el 
# índice de masa corporal
ggplot(df_obesidad, aes(x = uso_tecnologia_cat, y = imc)) +
  geom_boxplot()

# Agrupar por uso de tecnología y resumir por índice de masa 
# corporal
df_obesidad |>
  group_by(uso_tecnologia_cat) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  )
# A tibble: 3 × 3
# Groups:   uso_tecnologia_cat [3]
#  uso_tecnologia_cat med_imc avg_imc
#  <ord>                <dbl>   <dbl>
#1 0-2h                  30.9    30.6
#2 3-5h                  29.4    30.6
#3 >5h                   27.5    26.9

###################################
##
## 13.- Consumo de alcohol
##
###################################

# Conocer el tipo de la variable conusmo de alcohol
class(df_obesidad$consume_alcohol) # produce:
#[1] "ordered" "factor" 
# Conocer los valores de la variable de consumo de alcohol
unique(df_obesidad$consume_alcohol) # produce:
#[1] ocasional nunca     frecuente
#Levels: nunca < ocasional < frecuente

# Diagrama de caja que relaciona el consumo de alcohol con el
# índice de masa corporal
ggplot(df_obesidad, aes(x = consume_alcohol, y = imc)) +
  geom_boxplot()

# Gráfico de barras que relaciona el consumo de alcohol con el 
# tipo de obesidad
ggplot(df_obesidad, aes(x = consume_alcohol, fill = tipo_obesidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    x = "Consumo de Alcohol",
    title = "Los tomadores ocasionales son más obesos",
    subtitle = "Los tomadores ocasionales son más obesos grado 2 y 3",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()
  
# Agrupar la tabla de obesidad por consumo de alcohol y resumir por
# índice de masa corporal
df_obesidad |>
  group_by(consume_alcohol) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 3 × 3
# Groups:   consume_alcohol [3]
#  consume_alcohol med_imc avg_imc
#  <ord>             <dbl>   <dbl>
#1 nunca              27.6    26.7
#2 ocasional          32.2    31.6
#3 frecuente          27.0    27.0
  
###############################
##
## 14.- Medio de transporte
## 
###############################

# Conocer el tipo de la variable de medio de transporte
class(df_obesidad$medio_transporte) # produce: factor
# Conocer los valores de la variable de medio de transporte 
unique(df_obesidad$medio_transporte) # produce:
#[1] publico auto    camina  moto    <NA>   
#  Levels: publico auto moto bicicleta camina

# Diagrama de caja que relaciona el medio de transporte con el
# índice de masa corporal
ggplot(df_obesidad, aes(x = medio_transporte, y = imc)) +
  geom_boxplot() +
  labs(
    x = "Medio de transporte",
    y = "Índice de masa corporal",
    title = "Los que no usan transporte público ni auto son más delgados",
    subtitle = "Los que usan transporte público tienen un imc mayor a 30",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()

# Gráfico de barras que relaciona el medio de transporte con el 
# tipo de obesidad
ggplot(df_obesidad, aes(x = medio_transporte, 
                        fill = tipo_obesidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Porcentaje", labels = label_percent()) +
  labs(
    x = "Medio de transporte",
    color = "Tipo de Obeisdad",
    title = "Los que caminan y usan bicicleta tienen menos obesidad",
    subtitle = "Los que usan auto y transporte público tienen más obesidad",
    caption = "Datos del dataset de riesgo de obesidad de Kaggle"
  ) +
  theme_linedraw()

# Agrupar por medio de transporte y resumir por índice de masa 
# corporal
df_obesidad |>
  group_by(medio_transporte) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 5 × 3
# Groups:   medio_transporte [5]
#  medio_transporte med_imc avg_imc
#  <fct>              <dbl>   <dbl>
#1 publico             30.0    30.5
#2 auto                29.4    29.9
#3 moto                24.5    25.5
#4 bicicleta           24.4    24.5
#5 camina              24.2    23.7

# Contar por tipo de obesidad y medio de transporte, agrupar por tipo
# de obesidad y elegir el máximo. Sacar la moda de medio de 
# transporte  por tipo de obesidad
df_obesidad |>
  count(tipo_obesidad, medio_transporte) |>
  group_by(tipo_obesidad) |>
  slice_max(n, n = 1) #  produce:
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad medio_transporte     n
#  <ord>         <fct>            <int>
#1 desnutricion  publico           2111
#2 peso_normal   publico           2846
#3 sobrepeso     publico           3386
#4 obesidad_1    publico           2301
#5 obesidad_2    publico           2818
#6 obesidad_3    publico           3225

# Moda con 3 tipos de peso 
df_sobre_obesidad |>
  count(imc_grupos3, medio_transporte) |>
  group_by(imc_grupos3) |>
  slice_max(n, n = 1) # produce:
# A tibble: 3 × 3
# Groups:   imc_grupos3 [3]
#  imc_grupos3 medio_transporte     n
#  <ord>       <fct>            <int>
#1 peso_normal publico           2846
#2 sobrepeso   publico           3386
#3 obesidad    publico           8344
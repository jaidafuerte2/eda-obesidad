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

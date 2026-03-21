##########################################################
##                                                      ##  
##   Análisis explortorio de la dataset heart disease   ##
##                                                      ##
##########################################################

library(tidyverse)
library(scales)
source("eda-obesidad/load-data/load-heart.R")

##################################
##
## 0.- Índice de Masa Corporal
##
##################################

# Conocer el tiepo de la variable de índice de masa corporal
class(df_corazon$imc) # produce: numeric
# Conocer los valores de la variable de índice de masa corporal
unique(df_corazon$imc)[1:20] # produce:
#[1] 24.99159 25.22180 29.85545 24.13048 20.48629 28.14468 18.04233
#[8] 34.73668 34.49311 30.14215 34.44762 31.73962 33.34402 19.42243
#[15] 37.38784 32.16649 31.43388 26.51930 18.61898 37.83216

# Valor mínimo del índice de masa corporal
min(df_corazon$imc, na.rm = TRUE) # produce: [1] 18.00284
# Valor máximo del índice de masa corporal
max(df_corazon$imc, na.rm = TRUE) # produce: [1] 39.99695
# Resumen de la variable de pindice de masa corporal
summary(df_corazon$imc) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#18.00   23.66   29.08   29.08   34.52   40.00      22 

# Histograma de la variable de índice de masa corporal
ggplot(df_corazon, aes(x = imc)) +
  geom_histogram(binwidth = 1, na.rm = TRUE)

# Gráfico de densidad de la variable de índice de masa corporal
ggplot(df_corazon, aes(x = imc)) +
  geom_density(na.rm = TRUE)

#############################
##
## 0.- Tipo de Obesidad
##
#############################

# Conocer el tipo de la variable de tipo de obesidad
class(df_corazon$tipo_obesidad) # produce: factor

# Conocer los valores de la variable de tipo de obesidad
unique(df_corazon$tipo_obesidad) # produce: 
#[1] peso_normal  sobrepeso    desnutricion obesidad_1   obesidad_2  
#[6] <NA>        
#6 Levels: desnutricion peso_normal sobrepeso obesidad_1 ... obesidad_3

# Gráfico de barras de la variable tipo de obesidad
ggplot(df_corazon |> drop_na(tipo_obesidad), 
       aes(x = tipo_obesidad)) +
  geom_bar() 

#############################
##
## 1.- Edad
##
#############################

# Conocer el tipo de la variable edad
class(df_corazon$edad) # produce: numeric

# Conocer los valores de la variable edad
unique(df_corazon$edad) # produce: 

# Conocer el tipo de la variable edad
class(df_corazon$edad) # produce: 
#[1] 56 69 46 32 60 25 78 38 75 36 40 28 41 70 53 57 20 39 19 61 47 55 77
#[24] 50 29 42 66 44 76 80 59 45 33 79 64 68 72 74 54 24 26 35 21 31 67 43
#[47] 37 52 34 23 71 51 27 48 65 62 58 18 22 30 49 73 63 NA

# Conocer cuánto valores NA tiene la variable edad
sum(is.na(df_corazon$edad)) # produce: 29

# Resumen de la variable edad
summary(df_corazon$edad) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#18.0    34.0    49.0    49.3    65.0    80.0      29

# Diagrama de dispersión de la variable de edad
ggplot(df_corazon, aes(x = edad, y = imc)) +
  geom_point(alpha = 1/5)

# Diagrama de caja que relaciona el tipo de obesidad con la edad
ggplot(df_corazon |> drop_na(tipo_obesidad), 
       aes(x = tipo_obesidad, y = edad)) +
  geom_boxplot()

df_corazon |>drop_na(tipo_obesidad) |> 
  group_by(tipo_obesidad) |>
  summarise(
    med_edad = median(edad, na.rm = TRUE),
    .groups = "keep"
  )

cor(df_corazon$edad, df_corazon$imc, use = "complete.obs")


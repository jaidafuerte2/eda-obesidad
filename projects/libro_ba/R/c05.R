########################
##                    ##
## Capítulo 5         ## 
##                    ##
########################

library(readr)
library(dplyr)
library(tidyverse)
library(skimr)

# Cargar la dataset de marketing
marketing <- read_delim(
  "eda-obesidad/projects/libro_ba/data/marketing_campaign.csv",
  delim = ";"
  #guess_max = 10000,
  #show_col_types = FALSE
)
glimpse(marketing) # produce:
skim(marketing) # produce:

###################
##
## Response
##
###################

# Calcular la proporción general
marketing %>%
  summarise(
    conversion_rate = mean(Response)
  ) # produce:
# A tibble: 1 × 1
#    conversion_rate
#              <dbl>
#  1           0.149

# Gráfico de barras de Conteo básico de la variable Response
marketing %>%
  ggplot(aes(x = factor(Response))) +
  geom_bar() +
  labs(
    title = "Respuesta a la campaña",
    x = "Respuesta",
    y = "Número de clientes"
  )

###########################
##
## Campañas
##
###########################

marketing %>%
  summarise(
    cmp1 = mean(AcceptedCmp1),
    cmp2 = mean(AcceptedCmp2),
    cmp3 = mean(AcceptedCmp3),
    cmp4 = mean(AcceptedCmp4),
    cmp5 = mean(AcceptedCmp5)
  ) # produce:
# A tibble: 1 × 5
#    cmp1   cmp2   cmp3   cmp4   cmp5
#   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#1 0.0643 0.0134 0.0728 0.0746 0.0728

# Alargar el resumen
long_marketing <- marketing %>%
  summarise(
    cmp1 = mean(AcceptedCmp1),
    cmp2 = mean(AcceptedCmp2),
    cmp3 = mean(AcceptedCmp3),
    cmp4 = mean(AcceptedCmp4),
    cmp5 = mean(AcceptedCmp5)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "campania",
    values_to = "conversion"
  )
long_marketing # produce:
# A tibble: 5 × 2
#  campania conversion
#  <chr>         <dbl>
#1 cmp1         0.0643
#2 cmp2         0.0134
#3 cmp3         0.0728
#4 cmp4         0.0746
#5 cmp5         0.0728

# Grafico de columnas que compara las conversiones de las campañas 
long_marketing %>%
  ggplot(aes(x = campania, y = conversion)) +
  geom_col() +
  labs(
    title = "Conversiones promedio de la campaña",
    x = "Campañas",
    y = "Tasa de conversión"
  )

#################
##
## Simulación
##
#################

############## conversión ################

set.seed(123)

# Simular 1000 conversiones
simulaciones <- tibble(
  conversion = rbinom(
    1000,
    size = 100, # 100 clientes
    prob = 0.15 # probabilidad de conversión de cada cliente
  )
) 
simulaciones[1:6,] # produce:
# A tibble: 6 × 1
#  conversion
#       <int>
#1         20
#2         16
#3         14
#4         15
#5         19
#6         14

# Densidad de la conversión de las simulaciones
simulaciones %>%
  ggplot(aes(x = conversion)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribución plausible de conversiones",
    x = "Porcentaje de conversión",
    y = "Densidad"
  )

############## incertidumbre #################

# Simular conversiones
conversiones <- tibble(
  campania = c("A", "B", "C"),
  promedio = c(0.14, 0.16, 0.15),
  inferior = c(0.11, 0.13, 0.12),
  superior = c(0.17, 0.19, 0.18)
)

# Gráfico con incertidumbre
conversiones %>%
  ggplot(aes(x = campania, y = promedio)) + 
  geom_point(size = 3) + # size es el tamaño del punto
  geom_errorbar(
    aes(
      ymin = inferior,
      ymax = superior
    ), 
    width = 0.2 # es el tamaño del bigote
  ) +
  labs(
    title = "Conversiones con incertidumbre",
    x = "Campaña",
    y = "Conversión"
  )

#################
##
## Ejercicios
##
#################

#################
##
## Ejercicio 1 
##
#################

# Ejercicio 1

# Calcula la tasa general de conversión del dataset.

marketing %>%
  summarise(
    conversion_rate = mean(Response) 
  ) # produce:
# A tibble: 1 × 1
#  conversion_rate
#            <dbl>
#1           0.149

# Interpreta el resultado verbalmente.
# El porcentaje de respuesta de la campaña es 14.9%


#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Crea un gráfico de barras para comparar campañas.

# Resumir las campañas por promedio
campaigns <- marketing %>%
  summarise(
    cmp1 = mean(AcceptedCmp1),
    cmp2 = mean(AcceptedCmp2),
    cmp3 = mean(AcceptedCmp3),
    cmp4 = mean(AcceptedCmp4),
    cmp5 = mean(AcceptedCmp5),
  ) 
campaigns # produce:
# A tibble: 1 × 5
#    cmp1   cmp2   cmp3   cmp4   cmp5
#   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#1 0.0643 0.0134 0.0728 0.0746 0.0728

# Alargar la tabla campaign, pasar a formato largo
long_campaign <- campaigns %>%
  pivot_longer(
    everything(),
    names_to = "campaign",
    values_to = "conversion"
  )
long_campaign # produce:
# A tibble: 5 × 2
# campaign conversion
# <chr>         <dbl>
#1 cmp1         0.0643
#2 cmp2         0.0134
#3 cmp3         0.0728
#4 cmp4         0.0746
#5 cmp5         0.0728

# Crear un gráfico de columnas de las campañas publicitarias
long_campaign %>%
  ggplot(aes(x = campaign, y = conversion)) +
  geom_col()
# ¿Qué campañas parecen más prometedoras? Las tres últimas
  
#################
##
## Ejercicio 3 
##
#################

# Ejercicio 3

# Simula múltiples escenarios plausibles usando rbinom().

set.seed(123)

# Simular 1000 conversiones
simulaciones <- tibble(
  conversion = rbinom(
    1000,
    size = 100,
    prob = 0.15
  )
)
simulaciones[1:6,] # produce:
# A tibble: 6 × 1
#  conversion
#       <int>
#1         16
#2         19
#3         16
#4         17
#5         10
#6         17

simulaciones %>%
  ggplot(aes(x = conversion)) +
  geom_density(fill = "yellow", alpha = 0.5)
# ¿Qué tan variable parece el futuro? Siendo la media cercana a 15,
# el gráfico de densidad muestra que hay variabilidad incluso de
# +- 10

#################
##
## Ejercicio 4
##
#################  
  
# Ejercicio 4

# Construye un gráfico con intervalos plausibles.

# Simular porcentaje de conversiones
conversiones <- tibble(
  campania = c("primera", "segunda", "tercera"),
  promedio = c(15, 20, 25),
  inferior = c(10, 17, 24),
  superior = c(20, 23, 26)
)

# Graficar intervalos plausibles de las conversiones
conversiones %>%
  ggplot(aes(x = campania, y = promedio)) +
  geom_point() +
  geom_errorbar(aes(ymin = inferior, ymax = superior),
                width = 0.2) 
# ¿Qué campañas muestran mayor incertidumbre? la primera y la segunda

  
#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Reflexiona:
  
#  ¿Por qué buscar certeza absoluta puede ser peligroso en 
#   negocios? porque es difícil encontrar certeza absoluta,
#   y se puede esperar demasiado al intentar encontrarla
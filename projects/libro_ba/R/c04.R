########################
##                    ##
## Capítulo 4         ## 
##                    ##
########################


library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset de marketing
marketing <- read_delim(
  "eda-obesidad/projects/libro_ba/data/marketing_campaign.csv",
  delim = ";"
  #guess_max = 10000,
  #show_col_types = FALSE
)
#glimpse(marketing) # produce:
head(marketing) # produce:

marketing %>%
  count(Response) # produce:
# A tibble: 2 × 2
#  Response     n
#     <dbl> <int>
#1        0  1906
#2        1   334

# Contar las respuestas a la campaña y sacar la proporción
marketing %>%
  count(Response) %>%
  mutate(
    proporcion = n / sum(n)
  ) # produce:
# A tibble: 2 × 3
#Response     n proporcion
#     <dbl> <int>      <dbl>
#1        0  1906      0.851
#2        1   334      0.149

# Visualización simple de las conversiones
marketing %>%
  count(Response) %>%
  ggplot(aes(x = factor(Response), y = n)) +
  geom_col() +
  labs(
    title = "Clientes que respondieron a la campaña",
    x = "Respuesta",
    y = "Numero de clientes"
  )

# Comparar creencia inicial con creencia después de observar datos
beliefs <- tibble(
  etapa = c("Antes de la campaña", "Después de observar datos"),
  probabilidad = c(0.10, 0.149)
)
beliefs # produce:
# A tibble: 2 × 2
#  etapa                     probabilidad
#  <chr>                            <dbl>
#1 Antes de la campaña              0.1  
#2 Después de observar datos        0.149

# Gráfico de la etapa vs la probabilidad
ggplot(beliefs, aes(
  x = etapa, 
  y = probabilidad
)) +
  geom_col() +
  ylim(0, 1) + # Para mostrarel gráfico en la escala de 0 a 1 en el
               # eje Y
  labs(
    title = "Cómo cambia una creencia con nueva evidencia",
    y = "Probabilidad estimada"
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

# Actualizar creencias intuitivamente

# Imagina:
  
#  una campaña normalmente convierte 5%
# en una prueba pequeña convierte 15%

# Pregunta:
  
#  ¿deberías cambiar totalmente tu opinión? Creo que no
#  ¿o sólo parcialmente? Parcialmente porque se debería incluir
#   el conocimiento anterior
  
#  Explica intuitivamente por qué.

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Frecuencias naturales

# Supongamos:
  
#  100 clientes reciben una oferta
#  18 compran

# Describe la situación usando:
  
#  proporciones
#  frecuencias naturales
#  interpretación intuitiva

# total de clientes = 100
# total de clientes que compraron = 18 => p(A) = 18% (18/100 = 0.18)

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3
# Interpretar evidencia

# Una campaña tiene pocas conversiones al inicio.

# Después mejora considerablemente.

# Pregunta:
  
#  ¿qué podría explicar ese cambio? Puede ser que las tendencias 
#   de temporada
#  ¿la evidencia inicial era suficiente? No, había que actualizar

#################
##
## Ejercicio 4
##
#################

# Ejercicio 4

# Pequeño experimento en R

# Modificar este código:
  
beliefs <- tibble(
  etapa = c("Antes", "Después"),
  probabilidad = c(0.15, 0.35)
)
  
ggplot(beliefs, aes(x = etapa, y = probabilidad)) +
  geom_col()

######## beliefs1 ########

beliefs1 <- tibble(
  etapa = c("Antes", "Después"),
  probabilidad = c(0.1, 0.4)
)

ggplot(beliefs1, aes(x = etapa, y = probabilidad)) +
  geom_col()

######## beliefs2 ########

beliefs2 <- tibble(
  etapa = c("Antes", "Después"),
  probabilidad = c(0.01, 0.17)
)

ggplot(beliefs2, aes(x = etapa, y = probabilidad)) +
  geom_col()

########### otra forma ############

beliefs <- tibble(
  escenario = c(
    "Evidencia débil",
    "Evidencia moderada",
    "Evidencia fuerte"
  ),
  antes = c(0.15, 0.15, 0.15),
  despues = c(0.20, 0.35, 0.70)
)

beliefs # produce:
# A tibble: 3 × 3
#  escenario          antes despues
#  <chr>              <dbl>   <dbl>
#1 Evidencia débil     0.15    0.2 
#2 Evidencia moderada  0.15    0.35
#3 Evidencia fuerte    0.15    0.7 

# Cambiar la tabla de formato ancho a formato largo
beliefs %>%
  pivot_longer(
    cols = c(antes, despues),
    names_to = "momento",
    values_to = "probabilidad"
  )
# A tibble: 6 × 3
#  escenario          momento probabilidad
#  <chr>              <chr>          <dbl>
#1 Evidencia débil    antes           0.15
#2 Evidencia débil    despues         0.2 
#3 Evidencia moderada antes           0.15
#4 Evidencia moderada despues         0.35
#5 Evidencia fuerte   antes           0.15
#6 Evidencia fuerte   despues         0.7 

# Visualizar

beliefs %>%
  pivot_longer( # Cambia la tabla de formato ancho a formato largo
    cols = c(antes, despues),
    names_to = "momento",
    values_to = "probabilidad"
  ) %>%
  ggplot(aes(
    x = momento,
    y = probabilidad,
    group = escenario
  )) +
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(~escenario) +
  ylim(0, 1) +
  labs(
    title = "Cómo distintas evidencias cambian creencias",
    y = "Probabilidad"
  )

#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Reflexión conceptual

# Explica con tus propias palabras:
  
#  “Bayes trata sobre plausibilidad, no sobre certeza absoluta.”

# Bayes no da respuestas de sí y no, da rangos de posibilidad 
# y acutaliza probabilidades.
########################
##                    ##
## Capítulo 13        ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)
library(posterior) 
library(bayesplot)

# Cargar la dataset de telco
telco <- read_csv(
  "eda-obesidad/projects/libro_ba/data/telco_customer_churn.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

#########################
##
## Preparando los datos
##
#########################

#glimpse(telco) # produce:
class(telco$Churn) # produce: "character"
class(telco$TotalCharges) # produce: "numeric"

# Cambiar el Churn a numárica binaria y cambiar la variable de cargos 
# totales a numérica 
telco <- telco %>%
  mutate(
    Churn = if_else(Churn == "Yes", 1, 0),
    TotalCharges = as.numeric(TotalCharges)
  ) %>%
  drop_na(TotalCharges)
class(telco$Churn) # produce: "numeric"
class(telco$TotalCharges) # produce: "numeric"

# Crear un modelo para conocer cómo afecta la antiguedad y los cargos
# mensuales en el churn
modelo_churn <- stan_glm(
  Churn ~ tenure + MonthlyCharges,
  data = telco,
  family = binomial(),
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_churn) # produce:
#                        5%         95%
#(Intercept)    -1.93832391 -1.64829674
#tenure         -0.05781402 -0.05238565
#MonthlyCharges  0.03064235  0.03514132
# Interprettación: independientemente de los cargos mensuales, la 
# antiguedad disminuye el churn y independientemente de la antiguedad,
# los cargos mensuales aumentan el churn.

# Calcular la probabilidad de churn para cada cliente
riesgo_promedio <- posterior_epred(modelo_churn)
riesgo_promedio # produce:
#iterations         1         2         3          4         5         6
#      [1,] 0.2809635 0.1405711 0.4529702 0.05239303 0.5930191 0.7357173
#
#iterations         7         8         9         10        11         12
#      [1,] 0.4787448 0.1933424 0.5294404 0.03397698 0.2864393 0.10771873
#[ reached 'max' / getOption("max.print") -- omitted 999 rows and 7020 
# columns (of 7032) ]

# Crear una nueva variable con el riesgo promedio de churn
telco <- telco %>%
  mutate(
    riesgo_churn = colMeans(riesgo_promedio)
  )
telco[,-20:-1] # produce:
# A tibble: 7,032 × 2
#  Churn riesgo_churn
#  <dbl>        <dbl>
#1     0       0.297 
#2     0       0.143 
#3     1       0.468 
#4     0       0.0534
#5     1       0.605 
#...
#9     1       0.529 
#10     0       0.0337
# ℹ 7,022 more rows
# ℹ Use `print(n = ...)` to see more rows

# Histograma que muestra la distribución del riesgo de churn en el 
# modelo 
ggplot(telco, aes(riesgo_churn)) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Distribución del riesgo estimado de churn",
    x = "Probabilidad estimada",
    y = "Clientes"
  )
# ¿Qué significa el riesgo de churn ahora que se toman en cuenta
# dos variables?

# Intervenir cuando el riesgo de churn es mayor al 70%
telco <- telco %>%
  mutate(
    intervenir_70 =
      riesgo_churn > 0.70
  )
telco[,-20:-1] # produce:
# A tibble: 7,032 × 3
#  Churn riesgo_churn intervenir_70
#  <dbl>        <dbl> <lgl>        
#1     0       0.297  FALSE        
#2     0       0.143  FALSE        
#3     1       0.468  FALSE        
#4     0       0.0534 FALSE        
#5     1       0.605  FALSE        
#...       
#8     0       0.204  FALSE        
#9     1       0.529  FALSE        
## ℹ 7,022 more rows
## ℹ Use `print(n = ...)` to see more rows

# Conocer a cuantos clientes de los 7043 hay que intervenir
telco %>%
  count(intervenir_70) # produce:
# A tibble: 2 × 2
#  intervenir_70     n
#  <lgl>         <int>
#1 FALSE          6781
#2 TRUE            251

# Crear una nueva variable con los clientes que tienen un riesgo de
# churn mayor al 40%
telco <- telco %>%
  mutate(
    intervenir_40 =
      riesgo_churn > 0.40
  )
# Conocer cuantos clientes tienen un riesgo de churn mayor al 40%
telco |>
  count(intervenir_40) # produce:
# A tibble: 2 × 2
#  intervenir_40     n
#  <lgl>         <int>
#1 FALSE          5096
#2 TRUE           1936

# Ordenar telco 
telco_ordenado <- telco %>%
  arrange(desc(riesgo_churn)) %>%
  slice_head(n = 10) # Escoger las 10 primeras observaciones
telco_ordenado[,-20:-1] # produce:
# A tibble: 100 × 4
#  Churn riesgo_churn intervenir_70 intervenir_40
#  <dbl>        <dbl> <lgl>         <lgl>        
#1     0        0.831 TRUE          TRUE         
#2     0        0.831 TRUE          TRUE         
#3     1        0.823 TRUE          TRUE         
#...        
#8     1        0.817 TRUE          TRUE         
#9     1        0.816 TRUE          TRUE         
# ℹ 1 more rows
# ℹ Use `print(n = ...)` to see more rows

# Mostrar los 10 clientes con mayor riesgo
telco_ordenado %>%
  ggplot(
    aes(
      reorder(customerID, riesgo_churn),
      riesgo_churn
    )
  ) +
  geom_col() + # Mostrar columnas
  coord_flip() + # Cambiar el eje para poder ver los ID's
  labs(
    title = "10 clientes con mayor riesgo estimado",
    x = "",
    y = "Probabilidad de churn"
  )

reorder2 <- reorder(
  c("A","B","C","D"),
  c(0.2,0.8,0.4,0.1)
) # produce:
reorder2

######################
##
## Visualizar costos
##
######################   

umbrales <- seq(
  0.2,
  0.8,
  by = 0.05
)
umbrales # produce:
# [1] 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 
# 0.70 0.75 0.80

# Crear una tabla con los umbrales y los clientes intervenidos
escenarios <- map_dfr( # Una especie de map que genera o une tablas
  umbrales,
  function(u){  # función anónima, la forma lamda de r
    
    tibble(  # crear la tabla con las variables
      umbral = u, # umbral e
      intervenidos =  # intervenidos
        sum(telco$riesgo_churn > u) # aplicar sum() a cada umbral 
    )
    
  }
)
escenarios # produce:
# A tibble: 13 × 2
#  umbral intervenidos
#    <dbl>        <int>
#1   0.2          3560
#2   0.25         2921
#3   0.3          2555
#4   0.35         2259
#5   0.4          1936
#6   0.45         1586
#7   0.5          1308
#8   0.55         1070
#9   0.6           837
#10   0.65          493
#11   0.7           251
#12   0.75           97
#13   0.8            15

# Visualizar los clientes intervenidos según el umbral elegido
ggplot(
  escenarios,
  aes(umbral, intervenidos)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Clientes intervenidos según el umbral elegido",
    x = "Umbral",
    y = "Clientes priorizados"
  )

#########################
##
## Ejercicios
##
#########################

# Ejercicio 1

# Explique con sus propias palabras la diferencia entre:
  
# - estimar una probabilidad
# - tomar una decisión

# Respuesta: Estimar es sólo conseguir las probabilidades. Tomar 
# una decisión es usar esas probabilidades para ejecutar una 
# acción

# Ejercicio 2

# Describa un ejemplo de falso positivo en una campaña de retención.

# Respuesta: Un cliente que realmente no iba a abandonar a la empresa
# pero se tomó una acción para retenerlo

# ¿Cuál sería su costo? La acción para retenerlo
  
# Ejercicio 3
  
#  Describa un ejemplo de falso negativo.

# Respuesta: Un cliente que abandonó la empresa pero no se tomó 
# ninguna acción para retenerlo porque se pensaba que no tenía 
# suficiente riesgo

#  ¿Por qué podría ser más costoso? Porque para retener a un cliente
#  se podrían usar los recursos que ya dispone una empresa, sin 
#  necesidad de entrar en gastos adicionales, en cambio perder
#  un cliente es una pérdida directa. Claro que es un poco relativo
  
# Ejercicio 4
  
#  Repita el análisis utilizando:
  
# - 30%
# - 50%
# - 70%

# como umbrales de intervención.

# Crear una nueva variable con los clientes que tienen un riesgo de
# churn mayor al 30%
telco <- telco %>%
  mutate(
    intervenir_30 =
      riesgo_churn > 0.30
  )
# Conocer cuantos clientes tienen un riesgo de churn mayor al 30%
telco |>
  count(intervenir_30) # produce: 2556

# Crear una nueva variable con los clientes que tienen un riesgo de
# churn mayor al 50%
telco <- telco %>%
  mutate(
    intervenir_50 =
      riesgo_churn > 0.50
  )
# Conocer cuantos clientes tienen un riesgo de churn mayor al 50%
telco |>
  count(intervenir_50) # produce: 1308

# Crear una nueva variable con los clientes que tienen un riesgo de
# churn mayor al 70%
telco <- telco %>%
  mutate(
    intervenir_70 =
      riesgo_churn > 0.70
  )
# Conocer cuantos clientes tienen un riesgo de churn mayor al 70%
telco |>
  count(intervenir_70) # produce: 251

######## otra forma ###############

umbrales <- seq(
  0.3,
  0.7,
  by = 0.2
)
umbrales # produce: [1] 0.3 0.5 0.7

# Crear una tabla con los umbrales y los clientes intervenidos
escenarios <- map_dfr(
  umbrales,
  function(u){
    
    tibble(
      umbral = u,
      intervenidos =
        sum(telco$riesgo_churn > u)
    )
    
  }
)
escenarios # produce:
#  umbral intervenidos
#   <dbl>        <int>
#1    0.3         2556
#2    0.5         1308
#3    0.7          251

# Visualizar los clientes intervenidos según el umbral elegido
ggplot(
  escenarios,
  aes(umbral, intervenidos)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Clientes intervenidos según el umbral elegido",
    x = "Umbral",
    y = "Clientes priorizados"
  )

# ¿Qué cambia? Mientras mayor es el riesgo menor es la cantidad de
# clientes


# Ejercicio 5
  
# Construya un gráfico que muestre únicamente los 50 clientes con 
# mayor riesgo estimado.

# Ordenar telco 
telco_ordenado <- telco %>%
  arrange(desc(riesgo_churn)) %>%
  slice_head(n = 50) # Escoger las 10 primeras observaciones
telco_ordenado[,-20:-1] # produce:
# A tibble: 10 × 6
#  Churn riesgo_churn intervenir_70 intervenir_40 intervenir_30
#  <dbl>        <dbl> <lgl>         <lgl>         <lgl>        
#1     0        0.832 TRUE          TRUE          TRUE         
#2     0        0.832 TRUE          TRUE          TRUE         
#3     1        0.823 TRUE          TRUE          TRUE         
#4     1        0.822 TRUE          TRUE          TRUE         
#5     1        0.822 TRUE          TRUE          TRUE         
#6     1        0.819 TRUE          TRUE          TRUE       


# Mostrar los 10 clientes con mayor riesgo
telco_ordenado %>%
  ggplot(
    aes(
      reorder(customerID, riesgo_churn), # ordena customerID según 
      # el riesgo (riesgo_churn). Por ejemplo: 
      # reorder(c("A","B","C","D"), c(0.2,0.8,0.4,0.1)) # produce:
      # D A C B
      riesgo_churn
    )
  ) +
  geom_col() + # Mostrar columnas
  coord_flip() + # Cambiar el eje para poder ver los ID's
  labs(
    title = "10 clientes con mayor riesgo estimado",
    x = "",
    y = "Probabilidad de churn"
  )

# Ejercicio 6

# Suponga que cada intervención cuesta $10.

# ¿Cuántos clientes intervendría con cada umbral?

escenarios # produce:
#  umbral intervenidos
#   <dbl>        <int>
#1    0.3         2556 = 25560
#2    0.5         1308 = 13080
#3    0.7          251 = 2510
  
# Ejercicio 7
  
#  Reflexione:
  
#  `¿Es preferible cometer más falsos positivos o más falsos 
#  negativos?`

# ¿Depende del negocio? 
  
#  Explique.

# Es relativo, sí depende del negocio. 
# Muchas veces las estrategias de retención de clientes se pueden 
# desarrollar con los recursos de la propia empresa pero a veces
# NO
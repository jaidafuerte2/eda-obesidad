########################
##                    ##
## Capítulo 12        ## 
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

#############################
##
## Creación del modelo
##
#############################

# Crear una variable Churn numérica
telco <- telco %>%
  mutate(
    Churn_num =
      if_else(
        Churn == "Yes",
        1,
        0
      )
  )
class(telco$Churn_num) # produce: numeric
unique(telco$Churn_num) # produce: [1] 0 1

# Crear un modelo logístico bayesiano para conocer cuanto influye 
# la antiguedad en la cancelación de suscripción
modelo_churn <- stan_glm(
  Churn_num ~ tenure,
  data = telco,
  family = binomial(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0
) 
coef(modelo_churn) # produce:
#(Intercept)      tenure 
# 0.02615887 -0.03881346 
posterior_interval(modelo_churn) # produce:
#                     5%         95%
#(Intercept) -0.03609829  0.09296688
#tenure      -0.04111115 -0.03648721

prob_churn <- posterior_epred(modelo_churn)
# Ver esta tabla de 7043 clientes y 1000 simulaciones posteriores por
# cliente. Y en probabilidades
prob_churn # produce:
#iterations         1         2         3         4         5         6
#      [1,] 0.5101150 0.2140617 0.4999567 0.1483450 0.4999567 0.4392994
#
#iterations         7         8         9         10        11        12
#      [1,] 0.3072625 0.4193887 0.2579250 0.08028362 0.3900267 0.3614408
#[ reached 'max' / getOption("max.print") -- omitted 999 rows and 6043 columns ]

# Calcular la probabilidad posterior promedio
riesgo_promedio <- colMeans(prob_churn)
# Mostrar el riesgo promedio por cada uno de los 7043 clientes
riesgo_promedio # produce:
#         1          2          3          4          5          6 
#0.49713754 0.21562457 0.48744510 0.15216332 0.48744510 0.42973387 
#         7          8          9         10         11         12 
#0.30449518 0.41083535 0.25756021 0.08500581 0.38298890 0.35588940 
#[ reached 'max' / getOption("max.print") -- omitted 6043 entries ]

# Crear una nueva tabla con una nueva variable de riesgo promedio
telco_riesgo <- telco |>
  mutate(
    riesgo = riesgo_promedio
  )
# Histograma de riesgo de churn por antiguedad. Este gráfico responde,
# cómo se distribuye el churn en toda la cartera de clientes
ggplot(
  telco_riesgo,
  aes(x = riesgo)
) +
  geom_histogram(
    bins = 30
  )

# Ordenar los clientes por riesgo
telco_riesgo |>
  arrange(desc(riesgo))

# Ver como cambia el riesgo a través de l cartera completa
telco_riesgo |>
  arrange(desc(riesgo)) |>
  mutate(
    cliente = row_number()
  ) |>
  ggplot(
    aes(
      x = cliente,
      y = riesgo
    )
  ) +
  geom_point(alpha = 0.9)

# Segmentar el riesgo
telco_riesgo <- telco_riesgo |>
  mutate(
    segmento = case_when(
      riesgo < 0.20 ~ "Bajo",
      riesgo < 0.40 ~ "Moderado",
      TRUE ~ "Alto"
    )
  )

# Histograma de la probabilidad de churn según la antiguedad
ggplot(
  telco_riesgo,
  aes(
    x = riesgo,
    fill = segmento
  )
) +
  geom_histogram(
    bins = 40
  )

# Histograma de tenure
ggplot(
  telco,
  aes(x = tenure)
) +
  geom_histogram()

#############################
##
## Incertidumbre individual
##
#############################

# Mostrar las 1000 simulaciones posteriores del cliente número 25 (de
# 7043)
prob_churn[25,]
#         1          2          3          4          5          6 
#0.50334703 0.21441539 0.49340882 0.14984549 0.49340882 0.43416109 
#         7          8          9         10         11         12 
#0.30545427 0.41474035 0.25731223 0.08228928 0.38611634 0.35825858 
#[ reached 'max' / getOption("max.print") -- omitted 6043 entries ]

# Resumir las simulaciones posteriores
quantile(
  prob_churn[, 25],
  probs = c(0.05, 0.5, 0.95)
) # produce:
#       5%       50%       95% 
#0.1240423 0.1331091 0.1429131

########################
##
## Comparar 2 clientes
##
########################

# Seleccionar 2 clientes 
clientes <- c(25, 120)

# Visualizar la distribución del riesgo de churn de los 
# dos clientes
tibble(
  riesgo = c(
    prob_churn[,25],
    prob_churn[,1200]
  ),
  cliente = rep(
    c("Cliente A","Cliente B"),
    each = nrow(prob_churn)
  )
) |>
  ggplot(
    aes(
      x = riesgo,
      fill = cliente
    )
  ) +
  geom_density(alpha = 0.4)

# Crear una tabla de una sóla columna con la probabilidad de 
# de churn del cliente 25 y del cliente 1200
tibble(
  riesgo = c(
    prob_churn[,25],
    prob_churn[,1200]
  )
) # produce:
# A tibble: 2,000 × 1
#   riesgo
#   <dbl>
#1  0.129
#2  0.141
#3  0.125
#4  0.137
#5  0.140
#6  0.138
# ℹ 1,994 more rows
# ℹ Use `print(n = ...)` to see more rows

# Crear una tabla de una sóla columna con la probabilidad o riesgo de 
# de churn del cliente 25 y del cliente 1200 y otra variable 
# etiquetando a los dos clientes 
churn_dos_clientes <- tibble(
  riesgo = c(
    prob_churn[,25],
    prob_churn[,1200]
  ),
  cliente = rep(
    c("Cliente A","Cliente B"),
    each = nrow(prob_churn)
  )
)

# Conocer cuantas filas tiene prob_churn (prob_churn tiene 7043
# filas y 1000 filas)
nrow(prob_churn) # produce: [1] 1000

rep(
  c("Cliente A","Cliente B"),
  each = 10
) # produce:
#[1] "Cliente A" "Cliente A" "Cliente A" "Cliente A" "Cliente A"
#[6] "Cliente A" "Cliente A" "Cliente A" "Cliente A" "Cliente A"
#[11] "Cliente B" "Cliente B" "Cliente B" "Cliente B" "Cliente B"
#[16] "Cliente B" "Cliente B" "Cliente B" "Cliente B" "Cliente B"

########################
##
## Comparar 2 clientes
## versión desacoplada
##
########################

# Extraer todas las probabilidades posteriores
# del Cliente A (observación 25)
riesgo_cliente_a <- prob_churn[, 25]

# Extraer todas las probabilidades posteriores
# del Cliente B (observación 1200)
riesgo_cliente_b <- prob_churn[, 1200]

# Crear una tabla con las probabilidades de churn del cliente A
# y el cliente B. Con las probabilidades en la variable riesgo
# y los cientes en la variable cliente
comparacion_clientes <- tibble(
  riesgo = c(
    riesgo_cliente_a,
    riesgo_cliente_b
  ),
  cliente = c(
    rep( # Repetir "Cliente A" 1000 veces
      "Cliente A",
      length(riesgo_cliente_a)
    ),
    rep( # Repetir "Cliente B" 1000 veces
      "Cliente B",
      length(riesgo_cliente_b)
    )
  )
) 
comparacion_clientes # produce:
# A tibble: 2,000 × 2
#  riesgo cliente  
#   <dbl> <chr>    
#1  0.129 Cliente A
#2  0.141 Cliente A
#3  0.125 Cliente A
#4  0.137 Cliente A
#5  0.140 Cliente A
#6  0.138 Cliente A
#7  0.141 Cliente A
#8  0.125 Cliente A
#9  0.122 Cliente A
#10  0.133 Cliente A
# ℹ 1,990 more rows
# ℹ Use `print(n = ...)` to see more rows

# Graficar las distribuciones  de de los clientes A y B
ggplot(
  comparacion_clientes,
  aes(
    x = riesgo,
    fill = cliente
  )
) +
  geom_density(
    alpha = 0.4
  )

#############################
##
## Escenarios conceretos
##
############################

# Crear tabla con una columna de meses
nuevo <- tibble(
  tenure = c(12, 24, 36, 48)
)

# Crear una matriz de simulaciones posteriores para antiguedades de
# 12, 24, 36 y 48 meses
posterior_epred(
  modelo_churn,
  newdata = nuevo
) # produce:
#iterations         1         2         3         4
#      [1,] 0.3997370 0.2902384 0.2007054 0.1335930
#      [2,] 0.3863813 0.2894598 0.2085860 0.1456755
#      [3,] 0.3987241 0.2875255 0.1971694 0.1300260
#      [4,] 0.3913436 0.2899387 0.2059202 0.1414001
#      ...
#      [249,] 0.3927721 0.2924660 0.2089609 0.1444312
#      [250,] 0.3956532 0.2923252 0.2067504 0.1412273
#  [ reached 'max' / getOption("max.print") -- omitted 750 rows ]

# Crear una matriz de simulaciones posteriores para antiguedades de
# 12, 24, 36 y 48 meses y sacar los promedios
posterior_epred(
  modelo_churn,
  newdata = nuevo
) |>
  colMeans() # produce:
#        1         2         3         4 
#0.3921958 0.2883205 0.2027992 0.1377650 

##################
##
## Ejercicios
##
##################

##################
##
## Ejercicio 1
##
##################

# Ejercicio 1

# Seleccione tres clientes:
  
#  uno de riesgo bajo;
# uno de riesgo moderado;
# uno de riesgo alto.

# Reordenar el riesgo
telco_risk <- telco_riesgo |>
  arrange(riesgo) 
telco_risk[, -20:-1] # produce:
# A tibble: 7,043 × 4
#  Churn Churn_num riesgo segmento
#  <chr>     <dbl>  <dbl> <chr>   
#1 No            0 0.0593 Bajo    
#2 No            0 0.0593 Bajo    
#3 No            0 0.0593 Bajo    
#4 No            0 0.0593 Bajo    
#5 No            0 0.0593 Bajo    
#6 No            0 0.0593 Bajo    
# ℹ 7,033 more rows
# ℹ Use `print(n = ...)` to see more rows

#glimpse(telco_risk) # produce:

# Seleccionemos:
  
cliente_bajo <- telco_risk[100, ]
cliente_medio <- telco_risk[3500, ]
cliente_alto <- telco_risk[6900, ]

cliente_bajo$riesgo # produce:
#      1779 
#0.05934361 
cliente_medio$riesgo # produce:
#     2383 
#0.2502145 
cliente_alto$riesgo # produce:
#     5554 
#0.4971375 

# Describa cómo interpretaría cada probabilidad.
# Riesgo del 6% es un bajo riesgo de que un cliente abandone una 
# empresa; riesgo del 25% representa un riesgo representa un riesgo 
# moderado de que un cliente abandone una empresa; riesgo del 50%
# representa un riesgo alto de que un cliente abandone una empresa

##################
##
## Ejercicio 2
##
##################

# Ejercicio 2

# Construya un histograma de riesgos.

# Histograma de la probabilidad de churn según la antiguedad
ggplot(
  telco_riesgo,
  aes(
    x = riesgo,
    fill = segmento
  )
) +
  geom_histogram(
    bins = 40
  )

# ¿Qué le dice sobre la distribución del riesgo en la cartera de 
# clientes? Lo que dice la distribución (en U) es que muchos clientes
# tienen un bajo churn, pocos clientes tienen un churn moderado y
# muchos clientes tienen un churn alto
  
##################
##
## Ejercicio 3
##
##################

#  Ejercicio 3
# Compare las distribuciones posteriores de dos clientes distintos.

# Resumir las simulaciones posteriores del cliente 25
quantile(
  prob_churn[, 25],
  probs = c(0.05, 0.5, 0.95)
) # produce:
#       5%       50%       95% 
#0.1240423 0.1331091 0.1429131 

# Resumir las simulaciones posteriores del cliente 26
quantile(
  prob_churn[, 26],
  probs = c(0.05, 0.5, 0.95)
) # produce:

#       5%       50%       95% 
#0.2335072 0.2429537 0.2524561 

# ¿Existe superposición? No hay superposición  
#  ¿Qué implica esa superposición? Como no hay superposición, esto
# significa que cada simulación del modelo no tiene tanta 
# incertidumbre


##################
##
## Ejercicio 4
##
##################
  
#  Ejercicio 4

# Modifique los puntos de corte de los segmentos.

# Segmentar el riesgo
telco_riesgo <- telco_riesgo |>
  mutate(
    segmento = case_when(
      riesgo < 0.20 ~ "Bajo",
      riesgo < 0.40 ~ "Moderado",
      TRUE ~ "Alto"
    )
  )
count(telco_riesgo, segmento) # produce:
# A tibble: 3 × 2
#  segmento     n
#  <chr>    <int>
#1 Alto      2069
#2 Bajo      3001
#3 Moderado  1973

# Segmentar el riesgo
telco_riesgo <- telco_riesgo |>
  mutate(
    segmento = case_when(
      riesgo < 0.17 ~ "Bajo",
      riesgo < 0.34 ~ "Moderado",
      TRUE ~ "Alto"
    )
  )
count(telco_riesgo, segmento) # produce:
# A tibble: 3 × 2
#  segmento     n
#  <chr>    <int>
#1 Alto      2637
#2 Bajo      2687
#3 Moderado  1719


# ¿Cómo cambia el número de clientes clasificados 
# como de alto riesgo? Los de alto riesgo aumentan cerca de 600
  
##################
##
## Ejercicio 5
##
##################

#  Ejercicio 5

# Suponga que sólo puede intervenir sobre el 10% de los clientes.

# ¿Cómo los seleccionaría?

# Ordenar las observaciones en orden ascendente
clientes_prioritarios <- telco_riesgo |>
  arrange(
    desc(riesgo)
  )

# Seleccionar el 10% superior:
n_clientes <- nrow(
    clientes_prioritarios
  )
n_clientes # produce: 7043

nrow(clientes_prioritarios) # produce: 7043

# Seleccionar los 704 (10%) clientes con más riesgo de churn
top_10 <- clientes_prioritarios |>
  slice(
    1:round(
      n_clientes * 0.10
    )
  )
top_10[,-20:-1] # produce:
# A tibble: 704 × 4
#  Churn Churn_num riesgo segmento
#  <chr>     <dbl>  <dbl> <chr>   
#1 No            0  0.507 Alto    
#2 No            0  0.507 Alto    
#3 No            0  0.507 Alto    
#4 No            0  0.507 Alto    
#5 No            0  0.507 Alto    
#6 No            0  0.507 Alto    
# ℹ 694 more rows
# ℹ Use `print(n = ...)` to see more rows
  
slice(clientes_prioritarios, 1:6)[,-20:-1] # produce:
# A tibble: 6 × 4
#  Churn Churn_num riesgo segmento
#  <chr>     <dbl>  <dbl> <chr>   
#1 No            0  0.507 Alto    
#2 No            0  0.507 Alto    
#3 No            0  0.507 Alto    
#4 No            0  0.507 Alto    
#5 No            0  0.507 Alto    
#6 No            0  0.507 Alto 

slice(clientes_prioritarios, 1:(7043 * 0.1))[,-20:-1] # produce:
# A tibble: 704 × 4
#  Churn Churn_num riesgo segmento
#  <chr>     <dbl>  <dbl> <chr>   
#1 No            0  0.507 Alto    
#2 No            0  0.507 Alto    
#3 No            0  0.507 Alto    
#4 No            0  0.507 Alto    
#5 No            0  0.507 Alto    
#6 No            0  0.507 Alto    
#7 No            0  0.507 Alto    
#8 No            0  0.507 Alto    
#9 No            0  0.507 Alto    
#10 No            0  0.507 Alto    
# ℹ 694 more rows
# ℹ Use `print(n = ...)` to see more rows

#  ¿Utilizaría únicamente la probabilidad de abandono?
# No
  
#  ¿Por qué? Porque puede haber otras características adicionales
#  importantes a tener en cuenta
########################
##                    ##
## Capítulo 3         ## 
##                    ##
########################


library(readr)
library(dplyr)
library(tidyverse)

##########################
##
## 3.4 Lanzar una moneda
##
##########################

# Generar semilla aleatoria
set.seed(123)

# Simular cien lanzamientos
lanzamientos <- sample( # elige entre Cara y Sello 100 veces
  c("Cara", "Sello"),
  size = 100,
  replace = TRUE # Después de elegir el resultado, vuelve a ponerlo
                 # disponible "encéralo"  
)
lanzamientos # produce:
#[1] "Sello" "Sello" "Cara"  "Sello" "Cara"  "Sello" "Sello" "Cara" 
#[9] "Sello" "Cara"  "Cara"  "Sello" "Sello" "Sello" "Cara"  "Sello"
#[17] "Sello" "Cara"  "Sello" "Cara"  "Sello" "Sello" "Cara"  "Sello"
table(lanzamientos) # produce
#lanzamientos 
# Cara Sello 
#   38    62 

set.seed(456)
# Simular cin nuevos lanzamientos
lanzamientos_2 <- sample(
  c("Cara", "Sello"),
  size = 100,
  replace = TRUE
)
lanzamientos_2 # produce:
#[1] "Cara"  "Sello" "Sello" "Sello" "Sello" "Cara"  "Cara"  "Sello"
#[9] "Sello" "Sello" "Sello" "Sello" "Cara"  "Cara"  "Sello" "Cara" 
#[17] "Sello" "Sello" "Sello" "Cara"  "Sello" "Cara"  "Sello" "Sello"
table(lanzamientos_2) # produce:
#lanzamientos_2
# Cara Sello 
#   56    44 

##########################
##
## 3.6 Ventas futuras
##
##########################

# Cargar la dataset de superstore sales
sales <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# Distribución de las ventas
sales %>%
  ggplot(aes(x = Sales)) +
  geom_histogram(
    bins = 1000
  ) +
  coord_cartesian(xlim = c(0, 750)) +
  labs(
    title = "Distribución de las ventas",
    x = "Ventas",
    y = "Frecuencia"
  )
  
############### Simulación ################

set.seed(123)

ventas_simuladas <- sample(
  sales$Sales,
  size = 1000,  # Elige 1000 ventas aleatorias de Sales
  replace = TRUE
)

ventas_simuladas # produce:
#[1]   82.5240  377.9700   12.9600   40.7520   46.4400   90.8820
#[7]   14.7300   53.0400  639.9680  146.7300  238.6200   16.5200
#[13]    9.2960  305.0100   73.2000  269.9000  161.5680   25.0600
sim_df <- tibble(
  ventas_simuladas
)
sim_df # produce:
# A tibble: 1,000 × 1
#ventas_simuladas
#             <dbl>
#1             82.5
#2            378. 
#3             13.0
#4             40.8
#5             46.4
#6             90.9

sim_df %>%
  ggplot(aes(x = ventas_simuladas)) +
  geom_density() +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(
    title = "Ventas Simuladas",
    x = "Ventas",
    y = "Densidad"
  )


############# Simular ingresos mensuales #################

set.seed(123)

# Simular ventas mensuales. Simular 1000 ventas totales que se 
# generaron tomando al azar 100 ventas de Sales
simulaciones_mensuales <- replicate( # Replicar
  1000, # Replicar 1000 veces
  sum(sample(  # Sumar 100 ventas aleatorias
    sales$Sales,
    size = 100, # Elige 100 ventas aleatorias
    replace = TRUE
  ))
)
simulaciones_mensuales # produce:
#[1] 28158.31 34086.36 21046.46 19955.81 23707.13 19474.03 16318.57
#[8] 18800.22 18395.45 19462.90 15216.73 20176.15 22947.57 12982.12
#[15] 16851.14 28333.90 31131.15 17209.33 21835.14 24960.71 27644.74
mensual_df <- tibble(
  ingresos = simulaciones_mensuales
)
mensual_df # produce:
# A tibble: 1,000 × 1
#ingresos
#<dbl>
#1   28158.
#2   34086.
#3   21046.
#4   19956.
#5   23707.
#6   19474.
# ℹ 990 more rows
# ℹ Use `print(n = ...)` to see more rows

# Gráfico de los posibles ingresos mensuales
mensual_df %>%
  ggplot(aes(x = ingresos)) +
  geom_histogram(
    bins = 40
  ) +
  labs(
    title = "Posibles ingresos mensuales",
    x = "Ingresos simulados",
    y = "Frecuencia"
  )

##################################
##
## 3.11 Campañas de Marketing 
##
##################################

# Cargar la dataset de marketing
marketing <- read_csv(
  "eda-obesidad/projects/libro_ba/data/marketing_campaign.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

set.seed(123)

# Simular 1000 campañas distintas donde cada campaña tiene 100
# personas y cada persona tiene 10% de probabilidad de convertir
conversiones <- rbinom( # genera resultados binomiales aleatorios
  n = 1000, # Crear 1000 simulaciones distintas
  size = 100, # Cada simulación tendrá 100 personas
  prob = 0.10 # Cada persona tiene 10% de probabilidad de convertir
)
conversiones # produce:
#[1] 14  8 12 10 10  8  9 12  4  9  3 10  9 13 13  9  8  8  7 11  5 11
#[23] 11 11  8 13  9 10  4  9 10  4  8 11  8 10  8 13 12  9 10  8  7 10
#[45]  7 11 10  9 17  6  7 16 11  5 12 13  8  4  7 10 10  8  9 12  6  6
conv_df <- tibble(
  conversiones
) 
conv_df # produce:
# A tibble: 1,000 × 1
#    conversiones
#           <int>
#1           14
#2            8
#3           12
#4           10
#5           10
#6            8
# ℹ 990 more rows
# ℹ Use `print(n = ...)` to see more rows

# Distribución de las conversiones simuladas
conv_df %>%
  ggplot(aes(x = conversiones)) +
  geom_histogram(
    bins = 40
  ) + 
  labs(
    title = "Conversiones simuladas",
    x = "Conversiones",
    y = "Frecuencias"
  )

######################
##
## 3.14 Churn
##
######################

# Cargar la dataset de obesidad
telco <- read_csv(
  "eda-obesidad/projects/libro_ba/data/telco_customer_churn.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

set.seed(123)

# Simular 100 escenarios distintos donde en cada escenario hay 500
# clientes y cada cliente tiene 20% de probabilidad de abandonar
clientes_abandono <- rbinom(
  n = 100, # crear 100 mundos posibles o simulaciones
  size = 500, # cada simulación incluye 500 clientes
  prob = 0.2 # probabilidad de abadono o riesgo de churn 
)
clientes_abandono # produce:
#[1]  97  96 107 101 102 106  90  98  99 106 111  92 106  97  89 106  93
#[18] 112 106 104  93 123  99  99 102 101 107  91  95  98 120 100 105  94
#[35] 113  99 110 105 108  92  94  98 108  96  87  99  94  92 103 105 103
churn_df <- tibble(
  abandono = clientes_abandono
)
churn_df # produce:
# A tibble: 100 × 1
#  abandono
#     <int>
#1       97
#2       96
#3      107
#4      101
#5      102
#6      106
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# Visualizar el churn simulado
churn_df %>%
  ggplot(aes(x = abandono)) +
  geom_histogram(
    bins = 70
  ) +
  labs(
    title = "Abandono simulado",
    x = "Abandono",
    y = "Frecuencia"
  )

###########################
##
## 3.16 Mundos Posibles
##
###########################

set.seed(123)

# Simular trayectorias futuras
trayectorias <- tibble(
  dia = rep(1:30, 50), # Repetir la secuencia del 1 al 30 (días del
                       # mes) 50 veces. (porque queremos 50 simulaciones)
                       # cada una con 30 días que tiene un mes) 
  ventas = cumsum(rnorm(1500, mean = 100, sd = 20)), # rnorm genera
                       # 1500 números aleatorios normales con media
                       # esperada de 100 y desviación de 20
                       # cumsum va acumulando cada uno de los 1500
                       # valores (acumula los cambios diarios)
  simulacion = rep(1:50, each = 30) # Crear los id's de simulación
                       # significa que repita cada número, del 1 al
                       # 50, 30 veces 
)
trayectorias # produce:
# A tibble: 1,500 × 3
#    dia ventas simulacion
#  <int>  <dbl>      <int>
#1     1   91.0          1
#2     2  222.           1
#3     3  339.           1
#4     4  481.           1
#5     5  536.           1
#6     6  659.           1
## ℹ 1,490 more rows
## ℹ Use `print(n = ...)` to see more rows

# Mejorar las trayectorias futuras para que puedan verse bien en
# el gráfico. Este código permite  que las ventas se puedan "encerar"
# al inicio de cada mes
trayectorias2 <- tibble(
  simulacion = rep(1:50, each = 30),
  dia = rep(1:30, 50),
  cambio = rnorm(1500, mean = 100, sd = 20)
) %>%
  group_by(simulacion) %>%
  mutate(
    ventas = cumsum(cambio)
  )

# Visualizar trayectorias futuras
trayectorias2 %>%
  ggplot(aes(
    x = dia,
    y = ventas,
    group = simulacion # Cada simulación debe dibujarse como
    # una línea separada
  )) +
  #coord_cartesian(ylim = c(0, 500)) +
  geom_line(alpha = 0.2) + # dibuja líneas conectando puntos
  labs(
    title = "Múltiples futuros plausibles",
    x = "Día",
    y = "Ventas acumuladas"
  )
# Visualizar trayectorias futuras
trayectorias2 %>%
  ggplot(aes(
    x = dia,
    y = ventas,
    group = simulacion # Cada simulación debe dibujarse como
                       # una línea separada
  )) +
  #coord_cartesian(ylim = c(0, 500)) +
  geom_line(alpha = 0.2) + # dibuja líneas conectando puntos
  labs(
    title = "Múltiples futuros plausibles",
    x = "Día",
    y = "Ventas acumuladas"
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

# Simula 1000 lanzamientos de una moneda con probabilidad de 
# cara igual a 0.7.

# Generar semilla aleatoria
set.seed(123)

# Simular mil lanzamientos
lanzamientos_70 <- sample( # elige entre Cara y Sello 100 veces
  c("Cara", "Cara", "Cara", "Cara", "Cara", "Cara", "Cara", 
    "Sello", "Sello", "Sello"),
  size = 1000,
  replace = TRUE # Después de elegir el resultado, vuelve a ponerlo
  # disponible "encéralo"  
)
lanzamientos_70[1:20] # produce:
#[1] "Cara"  "Cara"  "Sello" "Cara"  "Cara"  "Cara"  "Cara"  "Cara" 
#[9] "Sello" "Sello" "Cara"  "Cara"  "Sello" "Sello" "Sello" "Cara" 
#[17] "Sello" "Sello" "Cara"  "Sello"

table(lanzamientos_70) # produce:
#Cara Sello 
#698   302 

############## Otra forma ###############

set.seed(123)

moneda <- sample(
  c("Cara", "Sello"),
  size = 1000,
  replace = TRUE,
  prob = c(0.7, 0.3)
)

table(moneda)
#moneda
#Cara Sello 
#705   295 

# ¿Qué observas? Que los resultados se acercan mucho al 70% para
# cara
# ¿Cómo cambia respecto a una moneda justa? Casi 20%

#################
##
## Ejercicio 2 
##
#################

# Ejercicio 2

# Usando el dataset de Superstore:
  


summary(sales$Sales) # produce:
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.444    17.280    54.490   229.858   209.940 22638.480 

################# Con 100 ##################

set.seed(123)

# Simular ventas mensuales. Simular 1000 ventas totales que se 
# generaron tomando al azar 100 ventas de Sales
simulaciones_mensuales_100 <- replicate( # Replicar
  1000, # Replicar 1000 veces
  sum(sample(  # Sumar 100 ventas aleatorias
    sales$Sales,
    size = 100, # Elige 100 ventas aleatorias
    replace = TRUE
  ))
)
simulaciones_mensuales_100[1:20] # produce:
#[1] 23609.72 21562.46 13097.79 24579.87 23752.78 21985.92 22723.97
#[8] 24403.10 21328.16 33776.10 13914.36 19650.31 25992.42 17023.92
#[15] 23311.81 17453.23 27646.25 22985.86 28687.60 25928.80

############## Con 1000 ##############

set.seed(123)

# Simular ventas mensuales. Simular 1000 ventas totales que se 
# generaron tomando al azar 100 ventas de Sales
simulaciones_mensuales_1000 <- replicate( # Replicar
  1000, # Replicar 1000 veces
  sum(sample(  # Sumar 1000 ventas aleatorias
    sales$Sales,
    size = 1000, # Elige 1000 ventas aleatorias
    replace = TRUE
  ))
)
simulaciones_mensuales_1000[1:20] # produce:
#[1] 237214.1 205180.8 226117.3 236737.8 231648.3 228233.2 259762.3
#[8] 224200.9 220598.3 219280.2 237597.1 237070.3 201278.2 246220.6
#[15] 219970.4 233215.2 246209.8 213763.7 245971.1 245761.6

############### Con 10000 #####################

set.seed(123)

# Simular ventas mensuales. Simular 10000 ventas totales que se 
# generaron tomando al azar 100 ventas de Sales
simulaciones_mensuales_10000 <- replicate( # Replicar
  1000, # Replicar 1000 veces
  sum(sample(  # Sumar 10000 ventas aleatorias
    sales$Sales,
    size = 10000, # Elige 10000 ventas aleatorias
    replace = TRUE
  ))
)
simulaciones_mensuales_10000[1:20] # produce:
#[1] 2288973 2327058 2319553 2296721 2401697 2221580 2308990 2293268
#[9] 2229417 2304872 2235173 2300950 2317492 2188750 2282655 2293902
#[17] 2305682 2299854 2326645 2309243

# simula ingresos mensuales usando distintos tamaños de muestra,
# compara la variabilidad obtenida.

tibble(
  ingresos = c(simulaciones_mensuales_100, 
               simulaciones_mensuales_1000),
  grupo = c(
    rep("100 ventas", 1000),
    rep("1000 ventas", 1000)
  )
) %>%
  ggplot(aes(x = ingresos)) +
  geom_histogram(bins = 40) +
  facet_wrap(~grupo, scales = "free") +
  labs(
    title = "Distribuciones simuladas",
    x = "Ingresos",
    y = "Frecuencia"
  )

# La varibilidad es porcentualmente menor cuando aumentan las
# ventas simuladas

#################
##
## Ejercicio 3 
##
#################

# Ejercicio 3

# Modifica la probabilidad de conversión:
  
#   5%
# 10%
# 20%

############## Con 5% #############

set.seed(123)

# Simular 1000 campañas distintas donde cada campaña tiene 100
# personas y cada persona tiene 5% de probabilidad de convertir
conversiones_5 <- rbinom( # genera resultados binomiales aleatorios
  n = 1000, # Crear 1000 simulaciones distintas
  size = 100, # Cada simulación tendrá 100 personas
  prob = 0.05 # Cada persona tiene 5% de probabilidad de convertir
)
conversiones_5[1:20] # produce:
# [1] 4 7 4 8 9 2 5 8 5 5 9 5 6 5 2 8 3 2 4 9

############## Con 10% #############

set.seed(123)

# Simular 1000 campañas distintas donde cada campaña tiene 100
# personas y cada persona tiene 10% de probabilidad de convertir
conversiones_10 <- rbinom( # genera resultados binomiales aleatorios
  n = 1000, # Crear 1000 simulaciones distintas
  size = 100, # Cada simulación tendrá 100 personas
  prob = 0.10 # Cada persona tiene 10% de probabilidad de convertir
)
conversiones_10[1:20] # produce:
# [1]  8 12  9 14 15  5 10 14 10 10 15 10 11 10  6 14  8  5  9 15

############## Con 10% #############

set.seed(123)

# Simular 1000 campañas distintas donde cada campaña tiene 100
# personas y cada persona tiene 20% de probabilidad de convertir
conversiones_20 <- rbinom( # genera resultados binomiales aleatorios
  n = 1000, # Crear 1000 simulaciones distintas
  size = 100, # Cada simulación tendrá 100 personas
  prob = 0.20 # Cada persona tiene 20% de probabilidad de convertir
)
conversiones_20[1:20] # produce:
# [1] 18 23 19 25 26 13 20 25 20 19 27 19 22 21 15 25 17 13 18 27

########### Visualización ##################

tibble(
  conversiones = c(conversiones_5, conversiones_10, 
                   conversiones_20),
  grupo = c(
    rep("5%", 1000),
    rep("10%", 1000),
    rep("20%", 1000)
  )
) %>%
  ggplot(aes(x = conversiones)) +
  geom_histogram(bins = 20) +
  facet_wrap(~grupo) +
  labs(
    title = "Simulación de conversiones",
    x = "Conversiones",
    y = "Frecuencia"
  )

# ¿Cómo cambian las distribuciones simuladas? Aumentan porque
# tienen más probabilidad de convertir

#################
##
## Ejercicio 4 
##
#################

# Ejercicio 4

# Simula churn con:
  
#  10%,
#  20%,
#  35%.

############### Con 10% ################

set.seed(123)

# Simular 100 escenarios distintos donde en cada escenario hay 500
# clientes y cada cliente tiene 10% de probabilidad de abandonar
churn_1 <- rbinom(
  n = 1000, # crear 100 mundos posibles o simulaciones
  size = 500, # cada simulación incluye 500 clientes
  prob = 0.1 # probabilidad de abadono o riesgo de churn 
)
churn_1[1:20] # produce:
#  [1] 56 57 47 49 44 56 42 44 58 54 40 54 54 50 60 49 34 46 52 48

############### Con 20% ################

set.seed(123)

# Simular 100 escenarios distintos donde en cada escenario hay 500
# clientes y cada cliente tiene 20% de probabilidad de abandonar
churn_2 <- rbinom(
  n = 1000, # crear 100 mundos posibles o simulaciones
  size = 500, # cada simulación incluye 500 clientes
  prob = 0.2 # probabilidad de abadono o riesgo de churn 
)
churn_2[1:20] # produce:
#[1]  96  98 101  85 120  97  89 106  95  83  95 106 106 116  93 104 105
#[18]  98 106 102

############### Con 35% ################

set.seed(123)

# Simular 100 escenarios distintos donde en cada escenario hay 500
# clientes y cada cliente tiene 35% de probabilidad de abandonar
churn_3 <- rbinom(
  n = 1000, # crear 100 mundos posibles o simulaciones
  size = 500, # cada simulación incluye 500 clientes
  prob = 0.35 # probabilidad de abadono o riesgo de churn 
)
churn_3[1:20] # produce:
#[1] 168 170 174 184 163 160 182 166 181 182 196 166 174 181 171 181 177
#[18] 173 166 167

# Visualización

tibble(
  abandono = c(churn_1, churn_2, churn_3),
  grupo = c(
    rep("10%", 1000),
    rep("20%", 1000),
    rep("35%", 1000)
  )
) %>%
  ggplot(aes(x = abandono)) +
  geom_density() +
  facet_wrap(~grupo) +
  labs(
    title = "Escenarios de churn",
    x = "Clientes que abandonan",
    y = "Densidad"
  )

# ¿Qué escenarios parecen más riesgosos? Por sentido común 20% y 35%

#################
##
## Ejercicio 5 
##
#################

# Ejercicio 5

# Crea múltiples trayectorias simuladas usando diferentes 
# niveles de variabilidad (sd).

############# Con desviación 5% ##############

trayectorias_5 <- tibble(
  simulacion = rep(1:50, each = 30),
  dia = rep(1:30, 50),
  cambio = rnorm(1500, mean = 100, sd = 5)
) %>%
  group_by(simulacion) %>%
  mutate(
    ventas = cumsum(cambio)
  )

# Visualizar trayectorias futuras
trayectorias_5 %>%
  ggplot(aes(
    x = dia,
    y = ventas,
    group = simulacion # Cada simulación debe dibujarse como
    # una línea separada
  )) +
  #coord_cartesian(ylim = c(0, 500)) +
  geom_line(alpha = 0.2) + # dibuja líneas conectando puntos
  labs(
    title = "Múltiples futuros plausibles",
    x = "Día",
    y = "Ventas acumuladas"
  )

############# Con desviación 20% ##############

trayectorias_20 <- tibble(
  simulacion = rep(1:50, each = 30),
  dia = rep(1:30, 50),
  cambio = rnorm(1500, mean = 100, sd = 20)
) %>%
  group_by(simulacion) %>%
  mutate(
    ventas = cumsum(cambio)
  )

# Visualizar trayectorias futuras
trayectorias_20 %>%
  ggplot(aes(
    x = dia,
    y = ventas,
    group = simulacion # Cada simulación debe dibujarse como
    # una línea separada
  )) +
  #coord_cartesian(ylim = c(0, 500)) +
  geom_line(alpha = 0.2) + # dibuja líneas conectando puntos
  labs(
    title = "Múltiples futuros plausibles",
    x = "Día",
    y = "Ventas acumuladas"
  )

############# Con desviación 35% ##############

trayectorias_35 <- tibble(
  simulacion = rep(1:50, each = 30),
  dia = rep(1:30, 50),
  cambio = rnorm(1500, mean = 100, sd = 35)
) %>%
  group_by(simulacion) %>%
  mutate(
    ventas = cumsum(cambio)
  )

# Visualizar trayectorias futuras
trayectorias_35 %>%
  ggplot(aes(
    x = dia,
    y = ventas,
    group = simulacion # Cada simulación debe dibujarse como
    # una línea separada
  )) +
  #coord_cartesian(ylim = c(0, 500)) +
  geom_line(alpha = 0.2) + # dibuja líneas conectando puntos
  labs(
    title = "Múltiples futuros plausibles",
    x = "Día",
    y = "Ventas acumuladas"
  )

# ¿Qué ocurre cuando aumenta la incertidumbre?
# Las líneas de la simulación se van abriendo o separando cuando 
# aumenta la desviación estándar
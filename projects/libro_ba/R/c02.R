########################
##                    ##
## Capítulo 2         ## 
##                    ##
########################


library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset de obesidad
sales <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

####################################
##
## Análisis con superstore sales
##
####################################

# Primer vistazo de la dataset
glimpse(sales)

# Revisar rápidamente algunas variables numéricas
sales %>%
  select(Sales, Profit, Discount, Quantity) %>%
  summary() # produce:
# Sales               Profit             Discount     
#Min.   :    0.444   Min.   :-6599.978   Min.   :0.0000  
#1st Qu.:   17.280   1st Qu.:    1.729   1st Qu.:0.0000  
#Median :   54.490   Median :    8.666   Median :0.2000  
#Mean   :  229.858   Mean   :   28.657   Mean   :0.1562  
#3rd Qu.:  209.940   3rd Qu.:   29.364   3rd Qu.:0.2000  
#Max.   :22638.480   Max.   : 8399.976   Max.   :0.8000  
#
#Quantity    
#Min.   : 1.00  
#1st Qu.: 2.00  
#Median : 3.00  
#Mean   : 3.79  
#3rd Qu.: 5.00  
#Max.   :14.00  

# Distribución de la ventas
ggplot(sales, aes(x = Sales)) +
  geom_histogram(
    bins = 100,
    fill = "steelblue",
    color = "white"
  ) + 
  #coord_cartesian(xlim = c(0, 1000)) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    title = "Distribución de ventas",
    x = "Sales",
    y = "Frecuencia"
  )

# Densidad de las ventas
ggplot(sales, aes(x = Sales)) +
  geom_density(
    fill = "skyblue",
    alpha = 0.5
  ) +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(
    title = "Densidad de ventas",
    x = "Sales",
    y = "Densidad"
  )

# Resumir las ventas por media y mediana
sales %>%
  summarise(
    media_sales = mean(Sales),
    mediana_sales = median(Sales)
  )

# Resumen de percentiles de las ventas
sales %>%
  summarise(
    p25 = quantile(Sales, 0.25),
    mediana = median(Sales),
    p75 = quantile(Sales, 0.75)
  ) # produce:
# A tibble: 1 × 3
#    p25 mediana   p75
#  <dbl>   <dbl> <dbl>
#1  17.3    54.5  210.

# Diagrama de caja de las ventas
ggplot(sales, aes(y = Sales)) +
  geom_boxplot(fill = "tomato") +
  labs(
    title = "Boxplot de ventas",
    y = "ventas"
  )

# Distribución de las ganancias
ggplot(sales, aes(x = Profit)) +
  geom_histogram(
    bins = 500,
    fill = "darkgreen",
    color = "white"
  ) + 
  coord_cartesian(xlim = c(-750, 1000)) +
  labs(
    title = "Distribución de ganancias",
    x = "Profit",
    y = "Frecuencia"
  )

# Diagrama de densidad de las ventas, coloreado por Category
ggplot(sales, aes(x = Sales, fill = Category)) +
  geom_density(alpha = 0.4) +
  coord_cartesian(xlim = c(0, 600)) +
  labs(
    title = "Distribución de ventas por categoría",
    x = "Sales",
    y = "Densidad"
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

# Construye un histograma para Quantity.

ggplot(sales, aes(x = Quantity)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución de la cantidad",
    x = "Cantidad de producto",
    y = "Frecuencia"
  )

# Describe:
  
# forma : Normal
# dispersión: No es muy disperso
# posibles asimetrías: Cola hacia la derecha

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Compara:
  
#  promedio,
#  mediana,
#  percentil 90

sales %>%
  summarise(
    promedio_ventas = mean(Sales),
    mediana_ventas = median(Sales),
    p90 = quantile(Sales, 0.9)
  ) # produce:

#    promedio_ventas mediana_ventas   p90
#              <dbl>          <dbl> <dbl>
#  1            230.           54.5  573.

# de Sales.

# ¿Qué diferencias observas?
# La media es mayor que la mediana y el percentil 90 es muchísimo 
#  más alto. Lo que indica fuerte asimetría, presencia de ventas
#  extremas y cola derecha larga

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3

# Crea boxplots de Profit por Category.

ggplot(sales, aes(x = Category, y = Profit)) +
  geom_boxplot(fill = "green") +
  labs(
    title = "Diagrama de caja de Categorías vs Ganacias",
    x = "Categoría",
    y = "Ganancias"
  )

# Con zoom en el eje Y
ggplot(sales, aes(x = Category, y = Profit)) +
  geom_boxplot(fill = "green") +
  coord_cartesian(ylim = c(-100, 100)) +
  labs(
    title = "Diagrama de caja de Categorías vs Ganacias",
    x = "Categoría",
    y = "Ganancias"
  )
# Pregunta:
  
#  ¿Qué categorías parecen más variables? Tecnología y suminitros
#   de oficina

#################
##
## Ejercicio 4
##
#################

# Explora si existen ventas extremadamente altas.

# Usa:
  
sales %>% 
  select(Sales, Profit, Category) %>%
  arrange(desc(Sales)) %>%
  head(10) # produce:
# A tibble: 9,994 × 1
#  Sales
#  <dbl>
#1 22638.
#2 17500.
#3 14000.
#4 11200.
#5 10500.
#6  9893.
#7  9450.
#8  9100.
#9  8750.
#10  8400.

# Pregunta:
  
#  ¿Qué implicaciones podrían tener estos extremos en 
#   decisiones reales? Alteran mucho el promedio

#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Construye curvas de densidad por región.

ggplot(sales, aes(x = Sales, color = Region)) +
  geom_density() +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(
    title = "Gráfico de Ventas por Región",
    x = "Ventas",
    y = "Proporción"
  )

# Pregunta:
  
#  ¿Todas las regiones parecen comportarse igual? Sí, todas 
#   Parecen comportarse parecido
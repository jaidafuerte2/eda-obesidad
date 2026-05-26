########################
##                    ##
## Capítulo 1         ## 
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


# Explorar las primeras filas
head(sales) # produce:
# A tibble: 6 × 18
#  `Row ID` `Order ID`  `Order Date` `Ship Date` `Ship Mode` `Customer ID`
#     <dbl> <chr>       <chr>        <chr>       <chr>       <chr>        
#1        1 CA-2017-15… 08/11/2017   11/11/2017  Second Cla… CG-12520     
#2        2 CA-2017-15… 08/11/2017   11/11/2017  Second Cla… CG-12520     
#3        3 CA-2017-13… 12/06/2017   16/06/2017  Second Cla… DV-13045     
#4        4 US-2016-10… 11/10/2016   18/10/2016  Standard C… SO-20335     
#5        5 US-2016-10… 11/10/2016   18/10/2016  Standard C… SO-20335     
#6        6 CA-2015-11… 09/06/2015   14/06/2015  Standard C… BH-11710     
# ℹ 12 more variables: `Customer Name` <chr>, Segment <chr>,
#   Country <chr>, City <chr>, State <chr>, `Postal Code` <dbl>,
#   Region <chr>, `Product ID` <chr>, Category <chr>,
#   `Sub-Category` <chr>, `Product Name` <chr>, Sales <dbl>

# Explorar la estructura
#glimpse(sales) # produce:
#Rows: 9,994                                                              
#Columns: 21
#$ `Row ID`        <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, …
#$ `Order ID`      <chr> "CA-2016-152156", "CA-2016-152156", "CA-2016-13…
#$ `Order Date`    <chr> "11/8/2016", "11/8/2016", "6/12/2016", "10/11/2…
#$ `Ship Date`     <chr> "11/11/2016", "11/11/2016", "6/16/2016", "10/18…
#$ `Ship Mode`     <chr> "Second Class", "Second Class", "Second Class",…
#$ `Customer ID`   <chr> "CG-12520", "CG-12520", "DV-13045", "SO-20335",…
#$ `Customer Name` <chr> "Claire Gute", "Claire Gute", "Darrin Van Huff"…
#$ Segment         <chr> "Consumer", "Consumer", "Corporate", "Consumer"…
#$ Country         <chr> "United States", "United States", "United State…
#$ City            <chr> "Henderson", "Henderson", "Los Angeles", "Fort …
#$ State           <chr> "Kentucky", "Kentucky", "California", "Florida"…
#$ `Postal Code`   <chr> "42420", "42420", "90036", "33311", "33311", "9…
#$ Region          <chr> "South", "South", "West", "South", "South", "We…
#$ `Product ID`    <chr> "FUR-BO-10001798", "FUR-CH-10000454", "OFF-LA-1…
#$ Category        <chr> "Furniture", "Furniture", "Office Supplies", "F…
#$ `Sub-Category`  <chr> "Bookcases", "Chairs", "Labels", "Tables", "Sto…
#$ `Product Name`  <chr> "Bush Somerset Collection Bookcase", "Hon Delux…
#$ Sales           <dbl> 261.9600, 731.9400, 14.6200, 957.5775, 22.3680,…
#$ Quantity        <dbl> 2, 3, 2, 5, 2, 7, 4, 6, 3, 5, 9, 4, 3, 3, 5, 3,…
#$ Discount        <dbl> 0.00, 0.00, 0.00, 0.45, 0.20, 0.00, 0.00, 0.20,…
#$ Profit          <dbl> 41.9136, 219.5820, 6.8714, -383.0310, 2.5164, 1…

# Visualizar las ventas
ggplot(sales, aes(x = Sales)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de ventas",
    x = "Ventas",
    y = "Cantidad de pedidos"
  )

# Diagrama de dispersión que relaciona las ventas con las ganancias
ggplot(sales, aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Relación entre ventas y ganancias",
    x = "Ventas",
    y = "Ganancia"
  )

# Diagrama de dispersión que muestra la relación entre descuentos
# y ganacias
ggplot(sales, aes(x = Discount, y = Profit)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Descuentos y ganancias",
    x = "Descuento",
    y = "Ganancia"
  )

# Diagrama de caja que relaciona las categorías de los productos
# con las ventas
ggplot(sales, aes(x = Category, y = Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribución de ventas por categoría",
    x = "Categoría",
    y = "Ventas"
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

# Ejercicio 1 — Explorar otra variable

# Construye un histograma para:
  
#  Profit
# o Discount

# Hsitograma de la ganancias
ggplot(sales, aes(x = Profit)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de ganancias por",
    x = "ganancias",
    y = "cantidad de pedidos"
  )

# Reflexiona:

#  ¿la distribución parece simétrica? No
#  ¿existen valores extremos? Sí
#  ¿la mayoría de observaciones están concentradas en una zona?
#   Sí, los paquetes que dejan ganancias cercanas a cero son los
#   que más se piden

# Histograma de los descuentos
ggplot(sales, aes(x = Discount)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribución de Descuentos por",
    x = "descuentos",
    y = "cantidad de pedidos"
  )

# Reflexiona:

#  ¿la distribución parece simétrica? No
#  ¿existen valores extremos? No tanto
#  ¿la mayoría de observaciones están concentradas en una zona?
#   Sí, al parecer hay más pedidos de la mercadería sin descuento o
#   con poco descuento

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2 — Comparar categorías

# Crea un boxplot usando:
  
#  Segment
# o Region

# Diagrama de caja que relaciona los segmentos con las ventas
ggplot(sales, aes(x = Segment, y = Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribución de ventas por segmento",
    x = "Segmentos",
    y = "Ventas"
  )

# Pregunta:
  
#  ¿qué grupo parece más variable? Oficina 
#  ¿qué grupo parece más estable? Consumidor

# Diagrama de caja que relaciona la región con las ventas
ggplot(sales, aes(x = Region, y = Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribución de ventas por región",
    x = "Región",
    y = "Ventas"
  )

# Pregunta:

#  ¿qué grupo parece más variable? Sur
#  ¿qué grupo parece más estable? Oeste

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3 — Buscar pedidos extremos

# Usa:
  
# sales |> arrange(desc(Sales))
# o

arrange(sales, desc(Sales))

# o

# View(arrange(sales, desc(Sales)))

# Explora los pedidos con ventas más altas.

# Pregunta:
  
#  ¿también generan altas ganancias? Nope , hay algunos que generan
#   pérdidas
#  ¿o algunos parecen poco rentables? Algunos generan pocas ganancias
#   e incluso pérdidas

#################
##
## Ejercicio 4
##
#################

# Ejercicio 4 — Reflexión conceptual

# Piensa en esta pregunta:
  
#  ¿Por qué dos negocios con el mismo promedio de ventas podrían 
#   tener riesgos completamente distintos?
  
#  No necesitas responder matemáticamente.

# Intenta responder usando:
  
#  variabilidad,
#  estabilidad,
#  incertidumbre,
# distribución,
# y comportamiento de clientes.

# Respuesta: Porque pueden tener ganancias distintas, dependiendo
# de los descuentos y el margen de ganancias que tengan. También
# depende mucho de la distribución, puede haber ciertas ventas
# muy infrecuentes, con costos altos pero generan una gran 
# incertidumbre porque son ventas muy atípicas.


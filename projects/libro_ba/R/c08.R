########################
##                    ##
## Capítulo 7         ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)

# Cargar la dataset de ventas
superstore <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# Escalar los ingresos para que no salgan los valores de las 
# simulaciones tan pequeñas
superstore <- superstore %>%
  mutate(
    Discount_c = Discount * 100
  )

#########################
##
## Ajustando el modelo
##
#########################

library(rstanarm)

modelo_profit <- stan_glm(
  Profit ~ Discount_c,
  data = superstore,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_profit) # produce:
#          Median MAD_SD
#(Intercept) 67.7    2.8  
#Discount_c  -2.5    0.1  
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 228.5    1.6 
#
# Interpretación: Es plausible según los datos del modelos que la 
# mediana de las ganancias sea 68 dólares, cuando el descuento es 
# cero. Sin embargo por cada punto percentual de descuento las
# ganancias disminuyen 2.5 dóalares. A pesar de esto todavía hay 229
# dólares en ganancias que no se pueden explicar por los descuentos.

#########################
##
## Visualizar la
## incertidumbre
##
#########################

library(posterior) # produce:

# Extraer la distribución posterior 
draws_profit <- posterior::as_draws_df(modelo_profit) 
draws_profit # produce:
# A draws_df: 500 iterations, 2 chains, and 3 variables
#   (Intercept) Discount_c sigma
#1           70       -2.7   229
#2           69       -2.6   229
#3           70       -2.7   229
#4           69       -2.5   229
#5           68       -2.4   227
# ... with 990 more draws
# ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# Visualizar la incertidumbre
draws_profit %>%
  ggplot(aes(x = Discount_c)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(
    title = "Distribución posterior de la pendiente",
    x = "Pendiente - Descuento",
    y = "Densidad"
  )
# Interpretación : El gráfico muestra que es plausible que dentro
# de una buena parte de la población de estudio cada punto porcentual de
# descuento disminuya las ganancias entre 2.7 y 2.3 dólares.

###################
##
## Intervalos 
## creíbles
##
###################

# Crear los intervalos posteriores
posterior_interval(
  modelo_profit,
  prob = 0.95
  ) # produce:
#                  2.5%      97.5%
#(Intercept)  62.230017  73.282706
#Discount_c   -2.720472  -2.278897
#sigma       225.498713 231.584788

# Interpretación: Es plausible que por cada punto porcentual de 
# descuento las ganancias disminuyan en tre 2.27 y y 2.72 dólares
# en la población de estudio

# Visualizando los intervalos plausibles de la pendiente (descuento)
library(bayesplot) # produce:
mcmc_intervals(
  as.matrix(modelo_profit),
  pars = c("Discount_c")
)
# Interpretación: el punto central es la mediana posterior, la
# franja oscura suele representar el intervalo creíble central 
# del 50%. Y la línea más delgada representa el intervalo creíble
# del 90 0 95% . Entonces lo que no es el punto central es la 
# incertidumbre.

# Convertir las simulaciones posteriores del modelo en una matriz
as.matrix(modelo_profit) # produce:
#          parameters
#iterations (Intercept) Discount_c    sigma
#      [1,]    64.33968  -2.292022 227.5134
#      [2,]    66.57536  -2.441976 229.3877
#      [3,]    70.57756  -2.566881 228.8262
#      [4,]    66.77010  -2.418824 228.7178
#      [5,]    64.67698  -2.247089 228.4859
#      [6,]    65.65913  -2.437365 227.6615
#[ reached 'max' / getOption("max.print") -- omitted 667 rows ]

# Visualizar los intervalos plausibles y la mediana del intercepto
# (ganancias) y la pendiente (descuento)
mcmc_intervals(
  as.matrix(modelo_profit),
  pars = c("(Intercept)","Discount_c")
)


########################
##
## Bandas plausibles
##
########################

# Representar la incertidumbre directamente sobre la recta de la 
# regresión
superstore %>%
  ggplot(aes(x = Discount_c, y = Profit)) +
  geom_point(alpha = 0.2) +
  stat_smooth(
    method = "lm"
  ) 

# Representar la incertidumbre directamente sobre la recta de la 
# regresión con zoom en el eje Y
superstore %>%
  ggplot(aes(x = Discount_c, y = Profit)) +
  geom_point(alpha = 0.2) +
  stat_smooth(
    method = "lm"
  ) +
  coord_cartesian(ylim = c(-500, 500))

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

# Ajusta nuevamente el modelo:
  
#  Profit ~ Discount

modelo_profit <- stan_glm(
  Profit ~ Discount_c,
  data = superstore,
  chains = 2,
  iter = 1000,
  refresh = 0
)

posterior_interval(modelo_profit) # produce:
#                    5%       95%
#(Intercept)  63.152603  72.32911
#Discount_c   -2.671678  -2.31407
#sigma       225.979739 231.15748

# y describe verbalmente la distribución posterior de la pendiente.

# No menciones valores exactos.

# Concéntrate en interpretar plausibilidad.

# Respuesta: dados los datos del modelo, es plausible que cada punto
# porcentual de descuento disminuya las ganancias entre 2.67 y 2.31
# dólares

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Obtén un intervalo creíble del 95%.

posterior_interval(
  modelo_profit,
  prob = 0.95
  ) # produce:
#                  2.5%      97.5%
#(Intercept)  62.351513  73.234313
#Discount_c   -2.702356  -2.282783
#sigma       225.388255 231.795472

# Explica su significado utilizando lenguaje natural.

# Evita terminología técnica.

# En la mayoría de los casos, el modelo indica que es posible que
# cada punto porcentual de descuento disminuya un poco las ganancias

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3

# Construye un gráfico de densidad posterior para la pendiente.

mcmc_intervals(
  as.matrix(modelo_profit),
  pars = c("Discount_c")
)

# Responde:
  
#  ¿La incertidumbre es amplia o estrecha? Varía desde -2.67 hasta
#  -2.32 dólares (más o menos). Es pequeña a mi manera de ver
#  ¿Qué podría explicar esa amplitud? La ganancias dependen de 
#  muchísimos factores más  allá de los descuentos



#################
##
## Ejercicio 4
##
#################

#  Ejercicio 4

# Escribe tres interpretaciones incorrectas y tres interpretaciones correctas 
# de un intervalo creíble.

# Correctas:
# 1) Es plausible que dadas las siimulaciones del modelo cada punto
#    porcentual de descuento se asocia a una disminución de ganancias
#    cercana a 2.5 dólares
#  2) Dadas las simulaciones, el modelo sugiere que es muy posible
#     que los descuentos causen un poco de pérdidas
#  3) Dadas las simulaciones del modelos es plausible que cada punto
#     porcentual de descuento produzca pérdidas de entre 2.7 y 2.3 
#     dólares

# Incorrectas:
# 1) El modelo confirma que cada punto porcentual de descuento causa
#    una pérdida de 2.5 dóalres
# 2) El modelo demuestra que los descuentos causan fuertes pérdidas
# 3) El modelo confirma que cada punto porcentual  de descuento
#   causa pérdidas exactas de entre 2.7 y 2.3 dólares

#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Reflexiona:
  
#  ¿Por qué una empresa podría tomar mejores decisiones cuando 
#   reconoce explícitamente la incertidumbre? Yo creo que es porque
#   la incertidumbre te da un rango más amplio para el mejor escenario.
#   Un promedio es demasiado optimista (o demasiado pesismista 
#   según como se lo vea) pero tener en cuenta el caso peor te ayuda
#   a ser "discreto" tomando decisiones

#################
##
## Ejercicio 6
##
#################

#  Ejercicio 6

# Identifica al menos tres variables que podrían influir sobre Profit pero que 
# no aparecen en el modelo.

glimpse(superstore) # produce:

# 1) Cantidad, 2) Ventas,

# ¿Cómo podría afectar esto la interpretación? Podrían ser un factor
# de confusión debido a que el precio de la venta y la cantidad de
# producto van a afectar las ganancias en mayor o menor medida
# que los descuentos. Ajustar el modelo por estas variables podría
# ser adecuado.

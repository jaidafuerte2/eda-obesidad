########################
##                    ##
## Capítulo 7         ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)
library(posterior)
library(bayesplot)

# Cargar la dataset de ventas
superstore <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)
#glimpse(superstore) # produce:

# Escalar los ingresos para que no salgan los valores de las 
# simulaciones tan pequeñas
superstore <- superstore %>%
  mutate(
    Discount_c = Discount * 100
  )

##################################
##
## Explorar las relaciones
## visualmente
##
##################################

# Diagrama de dispersión que relaciona las ventas con los descuentos
superstore %>%
  ggplot(
    aes(
      x = Discount_c,
      y = Sales
    )
  ) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm", # lineal model
    se = TRUE # standar error
  ) +
  #coord_cartesian(ylim = c(0, 1000)) +
  labs(
    title = "Descuento y ventas",
    x = "Discount",
    y = "Sales"
  )

# Diagrama de dispersión que relaciona las ventas con los descuentos
# con zoom en el eje Y
superstore %>%
  ggplot(
    aes(
      x = Discount_c,
      y = Sales
    )
  ) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm", # lineal model
    se = TRUE # standar error
  ) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(
    title = "Descuento y ventas",
    x = "Discount",
    y = "Sales"
  )

# Diagrama de dispersión que relaciona el descuento vs las ganancias
superstore %>%
  ggplot(
    aes(x = Discount_c, y = Profit)
  ) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = lm, # lineal model
    se = TRUE # standar error
  ) +
  labs(
    title = "Descuento vs Ganancias"
  )

# Diagrama de dispersión que relaciona el descuento vs las ganancias
# con zoom en el eje de las Y
superstore %>%
  ggplot(
    aes(x = Discount_c, y = Profit)
  ) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = lm, # lineal model
    se = TRUE # standar error
  ) +
  coord_cartesian(ylim = c(-200, 200)) +
  labs(
    title = "Descuento vs Ganancias"
  )

# Diagrama de dispersión y tendencia de las ventas vs las ganacias 
superstore %>%
  ggplot(aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm", # lineal model
    se = TRUE # standar error
  ) +
  labs(
    title = "Ventas vs Gananvias"
  )


##########################
##
## Regresión Bayesiana
##
##########################

# Relacionar el descuento con la ganancia
modelo_profit <- stan_glm(
  Profit ~ Discount_c,
  data = superstore,
  family = gaussian(), # la variable respuesta (Profit) es numérica
                       # y continua; podría ser: -150, 12.4, etc. Así
                       # que una distribución normal sí tiene sentido
  chains = 2, # 2 exploraciones
  iter = 1000, # cada exploración obtendrá 1000 muestras
  refresh = 0
)
print(modelo_profit) # produce:
#          Median MAD_SD
#(Intercept) 67.7    2.9  
#Discount_c  -2.5    0.1  
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 228.6    1.7 
#
# Interpretación: el intercepto dice que cuando el descuento es 0% 
# la ganancia promedio esperada es 67.7 . 
# Discount_c = -2.5 es la pendiente que se
# interpreta como que por cada 1 punto porcentual adicional de 
# descuento , la ganancia promedio disminuye aproximadamente
# 2.5 dólares. Sigma nos dice que aunque cada punto porcentual 
# de descuento disminuye 2.5 dólares de ganancias, hay 237 dólares
# de ganancias  que no se pueden explicar por el descuento



# Conocer la incertidumbre de la relación del descuento con las
# ganancias
posterior_interval(
  modelo_profit,
  prob = 0.95
)
#                 2.5%      97.5%
#(Intercept)  62.14556  73.550248
#Discount_c   -2.70576  -2.292317
#sigma       225.14874 231.914454
#
# Interpretación: cada 1% de descuento disminuye las ganancias 
# entre 2.7 y 2.29 dólares. Sin embargo todavía hay entre 225 y 232
# dólares de ganancias que no se pueden explicar por los descuentos

# Poner en formato de tabla las variables y datos del modelo:
# intercepto (ganancias-profit), Discount_c (descuento) y sigma
posterior::as_draws_df(modelo_profit) # produce:
# A draws_df: 500 iterations, 2 chains, and 3 variables
#   (Intercept) Discount_c sigma
#1           68       -2.5   230
#2           70       -2.6   229
#3           67       -2.6   228
#4           68       -2.5   229
#5           67       -2.5   227
#6           65       -2.4   230
# ... with 990 more draws
# ... hidden reserved variables {'.chain', '.iteration', '.draw'} 

# Visualizar la distribución posterior de la pendiente
posterior::as_draws_df(modelo_profit) %>%
  ggplot(
    aes(x = Discount_c)
  ) +
  geom_density() +
  labs(
    title = "Distribución posterior de la pendiente"
  )

posterior::as_draws_df(modelo_profit)

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

# Construya un scatterplot para:

#  Quantity versus Sales

# Diagrama de dispersión que relaciona las ventas con la cantidad
superstore %>%
  ggplot(aes(x = Sales, y = Quantity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Ventas vs cantidad"
  )


# Preguntas:
  
#  ¿Existe una tendencia visible? Sí, obvio
#  ¿Es fuerte o débil? Muy visible
#  ¿Hay mucha dispersión? Un poco

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Construya un modelo:
  
#  Sales ~ Discount

# Explicar las ventas usando el porcentaje de descuento 
modelo_sales <- stan_glm(
  Sales ~ Discount_c,
  data = superstore,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_sales) # produce:
#           Median MAD_SD
#(Intercept) 243.0    7.6 
#Discount_c   -0.8    0.3 
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 622.9    4.5 

# Interprete la pendiente utilizando lenguaje probabilístico.
# La media de las ventas es 243 cuando el descuento es cero. Y por cada
# punto porcentual de descuento las ventas disminuyen 80 centavos
# sin embargo hay 623 dólares en ventas que no se pueden explicar 
# por el descuento.

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3

# Compare visualmente:
  
#  Discount vs Sales
#  Discount vs Profit

# intervalo posterior del modelo de ventas
posterior_interval(modelo_sales) # produce:
#                    5%         95%
#(Intercept) 230.166022 256.2375960
#Discount_c   -1.327394  -0.3446388
#sigma       615.331975 631.1696857

posterior::as_draws_df(modelo_sales) # produce:
# A draws_df: 500 iterations, 2 chains, and 3 variables
#   (Intercept) Discount_c sigma
#1          239      -0.94   622
#2          242      -1.17   619
#3          252      -0.95   621
#4          245      -0.74   623
#5          252      -1.17   622
#6          250      -0.94   625
# ... with 990 more draws
# ... hidden reserved variables {'.chain', '.iteration', '.draw'}
  
# Visualizar la pendiente
posterior::as_draws_df(modelo_sales) %>%
  ggplot(aes(x = Discount_c)) +
  geom_density() +
  labs(
    title = "Distribución posterior de la pendiente"
  )

# ¿Las relaciones parecen similares? Sí , la relación es inversa

#  ¿En qué se diferencian? Los descuentos afectan más a las ganancias
#  que a las ventas y la incertidumbre, la variabilidad es mayor
# en las ventas que en las ganancias.

#################
##
## Ejercicio 4
##
#################

# Ejercicio 4

# Obtenga el intervalo posterior de la pendiente para:
  
#  Profit ~ Discount

modelo_profit2 <- stan_glm(
  Profit ~ Discount_c,
  data = superstore,
  family = gaussian,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_profit2)
#          Median MAD_SD
#(Intercept) 67.5    3.0  
#Discount_c  -2.5    0.1  
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 228.7    1.7 

posterior_interval(modelo_profit2) # produce:
#                    5%        95%
#(Intercept)  62.611885  72.383520
#Discount_c   -2.666779  -2.304444
#sigma       225.807079 231.355942

# ¿La incertidumbre es grande o pequeña? Pequeña

#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Busque una relación que le parezca interesante dentro del dataset.

# Por ejemplo:
  
#  Quantity vs Profit
# Sales vs Quantity
# Sales vs Discount

# Diagrama de dispersión de Quantity vs Profit
superstore %>%
  ggplot(aes(x = Profit, y = Quantity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Ganancias vs Cantidad"
  )

# Conocer cuanto afecta la cantidad a las ganancias
modelo_profit3 <- stan_glm(
  Profit ~ Quantity,
  data = superstore,
  family = gaussian,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_profit3) # produce:
#         Median MAD_SD
#(Intercept) 2.2    4.8   
#Quantity    7.0    1.1   
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 233.8    1.7 
#
# Interpretación: Cada unidad vendida de un producto, produce 7 
# dólares de ganancia. Sin embargo todavía hay 233.8 dólares de
# ganancias que no se pueden explicar por la cantidad

# Intervalo posterior del modelo
posterior_interval(modelo_profit3) # produce:
#                    5%        95%
#(Intercept)  -5.230735   9.630881
#Quantity      5.259053   8.634866
#sigma       231.103278 236.692507
# 
# Interpretación: Cada unidad de producto que se vende aumenta 
# las ganancias entre 5.2 y 8.6 dólares. Sin embargo hay entre 
# 231 y 236 dólares en ganancias que no se pueden explicar por la 
# cantidad

# Visualizar la pendiente
posterior::as_draws_df(modelo_profit3) %>%
  ggplot(aes(x = Quantity)) +
  geom_density() +
  labs(
    title = "Distribución posterior de la pendiente"
  )

# Explique:
  
#  qué observa: Las ganancias aumentan cuando de vende más cantidad
#  qué incertidumbre existe: existe incertidumbre considerable
#  qué conclusiones serían razonables: que en efecto más cantidad
#  de producto, efectivamente se asocia a más ganacias
#  qué conclusiones serían exageradas: Que vender al mayoreo con
#  reducción de precios y ofertas me dará un montón de ganancias
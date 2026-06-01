########################
##                    ##
## Capítulo 6         ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)
library(posterior)
library(bayesplot)

# Cargar la dataset de marketing
marketing <- read_delim(
  "eda-obesidad/projects/libro_ba/data/marketing_campaign.csv",
  delim = ";"
  #guess_max = 10000,
  #show_col_types = FALSE
)
glimpse(marketing) # produce:

# Escalar los ingresos para que no salgan los valores de las 
# simulaciones tan pequeñas
marketing <- marketing %>%
  mutate(
    Income_k = Income / 1000
  )

# Entender como se relaciona Income con Response
modelo_ingresos <- stan_glm(
  Response ~ Income_k,
  data = marketing,
  family = binomial(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0 # para que no se vea el procgreso de ejecución
)
print(modelo_ingresos) # produce:
#            Median MAD_SD
#(Intercept) -2.6    0.2  
#Income       0.0    0.0  

# Ver las simulaciones posteriores individuales del modelo y 
# convertirlas en un dataframe
posterior <- as_draws_df(modelo_ingresos)
posterior # produce:
# A draws_df: 500 iterations, 2 chains, and 2 variables
#   (Intercept) Income_k
#1         -2.7    0.020
#2         -2.6    0.017
#3         -2.7    0.016
#4         -2.6    0.016
#5         -2.5    0.014
# ... with 990 more draws
# Interpretación: la columna de income_K sugiere que cada 1000 dólares
# extras de ganancias o Income, aumenta un poco la probabilidad de
# respuesta a la campaña o Response en casi todos los escenarios. Lo que 
# sugiere que los clientes con mayores ingresos tienden a presentar 
# una probabilidad ligeramente superior de aceptar la campaña

# visualizar la distribución posterior del coeficiente asociado a
# income. Es decir este gráfico muestra cuánto aumenta la probabilidad
# de Respuesta o Response cada 1000 dólares extras de ganancia
posterior %>%
  ggplot(aes(x = Income_k)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribución opsterior del efecto de income",
    x = "Coeficiente plausible",
    y = "Densidad"
  )

# Intervalo posterior de la relación de Income con Response
posterior_interval(modelo_ingresos, prob = 0.95) # produce:
#                    2.5%       97.5%
#(Intercept) -2.95246604 -2.27262622
#Income_k     0.01044604  0.02170858
# Interpretación: Cada 1000 dólares de mayores ganancias, se asocian
# con un aumento de probabilidad de respuesta a la publicidad de entre
# 0.0104 y 0.0217 . También dice el modelo que cuando los ingresos 
# son iguales a cero (algo no imposible pero sí improbable) la 
# probabilidad de una respuesta Response a un campaña publicitaria
# es entre -2.95 y -2.27 (en la escala interna del modelo log-odds)
# es decir : 1 / (1 + exp(2.6)) = 0.07 +- 7% entre -2.95 (5%) y 
# -2.27 (9%). Es decir para una persona de cero de ingresos la
# probabilidad de respuesta Response está entre 5% y 9%



# Tomar todos los escenarios plausibles del modelo y calcular  las 
# predicciones correspondientes.
predicciones <- posterior_linpred( # posterior_linpred: utiliza 
                                   # todas las simulaciones generadas
                                   # por el modelo para calcular 
                                   # predicciones
  modelo_ingresos,
  transform = TRUE # no me des log-odds, dame probabilidades
) # produce:
predicciones
#iterations         1         2         3          4         5         6
#      [1,] 0.1759830 0.1443028 0.2185837 0.10206425 0.1764336 0.1890515
#
#iterations         7          8          9         10         12
#      [1,] 0.1688315 0.11525506 0.10906808 0.06945933 0.07189526
#
#iterations        13        14         15        16         17
#      [1,] 0.1906532 0.1795424 0.08617899 0.2592450 0.13354119
#
# Interpretación: La probabilidad de respuesta a la campaña podría
# ser 17.6 , 14.4 , 21.8 , 10.2 , etc.

# Histograma para Visualizar predicciones (futuros plausibles) 
hist(predicciones[1, ])

# Diagrama de densidad para Visualizar predicciones (futuros 
# plausibles)
tibble(
  probabilidad = predicciones[1, ]
) %>%
  ggplot(aes(x = probabilidad)) +
  geom_density(fill = "skyblue")

# Visualizar distintas curvas plausibles
matplot(
  t(predicciones[1:50, ]),
  type = "l",
  lty = 1,
  col = rgb(1, 0, 1, 0.1)
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

#Modifica el número de iteraciones:
  
# iter = 500

# Entender como se relaciona Income con Response
modelo_ingresos_500 <- stan_glm(
  Response ~ Income_k,
  data = marketing,
  family = binomial(),
  chains = 2,
  iter = 500,
  seed = 1234,
  refresh = 0 # para que no se vea el procgreso de ejecución
)
print(modelo_ingresos_500) # produce:
#             Median MAD_SD
#(Intercept) -2.6    0.2  
#Income_k     0.0    0.0 

# y luego:
  
#  iter = 2000

# Entender como se relaciona Income con Response
modelo_ingresos_2000 <- stan_glm(
  Response ~ Income_k,
  data = marketing,
  family = binomial(),
  chains = 2,
  iter = 2000,
  seed = 1234,
  refresh = 0 # para que no se vea el procgreso de ejecución
)
print(modelo_ingresos_2000) # produce:
#             Median MAD_SD
#(Intercept) -2.6    0.2  
#Income_k     0.0    0.0  
# Pregunta:
  
#  ¿cómo cambia la distribución posterior?

# iter = 500
posterior_interval(modelo_ingresos_500) # produce:
#                     5%         95%
#(Intercept) -2.86677199 -2.34333507
#Income_k     0.01169916  0.02066207

# iter = 2000
posterior_interval(modelo_ingresos_2000) # produce:
#                    5%         95%
#(Intercept) -2.8974146 -2.34140207
#Income_k     0.0116976  0.02063749

#  ¿qué diferencias visuales observas?
# El intervalo se achica un poco (muy poco) cuando iter es mayor

# Con 500
# Ver las simulaciones posteriores individuales del modelo y 
# convertirlas en un dataframe
posterior_500 <- as_draws_df(modelo_ingresos_500)
#
# visualizar la distribución posterior del coeficiente asociado a
# income. Es decir este gráfico muestra cuánto aumenta la probabilidad
# de Respuesta o Response cada 1000 dólares extras de ganancia
posterior_500 %>%
  ggplot(aes(x = Income_k)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribución opsterior del efecto de income",
    x = "Coeficiente plausible",
    y = "Densidad"
  )

# Con 2000
# Ver las simulaciones posteriores individuales del modelo y 
# convertirlas en un dataframe
posterior_2000 <- as_draws_df(modelo_ingresos_2000)
#
# visualizar la distribución posterior del coeficiente asociado a
# income. Es decir este gráfico muestra cuánto aumenta la probabilidad
# de Respuesta o Response cada 1000 dólares extras de ganancia
posterior_2000 %>%
  ggplot(aes(x = Income_k)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribución opsterior del efecto de income",
    x = "Coeficiente plausible",
    y = "Densidad"
  )

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Construye otro modelo usando:

#  Age
# o MntWines # cantidad de dinero gastado en vinos

# Escalar el gasto en vinos para que no salgan los valores de las 
# simulaciones tan pequeñas
marketing <- marketing %>%
  mutate(
    Wine_10 = MntWines / 10
  )

# Entender como se relaciona MntWines con Response
modelo_vinos <- stan_glm(
  Response ~ Wine_10,
  data = marketing,
  family = binomial(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0 # para ocultar el progreso de la ejecución
)
print(modelo_vinos) # produce:
#            Median MAD_SD
#(Intercept) -2.4    0.1  
#MntWines     0.0    0.0 

posterior_interval(modelo_vinos) # produce:
#                    5%         95%
#(Intercept) -2.5779883 -2.25014870
#Wine_10      0.0148633  0.02048194

# Ver las simulaciones posteriores individuales del modelo y 
# convertirlas en un dataframe
posterior_wine <- as_draws_df(modelo_vinos)

# visualizar la distribución posterior del coeficiente asociado a
# vinos. Es decir este gráfico muestra cuánto aumenta la probabilidad
# de Respuesta o Response cada 10 dólares gastados en vino
posterior_wine %>%
  ggplot(aes(x = Wine_10)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribución opsterior del efecto de wine",
    x = "Coeficiente plausible",
    y = "Densidad"
  )

# como predictor.

# Pregunta:

#   ¿la incertidumbre parece mayor o menor? El intervalo
#    es ligeramente menor

#################
##
## Ejercicio 3
##
#################

#Ejercicio 3

#Visualiza múltiples escenarios plausibles usando:

#  posterior_linpred()

# Tomar todos los escenarios plausibles del modelo y calcular  las 
# predicciones correspondientes.
predicciones_vinos <- posterior_linpred(
  modelo_vinos,
  transform = TRUE # no me des log-odds, dame probabilidades
) # produce:
predicciones_vinos # produce:
#iterations         1          2         3          4          5
#      [1,] 0.2373422 0.09451429 0.1775267 0.09451429 0.12173381
#
#iterations         6         7          8          9         10
#      [1,] 0.2028408 0.1338229 0.10471241 0.09496472 0.09709222
#
#iterations         11         12         13         14         15
#      [1,] 0.09361916 0.09376782 0.12571939 0.13341760 0.09332248

# Histograma para Visualizar predicciones (futuros plausibles) 
hist(predicciones_vinos[1, ])

# Diagrama de densidad para Visualizar predicciones (futuros 
# plausibles)
tibble(
  probabilidad = predicciones_vinos[1, ]
) %>%
  ggplot(aes(x = probabilidad)) +
  geom_density(fill = "skyblue")

#Pregunta:

#  ¿qué transmite visualmente el “abanico” de posibilidades?
# Lo que veo es que cada 10 dólares gastados de vino aumenta 
# incluso hasta un 40% las respuestas Responses. 
# Para explicar esto me agradó mucho el histograma porque muestra
# las frecuencias absolutas entonces muestra que los que aumentan 
# hasta un 10% de Responses son unos 800 clientes, los que aumentan
# hasta un 15% de Responses son unos 600 clientes, los que aumentan
# hasta un 20% de Responses son unos 300 clientes, etc.

Ejercicio 4

Interpreta verbalmente un intervalo plausible.

Evita lenguaje matemático complejo.

Intenta explicarlo como si hablaras con:
  
  un gerente,
un médico,
o una persona sin formación estadística.
Ejercicio 5

Reflexión conceptual:
  
  ¿Por qué podría ser peligroso tomar decisiones empresariales ignorando incertidumbre?
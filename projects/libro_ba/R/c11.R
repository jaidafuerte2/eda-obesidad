########################
##                    ##
## Capítulo 11        ## 
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

# Explorar telco
#glimpse(telco) # produce:

################################
##
## Exploración inicial 
## del data set
##
################################

# Ver la distribución del Churn
ggplot(telco, aes(x = Churn)) +
  geom_bar()

# Ver la antiguedad del cliente en meses
ggplot(telco, aes(x= tenure)) +
  geom_histogram(bins = 30)

# Ver los que cancelan suscripción en la distribución de antiguedad
# del cliente
ggplot(telco, aes(x= tenure, fill = Churn)) +
  geom_histogram(bins = 30)

# Ver los que cancelan suscripción en la distribución de pagos
# mensuales del cliente
ggplot(telco, aes(x= MonthlyCharges, fill = Churn)) +
  geom_histogram(bins = 30)#, position = "fill")

# Ver los que cancelan según el tipo de contrato
ggplot(telco, aes(x= Contract, fill = Churn)) +
  geom_bar(position = "fill")

################################
##
## Primer Modelo logístico 
## Bayesiano
##
################################

class(telco$Churn) # produce: "Character"
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
print(modelo_churn) # produce:
#            Median MAD_SD
#(Intercept) 0.0    0.0   
#tenure      0.0    0.0 

summary(modelo_churn) # produce:
#Estimates:
#            mean   sd   10%   50%   90%
#(Intercept) 0.0    0.0  0.0   0.0   0.1  
#tenure      0.0    0.0  0.0   0.0   0.0  
#
#Fit Diagnostics:
#        mean   sd   10%   50%   90%
#mean_PPD 0.3    0.0  0.3   0.3   0.3  
#
#MCMC diagnostics
#mcse Rhat n_eff
#(Intercept)   0.0  1.0  621  
#tenure        0.0  1.0  425  
#mean_PPD      0.0  1.0  669  
#log-posterior 0.1  1.0  367  

coef(modelo_churn) # produce:
#(Intercept)      tenure 
# 0.02615887 -0.03881346 
# Interpretación: Por cada mes de antiguedad, disminuye el churn o 
# probabilidad de cancelación en 0.0388 en la escala logística
# interna, NO en probabilidad, NO  significa 3.8% de disminución 
# del churn

# Mostrar las 6 primeras filas de las simulaciones del modelo
as.matrix(modelo_churn) |> head() # produce:
#          parameters
#iterations (Intercept)      tenure
#      [1,]  0.08110424 -0.04063877
#      [2,] -0.02708866 -0.03628836
#      [3,]  0.08586388 -0.04138735
#      [4,]  0.01234581 -0.03783448
#      [5,]  0.00996478 -0.03722256
#      [6,]  0.03381633 -0.03812044

as.matrix(modelo_churn) # produce:
#            parameters
#iterations    (Intercept)      tenure
#      [1,]  0.08110424009 -0.04063877
#      [2,] -0.02708865516 -0.03628836
#      [3,]  0.08586387568 -0.04138735
#      [4,]  0.01234581340 -0.03783448
#      [5,]  0.00996478016 -0.03722256
#      [6,]  0.03381633109 -0.03812044
#[ reached 'max' / getOption("max.print") -- omitted 500 rows ]

posterior_interval(modelo_churn) # produce:
#                     5%         95%
#(Intercept) -0.03609829  0.09296688
#tenure      -0.04111115 -0.03648721
# Interpretación: La evidencia bayesiana de que cada mes de antiguedad
# disminuye el churn es bastante fuerte pues el intervalo no 
# incluye al cero y es negativo. Además la incertidumbre es pequeña

count(telco, Churn) # produce:
# A tibble: 2 × 2
#  Churn     n
#  <chr> <int>
#1 No     5174
#2 Yes    1869

count(telco, Churn_num) # produce:
#  Churn_num     n
#      <dbl> <int>
#1         0  5174
#2         1  1869

############### Algo más pedagógico ##################
posterior::summarise_draws(
  as.matrix(modelo_churn)
) # produce:
# A tibble: 2 × 10
#  variable    mean  median      sd     mad      q5     q95  rhat ess_bulk
#  <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>    <dbl>
#1 (Interc…  0.0273  0.0262 0.0406  0.0398  -0.0361  0.0930  1.00     623.
#2 tenure   -0.0388 -0.0388 0.00141 0.00140 -0.0411 -0.0365  1.00     429.
# ℹ 1 more variable: ess_tail <dbl>

bayesplot::mcmc_intervals(
  as.matrix(modelo_churn),
  pars = c("(Intercept)", "tenure")
)

bayesplot::mcmc_intervals(
  as.matrix(modelo_churn),
  pars = c("tenure")
)
# Interpretación: el gráfico muestra que el punto central o mediana 
# de la disminución del churn por cada mes de antiguedad es negativo
# y el 50% de la población está entre -0.040 y 0.038 en la escala
# logística interna y el 90% está entre -0.041 y 0.036
##########################
##
## Visualizar el riesgo
##
##########################

# Probabilidades predichas

grid_tenure <- tibble(
  tenure = seq(
    min(telco$tenure),
    max(telco$tenure),
    length.out = 100
  )
)
grid_tenure # produce:
# A tibble: 100 × 1
#   tenure
#   <dbl>
#1  0    
#2  0.727
#3  1.45 
#4  2.18 
#5  2.91 
#6  3.64 
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# seq() genera secuencias de números
seq(
  min(telco$tenure), # busca el valor mínimo de tenure
  max(telco$tenure), # busca el valor máximo de tenure
  length.out = 100 # Indica 100 puntos o marcas igualmente espaciadas
) # produce:
#[1]  0.0000000  0.7272727  1.4545455  2.1818182  2.9090909  3.6363636
#[7]  4.3636364  5.0909091  5.8181818  6.5454545  7.2727273  8.0000000
#[13]  8.7272727  9.4545455 10.1818182 10.9090909 11.6363636 12.3636364

# epred = expected prediciton
# Conocer la probabilidad de churn dado cierto nivel de antiguedad
pred <- posterior_epred(
  modelo_churn, # Modelo del efecto de la antiguedad en el churn
  newdata = grid_tenure # Meses de antiguedad
)
pred # produce:
#iterations         1         2         3         4         5         6
#[1,] 0.5202650 0.5128843 0.5054981 0.4981095 0.4907217 0.4833379
#[2,] 0.4932283 0.4866331 0.4800427 0.4734592 0.4668849 0.4603220
#[3,] 0.5214528 0.5139374 0.5064157 0.4988911 0.4913669 0.4838467
#[4,] 0.5030864 0.4962075 0.4893301 0.4824567 0.4755899 0.4687323
#[9,] 0.5311772 0.5233973 0.5156060 0.5078071 0.5000044 0.4922018
#[10,] 0.4878905 0.4811202 0.4743569 0.4676030 0.4608609 0.4541331
#[ reached 'max' / getOption("max.print") -- omitted 990 rows, 95 cols ]
# Las filas son la distribución posterior, es decir 1000 filas de 
# distribución posterior para cada una de las 100 columnas
dim(pred) # produce: [1] 1000  100

# Crear una nueva tabla con la mediana, el cuantil 5 y el cuantil 
# 95 de las probabilidades de grid_tenure. Es decir, para cada una de
# las 100 columnas de grid_tenure, calcular la mediana y los límites
probabilidades <- grid_tenure %>%
  mutate(
    # Para cada columna de pred calcula la mediana
    mediana = apply(
      pred,
      2,
      median
    ),
    # Para cada columna calcula el cuantil 5
    li = apply(
      pred,
      2,
      quantile,
      probs = 0.05
    ),
    # Para cada columna calcula el cuantil 95
    ls = apply(
      pred,
      2,
      quantile,
      probs = 0.95
    )
  )
probabilidades # produce:
# A tibble: 100 × 4
#  tenure mediana    li    ls
#   <dbl>   <dbl> <dbl> <dbl>
#1  0       0.507 0.491 0.523
#2  0.727   0.499 0.484 0.516
#3  1.45    0.492 0.478 0.509
#4  2.18    0.485 0.471 0.501
#...
#9  5.82    0.450 0.437 0.465
#10  6.55    0.443 0.430 0.458
# ℹ 90 more rows
# ℹ Use `print(n = ...)` to see more rows

# Curva logística
ggplot(
  probabilidades,
  aes(
    x = tenure,
    y = mediana
  )
) +
  # Graficar la banda con los límites
  geom_ribbon(
    aes(
      ymin = li,
      ymax = ls
    ),
    alpha = 0.2
  ) +
  # Graficar la mediana
  geom_line()

######################
##
## Ejercicios
##
######################

######################
##
## Ejercicio 1
##
######################

# Ejercicio 1

# Construya un modelo:
  
#  Churn_num ~ MonthlyCharges

# Ver los que cancelan suscripción en la distribución de pagos
# mensuales del cliente
ggplot(telco, aes(x= MonthlyCharges, fill = Churn)) +
  geom_histogram(bins = 30, position = "fill")

# Crear un modelo logístico bayesiano para conocer cuanto influye 
# el pago mensual en la cancelación de suscripción
modelo_cargos <- stan_glm(
  Churn_num ~ MonthlyCharges,
  data = telco,
  family = binomial(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0
)
# conocer la mediana del cambio de churn por cada dólar pagado 
# mensualmente
coef(modelo_cargos) # produce:
# (Intercept) MonthlyCharges 
#-2.06357304     0.01544895
# Interpretación: Por cada dólar pagado, el churn aumenta 0.015 punto
# en la escala logística interna 

# Conocer el intervalo posterior del efecto de los cargos mensuales 
# sobre el churn
posterior_interval(modelo_cargos) # produce:
#                       5%         95%
#(Intercept)    -2.1828784 -1.94679482
#MonthlyCharges  0.0139225  0.01685773
# Interpretación: por cada dólar pagado mensualmente, el riesgo de
# churn aumenta entre 0.014 y 0.016 puntos en la escala logística
# interna 

# Transformar en matriz los intervalos posteriores
#as.matrix(modelo_cargos) # produce:

# Resumir la matriz de intervalos posteriores
posterior::summarise_draws(
  as.matrix(modelo_cargos)
) # produce:
# A tibble: 2 × 10
#     variable    mean  median      sd     mad      q5     q95  rhat ess_bulk
#     <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>    <dbl>
#  1 (Interc… -2.07   -2.06   7.05e-2 7.04e-2 -2.18   -1.95    1.00     740.
#  2 Monthly…  0.0154  0.0154 8.96e-4 8.75e-4  0.0139  0.0169  1.00     756.
# Interpretación: La incertidumbre es pequeña debido a que la
# desviación estándar es cercana a 0.000896

# Visualizar el intervalo posterior de los pagos mensuales
bayesplot::mcmc_intervals(
  as.matrix(modelo_cargos),
  pars = c("MonthlyCharges")
)

# Genera una tabla con secuencias de números (seq()) (100 números) 
# entre el valor mínimo y máximo de la variable MonthlyCharges en un
# intervalo bien definido
grid_cargos <- tibble(
  MonthlyCharges = seq(
    min(telco$MonthlyCharges), # Busca el valor mínimo
    max(telco$MonthlyCharges), # Busca el valor máximo
    length.out = 100 # Indica 100 puntos o marcas igualmente 
                     # espaciadas 
  )
)
grid_cargos # produce:

# epred = expected prediciton
# Conocer la probabilidad de churn dado cierto pago mensual
pred_cargos <- posterior_epred(
  modelo_cargos, # Modelo del efecto de la antiguedad en el churn
  newdata = grid_cargos # Pago mensual
)
pred_cargos # produce:
#iterations         1         2         3         4         5         6
#      [1,] 0.3406153 0.3314115 0.3223348 0.3133902 0.3045823 0.2959152
#      [2,] 0.3341774 0.3260315 0.3179893 0.3100542 0.3022294 0.2945178
#      [3,] 0.3386188 0.3292742 0.3200626 0.3109892 0.3020588 0.2932757
#      [4,] 0.3366780 0.3281551 0.3197439 0.3114484 0.3032721 0.2952185
#      ....  
#      [9,] 0.3411022 0.3313790 0.3217976 0.3123637 0.3030829 0.2939599
#      [10,] 0.3254853 0.3172337 0.3090955 0.3010739 0.2931723 0.2853933
#[ reached 'max' / getOption("max.print") -- omitted 990 rows - 94 cols ]

dim(pred_cargos) # produce: [1] 1000  100

# Crear una nueva tabla con la mediana, el cuantil 5 y el cuantil 
# 95 de las probabilidades de grid_catgos. Es decir, para cada una de
# las 100 columnas de grid_cargos, calcular la mediana y los límites
prob_cargos <- grid_cargos %>%
  mutate(
    # Para cada columna de pred calcula la mediana
    mediana = apply(
      pred_cargos,
      2,
      median
    ),
    # Para cada columna calcula el cuantil 5
    li = apply(
      pred_cargos,
      2,
      quantile,
      probs = 0.05
    ),
    # Para cada columna calcula el cuantil 95
    ls = apply(
      pred_cargos,
      2,
      quantile,
      probs = 0.95
    )
  )
prob_cargos # produce:
# A tibble: 100 × 4
#  MonthlyCharges mediana    li    ls
#           <dbl>   <dbl> <dbl> <dbl>
#1           18.2   0.336 0.325 0.347
#2           19.3   0.327 0.317 0.338
#3           20.3   0.319 0.308 0.329
#4           21.3   0.310 0.300 0.320
#            ...
#9           26.4   0.270 0.260 0.279
#10          27.4   0.262 0.253 0.272
## ℹ 90 more rows
## ℹ Use `print(n = ...)` to see more rows

# Curva logística de los cargos mensuales vs el churn
ggplot(
  prob_cargos,
  aes(
    x = MonthlyCharges,
    y = mediana
  )
) +
  # Graficar la banda con los límites
  geom_ribbon(
    aes(
      ymin = li,
      ymax = ls
    ),
    alpha = 0.2
  ) +
  # Graficar la mediana
  geom_line() +
  labs(
    title = "Curva logística de los cargos mesuales vs el churn",
    x = "Cargos mensuales",
    y = "Churn"
  )

######################
##
## Ejercicio 2
##
######################

# Ejercicio 2

# Construya un modelo:
  
#  Churn_num ~ Contract

# Ver los que cancelan según el tipo de contrato
ggplot(telco, aes(x= Contract, fill = Churn)) +
  geom_bar(position = "fill")

# Crear un modelo logístico bayesiano para conocer cuanto influye 
# el pago mensual en la cancelación de suscripción
modelo_contrato <- stan_glm(
  Churn_num ~ Contract,
  data = telco,
  family = binomial(),
  chains = 2,
  iter = 1000,
  seed = 1234,
  refresh = 0
)
# conocer la mediana del cambio de churn por cada dólar pagado 
# mensualmente
coef(modelo_contrato) # produce:
#(Intercept) ContractOne year ContractTwo year 
#-0.2941638       -1.7754929       -3.2543206

# Conocer el intervalo posterior del efecto del tipo de contrato 
# sobre el churn
posterior_interval(modelo_contrato) # produce:
#                        5%        95%
#(Intercept)      -0.350713 -0.2383492
#ContractOne year -1.915127 -1.6324898
#ContractTwo year -3.544587 -3.0153806

# Resumir la matriz de intervalos posteriores
posterior::summarise_draws(
  as.matrix(modelo_contrato)
) # produce:
# A tibble: 3 × 10
#  variable         mean median     sd    mad     q5    q95  rhat ess_bulk
#  <chr>           <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>
#1 (Intercept)    -0.294 -0.294 0.0341 0.0330 -0.351 -0.238  1.02     961.
#2 ContractOne y… -1.77  -1.78  0.0857 0.0812 -1.92  -1.63   1.00     395.
#3 ContractTwo y… -3.26  -3.25  0.158  0.150  -3.54  -3.02   1.00     314.

# Visualizar el intervalo posterior de los pagos mensuales
bayesplot::mcmc_intervals(
  as.matrix(modelo_contrato),
  pars = c("(Intercept)", "ContractOne year", "ContractTwo year")
)
# Interpretación: En este gráfico se observa bastante bien como
# el riesgo de churn disminuye con contratos de dos años

nuevo <- tibble(
  Contract = c(
    "Month-to-month",
    "One year",
    "Two year"
  )
)

pred_contrato <- posterior_epred(
  modelo_contrato,
  newdata = nuevo
) # produce:
# Imprimir 6 de las 1000 probabilidades posteriores para month to
# month, one year y two year
pred_contrato[1:6,] # produce:
#iterations         1         2          3
#      [1,] 0.4301302 0.1119622 0.02429004
#      [2,] 0.4252378 0.1137865 0.02855035
#      [3,] 0.4244870 0.1143199 0.02566325
#      [4,] 0.4267037 0.1092528 0.03374708
#      [5,] 0.4207175 0.1185358 0.02628420
#      [6,] 0.4265503 0.1180472 0.02829075

# Crear una nueva tabla con la mediana de las probabilidades del
# modelo 
prob_contrato <- tibble(
  Contract = nuevo$Contract,
  mediana = apply(pred_contrato, 2, median)
) 
prob_contrato # produce:
# A tibble: 3 × 2
#  Contract       mediana
#  <chr>            <dbl>
#1 Month-to-month  0.427 
#2 One year        0.112 
#3 Two year        0.0280
# Interpretación: La probabilidad de churn del contrato de mes a 
# mes es 42%; el contrato de un año tiene una probabilidad de churn
# del 11% y el contrato de 2 años tiene una probabilidad de churn
# de 2%

# Gráfico de columnas de la probabilidad del tipo de contrato
ggplot(
  prob_contrato,
  aes(
    x = Contract,
    y = mediana
  )
) +
  geom_col()

# ¿Qué tipo de contrato parece asociado con menor riesgo?

# Respuesta: El contrato de dos años

######################
##
## Ejercicio 3
##
######################

# Ejercicio 3

# Genere una curva logística para MonthlyCharges.

# Curva logística de los cargos mensuales vs el churn
ggplot(
  prob_cargos,
  aes(
    x = MonthlyCharges,
    y = mediana
  )
) +
  # Graficar la banda con los límites
  geom_ribbon(
    aes(
      ymin = li,
      ymax = ls
    ),
    alpha = 0.2
  ) +
  # Graficar la mediana
  geom_line() +
  labs(
    title = "Curva logística de los cargos mesuales vs el churn",
    x = "Cargos mensuales",
    y = "Churn"
  )

# Interprete la incertidumbre posterior.

# Respuesta: La incettidumbre posterior es 0.000896 puntos en la 
# escala logística por eso se ve en el gráfico un ribbon delgado

######################
##
## Ejercicio 4
##
######################

# Ejercicio 4

# Compare dos clientes:
  
#  tenure = 5
#  tenure = 60

# ¿Quién parece tener mayor riesgo?

# Respeusta: Según la curva logística con 5 meses el chunk es ~0.47
# y con 60 meses el chunk es 0.1. El riesgo de chunk es mucho mayor
# a los 5 meses
  
# ¿Con cuánta certeza puede afirmarlo? Con un amplio grado de certeza
# porque la incertidumbre es muy baja

######################
##
## Ejercicio 5
##
######################

# Ejercicio 5

# Explique con sus propias palabras la diferencia entre:
  
#  una etiqueta de clasificación; La etiqueta es una opción única
#  y de acuerdo a esto interpretas, sin importar los intervalos
#  una probabilidad de clasificación: Una probabilidad de 
# clasificación se basa en intervalos plausibles y eso da más opciones
# de interpretación porque toma en cuenta casos extremos.


#######################
##
## Observaciones
##
#######################

# cambiar la escala logistica interna a probabilidades:

#posterior_epred(modelo_cargos)  #produce:

# Ver probabilidades y no puntos en la escala logística
clientes <- tibble(
  MonthlyCharges = c(20, 50, 80)
)
posterior_epred(
  modelo_cargos,
  newdata = clientes
) # produce: 
#iterations         1         2         3
#      [1,] 0.1406904 0.2108951 0.3037488
#      [2,] 0.1412645 0.2113216 0.3038294
#      [3,] 0.1554620 0.2168469 0.2940311
#      [4,] 0.1537755 0.2173652 0.2979908
#      [5,] 0.1503820 0.2151643 0.2980633
#      [6,] 0.1405475 0.2120108 0.3068378

pred_cargos <- posterior_epred(
  modelo_cargos,
  newdata = clientes
)

# Porbabilidad mediana para 20%, 50% y 80%
apply(pred, 2, median) # produce:
#        1         2         3 
#0.1470964 0.2146826 0.3026599  


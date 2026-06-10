########################
##                    ##
## Capítulo 11        ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)

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

as.matrix(modelo_churn) |> head() # produce:
#          parameters
#iterations (Intercept)      tenure
#      [1,]  0.08110424 -0.04063877
#      [2,] -0.02708866 -0.03628836
#      [3,]  0.08586388 -0.04138735
#      [4,]  0.01234581 -0.03783448
#      [5,]  0.00996478 -0.03722256
#      [6,]  0.03381633 -0.03812044

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

pred <- posterior_epred(
  modelo_churn,
  newdata = grid_tenure
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

probabilidades <- grid_tenure %>%
  mutate(
    mediana = apply(
      pred,
      2,
      median
    ),
    li = apply(
      pred,
      2,
      quantile,
      probs = 0.05
    ),
    ls = apply(
      pred,
      2,
      quantile,
      probs = 0.95
    )
  )

# Curva logística
ggplot(
  probabilidades,
  aes(
    x = tenure,
    y = mediana
  )
) +
  geom_ribbon(
    aes(
      ymin = li,
      ymax = ls
    ),
    alpha = 0.2
  ) +
  geom_line()

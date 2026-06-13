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
# cliente
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

#############################
##
## Incertidumbre individual
##
#############################

# Mostrar las simulaciones posteriores del cliente número 25 (de
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

# Visualizar
tibble(
  riesgo = c(
    prob_churn[,25],
    prob_churn[,120]
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

########################
##                    ##
## Capítulo 14        ## 
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

# Agregar la variable de fecha de orden, con formato de fecha; basado 
# en la variable existente 
superstore <- superstore %>%
  mutate(
    order_date = as.Date(
      `Order Date`,
      format = "%m/%d/%Y"
    )
  )
#glimpse(superstore) # produce:

#agrupar por fecha de orden 
sales_daily <- superstore %>%
  group_by(order_date) %>%
  summarise(
    sales = sum(Sales),
    .groups = "drop"
  )
sales_daily # produce:
# A tibble: 1,237 × 2
#  order_date   sales
#  <date>       <dbl>
#1 2014-01-03   16.4 
#2 2014-01-04  288.  
#3 2014-01-05   19.5 
#4 2014-01-06 4407.  
#5 2014-01-07   87.2 
#6 2014-01-09   40.5 
# ℹ 1,231 more rows
# ℹ Use `print(n = ...)` to see more rows

# Visualizar la evolución temporal de las ventas
ggplot(
  sales_daily,
  aes(
    x = order_date,
    y = sales
  )
) +
  geom_line() +
  #coord_cartesian(xlim = c(2014-01-01, 2014-12-31)) +
  labs(
    title = "Ventas diarias",
    x = "Fecha",
    y = "Ventas"
  )

# Graficar la tendencia de las ventas a lo largo del tiempo
ggplot(
  sales_daily,
  aes(
    x = order_date,
    y = sales
  )
) +
  geom_line(
    alpha = 0.4
  ) +
  geom_smooth(
    se = TRUE
  ) +
  labs(
    title = "Tendencia general de ventas"
  )

# Graficar la tendencia de las ventas a lo largo del tiempo con zoom
# en el eje Y
ggplot(
  sales_daily,
  aes(
    x = order_date,
    y = sales
  )
) +
  geom_line(
    alpha = 0.4
  ) +
  geom_smooth(
    se = TRUE
  ) +
  coord_cartesian(ylim = c(0, 4000)) +
  labs(
    title = "Tendencia general de ventas"
  )

# Crear una variable con el número de días de ventas ordenado
sales_daily <- sales_daily %>%
  mutate(
    day = row_number()
  )
sales_daily # produce:
# A tibble: 1,237 × 3
#  order_date   sales   day
#  <date>       <dbl> <int>
#1 2014-01-03   16.4      1
#2 2014-01-04  288.       2
#3 2014-01-05   19.5      3
#4 2014-01-06 4407.       4
#5 2014-01-07   87.2      5
#6 2014-01-09   40.5      6
# ℹ 1,227 more rows
# ℹ Use `print(n = ...)` to see more rows
#
summary(sales_daily$day) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1     310     619     619     928    1237 

# Modelo para predecir las ventas (como respuesta) según el tiempo 
# (como predictor)
model_forecast <- stan_glm(
  sales ~ day,
  data = sales_daily,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(model_forecast) # produce:
#                      5%         95%
#(Intercept)  953.2584876 1380.522116
#day            0.8218787    1.380088
#sigma       2201.5437341 2352.382220

# Construir una tabla con 90 días posteriores al último día de 
# ventas del modelo (con la tabla original)
future_days <- tibble(
  day = seq(
    max(sales_daily$day) + 1,
    max(sales_daily$day) + 90
  )
)
future_days # produce:
# A tibble: 90 × 1
#day
#<int>
#1  1238
#2  1239
#3  1240
#4  1241
#5  1242
#6  1243
# ℹ 80 more rows
# ℹ Use `print(n = ...)` to see more rows

# Simular escenarios plausibles
future_draws <- posterior_predict(
  model_forecast,
  newdata = future_days
)
# Imprimir las 1000 simulaciones posteriores (1000 filas) para cada 
# uno de los 90 días (90 columnas)
future_draws # produce:
#                1            2             3            4           5
#[1,]  1560.981551  4255.926287  1912.8674764  2638.489723  3307.28256
#[2,]  -375.866499  5951.773832  2731.9254363   367.480464  4798.67342
#[3,]  2433.520943 -1453.747715  1922.1357896   832.457613  5455.93793
#[4,]   218.269410  2518.913577  -206.2506851  1244.532426  1156.46026
#[5,]  4487.322348  3128.528793  6110.3179803  3415.367159  3882.61749
#[6,]   620.735257  1207.613476  1432.5129244  9352.342657  4504.50090
#[ reached 'max' / getOption("max.print") -- omitted 994 rows, 85 cols ]

# Resumir los escenarios plausibles del futuro (los 90 días del
# futuro)
forecast_summary <- tibble(
  day = future_days$day,
  median = apply(
    future_draws,
    2,
    median
  ),
  low = apply(
    future_draws,
    2,
    quantile,
    probs = 0.10
  ),
  high = apply(
    future_draws,
    2,
    quantile,
    probs = 0.90
  )
)
# Imprimir el resumen de los 90 días futuros
forecast_summary # produce:
# A tibble: 90 × 4
#    day median   low  high
#  <int>  <dbl> <dbl> <dbl>
#1  1238  2618. -249. 5277.
#2  1239  2385. -546. 5441.
#3  1240  2630. -272. 5463.
#4  1241  2603. -227. 5550.
#...
#9  1246  2605. -534. 5448.
#10  1247  2540. -429. 5402.
# ℹ 80 more rows
# ℹ Use `print(n = ...)` to see more rows

# Visualizar los escenarios plausibles de ventas futuras
ggplot(
  forecast_summary,
  aes(
    x = day,
    y = median
  )
) +
  geom_ribbon(
    aes(
      ymin = low,
      ymax = high
    ),
    alpha = 0.3
  ) +
  geom_line() +
  labs(
    title = "Escenarios plausibles de ventas futuras",
    y = "Ventas"
  )

# Visualizar los escenarios plausibles de ventas futuras, con zoom
# en el eje Y
ggplot(
  forecast_summary,
  aes(
    x = day,
    y = median
  )
) +
  geom_ribbon(
    aes(
      ymin = low,
      ymax = high
    ),
    alpha = 0.3
  ) +
  geom_line() +
  geom_smooth(
    se = TRUE
  ) +
  coord_cartesian(ylim = c(2500, 2750)) +
  labs(
    title = "Escenarios plausibles de ventas futuras",
    y = "Ventas"
  )

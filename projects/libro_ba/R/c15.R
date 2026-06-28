########################
##                    ##
## Capítulo 15        ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)
library(posterior) 
library(bayesplot)
library(lubridate)

# Cargar la dataset de ventas
superstore <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)
#glimpse(superstore) # produce:

class(superstore$`Order Date`) # produce: character
# Modificar el tipo de la variable de fecha de la orden de character
# a date y crear la variable mes que cambia esta fecha a su versión 
# del primer día del mes
ventas_mensuales <-
  superstore |>
  mutate(
    Order_Date = mdy(`Order Date`),
    mes = floor_date(Order_Date, "month")
  )
ventas_mensuales[,-20:-1] # produce:
#   Profit order_date Order_Date mes       
#    <dbl> <date>     <date>     <date>    
#1   41.9  2016-11-08 2016-11-08 2016-11-01
#2  220.   2016-11-08 2016-11-08 2016-11-01
#3    6.87 2016-06-12 2016-06-12 2016-06-01
#4 -383.   2015-10-11 2015-10-11 2015-10-01
#5    2.52 2015-10-11 2015-10-11 2015-10-01
#6   14.2  2014-06-09 2014-06-09 2014-06-01
# ℹ 9,988 more rows
# ℹ Use `print(n = ...)` to see more rows

# mdy() es una función de lubridate para cambiar una cadena con
# forma de fecha a una fecha real (tipo de dato fecha)
mdy("12/25/2023") # produce: [1] "2023-12-25"

# floor_date() también es una función de lubridate y redondea una
# fecha hacia atrás. Ejemplos:
floor_date(as.Date("2017-08-17"), "month") # produce:
#[1] "2017-08-01"
floor_date(as.Date("2017-08-17"), "week") # produce:
#[1] "2017-08-13"
floor_date(as.Date("2017-08-17"), "year") # produce:
#[1] "2017-01-01"

# Agrupar la tabla por ventas mesuales, resumir el total de ventas
# mensual y ordenarlo en orden ascendente
ventas_mensuales <- ventas_mensuales |>
  group_by(mes) |>
  summarise(
    ventas = sum(Sales),
    .groups = "drop"
  ) |>
  arrange(mes) # ordenar de forma ascendente
ventas_mensuales # produce:
# A tibble: 48 × 2
#  mes        ventas
#  <date>      <dbl>
#1 2014-01-01 14237.
#2 2014-02-01  4520.
#3 2014-03-01 55691.
#4 2014-04-01 28295.
#5 2014-05-01 23648.
#6 2014-06-01 34595.
# ℹ 42 more rows
# ℹ Use `print(n = ...)` to see more rows

# Gráfico de las ventas mensuales
ggplot(
  ventas_mensuales,
  aes(
    x = mes,
    y = ventas
  )
) +
  geom_line() +
  labs(
    title = "Ventas mensuales",
    x = NULL,
    y = "Ventas"
  )

# agregar una variable tiempo que numere a cada observación o mes
ventas_mensuales <-
  ventas_mensuales |>
  mutate(
    tiempo = row_number()
  )
ventas_mensuales # produce:
# A tibble: 48 × 3
#  mes        ventas tiempo
#  <date>      <dbl>  <int>
#1 2014-01-01 14237.      1
#2 2014-02-01  4520.      2
#3 2014-03-01 55691.      3
#4 2014-04-01 28295.      4
#5 2014-05-01 23648.      5
#6 2014-06-01 34595.      6
#...
# ℹ 42 more rows
# ℹ Use `print(n = ...)` to see more rows

# Crear un modelo lineal bayesiano que relacione el tiempo con las
# ventas
modelo_ventas <-
  stan_glm(
    ventas ~ tiempo,
    data = ventas_mensuales,
    family = gaussian(),
    chains = 2,
    iter = 1000,
    refresh = 0
  )
posterior_interval(modelo_ventas) # produce:
#                    5%       95%
#(Intercept) 15799.1767 36405.744
#tiempo        548.1685  1270.243
#sigma       19036.1629 26322.283
# Interpretación: Es plausible que cada mes esté relacionado con un 
# aumento de entre 548 y 1270 dólares en ventas

# Crear una tabla con la variable tiempo con 12 meses posteriores o
# futuros a la tabla original de 48 meses
futuro <-
  tibble(
    tiempo =
      max(ventas_mensuales$tiempo) +
      1:12
  )
futuro # produce:
# A tibble: 12 × 1
#  tiempo
#   <int>
#1     49
#2     50
#3     51
#4     52
#...
#11     59
#12     60

# Basado en el modelo modelo_ventas, generar 1000 simulaciones (1000
# filas) con los 12 meses futuros (12 columnas) 
simulaciones <-
  posterior_predict(
    modelo_ventas,
    newdata = futuro
  )
# Generar 1000 simulaciones (1000 filas) para los 12 meses (12 cols)
simulaciones # produce:
#              1           2           3          4          5
#[1,] 107250.778 100963.9464  42638.2838 108391.194  66408.903
#[2,]  71588.060  43071.5233  88240.6443  -8318.043  62705.471
#[3,]  69289.892  24697.2186  29413.0835  55865.895  73143.205
#[4,]  46410.183  80255.0979  90714.5932  83273.103  44138.779
#[5,]  79953.757  78401.5351  30355.6016 118776.473  73039.335
#[6,]  89276.093  79052.0451  90269.8292 106570.290 100101.509
#[ reached 'max' / getOption("max.print") -- omitted 994 rows,  7 cols]

# Cambiar las 1000 simulaciones de los 12 meses de tabla a matriz
escenarios1 <-
  as_draws_matrix(simulaciones)
escenarios1  # produce:
# A draws_matrix: 1000 iterations, 1 chains, and 12 variables
#  variable
#draw    1      2     3      4      5      6      7      8
#1  107251 100964 42638 108391  66409 104750  67902  87380
#2   71588  43072 88241  -8318  62705  64032  79679  67548
#3   69290  24697 29413  55866  73143  54308  92820  54025
#4   46410  80255 90715  83273  44139  79732  72263  61768
#5   79954  78402 30356 118776  73039 107424  41778 111138
#6   89276  79052 90270 106570 100102  88659 127867  85532
# ... with 994 more draws, and 4 more variables

# Tranformar a matriz ; y la matriz convertir en tibble
escenarios2 <-
  as_draws_matrix(simulaciones) |>
  as_tibble()
escenarios2 # produce:
# A tibble: 1,000 × 12
#  `1`        `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`   `10`  `11` 
#  <drws_mtr> <drw> <drw> <drw> <drw> <drw> <drw> <drw> <drw> <drw> <drw>
#1  61218.16   547… 5086… 5863…  376…  807… 4872… 5789…  641… 1059…  556…
#2  91912.63  1048… 6922… 7736… 1032…  929… 7030… 5922…  300… 1307…  617…
#3 102110.89   782… 8205… 9049…  456…  760… 5695… 9793… 1340… 1185…  970…
#4  76542.88   532… 4850… 4610…  394…  552… 8182… 8793…  899…  378…  589…
#5  83082.87   322… 5853… 7140…  692…  833… 2490… 5585… 1016…  994…  900…
#6  68147.76   879… 5090… 1909…  409…  736…  378… 6271…  680… 1402… 1114…
#...
# ℹ 994 more rows
# ℹ 1 more variable: `12` <drws_mtr>
# ℹ Use `print(n = ...)` to see more rows

# Agregar la variable sim (a la tibble) que numera a cada una de las 
# 1000 filas
escenarios3 <-
  as_draws_matrix(simulaciones) |>
  as_tibble() |>
  mutate(sim = row_number())
escenarios3[,-9:-1] # produce:
# A tibble: 1,000 × 4
#  `10`       `11`       `12`         sim
#  <drws_mtr> <drws_mtr> <drws_mtr> <int>
#1 105996.91   55612.18   85009.64      1
#2 130722.25   61772.16   94321.94      2
#3 118518.62   97051.87  111315.01      3
#4  37841.36   58941.25   82798.09      4
#5  99478.96   90015.33   45801.77      5
#6 140243.07  111404.53   10213.69      6
# ℹ 994 more rows
# ℹ Use `print(n = ...)` to see more rows

# Cambiar todas las simulaciones a formato largo
escenarios <-
  as_draws_matrix(simulaciones) |>
  as_tibble() |>
  mutate(sim = row_number()) |>
  pivot_longer(
    -sim,
    names_to = "mes",
    values_to = "ventas"
  )
# Producir una tabla que repite cada una de las 1000 simulaciones
# (sim )12 veces (12 meses) y cada simulación tiene un valor de
# ventas
escenarios # produce:
# A tibble: 12,000 × 3
#    sim mes   ventas    
#  <int> <chr> <drws_mtr>
#1     1 1      61218.16 
#2     1 2      54761.32 
#3     1 3      50869.36 
#4     1 4      58639.72 
#5     1 5      37626.51 
#6     1 6      80736.91 
# ℹ 11,994 more rows
# ℹ Use `print(n = ...)` to see more rows

# Conocer el tipo de la variable mes
class(escenarios$mes) # produce: [1] "character"

# Visualizar múltples escenarios de  ventas por mes 
ggplot(
  escenarios,
  aes(
    x = as.numeric(mes),
    y = ventas,
    group = sim
  )
) +
  geom_line(
    alpha = 0.05
  ) +
  labs(
    title = "Futuros plausibles simulados",
    x = "Mes"
  )

# Agrupar las simulaciones por mes y resumir por percentil 5, 50 y
# 95
bandas <-
  escenarios |>
  group_by(mes) |>
  summarise(
    p5 = quantile(ventas, 0.05),
    p50 = median(ventas),
    p95 = quantile(ventas, 0.95),
    .groups = "drop"
  )
bandas # produce:
# A tibble: 12 × 4
#  mes   p5            p50 p95       
#  <chr> <drws_mtr>  <dbl> <drws_mtr>
#1 1     30493.98   69344. 110165.2  
#2 10    39235.08   77815. 120405.0  
#3 11    39645.84   78782. 115935.2  
#4 12    37814.77   81284. 119789.1  
#5 2     32037.65   70769. 112506.7  
#6 3     32054.45   71438. 108330.9  
#7 4     36282.30   72075. 110264.5  
#8 5     32653.32   73049. 109815.9  
#9 6     33385.51   74446. 112600.7  
#10 7     38233.46   75124. 117386.3  
#11 8     37029.48   77006. 116139.4  
#12 9     36787.81   76603. 116816.2  

# Crear bandas predictivas de las ventas por mes
ggplot(
  bandas,
  aes(
    x = as.numeric(mes)
  )
) +
  geom_ribbon(
    aes(
      ymin = p5,
      ymax = p95
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50)
  ) +
  labs(
    title = "Bandas predictivas",
    x = "Mes",
    y = "Ventas"
  )

escenarios # produce:

# Escoger sólo las filas que en la variable mes tengan "12" en la
# tabla de escenarios que se cambió a formato largo
ultimo_mes <-
  escenarios |>
  filter(
    #mes == "12" #max(mes) # produce "9"
    as.numeric(mes) ==     # cambiar el tipo de la variable mes de 
      max(as.numeric(mes)) # character a numeric
  )
ultimo_mes # produce:
# A tibble: 1,000 × 3
#    sim mes   ventas    
#  <int> <chr> <drws_mtr>
#1     1 12     31386.65 
#2     2 12    122476.25 
#3     3 12     95514.56 
#4     4 12     76237.39 
#5     5 12     85282.01 
#6     6 12     98464.96 
#7     7 12     97497.97 
#8     8 12     74570.38 
#9     9 12     52139.96 
#10    10 12     66101.69 
# ℹ 990 more rows
# ℹ Use `print(n = ...)` to see more rows

# Distribución de las ventas de los diciembres de las simulaciones
ggplot(
  ultimo_mes,
  aes(x = ventas)
) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Distribución de ventas futuras",
    x = "Ventas en dólares",
    y = "cantidad"
  )

#######################
##
## Ejercicios
##
#######################

######################
##
## Ejercicio 1
##
######################

# Ejercicio 1
# Observe las trayectorias simuladas.

# ¿Todas siguen exactamente el mismo camino? No, para nada, hay
# trayectorias muy dispersas
# ¿Qué nos enseña esta variabilidad? Que el futuro es muy incierto
# y tiene muchas posibilidades

######################
##
## Ejercicio 2
##
######################

# Ejercicio 2

# Modifique el horizonte de proyección.

# Pruebe:
  
#  1:6

# y luego:
  
#  1:24

################ Horizonte con 6 #####################

# Crear una tabla con la variable tiempo con 6 meses posteriores o
# futuros a la tabla original de 48 meses
futuro_6 <-
  tibble(
    tiempo = 
      max(ventas_mensuales$tiempo) + 
      1:6
  )
futuro_6 # produce:
# A tibble: 6 × 1
#  tiempo
#   <int>
#1     49
#2     50
#3     51
#4     52
#5     53
#6     54

# Basado en el modelo modelo_ventas, generar 1000 simulaciones (1000
# filas) con los 6 meses futuros (6 columnas)
simulaciones_6 <-
  posterior_predict(
    modelo_ventas,
    newdata = futuro_6
  )
# Generar 6 de las 1000 simulaciones (1000 filas) y 5 de los 6 
# meses (6 columnas) 
simulaciones_6[1:6 ,1:5] # produce:
#             1        2        3         4         5
#[1,]  42237.13 83932.14 68345.89  95410.29  72474.21
#[2,]  22809.24 38889.47 76583.77 105747.31  61901.87
#[3,] 132382.76 26571.92 64251.93 104751.42 124291.87
#[4,]  89033.74 86659.79 63585.66  97554.28  87146.80
#[5,]  69044.59 97368.05 11271.45  91283.91 104894.09
#[6,]  83095.28 33884.56 60665.41  61409.73  41151.64

# Cambiar todas las simulaciones a formato largo
escenarios_6 <-
  as_draws_matrix(simulaciones_6) %>% # transformar a matriz
  as_tibble() %>% # transformar a tibble
  mutate(
    sim = row_number() # numerar las filas
  ) %>%
  pivot_longer(  # pasar a formato largo
    -sim, # No incluir sim en el cambio a formato largo
    names_to = "mes",
    values_to = "ventas"
  )
# Producir una tabla que repite cada una de las 1000 simulaciones
# (sim ) 6 veces  y cada simulación tiene un valor de ventas
escenarios_6[1:8,] # produce:
# A tibble: 8 × 3
#    sim mes   ventas    
#  <int> <chr> <drws_mtr>
#1     1 1     42237.13  
#2     1 2     83932.14  
#3     1 3     68345.89  
#4     1 4     95410.29  
#5     1 5     72474.21  
#6     1 6     62978.58  
#7     2 1     22809.24  
#8     2 2     38889.47

# Visualizar múltples escenarios de  ventas por mes
ggplot(
  escenarios_6,
  aes(
    x = as.numeric(mes), # Cambiar el tipo de character a numeric
    y = ventas,
    group = sim
  ) 
) +
  geom_line(alpha = 0.03) +
  labs(
    title = "Futuros plausibles simulados",
    x = "Mes"
  )

# Agrupar las simulaciones por mes y resumir por percentil 5, 50 y
# 95
bandas_6 <-
  escenarios_6 %>%
  group_by(mes) %>%
  summarise(
    p5 = quantile(ventas, 0.05),
    p50 = median(ventas),
    p95 = quantile(ventas, 0.95),
    .groups = "drop"
  )
# Mostrar 6 los 6 meses
bandas_6 # produce:
# A tibble: 6 × 4
#  mes   p5            p50 p95       
#  <chr> <drws_mtr>  <dbl> <drws_mtr>
#1 1     31860.56   69451. 110389.3  
#2 2     32360.14   70804. 108880.1  
#3 3     33457.18   72946. 113454.3  
#4 4     36637.43   73204. 112224.1  
#5 5     28608.01   73006. 110821.5  
#6 6     37003.20   75307. 113596.5 

# Crear bandas predictivas de las ventas por mes
ggplot(
  bandas_6,
  aes(
    x = as.numeric(mes)
  )
) +
  geom_ribbon(
    aes(
      ymin = p5,
      ymax = p95
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50)
  ) +
  labs(
    title = "Bandas predictivas",
    x = "Mes",
    y = "Ventas"
  )

################# Horizonte con 24 ####################

# Crear una tabla con la variable tiempo con 24 meses posteriores o
# futuros a la tabla original de 48 meses
futuro_24 <-
  tibble(
    tiempo = 
      max(ventas_mensuales$tiempo) + 
      1:24
  )
futuro_24 # produce:
# A tibble: 24 × 1
#  tiempo
#   <int>
#1     49
#2     50
#3     51
#4     52
#5     53
#6     54
# ℹ 18more rows
# ℹ Use `print(n = ...)` to see more rows

# Basado en el modelo modelo_ventas, generar 1000 simulaciones (1000
# filas) con los 24 meses futuros (24 columnas)
simulaciones_24 <-
  posterior_predict(
    modelo_ventas,
    newdata = futuro_24
  )
# Generar 6 de las 1000 simulaciones (1000 filas) y 5 de los 24 
# meses (24 columnas) 
simulaciones_24[1:6 ,1:5] # produce:
#             1        2        3        4         5
#[1,]  34602.45 37660.88 46664.33 80291.20  72290.10
#[2,]  95221.01 13047.34 49607.74 16221.95  40291.79
#[3,]  55271.37 30572.57 72256.30 57304.39  69490.35
#[4,] 137734.57 59612.06 19621.68 59344.84 109051.02
#[5,]  69095.70 28526.38 64708.73 73285.03 107839.98
#[6,]  61983.41 80690.10 57546.00 71287.02  35031.04

# Cambiar todas las simulaciones a formato largo
escenarios_24 <-
  as_draws_matrix(simulaciones_24) %>% # transformar a matriz
  as_tibble() %>% # transformar a tibble
  mutate(
    sim = row_number() # numerar las filas
  ) %>%
  pivot_longer(  # pasar a formato largo
    -sim, # No incluir sim en el cambio a formato largo
    names_to = "mes",
    values_to = "ventas"
  )
# Producir una tabla que repite cada una de las 1000 simulaciones
# (sim ) 24 veces  y cada simulación tiene un valor de ventas
escenarios_24[1:6,] # produce:
# A tibble: 6 × 3
#    sim mes   ventas    
#  <int> <chr> <drws_mtr>
#1     1 1     60793.35  
#2     1 2     66403.10  
#3     1 3     76106.95  
#4     1 4     76075.38  
#5     1 5     85807.11  
#6     1 6     89455.91  

# Visualizar múltples escenarios de  ventas por mes
ggplot(
  escenarios_24,
  aes(
    x = as.numeric(mes), # Cambiar el tipo de character a numeric
    y = ventas,
    group = sim
  ) 
) +
  geom_line(alpha = 0.03) +
  labs(
    title = "Futuros plausibles simulados",
    x = "Mes"
  )

# Agrupar las simulaciones por mes y resumir por percentil 5, 50 y
# 95
bandas_24 <-
  escenarios_24 %>%
  group_by(mes) %>%
  summarise(
    p5 = quantile(ventas, 0.05),
    p50 = median(ventas),
    p95 = quantile(ventas, 0.95),
    .groups = "drop"
  )
# Mostrar 6 de los 24 meses (24 filas)
bandas_24[1:6,] # produce:
# A tibble: 6 × 4
#  mes   p5            p50 p95       
#  <chr> <drws_mtr>  <dbl> <drws_mtr>
#1 1     30954.67   69700. 112351.5  
#2 10    42703.02   76954. 114139.6  
#3 11    38689.83   79624. 117172.7  
#4 12    43296.86   81253. 119367.5  
#5 13    41374.36   80151. 119511.2  
#6 14    39841.61   81677. 119457.4

ggplot(
  bandas_24,
  aes(
    x = as.numeric(mes)
  )
) +
  geom_ribbon(
    aes(
      ymin = p5,
      ymax = p95
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50)
  ) +
  labs(
    title = "Bandas predictivas",
    x = "Mes",
    y = "Ventas"
  )

# ¿Cómo cambia la incertidumbre? La incertidumbre no cambia mucho
# pero la tendencia se aplana con menos meses, con más meses la 
# tendencia tiene un pendiente más epinada

####################
##
## Ejercicio 3
##
####################

# Ejercicio 3

# Compare:
  
#  posterior_epred()

# 12 simulaciones
sims <-
  posterior_epred(
    modelo_ventas,
    newdata = futuro
  )

# Escenas
escenas <-
  as_draws_matrix(sims) %>% # transformar a matriz
  as_tibble() %>% # transformar a tibble
  mutate(
    sim = row_number() # numerar las filas
  ) %>%
  pivot_longer(  # pasar a formato largo
    -sim, # No incluir sim en el cambio a formato largo
    names_to = "mes",
    values_to = "ventas"
  )
escenas # produce:
# A tibble: 12,000 × 3
#    sim mes   ventas    
#  <int> <chr> <drws_mtr>
#1     1 1     69285.25  
#2     1 2     70250.62  
#3     1 3     71215.99  
#4     1 4     72181.36  
#5     1 5     73146.73  
#6     1 6     74112.10  
# ℹ 11,994 more rows
# ℹ Use `print(n = ...)` to see more rows

# Visualizar múltples escenarios de  ventas por mes
ggplot(
  escenas,
  aes(
    x = as.numeric(mes), # Cambiar el tipo de character a numeric
    y = ventas,
    group = sim
  ) 
) +
  geom_line(alpha = 0.03) +
  labs(
    title = "Futuros plausibles simulados",
    x = "Mes"
  )

# Agrupar las simulaciones por mes y resumir por percentil 5, 50 y
# 95
ribbons <-
  escenas %>%
  group_by(mes) %>%
  summarise(
    p5 = quantile(ventas, 0.05),
    p50 = median(ventas),
    p95 = quantile(ventas, 0.95),
    .groups = "drop"
  )
# Mostrar 6 de los 48 meses (48 filas)
ribbons # produce:

# Crear bandas predictivas de las ventas por mes
ggplot(
  ribbons,
  aes(
    x = as.numeric(mes)
  )
) +
  geom_ribbon(
    aes(
      ymin = p5,
      ymax = p95
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50)
  ) +
  labs(
    title = "Bandas predictivas",
    x = "Mes",
    y = "Ventas"
  )

# y

# posterior_predict()

# Crear bandas predictivas de las ventas por mes
ggplot(
  bandas,
  aes(
    x = as.numeric(mes)
  )
) +
  geom_ribbon(
    aes(
      ymin = p5,
      ymax = p95
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50)
  ) +
  labs(
    title = "Bandas predictivas",
    x = "Mes",
    y = "Ventas"
  )

# ¿Cuál produce escenarios más dispersos? posterior_predict() es mucho 
# más disperso que posterior_epred()
  
#  ¿Por qué? Porque posterior_epred() muestra la variabilidad de
#  la media mientras que posterior_predict() muestra la variabilidad
#  real de las ventas futuras

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4
# Construya escenarios:
  
#  percentil 10
#  percentil 50
#  percentil 90

# Agrupar las simulaciones por mes y resumir por percentil 10, 50 y
# 90
bandas2 <-
  escenarios %>%
  group_by(mes) %>%
  summarise(
    p10 = quantile(ventas, 0.10),
    p50 = median(ventas),
    p90 = quantile(ventas, 0.90),
    .groups = "drop"
  )
# Mostrar 6 de los 48 meses (48 filas)
bandas2[1:6,] # produce:
# A tibble: 6 × 4
#  mes   p10           p50 p90       
#  <chr> <drws_mtr>  <dbl> <drws_mtr>
#1 1     39766.53   69456. 100506.04 
#2 10    45329.30   78751. 109011.60 
#3 11    47540.69   80369. 110748.78 
#4 12    47691.89   80352. 110262.20 
#5 2     41171.76   71450.  97689.19 
#6 3     42670.83   72450. 101313.63 

# Crear bandas predictivas de las ventas por mes
ggplot(
  bandas2,
  aes(
    x = as.numeric(mes)
  )
) +
  geom_ribbon(
    aes(
      ymin = p10,
      ymax = p90
    ),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50)
  ) +
  labs(
    title = "Bandas predictivas",
    x = "Mes",
    y = "Ventas"
  )

# Interprete cada uno desde la perspectiva de inventario.
# Escenario favorable: las ventas van de 100mil a 110mil
# Escenario desfavorable: las ventas de 40mil a 50mil
# Escenario moderado: las ventas van de 70mil a 80mil


#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5

# Suponga que usted es responsable de presupuesto.

# ¿Qué decisiones cambiarían si únicamente observara el escenario 
# central? Asignaría demasiado presupuesto o muy poco presupuesto

# Con un escenario desfavorable: la pregunta real sería: podemos
# operar si las ventas caen a este nivel? Qué gastos podrían 
# reducirse? 

# Un escenario moderado: es una referencia útil para planificación 
# general. Pero no debe iterpretarse como una garantía

# En un escenario favorable: la preguanta es si hay suficiente inventario
# o si se puede atender una demanda tan alta?

# En resumen se debe tomar en cuenta todos los escenarios, creo que
# sobre todo a nivel de presupuesto y recursos, por ejemplo cual
# sería el mínimo presupuesto para enfrentarse a un escenario 
# favorable. Siempre habrá que hacer concesiones.
  
# ¿Qué riesgos estaría ignorando? El riesgo de quedarme sin stock
# o de quedarme con demasiado stock


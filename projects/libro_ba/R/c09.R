########################
##                    ##
## Capítulo 9         ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)
library(posterior)

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

#############################
##
## Construyendo el modelo
##
#############################

modelo_profit <- stan_glm(
  Profit ~ Discount_c,
  data = superstore,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)

#############################
##
## Generando escenarios
## futuoros
##
#############################

# Generar varios niveles de descuento
nuevos_descuentos <- tibble(
  Discount_c = seq(0, 80, by = 5)
)
nuevos_descuentos # produce:
# A tibble: 17 × 1
#  Discount_c
#       <dbl>
#1          0
#2          5
#3         10
#4         15
#5         20
#6         25
#7         30
#8         35
#9         40
#10         45
#11         50
#12         55
#13         60
#14         65
#15         70
#16         75
#17         80

# Generar una matriz de predicciones
predicciones <- posterior_predict(
  modelo_profit,
  newdata = nuevos_descuentos
)
predicciones # produce:
#                1            2            3             4
#[1,]  395.2643006   85.1893558  -69.5786656   85.95236214
#[2,]  393.4023992  -52.3313080  122.3270440  302.81986912
#[3,]  153.9273441  -10.1013609  152.8314060  -75.99999258
#[4,]  545.5515424  314.3692969  -45.5501910 -334.36270223
#[57,]  206.6967181   32.7313599  534.9006912   60.37294659
#[58,]   91.0540305  222.4137500  303.8311479 -280.93786354
#[ reached 'max' / getOption("max.print") -- omitted 942 rows ]

dim(predicciones) # produce:
# [1] 1000   17 # Significa 17 escenarios de descuento y 1000 futuros
# posibles en cada escenario

# posterior_predict() genera una matriz y cada fila representa una 
# simulación posterior y cada columna representa un nivel de 
# descuento (debería haber 17 columnas para simular porcentajes del
# 0 a 80 con intervalos de 5)

################################
##
## Resumir las predicciones
##
################################

# apply sigue este esquema apply(X, MARGIN, FUN) donde: X es la 
# matriz, MARGIN por donde recorrer (filas o columnas) y FUN es 
# la función a aplicar. Margin = 1 significa trabajar por filas y
# Margin = 2 significa trabajar por columnas
apply(predicciones, 2, quantile, probs = 0.05) # produce: 
#        1         2         3         4         5         6         7 
#-298.7388 -324.0070 -308.1649 -339.0513 -366.8629 -350.3767 -376.6441 
#        8         9        10        11        12        13        14 
#-405.3383 -389.1583 -408.2374 -430.2324 -423.2528 -491.6173 -491.9390 
#       15        16        17 
#-466.2172 -493.3355 -497.4468 
apply(predicciones, 2, quantile, probs = 0.05) # produce:
#        1         2         3         4         5         6         7 
#-298.7388 -324.0070 -308.1649 -339.0513 -366.8629 -350.3767 -376.6441 
#        8         9        10        11        12        13        14 
#-405.3383 -389.1583 -408.2374 -430.2324 -423.2528 -491.6173 -491.9390 
#       15        16        17 
#-466.2172 -493.3355 -497.4468 
apply(predicciones, 2, quantile, probs = 0.95) # produce:
#       1        2        3        4        5        6        7        8 
#450.9453 419.3697 401.0377 384.4148 400.7429 372.3952 353.1206 341.4879 
#       9       10       11       12       13       14       15       16 
#336.7358 307.6673 320.5703 312.3529 308.0159 283.0332 295.4681 275.3138 
#      17 
#243.4901 

# Crear una tabla de resumen de las predicciones 
pred_resumen <- tibble(
  Discount_c = nuevos_descuentos$Discount_c, # Descuento
  mediana = apply(predicciones, 2, median), # mediana
  li = apply(predicciones, 2, quantile, probs = 0.05), # límite inferiror
  ls = apply(predicciones, 2, quantile, probs = 0.95) # límite superior
)
pred_resumen # produce:
# A tibble: 17 × 4
#  Discount_c mediana    li    ls
#       <dbl>   <dbl> <dbl> <dbl>
#1          0   64.4  -319.  458.
#2          5   51.7  -334.  421.
#3         10   53.1  -337.  415.
#4         15   12.1  -351.  378.
#5         20   21.9  -369.  403.
#16        75 -115.   -492.  301.
#17        80 -140.   -500.  232.

# Visualizar bandas (ribbons) plausibles
ggplot(pred_resumen,
       aes(x = Discount_c,
           y = mediana)) +
  
  geom_ribbon(
    aes(ymin = li,
        ymax = ls),
    alpha = 0.3
  ) +
  
  geom_line(size = 1) +
  #geom_point() +
  #coord_cartesian(ylim = c(-150, 100)) +
  
  labs(
    title = "Beneficio esperado e incertidumbre predictiva",
    x = "Descuento (%)",
    y = "Profit esperado"
  )

##############################
##
## Visualizando múltiples
## futuros plausibles
##
##############################

# Matriz de predicciones posteriores para distintos tipos de 
# probabilidades
escenarios <- predicciones 
escenarios # produce:
#                1            2            3             4
#[1,]  395.2643006   85.1893558  -69.5786656   85.95236214
#[2,]  393.4023992  -52.3313080  122.3270440  302.81986912
#[3,]  153.9273441  -10.1013609  152.8314060  -75.99999258
#[4,]  545.5515424  314.3692969  -45.5501910 -334.36270223
#[57,]  206.6967181   32.7313599  534.9006912   60.37294659
#[58,]   91.0540305  222.4137500  303.8311479 -280.93786354
#[ reached 'max' / getOption("max.print") -- omitted 942 rows ]

# Cambiar la matriz a tabla o dataframe
escenarios <- predicciones %>%
  as.data.frame() 
escenarios # produce:
#                1            2            3             4
#[1,]  395.2643006   85.1893558  -69.5786656   85.95236214
#[2,]  393.4023992  -52.3313080  122.3270440  302.81986912
#[3,]  153.9273441  -10.1013609  152.8314060  -75.99999258
#[4,]  545.5515424  314.3692969  -45.5501910 -334.36270223
#[57,]  206.6967181   32.7313599  534.9006912   60.37294659
#[58,]   91.0540305  222.4137500  303.8311479 -280.93786354
#[ reached 'max' / getOption("max.print") -- omitted 942 rows ]

# Quedarse con una muestra de sólo 100 participantes
escenarios <- predicciones %>%
  as.data.frame() %>%
  slice_sample(n = 100) # escoger aleatoriamente 100 participantes
escenarios

# Crear una variable que numera cada una de las 100 filas
escenarios <- predicciones %>%
  as.data.frame() %>%
  slice_sample(n = 100) %>%
  mutate(simulacion = row_number()) # crear la variable simulacion
                                    # y numerar las filas
escenarios # produce:
#             16          17 simulacion
#1   -528.709537 -177.068449          1
#2   -872.385559 -265.961199          2
#3   -182.302034 -148.665087          3
#4     84.664442  168.017220          4
#5    350.811482 -114.950780          5
#54   -79.130241 -390.167545         54
#55   194.648765   37.419883         55
#[ reached 'max' / getOption("max.print") -- omitted 45 rows ]

# Mostrar en formato largo los escenarios, donde los escenarios
# van del 1 al 17 y simulacion va del 1 al 100
escenarios <- predicciones %>%
  as.data.frame() %>% # Cambia de formato matriz a dataframe
  slice_sample(n = 100) %>% # Se toma una muestra de 100 
  mutate(simulacion = row_number()) %>% # Se crea una variable para
                                        # numerar las filas  
  pivot_longer( # Cambiar la tabla a formato largo
    -simulacion, # usa todas las columnas excepto simulacion
    names_to = "escenario",
    values_to = "profit"
  )
escenarios # produce
# A tibble: 1,700 × 3
#  simulacion escenario profit
#       <int> <chr>      <dbl>
#1          1 1           299.
#2          1 2           130.
#3          1 3           117.
#4          1 4           260.
#5          1 5           165.
#6          1 6           401.
# ℹ 1,690 more rows
# ℹ Use `print(n = ...)` to see more rows
#View(escenarios)

# Visualizar escenarios idividuales
ggplot(
  escenarios,
  aes(
    x = escenario,
    y = profit,
    group = simulacion
  )
) +
  geom_line(alpha = 0.15) 

########################
##
## 
##
########################

# Obtener predicciones para cada caso
escenarios_negocio <- tibble(
  Discount_c = c(5,20,40)
)

predicciones2 <- posterior_predict(
  modelo_profit,
  newdata = escenarios_negocio
) 
predicciones2# produce:
#                  1            2            3
#[1,]   -195.8875658   29.7119458  304.4893613
#[2,]    156.7788086  183.6402786 -135.6442989
#[3,]    100.8683999 -156.5089070 -173.5166827
#[4,]    232.4544288 -175.6453174 -177.6629761
#[5,]    -59.2142871   36.8321237  -49.8568452
#[332,]  156.6952994  -51.5336145 -781.7887290
#[333,]  180.0677055  111.1435764  228.4421532
#[ reached 'max' / getOption("max.print") -- omitted 667 rows ]

pred_resumen2 <- tibble(
  Discount_c = escenarios_negocio$Discount_c, # Descuento
  mediana = apply(predicciones2, 2, median), # mediana
  li = apply(predicciones2, 2, quantile, probs = 0.05), # límite inferiror
  ls = apply(predicciones2, 2, quantile, probs = 0.95) # límite superior
)
pred_resumen2 # produce:
# A tibble: 3 × 4
#  Discount_c mediana    li    ls
#       <dbl>   <dbl> <dbl> <dbl>
#1          5    49.4 -324.  470.
#2         20    24.5 -360.  401.
#3         40   -63.5 -436.  321.

# Visualizar bandas plausibles
ggplot(pred_resumen2,
       aes(x = Discount_c,
           y = mediana)) +
  
  geom_ribbon(
    aes(ymin = li,
        ymax = ls),
    alpha = 0.3
  ) +
  
  geom_line(size = 1) +
  
  labs(
    title = "Beneficio esperado e incertidumbre predictiva",
    x = "Descuento (%)",
    y = "Profit esperado"
  )

# Mostrar en formato largo los escenarios, donde los escenarios
# van del 1 al 3 y simulacion va del 1 al 100
escenarios2 <- predicciones2 %>%
  as.data.frame() %>% # Cambia de formato matriz a dataframe
  slice_sample(n = 100) %>% # Se toma una muestra de 100 
  mutate(simulacion = row_number()) %>% # Se crea una variable para
  # numerar las filas  
  pivot_longer( # Cambiar la tabla a formato largo
    -simulacion, # importante el signo "-" para mostrar la tabla
    # en formato largo
    names_to = "escenario",
    values_to = "profit"
  )
escenarios2 # produce
# A tibble: 300 × 3
#  simulacion escenario profit
#       <int> <chr>      <dbl>
#1          1 1          188. 
#2          1 2          370. 
#3          1 3          -66.5
#4          2 1          132. 
#9          3 3         -196. 
#10         4 1          178. 
# ℹ 290 more rows
# ℹ Use `print(n = ...)` to see more rows

# Visualizar escenarios idividuales
ggplot(
  escenarios2,
  aes(
    x = escenario,
    y = profit,
    group = simulacion
  )
) +
  geom_line(alpha = 0.15)

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
# Genera bandas plausibles utilizando:
  
#  Sales como variable respuesta.
# ¿Cómo cambia la incertidumbre?

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
#(Intercept) 243.2    7.4 
#Discount_c   -0.8    0.3 
#
#Auxiliary parameter(s):
#     Median MAD_SD
#sigma 623.4    4.2 

# Conocer el intervalo posterio
posterior_interval(
  modelo_sales,
  prob = 0.95) # produce:
#                  2.5%       97.5%
#(Intercept) 227.659278 258.5478257
#Discount_c   -1.462882  -0.2600537
#sigma       615.021961 632.0319227

# Generar una matriz de predicciones
predicciones3 <- posterior_predict(
  modelo_sales,
  newdata = nuevos_descuentos
) 
predicciones3 # produce (17 filas y 1000 columnas):
#                 1             2             3            4
#[1,]   224.4318398  -438.4221631   531.1825336  -302.417506
#[2,]  -111.3454053   860.4600093   137.1752551  -248.558239
#[3,]  -206.6092363   537.8060466   613.1275042  -161.505220
#[4,]   434.3450910  1078.3373282   354.3182434  -879.292646
#[57,]   546.9363099  1063.8614059   383.7815239  1221.613609
#[58,]   262.1696314   934.2018850   768.1901966  -484.806038
#[ reached 'max' / getOption("max.print") -- omitted 942 rows ]

# Crear una tabla de resumen de las predicciones (con 
# 17 filas de descuentos) 
pred_resumen3 <- tibble(
  Discount_c = nuevos_descuentos$Discount_c, # Descuento
  mediana = apply(predicciones3, 2, median), # mediana
  li = apply(predicciones3, 2, quantile, probs = 0.05), # límite inferiror
  ls = apply(predicciones3, 2, quantile, probs = 0.95) # límite superior
)
pred_resumen3 # produce:
# A tibble: 17 × 4
#  Discount_c mediana    li    ls
#       <dbl>   <dbl> <dbl> <dbl>
#1          0    247. -824. 1273.
#2          5    292. -732. 1314.
#3         10    229. -766. 1200.
#4         15    175. -899. 1207.
#16        75    191. -787. 1214.
#17        80    136. -877. 1208.

# Visualizar bandas (ribbons) plausibles
ggplot(pred_resumen3,
       aes(x = Discount_c,
           y = mediana)) +
  
  geom_ribbon( # Crea la banda con los límites superior e inferior
    aes(ymin = li,
        ymax = ls),
    alpha = 0.3
  ) +
  
  geom_line(size = 1) + # Crea la línea que une los puntos
  #geom_point() +
  #coord_cartesian(ylim = c(100, 300)) + # hace zoom
  #coord_cartesian(ylim = c(1000, 1500)) + # hace zoom
  #coord_cartesian(ylim = c(-1000, -500)) + # hace zoom
  
  labs(
    title = "Ventas esperadas e incertidumbre predictiva",
    x = "Descuento (%)",
    y = "Sales esperadas"
  )

# ¿Cómo cambia la incertidumbre? La incertidumbre se mantiene en todos
# los casos de descuento

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2
# Compara escenarios de descuento:
  
# 0%
# 10%
# 30%
# 50%

pred_resumen3 # produce:
#   Discount_c mediana    li    ls
#        <dbl>   <dbl> <dbl> <dbl>
#1           0    247. -824. 1273.
#3          10    229. -766. 1200.
#7          30    211. -771. 1245.
#11         50    188. -808. 1159.

# ¿Qué diferencias observas en las distribuciones predictivas?
# Las medianas disminuyen mientras más descuento. El límite superior
# disminuye un poco pero también a veces aumenta las ventas, no es 
# directamente proporcional siempre. El límite inferior también es
# variable

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3

# Construye un gráfico con 100 futuros plausibles utilizando 
# posterior_predict().

# Mostrar en formato largo los escenarios, donde los escenarios
# van del 1 al 17 y simulacion va del 1 al 100. Entonces en total
# hay 1700 filas
escenarios3 <- predicciones3 %>%
  as.data.frame() %>% # Cambia de formato matriz a dataframe
  slice_sample(n = 100) %>% # Se toma una muestra de 100 
  mutate(simulacion = row_number()) %>% # Se crea una variable para
  # numerar las filas  
  pivot_longer( # Cambiar la tabla a formato largo
    -simulacion, # usa todas las columnas excepto simulacion
    names_to = "escenario",
    values_to = "profit"
  )
escenarios3 # produce:
# A tibble: 1,700 × 3
#simulacion escenario profit
#       <int> <chr>      <dbl>
#1          1 1         -1148.
#2          1 2          -192.
#3          1 3           777.
#4          1 4          -245.
#9          1 9           355.
#10          1 10        -1077.
# ℹ 1,690 more rows
# ℹ Use `print(n = ...)` to see more rows

# Visualizar escenarios idividuales
ggplot(
  escenarios3,
  aes(
    x = escenario,
    y = profit,
    group = simulacion # agrupar por simulación
  )
) +
  geom_line(alpha = 0.15)

# ¿Qué te transmite visualmente? Que los escenarios posibles son
# muy, muy variables.

#################
##
## Ejercicio 4
##
#################

# Ejercicio 4

# Identifica un escenario donde el valor promedio parezca atractivo, 
# pero la incertidumbre sea muy amplia.

# En 25% de descuento hay unas ventas considerables y el peor escenario
# no es el peor de todos los descuentos. 

# ¿Tomarías esa decisión? Creo que sí

#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Explica con tus propias palabras la diferencia entre:
  
#  una predicción puntual
# una distribución predictiva
  
# Una predicción puntual es un sólo valor, una distribución 
# predictiva me da escenarios, el mejor y el peor y esto me ayuda 
# a tomar decisiones
#  ¿Por qué?


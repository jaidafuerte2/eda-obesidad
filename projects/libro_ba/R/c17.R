########################
##                    ##
## Capítulo 16        ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(rstanarm)
library(posterior) 
library(bayesplot)
library(lubridate)

# Cargar la dataset de ventas
sales <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)
#glimpse(sales) # produce:

###############################
##
## Primer Gráfico
##
###############################


# Agregar la variable de fecha de orden, con formato de fecha; basado 
# en la variable existente 
sales <- sales %>%
  mutate(
    Order_Date = as.Date(
      `Order Date`,
      format = "%m/%d/%Y"
    )
  )
sales[1:6, -20:-1] # produce:
# A tibble: 6 × 2
#   Profit Order_Date
#    <dbl> <date>    
#1   41.9  2016-11-08
#2  220.   2016-11-08
#3    6.87 2016-06-12
#4 -383.   2015-10-11
#5    2.52 2015-10-11
#6   14.2  2014-06-09

# Crear una fila de meses por fechas
ventas_mensuales <-
  sales |>
  mutate(
    Month = lubridate::floor_date(Order_Date, "month")
  ) 
ventas_mensuales[, -20:-1] # produce:
# A tibble: 9,994 × 3
#  Profit Order_Date Month     
#   <dbl> <date>     <date>    
#1   41.9  2016-11-08 2016-11-01
#2  220.   2016-11-08 2016-11-01
#3    6.87 2016-06-12 2016-06-01
#4 -383.   2015-10-11 2015-10-01
#5    2.52 2015-10-11 2015-10-01
#...
#9    5.78 2014-06-09 2014-06-01
#10  34.5  2014-06-09 2014-06-01
# ℹ 9,984 more rows
# ℹ Use `print(n = ...)` to see more rows

# Agrupar las ventas por mes 
ventas_mensuales <- ventas_mensuales|>
  group_by(Month) |>
  summarise(
    Sales = sum(Sales),
    .groups = "drop"
  ) # produce:
ventas_mensuales # produce:
# A tibble: 48 × 2
#  Month       Sales
#  <date>      <dbl>
#1 2014-01-01 14237.
#2 2014-02-01  4520.
#3 2014-03-01 55691.
#4 2014-04-01 28295.
#5 2014-05-01 23648.
#...
#9 2014-09-01 81777.
#10 2014-10-01 31453.
# ℹ 38 more rows
# ℹ Use `print(n = ...)` to see more rows


# Convertir el tiempo (Month) en un índice, crear un contador de meses
ventas_mensuales <-
  ventas_mensuales |>
  mutate(
    Month = row_number()
  ) 
ventas_mensuales # produce:
# A tibble: 48 × 2
#  Month  Sales
#  <int>  <dbl>
#1     1 14237.
#2     2  4520.
#3     3 55691.
#4     4 28295.
#5     5 23648.
#...
#9     9 81777.
#10   10 31453.
# ℹ 38 more rows
# ℹ Use `print(n = ...)` to see more rows


# Crear modelo de la influencia del tiempo sobre las ventas
modelo_ventas <-
  stan_glm(
    Sales ~ Month,
    data = ventas_mensuales,
    family = gaussian(),
    chains = 2,
    iter = 1000,
    refresh = 0
  )
posterior_interval(modelo_ventas) # produce:
#                    5%       95%
#(Intercept) 14776.3494 36616.790
#Month         524.4147  1315.291
#sigma       18962.0815 26539.889
# Interprestación: Dados los datos del modelo, parece plausible
# que cada mes aumenten las ventas entre 524 y 1315 dólares

modelo_ventas # produce:         
#             Median  MAD_SD 
#(Intercept) 25916.9  6717.0
#Month         898.0   240.4


# Crear nuevos meses para pronosticar
nuevos_meses <- tibble(
  Month = 49:60
)

# Generar predicciones posteriores
predicciones_posteriores <-
  posterior_predict(
    modelo_ventas,
    newdata = nuevos_meses
  )
dim(predicciones_posteriores) # produce: [1] 1000   12
predicciones_posteriores # produce:
#               1            2           3           4          5
#[1,]  52213.7358  95970.50516  70761.8757  80782.5583  65542.284
#[2,]  73843.7375  82474.57834 115237.3189  87516.6222  69005.097
#[3,]  49874.7803  66783.19573  80045.0748 111448.0819  74622.062
#[4,]  77977.5399 104660.58484  83274.0735  74342.4200  89250.099
#[5,]  57379.2223  92182.20606  73499.5017  50381.2874  62295.883
#[6,]  75743.7605  53405.68540  36649.4111  62405.3890  72119.808
#[ reached 'max' / getOption("max.print") -- omitted 994 rows, 12 cols ]

# Obtener el promedio esperado de las predicciones posteriores
mean_pred <- apply(
  predicciones_posteriores,
  2,
  mean
)


# Calcular el valor inferior (2.5% de las predicciones) de las
# predicciones posteriores
lower_pred <-
  apply(
    predicciones_posteriores,
    2,
    quantile,
    probs = 0.025
  )

# Calcular el valor superior (97.5% de las predicciones) de las
# predicciones posteriores
upper_pred <-
  apply(
    predicciones_posteriores,
    2,
    quantile,
    probs = 0.975
  )

# Construir un dataframe de predicciones
predicciones <-
  tibble(
    Month = nuevos_meses$Month,
    mean = mean_pred,
    lower = lower_pred,
    upper = upper_pred
  )
predicciones # produce:
# A tibble: 12 × 4
#  Month   mean  lower   upper
#  <int>  <dbl>  <dbl>   <dbl>
#1    49 70082. 24129. 117800.
#2    50 70771. 24227. 115999.
#3    51 71693. 25481. 118074.
#4    52 72747. 24218. 117847.
#5    53 74207. 27494. 121898.
#6    54 74110. 30224. 122063.
#7    55 75273. 27447. 123273.
#8    56 75868. 26843. 120725.
#9    57 76435. 29059. 121245.
#10   58 79181. 34179. 126731.
#11   59 78358. 30898. 123378.
#12   60 78962. 32568. 127589.


# Diagrama de líneas que relaciona el mes con las ventas. 
# Además de Gráfico de bandas para visualizar las ventas estimadas o
# o plausible de cada mes.
ggplot(
  predicciones,
  aes(
    x = Month,
    y = mean
  )
) +
  geom_line(size = 1) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.25
  ) +
  
  labs(
    title = "Pronóstico de ventas con incertidumbre",
    x = "Mes",
    y = "Ventas estimadas"
  )

# Diagrama de líneas que relaciona el mes con las ventas. 
# Además de Gráfico de bandas para visualizar las ventas estimadas o
# o plausible de cada mes. Con zoom en el eje Y
ggplot(
  predicciones,
  aes(
    x = Month,
    y = mean
  )
) +
  geom_line(size = 1) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.25
  ) +
  coord_cartesian(ylim = c(70000, 80000)) +
  labs(
    title = "Pronóstico de ventas con incertidumbre",
    x = "Mes",
    y = "Ventas estimadas"
  )

###############################
##
## Segundo Gráfico
##
###############################

# Extraer muestras posteriores 
posterior_samples <-
  as_draws_df(modelo_ventas)
#glimpse(posterior_samples) # produce:
#Rows: 1,000
#Columns: 6
#$ `(Intercept)` <dbl> 29163.71, 26962.19, 27447.20, 21780.84, 27994.75,…
#$ Month         <dbl> 777.3012, 899.9561, 919.3123, 1103.3027, 755.8933…
#$ sigma         <dbl> 21960.63, 22856.95, 19717.69, 21294.55, 21375.20,…
#$ .chain        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#$ .iteration    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…
#$ .draw         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…

posterior_samples # produce:
# A draws_df: 500 iterations, 2 chains, and 3 variables
#   (Intercept) Month sigma
#1        29164   777 21961
#2        26962   900 22857
#3        27447   919 19718
#4        21781  1103 21295
#5        27995   756 21375
#...
#9        37206   330 19392
#10       30950   524 30847

# Gráfico de la distribución posterior que muestra la variación 
# de las ventas cada mes
posterior_samples |>
  ggplot(
    aes(x = Month)
  ) +
  geom_density()


# Extraer muestras posteriores 
coeficientes <-
  as_draws_df(modelo_ventas)

# Distribución posterior la variación de las ventas cada mes
coeficientes |>
  ggplot(aes(x = Month)) +
  geom_density(
    fill = "steelblue",
    alpha = 0.4
  ) +
  labs(
    title = "Distribución posterior del tiempo sobre las ventas",
    x = "Coeficiente asociado a Month",
    y = "Densidad"
  )
# Interpretación : Cada mes se espera que  que las ventas lleguen a 
# ser incluso mayores a 1500 pero lo más probable es que las 
# ventas aumenten entre 800 y 1000 dólares, valores mayores o menores
# a este intervalo son mmenos probables

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

# Ejercicio 1. Reescribiendo conclusiones

# Reescriba las siguientes afirmaciones utilizando un lenguaje 
# probabilístico.

# 1. Las ventas serán de $95 000.
# Dados los datos del modelo es plausible que las ventas estén 
# alrededor de los 95000 

# 2. Este cliente cancelará el servicio. 
# Dados los datos que arroja el modelo, hay cierto riesgo de que el
# cliente cancele el servicio
# 3. La campaña será exitosa.
# Siguiendo los datos del modelo , hay alguna probabilidad de que la 
# campaña sea exitosa
# 4.- El nuevo precio aumentará las ventas.
# Dados los datos que arroja el modelo, es plausible que el nuevo precio
# aumente las ventas

######################
##
## Ejercicio 2
##
######################

# Ejercicio 2. Mejorando un gráfico

# Observe uno de los gráficos de predicción construidos en capítulos 
# anteriores.

# ¿Qué información adicional podría incorporarse para comunicar 
# mejor la incertidumbre? Por un lado el promedio, por otro lado, algo
# muy importante son las unidades en las que está el modelo.

######################
##
## Ejercicio 3
##
######################

# Ejercicio 3. Detectando falsas certezas

# Lea las siguientes frases e identifique cuáles transmiten una 
# falsa sensación de certeza.

# - La estrategia A funcionará mejor.
#   Esto es una afirmación contundente
# - Existe una alta probabilidad de que la estrategia A obtenga 
#   mejores resultados.
#   Esto es una afirmación sesgada hacia los "mejores resultados"
# - El proyecto tendrá éxito.
#   Esto es una afirmación
# - Los datos sugieren un escenario favorable para el proyecto.
#   No es una afirmación completa pero sí tiene un sesgo hacia
#   "un escenario favorable"

# Explique su respuesta.

######################
##
## Ejercicio 4
##
######################

# Ejercicio 4. Escribiendo para un gerente

# Imagine que un gerente le pregunta si debe aumentar el inventario 
# para el próximo trimestre.

# Utilizando uno de los modelos desarrollados en el libro, redacte 
# un informe breve (entre 150 y 200 palabras) que:
  
# - describa el escenario más plausible;
# - mencione la incertidumbre existente;
# - explique qué riesgos deberían considerarse antes de tomar la 
#   decisión;
# - evite afirmaciones categóricas.

######################
##
## Ejercicio 5
##
######################

# Ejercicio 5. Reflexión personal

# Piense en alguna noticia, informe o presentación que haya visto 
# recientemente.
# Otra vez podría pensar en estudios ultracientíficos de pérdida
# de peso.

# ¿Se comunicaban los resultados como certezas absolutas o como escenarios 
#  probabilísticos? A veces los estudios de pérdida de peso con ejercicio
#  a veces si muestran intervalos de pérdida de peso
  
# ¿Qué efecto cree que tuvo esa forma de comunicar la información sobre quienes 
#  debían tomar decisiones? Pues le da cierta "autoridad" al estudio
#  un halo de imparcialidad

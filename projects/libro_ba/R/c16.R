########################
##                    ##
## Capítulo 16        ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(dagitty)

# Cargar la dataset de ventas
sales <- read_csv(
  "eda-obesidad/projects/libro_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)
#glimpse(superstore) # produce:

# Diagrama de dispersión que relaciona el descuento con las ganancias
sales %>%
  ggplot(aes(x = Discount, y = Profit)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Descuento y beneficio",
    x = "Descuento",
    y = "Beneficio"
  )

# Tendencia y diagrama de dispersión que relaciona el descuento 
# con las ganancias
sales %>%
  ggplot(aes(Discount, Profit)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "steelblue"
  ) +
  labs(
    title = "Asociación entre descuento y beneficio",
    x = "Descuento",
    y = "Beneficio"
  )

# Tendencia y diagrama de dispersión que relaciona el descuento 
# con las ganancias. Con zoom en el eje Y
sales %>%
  ggplot(aes(Discount, Profit)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "steelblue"
  ) +
  coord_cartesian(ylim = c(-700, 700)) +
  labs(
    title = "Asociación entre descuento y beneficio",
    x = "Descuento",
    y = "Beneficio"
  )

# Línea suavizada de tendencia y diagrama de dispersión que relaciona
# las ventas con las ganancias
sales %>%
  ggplot(aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "steelblue"
  ) +
  labs(
    title = "Ventas y beneficio",
    x = "Ventas",
    y = "Beneficio"
  )



# Crear una variable de tipo factor que etiquete las filas por el
# porcentaje de descuento
sales <- sales %>%
  mutate(
    discount_group = cut( # convierte convierte una variable numerica
                          # en categorías tipo intervalos
      Discount,
      breaks = c(0, 0.1, 0.2, 0.3, 0.5, 1), # puntos donde se van a
      # cortar los intervalos
      include.lowest = TRUE # el primer intervalo pasa a incluir el
                            # valor mínimo porque (0, 0.1] no 
                            # incluye al cero
    )
  )
class(sales$discount_group) # produce: "factor"
unique(sales$discount_group) # produce:
#[1] [0,0.1]   (0.3,0.5] (0.1,0.2] (0.5,1]   (0.2,0.3]
#Levels: [0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.5] (0.5,1]

# Agrupar por descuento y resumir por promedio de ventas
grupos_descuento <- sales %>%
  group_by(discount_group) %>%
  summarise(
    mean_sales = mean(Sales),
    .groups = "drop"
  )
grupos_descuento # produce:
# A tibble: 5 × 2
#  discount_group mean_sales
#  <fct>               <dbl>
#1 [0,0.1]             233. 
#2 (0.1,0.2]           214. 
#3 (0.2,0.3]           455. 
#4 (0.3,0.5]           630. 
#5 (0.5,1]              75.0

# Gráfico de columnas del descuento vs el promedio de ventas
grupos_descuento %>%
  ggplot(aes(discount_group, mean_sales)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Ventas promedio según nivel de descuento",
    x = "Grupo de descuento",
    y = "Ventas promedio"
  )

# DAG de la influencia de la temporada sobre las ventas y el 
# descuento como factor de confusión
dag <- dagitty("
dag {
  Season -> Discount
  Season -> Sales
}
")
# Dibujar DAG de la influencia de la temporada sobre las ventas y 
# el descuento
plot(dag)

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


# Ejercicio 1. ¿Asociación o causalidad?

# Para cada una de las siguientes afirmaciones, indica si los datos 
# permiten concluir una relación causal o únicamente muestran una 
# asociación. Justifica tu respuesta.

# - Las sucursales con más vendedores obtienen mayores ingresos.
#   Asociación.- Ocasionalmente pueden tener más vendedores porque
#   deben atender más clientes, pero también puede ser por otras
#   razones
# - Los clientes que reciben descuentos compran más productos.
#   Asociación.- Puede ser que los descuentos se den en épocas 
#   especiales dónde de todas formas habrá muchas ventas
# - Las ventas aumentan durante los meses en que se incrementa la 
#   inversión en publicidad.
#   Asociación: También puede ser se invierte más en publicidad 
#   en épocas especiales cuando de todas formas habrá más ventas
# - Los productos con mayor precio generan mayores beneficios.
#   Asociación: También puede pasar que los productos con mayor 
#   precio tardan más en vender lo que crea una inversión sin 
#   retorno durante demasiado tiempo

# Pregunta de reflexión: ¿Qué información adicional necesitarías 
# para evaluar una posible relación causal? Creo que fecha podría
# ser una factor de confusión interesante.

######################
##
## Ejercicio 2
##
######################

# Ejercicio 2. Buscando variables de confusión
# Elige dos de las situaciones anteriores e identifica al menos 
# dos variables de confusión que podrían explicar el patrón observado.

# Por ejemplo:
  
# estacionalidad;
# tipo de producto;
# tamaño de la empresa;
# región;
# perfil del cliente.
# Explica cómo cada una podría influir simultáneamente sobre ambas 
# variables.

# Estacionalidad y región. (Cuándo y dónde). En cierto lugares,
# en ciertos meses del año, ciertos productos podrían salir más, por
# ejemplo en invierno, si vendes calefactores podrías ofrecer
# descuentos para sobrepasar a la competencia y así tener más ventas 

# Perfil de cliente y tipo de producto: Es posible que ciertos
# clientes de alto poder adquisitivo perciban que pueden tener un
# gran beneficio si compran varios productos al mismo tiempo
# cuando hay una promoción especial, pero tal vez no pase lo mismo con 
# clientes de bajo nivel adquisitivo.

######################
##
## Ejercicio 3
##
######################


# Ejercicio 3. Explorando asociaciones con Superstore
# Construye un gráfico de dispersión utilizando dos variables 
# numéricas del conjunto de datos Superstore Sales.

# Algunas combinaciones posibles son:
  
# Sales y Profit;
# Quantity y Sales;
# Discount y Profit.

# Diagrama de dispersión y línea suavizada de tendencia que 
# relaciona la cantidad con las ventas 
sales %>%
  ggplot(aes(x = Quantity, y = Sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "steelblue"
  ) + 
  labs(
    title = "Ventas según cantidad",
    x = "Cantidad",
    y = "Ventas"
  )

# Diagrama de dispersión y línea suavizada de tendencia que 
# relaciona la cantidad con las ventas. Con zoom en el eje Y
sales %>%
  ggplot(aes(x = Quantity, y = Sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "steelblue"
  ) + 
  coord_cartesian(ylim = c(0, 5000)) +
  labs(
    title = "Ventas según cantidad",
    x = "Cantidad",
    y = "Ventas"
  )


#Describe:
  
# el patrón observado; Cuando se venden más unidades de un producto
# se obtiene más diinero por venta
# la variabilidad presente; Si hay mucha variabilidad pues a veces
# se venden pocas unidades de producto y se obtiene mucho dinero
# por esa venta. Y a veces varias unidades de ciertos productos , no
# generan tanto dinero por venta. Depende también del precio de un 
# producto
# al menos dos explicaciones posibles para la asociación encontrada.
# Es de sentido común que si vendo dos unidades, recibiré más
# dinero que si vendo sólo una unidad.
# Evita concluir inmediatamente que una variable causa a la otra.

######################
##
## Ejercicio 4
##
######################

# Ejercicio 4. Construyendo un DAG sencillo
# Utilizando dagitty, dibuja un diagrama para representar una hipótesis causal relacionada con alguno de los siguientes escenarios:
  
# descuentos y ventas;
# publicidad y ventas;
# satisfacción del cliente y abandono (churn).
# Incluye al menos una tercera variable que pueda actuar como 
# variable de confusión.

# No existe un único diagrama correcto. Lo importante es que 
# las flechas representen una historia coherente.

# DAG de efecto del descuento y la publicidad sobre las ventas
dag_ventas <- dagitty("
  dag{
    Descuento -> Ventas
    Publicidad -> Ventas
  }
")
#Dibujar DAG
plot(dag_ventas)

######################
##
## Ejercicio 5
##
######################

# Ejercicio 5. Pensamiento crítico
# Piensa en una noticia reciente, un informe empresarial o una 
# publicación en redes sociales donde alguien haya afirmado que 
# una variable causó otra.

# El ejercicio ayuda a perder peso

# Responde las siguientes preguntas:
  
#  ¿Qué evidencia presentó? Un promedio del peso del grupo antes y 
#  después de la intervención
# 
#  ¿Podría existir una explicación alternativa?
#  Sí muchas.
#  ¿Qué información adicional te gustaría conocer antes de aceptar 
#  esa conclusión? Tipo de dieta y tiempo que duró la intervención
#  ¿Cómo cambiaría tu interpretación si aparecieran nuevos datos?
#  Durante cortos períodos de tiempo es fácil controlar el apetito
#  voraz que causa el ejercicio, pero controlar el apetito por muchos
#  meses o más de una año haciendo ejercicio es un poco difícil.
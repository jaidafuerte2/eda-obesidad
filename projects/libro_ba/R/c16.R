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
    discount_group = cut(
      Discount,
      breaks = c(0, 0.1, 0.2, 0.3, 0.5, 1),
      include.lowest = TRUE
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


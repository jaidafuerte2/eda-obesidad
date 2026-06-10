########################
##                    ##
## Capítulo 10        ## 
##                    ##
########################

options(scipen = 999) # desactivar la notación científica

library(tidyverse)

# Cargar la dataset de telco
telco <- read_csv(
  "eda-obesidad/projects/libro_ba/data/telco_customer_churn.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# Explorar telco
#glimpse(telco) # produce:
#Rows: 7,043
#Columns: 21
#$ customerID       <chr> "7590-VHVEG", "5575-GNVDE", "3668-QPYBK", "779…
#$ gender           <chr> "Female", "Male", "Male", "Male", "Female", "F…
#$ SeniorCitizen    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#$ Partner          <chr> "Yes", "No", "No", "No", "No", "No", "No", "No…
#$ Dependents       <chr> "No", "No", "No", "No", "No", "No", "Yes", "No…
#$ tenure           <dbl> 1, 34, 2, 45, 2, 8, 22, 10, 28, 62, 13, 16, 58…
#$ PhoneService     <chr> "No", "Yes", "Yes", "No", "Yes", "Yes", "Yes",…
#$ MultipleLines    <chr> "No phone service", "No", "No", "No phone serv…
#$ InternetService  <chr> "DSL", "DSL", "DSL", "DSL", "Fiber optic", "Fi…
#$ OnlineSecurity   <chr> "No", "Yes", "Yes", "Yes", "No", "No", "No", "…
#$ OnlineBackup     <chr> "Yes", "No", "Yes", "No", "No", "No", "Yes", "…
#$ DeviceProtection <chr> "No", "Yes", "No", "Yes", "No", "Yes", "No", "…
#$ TechSupport      <chr> "No", "No", "No", "Yes", "No", "No", "No", "No…
#$ StreamingTV      <chr> "No", "No", "No", "No", "No", "Yes", "Yes", "N…
#$ StreamingMovies  <chr> "No", "No", "No", "No", "No", "Yes", "No", "No…
#$ Contract         <chr> "Month-to-month", "One year", "Month-to-month"…
#$ PaperlessBilling <chr> "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes",…
#$ PaymentMethod    <chr> "Electronic check", "Mailed check", "Mailed ch…
#$ MonthlyCharges   <dbl> 29.85, 56.95, 53.85, 42.30, 70.70, 99.65, 89.1…
#$ TotalCharges     <dbl> 29.85, 1889.50, 108.15, 1840.75, 151.65, 820.5…
#$ Churn            <chr> "No", "No", "Yes", "No", "Yes", "Yes", "No", "…

# Observar la distribución general del churn (abandono del cliente)
telco %>%
  count(Churn) # produce:
# A tibble: 2 × 2
#  Churn     n
#  <chr> <int>
#1 No     5174
#2 Yes    1869

telco %>%
  count(Churn) %>%
  ggplot(
    aes(
      x = Churn,
      y = n, 
      fill = Churn
    )
  ) +
  geom_col() +
  labs(
    title = "Distribución del Churn",
    x = "Churn",
    y = "Cantidad"
  )

# Histograma de la antiguedad del cliente (tenure - en meses) y el 
# churn (abandono)
ggplot(
  telco,
  aes(
    x = tenure,
    fill = Churn
    )
) +
  geom_histogram(
    alpha = 0.7,
    bins = 30
  ) + 
  labs(
    title = "Histograma de antiguedad y cancelación o churn",
    x = "Antiguedad",
    y = "Cantidad"
  )
  
# Grafico de barras del tipo de contrato y la cancelación o churn
ggplot(
  telco,
  aes(
    x = Contract,
    fill = Churn
  )
) +
  geom_bar(
    position = "fill" # para ver las barras en proporción
  ) +
  labs(
    title = "Proporción de Contratos y Cancelación o churn",
    x = "Tipo de contrato",
    y = "Proporción"
  )

# Diagrama de densidad de los pagos mensuales y la cancelación o 
# churn
ggplot(
  telco,
  aes(
    x = MonthlyCharges,
    fill = Churn
  ) 
) +
  geom_density(
    alpha = 0.4
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

# Explora la distribución de la variable tenure.

# Histograma de la antiguedad del cliente (tenure - en meses) y el 
# churn (abandono)
ggplot(
  telco,
  aes(
    x = tenure,
    fill = Churn
  )
) +
  geom_histogram(
    alpha = 0.7,
    bins = 30
  ) + 
  labs(
    title = "Histograma de antiguedad y cancelación o churn",
    x = "Antiguedad",
    y = "Cantidad"
  )

# ¿Existen muchos clientes nuevos? Existen muchos clientes nuevos
#  ¿Existen muchos clientes antiguos? Existen muchos clientes 
# antiguos

#################
##
## Ejercicio 2
##
#################

# Ejercicio 2

# Construye un gráfico que compare churn según el método de pago.

ggplot(
  telco,
  aes(
    x = PaymentMethod,
    fill = Churn
    )
) + 
  geom_bar(
    position = "fill"
  )

# Versión volteada
ggplot(
  telco,
  aes(
    x = PaymentMethod,
    fill = Churn
  )
) +
  geom_bar(
    position = "fill"
  ) +
  coord_flip() # Para voltear el gráfico y que se puedan ver los
               #nombres de los valores 

# ¿Observas diferencias entre grupos?

unique(telco$PaymentMethod) # produce:
#[1] "Electronic check"          "Mailed check"             
#[3] "Bank transfer (automatic)" "Credit card (automatic)" 

# Sí, sólo el grupo de cheque electrónico tiene un porcentaje de 
# churn más alto que los otros tres métodos de pago

# ¿Qué interpretaciones serían razonables? Parecería que alguien
# que paga con cheque tal vez no está tan convencido de querer
# el producto o servicio
# ¿Qué interpretaciones serían exageradas? Que a la gente le molesta
#  que la empresa tenga esa forma de pago

#################
##
## Ejercicio 3
##
#################

# Ejercicio 3

# Identifica tres variables que creas que podrían estar 
# relacionadas con churn.

# 1) El tipo de servicio de internet, 2) Cargos totales 3) Tech
# Support
# Justifica tu respuesta utilizando intuición de negocio.
# El tipo de servicio de internet puede estar relacionado con la 
# percepción de un cliente del servicio porque tal vez la fibra
# óptica funcione distinto a otra forma de conexión a internet.
# Los cargos totales son importantes porque tal vez un cargo
# demasiado alto sea molesto para un cliente y no se lo espera
# Pero también puede ser que un cargo más alto es porque un 
# cliente busca más servicios y esto puede asociarse con una
# mejor percepción de la calidad. Contratar servicio técnico también
# podría asociarse con una mejor percepción del servicio.

#################
##
## Ejercicio 4
##
#################

# Ejercicio 4

# Supón que dos clientes presentan probabilidades de churn de:
  
#  15%
#  60%

# Si solo pudieras contactar a uno de ellos, ¿a cuál elegirías 
# y por qué? Obviamente elegiría al de mayor churn (60%) porque
# así es más probable que salve un cliente

#################
##
## Ejercicio 5
##
#################

# Ejercicio 5

# Reflexiona sobre la siguiente afirmación:
  
#  "Un modelo que asigna 80% de riesgo a un cliente no está 
# afirmando que abandonará con certeza."

# ¿Qué significa realmente esa probabilidad?
# En términos prácticos significa que debo ponerle más atención para
# que no cancele el contrato.
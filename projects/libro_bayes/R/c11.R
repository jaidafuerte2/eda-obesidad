library(dplyr)
library(tidyverse)
library(rstanarm)

# Cargar la dataset de obesidad
df_obesidad <- read_csv(
  "eda-obesidad/projects/libro_bayes/data/obesidad_nhanes.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

#######################
##
## Simulación
##
#######################

set.seed(123)

# Crear 100 pacientes
n <- 100

# Simular La dosis de aspirina y de índice de masa corporal tomando
# en cuenta la aspirina
datos_sim <- tibble(
  aspirina = runif(n, 0, 100),
  imc = 25 + 0.03 * aspirina + rnorm(n, 0, 2)
)

# Diagrama de dispersión que relaciona la dosis de aspirina con el
# índice de masa corporal
ggplot(datos_sim, aes(x = aspirina, y = imc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación simulada entre aspirina e IMC",
    x = "Dosis de aspirina",
    y = "IMC"
  )

# Construir el modelo bayesiano
modelo_sim <- stan_glm(
  imc ~ aspirina,
  data = datos_sim,
  chains = 2,
  iter = 1000,
  refresh = 0
)
# Ver resultados
#print(modelo_sim) # produce:
#            Median MAD_SD
#(Intercept) 25.1    0.2  
#aspirina     0.0    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 2.0    0.1   
# Intervalos posteriores
posterior_interval(modelo_sim) # produce:
#                     5%         95%
#(Intercept) 24.69329017 25.44270069
#aspirina     0.02465355  0.03768735
#sigma        1.87024154  2.13036062



###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar sólo las variables que voy a usar
nhanes_modelo <- df_obesidad %>%
  select(BMXBMI, RXD530) %>%
  drop_na()

# Filtrar a los que toman sólo hasta 500 mg de aspirina como dosis
my_nhanes_modelo <- nhanes_modelo %>%
  filter(
    RXD530 <= 2000
  )
# Diagrama de dispersión que relaciona a la dosis de aspirina con
# el índice de masa corporal
ggplot(my_nhanes_modelo, aes(x = RXD530, y = BMXBMI)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "IMC vs aspirina",
    x = "Dosis de aspirina",
    y = "IMC"
  )

# Construir el modelo bayesiano
modelo_nhanes <- stan_glm(
  BMXBMI ~ RXD530,
  data = my_nhanes_modelo,
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Resumen del modelo
#print(modelo_nhanes) # produce:
#            Median MAD_SD
#(Intercept) 30.2    0.3  
#RXD530       0.0    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 6.8    0.2 
# (Intercept) o imc base o imc promedio = 30.2 => significa que si la 
# dosis de aspirina fuera cero, 
# el imc esperado sería alrededor de 30.2. Clínicamente 
# significa que el paciente promedio sin aspirina tiene un imc de 
# más o menos 30.
# RXD530 es la pendiente. Responde a la pregunta: ¿Cuánto cambia el 
# IMC cuando cambia la aspirina? La interpretación directa es:
# El efecto es prácticamente 0
# Traducción clínica: No hay evidencia clara de que más aspirina 
# sea igual a mas imc o más aspirina sea igual a menos imc
# sigma = 6.8 : significa que el imc varía mucho alrededor del 
# prodmedio 

# Intervalos de credibilidad
posterior_interval(modelo_nhanes) # produce:
#                      5%          95%
#(Intercept) 29.657115495 30.800728882
#RXD530      -0.005447533  0.002081143
#sigma        6.551719544  7.080064008
# (Intercept) 29.65   30.80 significa que el imc base o promedio
# está entre 29.6 y 30.8; la traducción es que es muy probable 
# que el imc promedio esté en este rango
# RXD530 (LA CLAVE TOTAL) : el efecto de la aspirina (sobre el imc) es
# -0.005 y 0.002. Es decir, el efecto podría ser ligeramente 
# positivo o ligeramente negativo o cero.
# sigma: significa que la variabilidad está entre 6.5 y 7.0 es decir
# que la variabilidad es grande y consistente

# Ver la distribución
posterior <- as.matrix(modelo_nhanes)

# Visualizar con un histograma la distribución posterior de la 
# pendiente
hist(posterior[, "RXD530"], breaks = 30,
     main = "Distribución posterior de la pendiente",
     xlab = "Efecto de aspirina sobre IMC")

# Versión más clara con ggplot
posterior_df <- as_tibble(posterior)

ggplot(posterior_df, aes(x = RXD530)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribución posterior de la pendiente",
    x = "Efecto de la aspirina sobre IMC",
    y = "Frecuencia"
  )
# Aquí se ve una curva centrada cerca de cero y distribuida entre
# algo como -0.005 y 0.002. Hay mucha masa alrededor de cero, la 
# mayoría de valores posibles dicen que no hay un efecto claro o el
# efecto es muy pequeño. A la izquierda algunos valores dicen ligero
# efecto negativo y a la derecha dicen ligero efecto positivo.
# 👉 “la evidencia es compatible con ausencia de efecto”

####################
##
## Ejercicios
##
####################

#####################
##
## Ejercicio 1
##
#####################

# Ejercicio 1 — Leer el modelo como médico

# Sin usar código nuevo, usa tus resultados actuales.

#  Pregunta:
  
#  Responde con tus propias palabras:
  
#  ¿Cuál es el IMC promedio estimado?
#  Entre 29.6 y 30.8
#  ¿Qué significa la pendiente de RXD530?
#  Significa cuanto efecto tiene la aspirina sobre el índice de masa
#  corporal. En este caso es entre -0.005 y 0.002
#  ¿El intervalo de la pendiente incluye 0? Sí,
#  ¿Qué implica eso clínicamente? Que tal vez la aspirina no tenga
#  ningún efecto sobre el índice de masa corporal.

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — Probabilidad de efecto positivo

# Ahora sí, un paso más bayesiano.

mean(posterior[, "RXD530"] > 0) # produce: [1] 0.21

# Preguntas:
#  ¿Qué valor obtienes? 0.21
#  Interprétalo así:
  
#  👉 “La probabilidad de que el efecto sea positivo es 0.21”

# ¿Ese valor es suficiente para decir que hay relación?
# Creo que no.

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Detectar evidencia débil

# Observa la distribución posterior (tu gráfico).

# Preguntas:
#  ¿La distribución está muy concentrada o muy dispersa?
#  La distribución es normal
#  ¿Está claramente a un lado de 0 o centrada en 0?
#  Ligeramente sesgada hacia la izquierda del 0
#  ¿Cómo describirías la evidencia?

#  Opciones:
  
#  fuerte
#  moderada
#  débil
#  inexistente

#  No hay suficiente evidencia para decir que la aspirina
#  tiene efecto sobre el índice de masa corporal

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — Cambiar la pregunta clínica

# Ahora piensa diferente.

# En lugar de:
  
#  “¿la aspirina afecta el IMC?”

# Piensa:
  
#   “¿las personas que usan aspirina tienen diferente IMC?”

# Tarea

# Crea una variable binaria:

bin_nhanes_modelo <- my_nhanes_modelo %>%
  mutate(usa_aspirina = if_else(RXD530 <= 100, 1, 0)) %>%
  select(BMXBMI, usa_aspirina) %>%
  drop_na()
bin_nhanes_modelo

# Modelo:
modelo_bin <- stan_glm(
  BMXBMI ~ usa_aspirina,
  data = bin_nhanes_modelo,
  chains = 2,
  iter = 1000,
  refresh = 0
)
#print(modelo_bin) # produce:
#           Median MAD_SD
#(Intercept)  29.4    0.5  
#usa_aspirina  0.6    0.6  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 6.8    0.1   
posterior_interval(modelo_bin) # produce:
#                    5%       95%
#(Intercept)  28.520700 30.211233
#usa_aspirina -0.220379  1.593511
#sigma         6.554451  7.034011

#Preguntas:
#  ¿Cuál es la diferencia de IMC entre los grupos?
#  de 28.5 a 30.2 en índice de masa corporal y la variación de imc
#  por uso de aspirina menor o igual a 100mg es entre -0.2 y 1.5 
#  ¿El intervalo incluye 0? Sí
#  ¿Cambia la interpretación respecto al modelo anterior?
#  Un poco, creo que el sesgo varía, veamos...

# Ver la distribución
posterior <- as.matrix(modelo_bin)

# Visualizar con un histograma la distribución posterior de la 
# pendiente
hist(posterior[, "usa_aspirina"], breaks = 30,
     main = "Distribución posterior de la pendiente",
     xlab = "Efecto de aspirina sobre IMC")
# Efectivamente el sesgo va hacia la derecha de cero 0. Las dosis 
# menores de 100mg de aspirina se asocian un poco más a imc mayor.

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Interpretación clínica profunda

# Este es el más importante.

# Pregunta abierta:
  
#  Si encuentras una ligera asociación entre aspirina e IMC:
  
# ¿cuál es la explicación MÁS probable?
  
#  Opciones:
  
#  A. La aspirina causa obesidad
#  B. La obesidad causa uso de aspirina
#  C. Ambas están relacionadas con inflamación
#  D. Es completamente aleatorio

# Respuesta: la obesidad causa inflamación y ocasionalmente se
# puede prescribir menos de 100mg de aspirina como profilaxis
# de eventos cardiovasculares por inflamación
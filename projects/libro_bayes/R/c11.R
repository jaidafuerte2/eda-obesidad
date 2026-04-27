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
print(modelo_sim) # produce:
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
my_nhanes_modelo <- df_aspirina %>%
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
print(modelo_nhanes) # produce:
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
########################
##                    ##
## Capítulo 5         ## 
##                    ##
########################

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset de obesidad
df_obesidad <- read_csv(
  "eda-obesidad/projects/libro_bayes/data/obesidad_nhanes.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

set.seed(123)

# Creamos 100 pacientes simulados
pacientes <- tibble(
  id = 1:100,
  prob_diabetes = runif(100, 0.1, 0.7)  # creencias iniciales
) 
# Donde 100 es el número de observaciones , 0.1 es la probabilidad
# mínima y 0.7 es la probabilidad máxima
unique(pacientes$prob_diabetes)[1:20] # produce:
#[1] 0.4599934 0.2996941 0.3931678 0.6726843 0.3897414 0.6342101
#[7] 0.6486629 0.4652410 0.3464139 0.1882568 0.6611799 0.2807373
#[13] 0.1364323 0.6686362 0.5323578 0.1853766 0.4295708 0.6724547
#[19] 0.4512900 0.3427062

# Visualizamos la incertidumbre
ggplot(pacientes, aes(x = prob_diabetes)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(
    title = "Creencias iniciales sobre probabilidad de diabetes",
    x = "Probabilidad",
    y = "Número de pacientes"
  )

# Simulemos que medimos glicemia:
# Donde 100 es número de pacientes, 1 es un intento y prob_diabetes 
# es la probabilidad de diabetes
pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(100, 1, prob_diabetes)
  )
unique(pacientes$glicemia_alta) # produce: [1] 0 1
table(pacientes$glicemia_alta) # produce:
#0  1 
#57 43 

# Ahora actualizamos nuestra creencia:
pacientes <- pacientes %>%
  mutate(
    prob_actualizada = if_else(glicemia_alta == 1,
                               prob_diabetes + 0.2,
                               prob_diabetes - 0.1)
  )

# Visualizamos nuestra creencia actualizada 
ggplot(pacientes, aes(x = prob_actualizada)) +
  geom_histogram(bins = 15, fill = "darkred", color = "white") +
  labs(
    title = "Probabilidades actualizadas con nueva información",
    x = "Probabilidad",
    y = "Número de pacientes"
  )

############################
##
## Ejemplo con NHANES
##
############################

# Explorar glicemia
df_obesidad %>%
  filter(!is.na(LBXSGL)) %>%
  ggplot(aes(x = LBXSGL)) +
  geom_density(fill = "skyblue") +
  labs(
    title = "Distribución de glicemia en NHANES",
    x = "Glicemia (mg/dL)",
    y = "Densidad"
  )

# Relacionar glicemia con índice de masa corporal 
df_obesidad %>%
  filter(!is.na(LBXSGL), !is.na(BMXBMI)) %>%
  ggplot(aes(x = BMXBMI, y = LBXSGL)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(
    title = "IMC vs Glicemia",
    x = "IMC",
    y = "Glicemia"
  )

# Hemoglobina glicosilada
df_obesidad %>%
  filter(!is.na(LBXGH)) %>%
  ggplot(aes(x = LBXGH)) +
  geom_density(fill = "orange") +
  labs(
    title = "Distribución de HbA1c",
    x = "HbA1c (%)",
    y = "Densidad"
  )

# Relacionar hemoglobina glicosilada con índice de masa corporal 
df_obesidad %>%
  filter(!is.na(LBXGH), !is.na(BMXBMI)) %>%
  ggplot(aes(x = BMXBMI, y = LBXGH)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(
    title = "IMC vs Glicohemoglobina",
    x = "IMC",
    y = "Glicohemglobina"
  )

########################
##                    ##
##       Ejercicios   ## 
##                    ##
########################

################
##
## Ejercicio 1
##
################

# Ejercicio 1

# Cambia el número de pacientes a 1000.

# Creamos 1000 pacientes simulados
pacientes <- tibble(
  id = 1:1000,
  prob_diabetes = runif(1000, 0.1, 0.7)  # creencias iniciales
) 

# Simulemos que medimos glicemia:
pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(1000, 1, prob_diabetes)
  )
table(pacientes$glicemia_alta) # produce:
#  0   1 
#591 409

# Ahora actualizamos nuestra creencia:
pacientes <- pacientes %>%
  mutate(
    prob_actualizada = if_else(glicemia_alta == 1,
                               prob_diabetes + 0.2,
                               prob_diabetes - 0.1)
  )

# -> Pregunta:
  
#  ¿La distribución de glicemia_alta se parece más a prob_diabetes?
# Creo que sí:

pacientes %>%
  summarise(
    promedio_prob = mean(prob_diabetes),
    proporcion_glicemia = mean(glicemia_alta)
  ) # produce:
#  promedio_prob proporcion_glicemia
#          <dbl>               <dbl>
#1         0.399               0.405

################
##
## Ejercicio 2
##
################

# Ejercicio 2

# Crea una tabla:
  
table(pacientes$glicemia_alta) # produce:
#  0   1 
#604 396

# -> Pregunta:
  
#  ¿Qué proporción salió con glicemia alta? 39.6%
#  ¿Tiene sentido con las probabilidades iniciales? Claro, ese
# valor de glicemia está calculado con las probabilidades iniciales.
# 
# Simulemos que medimos glicemia con la probabilidad actualizada:
pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(1000, 1, prob_actualizada)
  )
table(pacientes$glicemia_alta) # produce:
#  0   1 
#566 434 
# Aquí aumentó el porcentaje de pacientes con glicemia alta a 43.4%

################
##
## Ejercicio 3
##
################

# Ejercicio 3

# Modifica la actualización:
  
#  + 0.4  y  - 0.2

# Actualizamos nuestra creencia:
pacientes <- pacientes %>%
  mutate(
    prob_actualizada = if_else(glicemia_alta == 1,
                               prob_diabetes + 0.4,
                               prob_diabetes - 0.2)
  )

# Simulemos que medimos glicemia con la probabilidad actualizada:
pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(1000, 1, prob_actualizada)
  )
table(pacientes$glicemia_alta) # produce:
#  0   1 
#373 332 

# -> Pregunta:
  
#  ¿Qué pasa con la distribución final? Dismimuye el porcentaje de
# glicemia alta a 33.2% . En la distribución del gráfico se obeserva
# que disminuye mucho la frecuencia cerca al 0.4 (40%) 

ggplot(pacientes, aes(x = prob_actualizada)) +
  geom_histogram(bins = 15, fill = "darkred", color = "white") +
  labs(
    title = "Probabilidades actualizadas con nueva información",
    x = "Probabilidad",
    y = "Número de pacientes"
  )
#  ¿Se vuelve más “extrema”? Sí, cerca al  40% (0.4) disminuye mucho
# la frecuencia y hay frecuencias menores a 0 y mayores a 1

################
##
## Ejercicio 4
##
################

# Ejercicio 4

# Asegura que las probabilidades no pasen de 0 o 1:
  
# pista:
  
#  pmin(pmax(prob_actualizada, 0), 1)

pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(1000, 1, 
                           pmin(pmax(prob_actualizada, 0), 1))
  )
table(pacientes$glicemia_alta) # produce:
# 0   1 
# 534 466

ggplot(pacientes, aes(x = pmin(pmax(prob_actualizada, 0), 1))) +
  geom_histogram(bins = 15, fill = "darkred", color = "white") +
  labs(
    title = "Probabilidades actualizadas con nueva información",
    x = "Probabilidad",
    y = "Número de pacientes"
  )

# -> Pregunta:
  
#  ¿Por qué esto es importante clínicamente? Porque en medicina
# nunca hay certezas absolutas.

################
##
## Ejercicio 5
##
################

# Crea una segunda evidencia:
  
#  hba1c_alta = rbinom(100, 1, prob_diabetes)

# Simulemos que medimos hemoglobina glicosilada:
# Donde 1000 es número de pacientes, 1 es un intento y prob_diabetes 
# es la probabilidad de diabetes
pacientes <- pacientes %>%
  mutate(
    hba1c_alta = rbinom(1000, 1, prob_diabetes)
  )

# Ahora actualiza dos veces:
  
#  prob_actualizada_2

# Actualizamos nuestra creencia:
pacientes <- pacientes %>%
  mutate(
    prob_actualizada_2 = if_else(hba1c_alta == 1,
                                 prob_diabetes + 0.4,
                                 prob_diabetes - 0.2)
  )

pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(1000, 1, 
                           pmin(pmax(prob_actualizada_2, 0), 1))
  )
table(pacientes$glicemia_alta) # produce:
#  0   1 
#534 466 

ggplot(pacientes, aes(x = pmin(pmax(prob_actualizada_2, 0), 1))) +
  geom_histogram(bins = 15, fill = "darkred", color = "white") +
  labs(
    title = "Probabilidades actualizadas con nueva información",
    x = "Probabilidad",
    y = "Número de pacientes"
  )

# ->  Pregunta:
  
#  ¿Qué pasa cuando tienes más evidencia? Se actualizan los valores
# de las variables, lo que debería darnos una mayor certeza o 
# disminuir la incertidumbre.

################
##
## Ejercicio 6
##
################

#Ejercicio 6

#Divide pacientes en dos grupos:
  
#  IMC alto
#  IMC normal

pacientes <- tibble(
  id = 1:100
) %>%
  mutate(
    imc = rnorm(100, mean = 28, sd = 4)
  )

pacientes <- pacientes %>%
  mutate(
    grupo_imc = if_else(imc >= 30, "alto", "normal")
  )

# Asigna mayor prob_diabetes a los de IMC alto.

pacientes <- pacientes %>%
  mutate(
    prob_diabetes = if_else(grupo_imc == "alto",
                            0.6,   # mayor riesgo
                            0.2)   # menor riesgo
  )

# Simular la glicemia
pacientes <- pacientes %>%
  mutate(
    glicemia_alta = rbinom(100, 1, prob_diabetes)
  )

# -> Pregunta:
  
#  ¿Cómo cambia la simulación? El la probabilidad de diabetes es
# más o menos similar al porcentaje de pacientes con glicemia alta
pacientes %>%
  group_by(grupo_imc) %>%
  summarise(
    prob_diabetes = mean(prob_diabetes),
    glicemia_observada = mean(glicemia_alta)
  )
# A tibble: 2 × 3
#   grupo_imc prob_media glicemia_observada
#   <chr>          <dbl>              <dbl>
# 1 alto             0.6              0.444
# 2 normal           0.2              0.205

  
################
##
## Ejercicio 7
##
################

# Ejercicio 7 (clave 🔥)

#Compara:
  
#  prob_diabetes (antes)
#  prob_actualizada (después)

# Haz dos histogramas.

ggplot(pacientes, aes(x = prob_diabetes)) +
  geom_histogram()

ggplot(pacientes, aes(x = prob_actualizada)) +
  geom_histogram()

# -> Pregunta:
  
#  ¿Cómo cambia tu “creencia colectiva”? Pasó de ser una distribución 
# más o menos uniforme a una distribución más específica, podría
# decirse que en la población empieza a existir una distribución
# específica de la probabilidad de diabetes, con dos grupos bien 
# definidos, unos con mucha probabilidad y otros con poca
# probabilidad de presentar diabetes. Tal vez por las edades o 
# cualquier otra característica específica de la población

################
##
## Ejercicio 8
##
################

# Ejercicio 8 (clínico puro)

#Responde sin código:
  
#  Un paciente con IMC alto pero glicemia normal
#  vs
#  Un paciente con IMC normal pero glicemia alta

#  -> ¿A cuál le das mayor probabilidad de alteración metabólica?
  
# -> Explica tu razonamiento (eso es Bayes)

# Yo le daría mayor probabilidad al de la glicemia alta
# porque la glicemia es una medición mucho más específica para 
# alteración metabólica.
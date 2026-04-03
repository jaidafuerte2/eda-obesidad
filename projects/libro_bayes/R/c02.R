########################
##                    ##
## Capítulo 2         ## 
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

n <- 200

# Crear una simulación de glicemia de pacientes
datos <- tibble(
  # Crear 200 observaciones con una media de 105 y una desviación 
  # estándar de 15
  glicemia = rnorm(n, mean = 105, sd = 15),
  # Categorizar a los que tienen alteración metabólica
  alteracion_metabolica = if_else(glicemia > 110, 1, 0)
)

# Histograma de la glicemia coloreado por alteración metabólica
datos %>%
  ggplot(aes(x = glicemia, fill = factor(alteracion_metabolica))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribución de glicemia y alteración metabólica",
    fill = "Alteración"
  )

# Crear la variable riesgo metabólico con los valores de glucosa y 
# hemoglobina glicosilada del nhanes
df_obesidad <- df_obesidad %>%
  mutate(
    riesgo_metabolico = if_else(
      # Glucosa y hemoglobina glicosilada
      LBXSGL > 110 | LBXGH > 5.7,
      1, 0
    )
  )

# Gráfico de densidad del el índice de masa corporal coloreado por
# rieso metabólico
df_obesidad %>%
  filter(!is.na(BMXBMI), !is.na(riesgo_metabolico)) %>%
  ggplot(aes(x = BMXBMI, fill = factor(riesgo_metabolico))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "IMC y riesgo metabólico",
    fill = "Riesgo"
  )

# Gráfico de densidad de la presión sistólica media  coloreado por
# riesgo metabólico
df_obesidad %>%
  filter(!is.na(BPXSY_mean), !is.na(riesgo_metabolico)) %>%
  ggplot(aes(x = BPXSY_mean, fill = factor(riesgo_metabolico))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Presión sistólica y riesgo metabólico",
    fill = "Riesgo"
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

# Ejercicio 1 — Describir vs inferir vs decidir
# -> Objetivo

#    Diferenciar claramente los tres niveles del capítulo.

# -> Instrucción

# Dado este paciente:
  
#  IMC: 33
#  glicemia: 112
#  HbA1c: 5.9
#  presión arterial: 140/90
#  sedentario

# Responde:
  
#  ¿Qué puedes describir (solo datos)? Obesidad grado1, prediabetes,
#  presión alta.  
#  ¿Qué puedes inferir (interpretación)? Que tiene malos hábitos
#  ¿Qué decisión clínica tomarías? Nutricional y tratar de llavar
#  una vida más activa (no ejercicio)

################
##
## Ejercicio 2
##
################

#Ejercicio 2 — Tu primer “prior clínico”
# -> Objetivo

# Hacer explícitas tus creencias iniciales.

# -> Instrucción

# Antes de ver cualquier dato:
  
#  Responde:
  
# ->  “En un paciente con obesidad, ¿qué tan probable crees que 
#     tenga alteración metabólica?”

# Baja
# Media
# Alta

# A mi parecer alta

# Luego justifica:
  
#  ¿En qué te basas?
#  ¿experiencia? Un poco, en la práctica, muchas veces los obesos
#  no tienen alteraciones metabólicas, a veces.
#  ¿fisiopatología? Mucho
#  ¿literatura? Mucho

################
##
## Ejercicio 3
##
################

# Ejercicio 3 — Actualizar creencias
# -> Objetivo

# Practicar el paso más importante: actualización

# -> Instrucción

# Partes de este prior:
  
#  -> “Probabilidad media de alteración metabólica”

# Ahora agregas datos uno por uno:
  
#  IMC = 31
#  glicemia = 108
#  HbA1c = 5.8
#  Sedentarismo
#  -> Pregunta

# Después de cada dato:
  
# ->   ¿tu creencia sube, baja o se mantiene? Entiendo tu punto. Mientras
# la obesidad grado 1 es un categoría anormal, la glicemia de 108 es
# normal y 5.8 es prediabetes; el sedentarismo indica que se mueve
# poco. Númericamente la creencia debería cambiar pero conceptualmente
# la resistencia a la insulina podría presentarse antes de la prediabetes
  
# -> Clave

# -> No necesitas números

# Necesitas esto:
  
#  “Mi grado de creencia cambió”

# Eso ES Bayes.

################
##
## Ejercicio 4
##
################

# Ejercicio 4 — Ver la incertidumbre en datos simulados
# -> Objetivo

# Visualizar que no hay separaciones perfectas

# -> Código base

set.seed(123)

n <- 300

datos <- tibble(
  glicemia = rnorm(n, mean = 105, sd = 15),
  alteracion = if_else(glicemia > 110, 1, 0)
)


# -> Instrucciones
# Haz un histograma de glicemia
# Colorea por alteracion
# Observa la superposición

datos %>%
  ggplot(aes(x = glicemia, fill = factor(alteracion))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribución de glicemia y alteración metabólica",
    fill = "Alteración"
  )

# -> Preguntas
# ¿Existe un punto donde puedas separar perfectamente sano/enfermo?
# No, porque hay superposición
#  ¿Qué pasa con valores como 108 o 109?
#  Aparecen dentro de los que tienen alteración. 


################
##
## Ejercicio 5
##
################

# Ejercicio 5 — NHANES: relación IMC y riesgo
# -> Objetivo

# Pasar de datos reales a interpretación

#  Instrucción
# Crea una variable:
df_obesidad <- df_obesidad %>%
  mutate(
    riesgo_metabolico = if_else(LBXSGL > 110 | LBXGH > 5.7, 1, 0)
  )
  
# Grafica densidad de IMC por riesgo
# Elimina NA correctamente

df_obesidad %>%
  filter(!is.na(BMXBMI),  !is.na(riesgo_metabolico)) %>%
  ggplot(aes(x = BMXBMI, fill = factor(riesgo_metabolico))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "IMC y riesgo metabólico",
    fill = "Riesgo"
  )

# -> Preguntas
# ¿Todos los pacientes con IMC alto tienen riesgo? No, muchos
# pacientes con obesidad incluso mórbida, no tienen riesgo
# ¿Hay pacientes con IMC bajo con riesgo? Sí, hay pacientes delgados
# con riesgo

################
##
## Ejercicio  6
##
################

# Ejercicio 6 — Construyendo una decisión clínica
# -> Objetivo

# Conectar datos → decisión

# -> Instrucción

# Elige un paciente de NHANES (o imagínalo):
  
#  Con variables:
  
# IMC: 32
# glicemia: 98
# HbA1c: 5.2
# presión: 110/70
# actividad física: No

# -> Responde
#  ¿Qué dicen los datos?
#  Qué es posible que esta persona no tenga diabetes ni hipertensión
#  Pero todavía tiene exceso de peso
#  ¿Qué crees que está pasando metabólicamente?
#  Este es un caso no tan atípico, donde el exceso de insulina enmascara
#  el exceso de glucosa, por eso hay obesidad pero normoglicemia. 
#  Todavía habría que investigar la presión
#  ¿Qué tan seguro estás? (alta/media/baja)
#  Seguro de que metabólicamente los valores de la glucosa son 
#  normales, el problema puede ser la insulina
#  ¿Qué harías clínicamente?
#  Mejorar lo hábitos.

################
##
## Ejercicio  7
##
################

# Ejercicio 7 — Detectando señales vs ruido
# -> Objetivo

# Pensar como analista real

# -> Instrucción

# Observa:
  
#  glicemia
# presión arterial

# en relación con riesgo metabólico

# -> Pregunta

# ¿Cuál variable parece:
  
#  más informativa? La hemoglobina glicosilada (tomando en cuenta
#  riesgo metabólico)
#  más ruidosa? La presión, porque si eres obeso, el exceso de insulina
#  explica por qué tienes la glucosa normal, pero no explica por qué
#  la presión es normal.

################
##
## Ejercicio  8
##
################

# Ejercicio final (muy importante)
# -> Reflexión

# Responde con tus palabras:
  
#  -> ¿Cuál es la diferencia entre:
  
# ver datos: ver , es ver
# entender datos: qué está realmente pasando con los datos
# usar datos para decidir: usar los datos como parte de la toma de
# decisiones dentro de un marco donde otra información también es
# importante. Usar datos para decidir sería algo así como usar los
# datos dentro de un contexto.


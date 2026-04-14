library(dplyr)
library(tidyverse)

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

# Crear una población ficticia de glicemia
glicemia_sim <- tibble(
  glicemia = rnorm(1000, mean = 100, sd = 15)
)

# Histograma de la glicemia
glicemia_sim %>%
  ggplot(aes(x = glicemia)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución simulada de glicemia",
    x = "Glicemia",
    y = "Número de personas"
  )

# Gráfico de densidad de la glicemia
glicemia_sim %>%
  ggplot(aes(x = glicemia)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(
    title = "Forma de la distribución de glicemia",
    x = "Glicemia",
    y = "Densidad"
  )


###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar sólo las variables que voy a usar
df_obesidad_dist <- df_obesidad %>%
  select(LBXSGL, BMXBMI, BPXSY_mean, RIDAGEYR) %>%
  drop_na()

# Histograma - Distribución de la glicemia
df_obesidad_dist %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 70, fill = "darkred", alpha = 0.7) +
  labs(
    title = "Distribución de glicemia (NHANES)",
    x = "Glicemia",
    y = "Frecuencia"
  )

# Histograma - Dsitribución del índice de masa corporal
df_obesidad_dist %>%
  ggplot(aes(x = BMXBMI)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  labs(
    title = "Distribución de IMC",
    x = "IMC",
    y = "Frecuencia"
  )

# Gráfico de densidad de la presión arterial sistólica 
df_obesidad_dist %>%
  ggplot(aes(x = BPXSY_mean)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(
    title = "Distribución de presión sistólica",
    x = "Presión sistólica",
    y = "Densidad"
  )

# Gráfico de densidad que compara la glicemia con la edad 
df_obesidad %>%
  filter(!is.na(LBXSGL), !is.na(RIDAGEYR)) %>%
  mutate(grupo_edad = if_else(RIDAGEYR < 50, "<50", "≥50")) %>%
  ggplot(aes(x = LBXSGL, fill = grupo_edad)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribución de glicemia por edad",
    x = "Glicemia",
    fill = "Grupo edad"
  )

#######################
##
## Ejercicios
##
#######################

#######################
##
## Ejercicio 1
##
#######################

# Ejercicio 1 — Entender drop_na()

# Crea un dataset con:
  
# - glicemia
# - IMC

# Incluye algunos NA.

df <- tibble(
  glicemia = c(90, 80, 70, NA, 120, NA, 150, 180, 95, NA),
  imc = c(25, 26, NA, 35, 33, 43, NA, 21, 45, NA)
)

#👉 Tareas:
  
#1. Cuenta cuántas filas hay antes => 10
#2. Aplica drop_na() =>

df %>% drop_na() # produce:
# A tibble: 5 × 2
#  glicemia   imc
#     <dbl> <dbl>
#1       90    25
#2       80    26
#3      120    33
#4      180    21
#5       95    45

#3. Cuenta cuántas quedan => 5
#4. Reflexiona:
  
#  ¿Qué tipo de pacientes estás eliminando? Sólo los que tienen 
# valores faltantes

#######################
##
## Ejercicio 2
##
#######################

# Ejercicio 2 — Comparar con y sin drop_na()

# Con NHANES:
  
# Grafica la distribución de glicemia SIN drop_na()

df_obesidad %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 70, fill = "darkred") +
  labs(
    title = "Distribución de glicemia (NHANES)",
    x = "Glicemia",
    y = "Frecuencia"
  )

# Luego CON drop_na()

df_obesidad_dist %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 70, fill = "darkred") +
  labs(
    title = "Distribución de glicemia (NHANES)",
    x = "Glicemia",
    y = "Frecuencia"
  )

# Compara:
  
#  👉 ¿Cambia la forma de la distribución? No realmente o muy poco

#######################
##
## Ejercicio 3
##
#######################

# Ejercicio 3 — Jugar con set.seed()

# 1. Genera 1000 valores de glicemia simulada SIN set.seed()

# Crear una población ficticia de glicemia
glicemia_sim <- tibble(
  glicemia = rnorm(1000, mean = 100, sd = 15)
)

# 2. Grafica

# Histograma de la glicemia
glicemia_sim %>%
  ggplot(aes(x = glicemia)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución simulada de glicemia",
    x = "Glicemia",
    y = "Número de personas"
  )

# 3. Vuelve a correr → observa cambios

# Crear una población ficticia de glicemia
glicemia_sim <- tibble(
  glicemia = rnorm(1000, mean = 100, sd = 15)
)

# Histograma de la glicemia
glicemia_sim %>%
  ggplot(aes(x = glicemia)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución simulada de glicemia",
    x = "Glicemia",
    y = "Número de personas"
  )

#Ahora:
  
#  4. Usa set.seed(42)

#set.seed(42)

#  5. Repite

# Crear una población ficticia de glicemia
glicemia_sim <- tibble(
  glicemia = rnorm(1000, mean = 100, sd = 15)
)

# Histograma de la glicemia
glicemia_sim %>%
  ggplot(aes(x = glicemia)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución simulada de glicemia",
    x = "Glicemia",
    y = "Número de personas"
  )
# 👉 Pregunta:
  
# ¿Qué cambia y qué no cambia? Sin set.seed(42) la distribución 
# cambia cada vez que genero las mil observaciones. Pero cuando 
# pongo set.seed(42) la distribución se mantiene incluso cuando
# genero las mil observaciones.

#######################
##
## Ejercicio 4
##
#######################

# Ejercicio 4 — Variabilidad (clave)

# Simula dos poblaciones:

#set.seed(123)

baja_var <- rnorm(1000, 100, 5)
alta_var <- rnorm(1000, 100, 20)

# Crear una población ficticia de glicemia
glicemia_sim1 <- tibble(
  glicemia = baja_var
  #glicemia = alta_var
)

# Crear una población ficticia de glicemia
glicemia_sim2 <- tibble(
  #glicemia = baja_var
  glicemia = alta_var
)

# 👉 Grafica ambas distribuciones

# Histograma de la glicemia con baja variabilidad
glicemia_sim1 %>%
  ggplot(aes(x = glicemia)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución simulada de glicemia",
    x = "Glicemia",
    y = "Número de personas"
  )

# Histograma de la glicemia con alta variabilidad
glicemia_sim2 %>%
  ggplot(aes(x = glicemia)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución simulada de glicemia",
    x = "Glicemia",
    y = "Número de personas"
  )

#Pregunta:
  
#  ¿Cuál representa mejor:
#  glicemia en ayunas? => Creo que el de baja variabilidad puesto
# que no hay valores por debajo de 80, creo que eso se parece más
# a una glicemia en ayunas. En cambio con alta variabilidad hay
# valores cercanos a 20 lo que no es parecido a la realidad.

#######################
##
## Ejercicio 5
##
#######################

# Ejercicio 5 — Distribución clínica real

# Con NHANES:
  
# 1. Grafica IMC

df_obesidad_dist %>%
  filter(!is.na(BMXBMI)) %>%
  ggplot(aes(x = BMXBMI)) +
  geom_histogram(bins = 50, fill = "green") +
  labs(
    title = "Distribución de imc",
    x = "Glicemia",
    y = "Número de personas"
  )

# 2. Identifica visualmente:
# - zona más común => Sobrepeso
# - valores extremos => Por encima de 50 y por debajo de 20

# Responde:
  
# - ¿Tiene sentido el punto de corte IMC = 30 viendo la distribución?
# A mi parecer NO

#######################
##
## Ejercicio 6
##
#######################

# Ejercicio 6 — Pensamiento clínico profundo

# Observa la distribución de glicemia.

df_obesidad_dist %>%
  filter(!is.na(LBXSGL)) %>%
  ggplot(aes(x = LBXSGL)) +
  geom_density(fill = "red", alpha = 0.4) +
  # geom_vline es una linea que pasa por el gráfico en 126
  geom_vline(xintercept = 126, color = "black", linetype = "dashed") +
  labs(
    title = "Distribución de imc",
    x = "Glicemia",
    y = "Número de personas"
  )
#Responde:
  
#  ¿Tiene sentido tratar igual a:
  
# -  glicemia 125
# -  glicemia 126?
  
#  👉 Explica usando el concepto de distribución
# Creo que sí, porque están en lugares muy similares de la 
# distribución

#######################
##
## Ejercicio 7
##
#######################

# 🔥 Ejercicio 7 (nivel siguiente)

# Crea dos grupos:
  
# -  IMC < 30
# -  IMC ≥ 30

# Y grafica la distribución de glicemia para ambos.

# Gráfico de densidad que compara la glicemia con el imc 
df_obesidad_dist %>%
  filter(!is.na(LBXSGL), !is.na(BMXBMI)) %>%
  mutate(grupo_imc = if_else(BMXBMI < 30, "<30", "≥30")) %>%
  ggplot(aes(x = LBXSGL, fill = grupo_imc)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribución de glicemia por edad",
    x = "Glicemia",
    fill = "Grupo edad"
  )

# 👉 Pregunta:
  
#  ¿Las distribuciones son realmente distintas o se solapan? Las
# distribuciones son  casi exactamente las mismas, se solapan 
# casi por completo, la glicemia sólo es ligeramente mayor en los
# obesos
  
#######################
##
## Ejercicio 8
##
#######################

# Ejercicio 8 — Detectar sesgo en la distribución

# Con NHANES:
  
#  Grafica la distribución de IMC (BMXBMI)
df_obesidad_dist %>%
  filter(!is.na(BMXBMI)) %>%
  ggplot(aes(x = BMXBMI)) +
  geom_histogram(bins = 50, fill = "green") +
  labs(
    title = "Distribución de imc",
    x = "Glicemia",
    y = "Número de personas"
  )
#  Observa la forma

# 👉 Preguntas:
  
#  ¿Es simétrica o sesgada? No es simétrica, tiene una cola
#  ¿Hacia qué lado? hacia la derecha
#  ¿Qué significa clínicamente ese sesgo? Que la población de estudio
#  Está acumulando peso o grasa

#######################
##
## Ejercicio 9
##
#######################

# Ejercicio 9 — Comparar histogramas vs densidad

# Para glicemia (LBXSGL):
  
#  Haz un geom_histogram()

df_obesidad_dist %>%
  filter(!is.na(LBXSGL)) %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 50, fill = "red", alpha = 0.4) +
  # geom_vline es una linea que pasa por el gráfico en 126
  geom_vline(xintercept = 126, color = "black", linetype = "dashed") +
  labs(
    title = "Distribución de imc",
    x = "Glicemia",
    y = "Número de personas"
  )

#  Haz un geom_density()

df_obesidad_dist %>%
  filter(!is.na(LBXSGL)) %>%
  ggplot(aes(x = LBXSGL)) +
  geom_density(fill = "red", alpha = 0.4) +
  # geom_vline es una linea que pasa por el gráfico en 126
  geom_vline(xintercept = 126, color = "black", linetype = "dashed") +
  labs(
    title = "Distribución de imc",
    x = "Glicemia",
    y = "Poporción de personas"
  )
#👉 Pregunta:
  
#  ¿Qué información te da cada uno que el otro no? El histograma me
# da la cantidad de observaciones en cierta resultado de la glicemia.
# Mientras que el gráfico de densidad me da la proporción de personas
# con cierto resultado de glicemia

#######################
##
## Ejercicio 10
##
#######################

# Ejercicio 10 — Identificar outliers clínicos

# Con presión sistólica (BPXSY_mean):
  
#  Grafica la distribución
df_obesidad_dist %>%
  filter(!is.na(BPXSY_mean)) %>%
  ggplot(aes(x = BPXSY_mean)) +
  geom_histogram(bins = 50, fill = "brown", alpha = 0.4) +
  labs(
    title = "Distribución de imc",
    x = "Presión sistólica",
    y = "Número de personas"
  )

# Identifica visualmente valores extremos

#👉 Pregunta:
  
#  ¿Qué valores considerarías clínicamente preocupantes?
# Creo que los valores por arriba de 150
#  ¿Son frecuentes o raros? Mayores a 150 no son tan raros, los 
# valores mayores a 180 sí son infrecuentes.

# Interpretación clínica
# ❓ ¿Qué valores son preocupantes?
# - ≥140 → hipertensión
# - ≥160 → alto riesgo
# - ≥180 → emergencia potencial

#######################
##
## Ejercicio 11
##
#######################

# Ejercicio 11 — Distribución vs punto de corte

# Para glicemia:
  
#  1. Grafica la distribución
#  2. Agrega una línea vertical en 126

# geom_vline(xintercept = 126, color = "red")

df_obesidad_dist %>%
  filter(!is.na(LBXSGL)) %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 50, fill = "yellow") +
  geom_vline(xintercept = 126, color = "red") +
  labs(
    title = "Distribución de imc",
    x = "Presión sistólica",
    y = "Número de personas"
  )


# Pregunta:
  
#  ¿La línea separa claramente dos poblaciones? NO, de hecho parece
# que 126 y 125 están en el mismo bin

#######################
##
## Ejercicio 12
##
#######################

# Ejercicio 12 — Superposición de distribuciones

# Divide IMC en:

df_obesidad_dist <- df_obesidad_dist %>%
  mutate(
    grupo_imc = case_when(
      BMXBMI < 25 ~ "normal",
      BMXBMI < 30 ~ "sobrepeso",
      BMXBMI >= 30 ~ "obesidad",
      TRUE ~ NA_character_
    )
  )

#  normal (<25)
# sobrepeso (25–30)
# obesidad (≥30)

# Y grafica la glicemia para cada grupo.

# Gráfico de densidad que compara la glicemia con el imc 
df_obesidad_dist %>%
  filter(!is.na(LBXSGL), !is.na(BMXBMI)) %>%
  ggplot(aes(x = LBXSGL, fill = grupo_imc)) +
  geom_density(alpha = 0.2) +
  labs(
    title = "Distribución de glicemia por edad",
    x = "Glicemia",
    fill = "Grupo imc"
  )

# 👉 Pregunta:
  
#  ¿Las distribuciones están separadas o se solapan? Se solapan sobre
# todo la glicemia de la obesidad y el sobrepeso

#######################
##
## Ejercicio 13
##
#######################

# Ejercicio 13 — Pensamiento bayesiano inicial

# Observa la distribución de glicemia.

# 👉 Pregunta:
  
#  Si tomas un paciente al azar con glicemia de 110,
#  ¿es más probable que sea “normal” o “prediabético”?
  
#  (No uses puntos de corte, usa la forma de la distribución)

# Creo que prediabético

#######################
##
## Ejercicio 14
##
#######################

# $ Divide por edad:
  
# -  <40
# -  40–60
# -  60

df_obesidad_dist <- df_obesidad_dist %>%
  mutate(
    grupo_edad = case_when(
      RIDAGEYR < 40 ~ "<40",
      RIDAGEYR < 60 ~ "<60",
      RIDAGEYR >= 60 ~ ">=60",
      TRUE ~ NA_character_
    )
  )
# Grafica glicemia.

# Gráfico de densidad que compara la glicemia con la edad 
df_obesidad_dist %>%
  filter(!is.na(LBXSGL), !is.na(RIDAGEYR)) %>%
  ggplot(aes(x = LBXSGL, fill = grupo_edad)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribución de glicemia por edad",
    x = "Glicemia",
    fill = "Grupo edad"
  )

#👉 Pregunta:
  
#  ¿La edad cambia el centro o la forma de la distribución?
# Cambia el centro, ligeramente hacia la derecha como si con la
# edad aumentaría la glicemia


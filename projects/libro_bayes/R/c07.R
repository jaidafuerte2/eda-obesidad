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

# Simular 200 pacientes
n <- 200

# Simular el índice de masa corporal y la insulina en una población 
# de 200 pacientes
df_sim <- tibble(
  imc = rnorm(n, mean = 27, sd = 4),
  insulina = 5 + 0.8 * imc + rnorm(n, 0, 5) # genera valores 
  # aleatorios con una media de 0 y una desviación de 5
)

# Visualizar insulna vs índice de masa corporal
ggplot(df_sim, aes(x = imc, y = insulina)) +
  geom_point(alpha = 0.6) +
  # method = "loess" genera una línea suavizada "adaptable" no recta
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Relación entre IMC e insulina (simulada)",
    x = "IMC",
    y = "Insulina"
  )

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar sólo las variables que voy a usar
df_joint <- df_obesidad %>%
  select(BMXBMI, LBXIN, BMXWAIST, LBXTR, RIAGENDR, RIDAGEYR) %>%
  drop_na()

# Diagrama de dispersión que relaciona el índice de masa corporal con
# los niveles de insulina
df_joint %>%
  filter(
    LBXIN <= 50,
    BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.1) +
    # se = FALSE : se usa para no mostrar la franja de error estándar
    geom_smooth(method = "loess", se = FALSE) +
    #geom_smooth()
    labs(
      title = "IMC vs Insulina",
      x = "IMC",
      y = "Insulina"
  )
# NOTA: la insulina tiende a estabilizarse porque hay pocos pacientes
# con un imc > 50; en fases avanzadas de la enfermedadmetabólica 
# el páncreas puede empezar a fallar y esto disminuye la secreción
# de insulina; esto puede reflejar una transición de hiperinsulinemia
# a disfunción beta pancréatica.

# Diagrama de dispersión que relaciona el índice de masa corporal con
# los niveles de insulina facetado por sexo donde 1 es hombres y 2 es 
# mujeres
df_joint %>%
  filter(
    LBXIN <= 50,
    BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = FALSE) +
    #geom_smooth()
    facet_wrap(~ RIAGENDR) +
    labs(
      title = "IMC vs Insulina por sexo",
      x = "IMC",
      y = "Insulina"
    )

# Diagrama de dispersión que relaciona la ciercunferencia de cintura
# con la trigliceridemia
df_joint %>%
  filter(
    LBXTR <= 500#,
    #BMXWAIST <= 150
  ) %>%
  ggplot(aes(x = BMXWAIST, y = LBXTR)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "Circunferencia de cintura vs Triglicéridos",
      x = "Cintura (cm)",
      y = "Triglicéridos"
    )
# NOTA: La curva se aplana y desciende porque: hay menos datos en
# extremos mayores a 135; hay saturación metabólica, al inicio 
# más grasa visceral aumenta triglicéridemia , pero luego el sistema
# ya está alterado y no sigue aumentando linealmente (como si el
# cuerpo tocara un techo); puede der que un paciente con una 
# circunferencia muy amplia ya esté tomando estatinas o fibratos

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

# Ejercicio 1 — Ver lo invisible

#👉 Objetivo: notar cómo cambia la relación

df_joint %>%
  filter(
    LBXIN <= 50#,
    #BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "loess", se = FALSE)

#❓ Preguntas
#  - ¿En qué rango de IMC la pendiente es más fuerte?
#    Entre 20 y 30 de imc aunque también entre 30 y 40 de imc
#  - ¿Dónde empieza a cambiar?
#    En 50-60 se empieza a aplanar
#  - ¿Hay zonas con más densidad de puntos?
#    Sí, entre 20 y 30 y entre 30 y 35

#######################
##
## Ejercicio 2
##
#######################

#Ejercicio 2 — Cambiar el lente

#👉 Cambia LOESS por línea recta:
  
#  geom_smooth(method = "lm")

df_joint %>%
  filter(
    LBXIN <= 50#,
    #BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE)

#❓ Preguntas
# - ¿Qué información pierdes? La información por grupos de peso
# - ¿Cuál describe mejor la realidad clínica?
#   Creo que la línea recta describe mejor la realidad clínica

#######################
##
## Ejercicio 3
##
#######################

# Ejercicio 3 — Estratificación clínica

#facet_wrap(~ RIAGENDR)

df_joint %>%
  filter(
    LBXIN <= 50#,
    #BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.2) +
    #geom_smooth(method = "loess", se = FALSE) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ RIAGENDR)

#❓ Preguntas

# ¿Las curvas son iguales en hombres y mujeres? No, las mujeres
# llegan a tener valores más altos de imc pero no es tan evidente la 
# meseta de insulina 
#  ¿Dónde hay más dispersión? Creo que un poco más en mujeres
#  ¿Qué hipótesis clínica puedes generar?
#  El páncreas de los hombres puede fallar más temprano

#######################
##
## Ejercicio 4
##
#######################

# Ejercicio 4 — Detectar extremos

# 👉 Filtra:
  
df_joint %>%
  filter(BMXBMI > 45) #%>%
  #summary()

#Luego grafica otra vez.

df_joint %>%
  filter(
    LBXIN > 45#,
    #BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "loess", se = FALSE)

#❓
# - ¿Cuántos pacientes hay?
# - ¿Tiene sentido la forma de la curva? Es una curva aplanada

#######################
##
## Ejercicio 5
##
#######################

# Ejercicio 5 — Nueva relación clínica

# 👉 Crea este gráfico:
  
df_joint %>%
  filter(
    LBXTR <= 1000#,
    #BMXWAIST <= 150
  ) %>%
  ggplot(aes(x = BMXWAIST, y = LBXTR)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE)
    #geom_smooth(method = "lm", se = FALSE)
  
#❓
#  ¿Dónde aumenta más rápido el riesgo? 
#  Entre 60 y 90 y 90 y 105
#  ¿Dónde se “aplana”?
#  Entre 105 y 120
#  ¿Qué explicación clínica propones?
#  uso de estatinas o fibratos, tal vez menos consumo de azúcares

#######################
##
## Ejercicio 6
##
#######################

#Ejercicio 6 — Pensamiento bayesiano

#👉 Elige un paciente:
  
#  IMC = 32 
#  Insulina = 8

#❓

# - ¿Dónde cae en la gráfica?
#   Debajo de la línea suavizada
# - ¿Es típico o atípico? Típico
# - ¿Cómo cambiaría tu creencia sobre su riesgo? Creería que tiene 
#   Un riesgo bajo porque aunque tiene un imc alto, su insulina es
#   más bien baja

#######################
##
## Ejercicio 7
##
#######################

# Ejercicio 7 — Crear hipótesis

#👉 Observa cualquier gráfico y responde: imc vs insulina
  
#  - ¿Qué patrón ves?
#    La insulina aumenta con el imc
#  - ¿Qué explicación biológica propones?
#    La insulina aumenta la grasa corporal
#  - ¿Qué variable falta?
#    Para determinar riesgo metabólico y riesgo de obesidad la
#    glicemia (y consecuentemente la resistencia a la insulina) 
#    pueden ser convenientes

#######################
##
## Ejercicio 8
##
#######################

# Ejercicio 8 — Densidad oculta

#👉 Objetivo: ver dónde están los pacientes realmente
df_joint %>%
  filter(
    LBXIN < 100,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_bin2d() +
    labs(title = "Densidad IMC vs Insulina")

#❓ Preguntas

# - ¿Dónde se concentran más pacientes? Puede ser que en sobrepeso
#   en valores menores de 12, tal vez normales
# - ¿Coincide con donde la curva cambia? Sí, entre 50 y 60 hay
#   muy pocos pacientes, justo donde se empieza a aplanar la curva
# - ¿Los extremos tienen suficiente información? Creo que no, creo
#   que se debe relacionar con algo más

#######################
##
## Ejercicio 9
##
#######################

# Ejercicio 9 — Filtrar por edad

df_joint %>%
  filter(
    LBXIN < 100,
    BMXBMI <= 60
  ) %>%
  #filter(RIDAGEYR > 50) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = FALSE)

# ¿Cambia la relación en mayores de 50? Yo la veo parecida
# ¿La curva se aplana antes? Sí, se aplana ligeramente antes, mientras
# que en menores de 50 se empieza a aplanar en 45 de imc, en mayores
# de 50 se empieza a aplanar en 50 de imc
# ¿Qué rol juega la edad? Parece que hay niveles un poco mayores
# de insulina a mayor edad.

#######################
##
## Ejercicio 10
##
#######################

# Ejercicio 10 — Subgrupos metabólicos

#👉 Crea grupos de triglicéridos:
  
df_joint <- df_joint %>%
  mutate(
    trig_cat = case_when(
      LBXTR < 150 ~ "normal",
      LBXTR < 200 ~ "limite",
      TRUE ~ "alto"
    )
  )

#Luego:
df_joint %>%
  filter(
    LBXIN < 100,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "loess", se = FALSE) +
    #geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ trig_cat)

#❓
# ¿Los pacientes con triglicéridos altos tienen más insulina?
# Sí la tendencia es mayor, es decir hay una pendiente mayor que llega
# hasta 45
# ¿Cambia la forma de la curva? Sí posterior a 30 de imc , los que 
# tienen alto colesterol tienen una ligera cumbre que luego decae
# cuando se empieza a llegar a 40 de imc.

#######################
##
## Ejercicio 11
##
#######################

#Ejercicio 11 — Buscar pacientes “raros”

df_joint %>%
  filter(BMXBMI > 30, LBXIN < 5) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
   geom_point(alpha = 0.3) +
   geom_smooth(method = "loess", se = FALSE)


#❓
# ¿Existen obesos con baja insulina? Sí
# ¿Qué explicaciones clínicas propones? Que estén a dieta, que 
# hayan ayunado muchas horas, tal vez metformina 

#######################
##
## Ejercicio 12
##
#######################

#Ejercicio 12 — Comparar dos relaciones

# Haz dos gráficos:
  
# 1. IMC vs insulina
df_joint %>%
  filter(
    LBXIN < 50,
    BMXBMI <= 60
  ) %>%
  #filter(RIDAGEYR > 50) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE)

# 2. cintura vs triglicéridos
df_joint %>%
  filter(
    LBXTR <= 375#,
    #BMXWAIST <= 150
  ) %>%
  ggplot(aes(x = BMXWAIST, y = LBXTR)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE)
#geom_smooth(method = "lm", se = FALSE)
# ❓
# ¿Cuál relación es más fuerte? Creo que insulina - imc
# ¿Cuál es más clínica? Pienso que insulina - imc
# ¿Cuál usarías en consulta? insulina - imc
# El problema con trigliceridos y circunferencia de cintura es que
# la curva decae mucho a partir de 120 cms

#######################
##
## Ejercicio 13
##
#######################

# Ejercicio 13 — Tu primer insight real

#👉 Elige UNA gráfica

#Responde:
  
#  ¿Qué patrón ves?
df_joint %>%
  filter(
    LBXIN < 50,
    BMXBMI <= 60
  ) %>%
  #filter(RIDAGEYR > 50) %>%
  ggplot(aes(x = BMXBMI, y = LBXIN)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE)

#  ¿Dónde cambia la pendiente? A partir de 50 
#  ¿Qué significa clínicamente? Que la insulina puede subir hasta
#  un tope
#  ¿Qué variable falta? Glicemia
library(dplyr)
library(tidyverse)
library(dagitty)

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

# Simular 1000 pacientes
n <- 1000

# Crear simulación con las variables actividad física, obesidad y
# glicemia
df_sim <- tibble(
  # Simular actividad física
  actividad = rnorm(n, mean = 0, sd = 1),
  # Simular obesidad con una media de cero y una desviación de uno
  obesidad = 0.7 * (-actividad) + rnorm(n, 0, 1),
  # Simular glicemia con una media de cero y una desviación de uno
  glicemia = 0.8 * obesidad + rnorm(n, 0, 1)
)

# Diagrama de dispersión que relaciona la obesidad con la glicemia
ggplot(df_sim, aes(x = obesidad, y = glicemia)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Obesidad vs Glicemia")

# Diagrama de dispersión que relaciona la actividad física con la 
# glicemia
ggplot(df_sim, aes(x = actividad, y = glicemia)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Actividad vs Glicemia")

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar sólo las variables que voy a usar
df_joint <- df_obesidad %>%
  select(BMXBMI, LBXSGL, BPXSY_mean, PAD680, PAQ655, PAD660,
         PAQ610, PAD615) %>%
  drop_na()

# Diagrama de dispersión que relaciona el índice de masa corporal
# con la glicemia
df_joint %>%
  filter(
    LBXSGL <= 500,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXSGL)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    labs(
      title = "IMC vs Glicemia",
      x = "IMC",
      y = "Glicemia"
    )

# Diagrama de dispersión que relaciona el índice de masa corporal 
# con la glicemia
df_joint %>%
  filter(
    BPXSY_mean <= 200,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = BPXSY_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "IMC vs Presión Arterial",
    x = "IMC",
    y = "Presión sistólica"
  )

# Diagrama de dispersión que relaciona el sedentarismo con el índice
# de masa corporal
df_joint %>%
  filter(
    PAD680 <= 1250,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = PAD680)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "IMC vs Sedentarismo",
    x = "IMC",
    y = "Sedentarismo"
  )

# Dag de sedentarismo, imc, glicemia y presión
dag <- dagitty("
dag {
  sedentarismo -> imc
  imc -> glicemia
  imc -> presion
}
")
#plot(dag)

############## Actividad recrativa vigorosa ###################

# Crear la variable de minutos de actividad física vigorosa 
# recreativa por semana
df_joint <- df_joint %>%
  mutate(
    PAQ655 = if_else(PAQ655 %in% c(77, 99), NA_real_, as.numeric(PAQ655)),
    PAD660 = if_else(PAD660 %in% c(7777, 9999), NA_real_, as.numeric(PAD660))
  ) %>%
  mutate(
    min_vigorosa_recrea_semana = PAQ655 * PAD660
  )

# Diagrama de dispersión que relaciona la actividad recreativa 
# vigorosa con el índice de masa corporal en trabajo
df_joint %>%
  filter(
    #min_vigorosa_semana <= 500,
    BMXBMI <= 45
  ) %>%
  ggplot(aes(x = BMXBMI, y = min_vigorosa_recrea_semana)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(
    title = "IMC vs Ejercicio",
    x = "IMC",
    y = "Ejercicio"
  )

######## Actividad laboral vigorosa ##########

# Crear la variable de minutos de actividad física vigorosa 
# recreativa por semana
df_joint <- df_joint %>%
  mutate(
    PAQ610 = if_else(PAQ610 %in% c(77, 99), NA_real_, 
                     as.numeric(PAQ610)),
    PAD615 = if_else(PAD615 %in% c(7777, 9999), NA_real_, 
                     as.numeric(PAD615))
  ) %>%
  mutate(
    min_vigorosa_semana_laboral = PAQ610 * PAD615
  )

# Diagrama de dispersión que relaciona la actividad recreativa 
# vigorosa con el índice de masa corporal
df_joint %>%
  filter(
    #min_vigorosa_semana_trabajo <= 500,
    BMXBMI <= 45
  ) %>%
  ggplot(aes(x = BMXBMI, y = min_vigorosa_semana_laboral)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(
    title = "IMC vs Actividad física laboral",
    x = "IMC",
    y = "Actividad física laboral"
  )

############### Actividad vigorosa total ######################

df_joint <- df_joint %>%
  mutate(
    min_vigorosa_total = coalesce(min_vigorosa_semana_laboral, 0) +
      coalesce(min_vigorosa_recrea_semana, 0)
  )

# Diagrama de dispersión que relaciona la actividad total 
# vigorosa con el índice de masa corporal
df_joint %>%
  filter(
    #min_vigorosa_semana_trabajo <= 500,
    BMXBMI <= 45
  ) %>%
  ggplot(aes(x = BMXBMI, y = min_vigorosa_total)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(
    title = "IMC vs Actividad vigorosa total",
    x = "IMC",
    y = "Actividad vigorosa total"
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

# Ejercicio 1 — Leer una historia causal

# Observa este DAG:
  
#  actividad_fisica → imc → glicemia

#Responde:
  
#  ¿Qué variable parece influir directamente sobre el IMC?
#  - La actividad física 
#  ¿Qué variable parece influir directamente sobre la glicemia?
#  - El índice de masa corporal
#  ¿La actividad física tiene un efecto directo o indirecto sobre la glicemia?
#  - Efecto indirecto
#  Explica esta historia como si se la contaras a un paciente.
#  - Moverse poco puede influir ligeramente en aumentar el peso de
#    una persona y el peso influye sobre la glicemia

#######################
##
## Ejercicio 2
##
#######################

# Ejercicio 2 — Crear una historia causal alternativa

# Supón que observas que las personas con mayor IMC también tienen mayor presión arterial.

# Dibuja dos historias causales distintas que puedan explicar esta asociación.

#Por ejemplo:
  
#  - Una donde el IMC cause presión arterial
#  - Otra donde ambas dependan de una tercera variable

# Puedes escribirlo así:
  
#  variable → variable

# Pregunta guía:
  
#  - ¿Podría existir una variable escondida que esté afectando a ambas?

# Claro, el exceso de insulina puede influir en el imc y la presión

# imc -> presión arterial
# insulinemia -> presión arterial
# hiperinsulinemia -> resistencia a la insulina
# resistenicia a la insulina ->  imc
# resistenicia a la insulina ->  presión arterial

#######################
##
## Ejercicio 3
##
#######################

# Ejercicio 3 — Correlación no significa causalidad

# Piensa en esta asociación:
  
#   Más actividad física vigorosa ↔ mayor IMC

# Propón al menos tres explicaciones posibles:
  
# - Una explicación donde el ejercicio aumente el IMC
#   Esto es común cuando se hace mucho ejercicio y aumenta el apetito 
# - Una explicación donde el IMC aumente el ejercicio
#   Personas que siguen indicaciones de un facultativo de hacer
#   ejercicio para bajar de peso
# - Una explicación donde exista otra variable involucrada
#   Tal vez personas de escasos recursos que relaizan labores física
#   Mal remuneradas y consumen alimentos baratos y engordantes

# Pista clínica:
  
#  Recuerda que algunas personas con obesidad empiezan a hacer mucho 
#  ejercicio justamente porque ya tienen obesidad.

#######################
##
## Ejercicio 4
##
#######################

# Ejercicio 4 — Dibujar tu primer DAG con obesidad

# Construye un DAG simple usando estas variables:
  
# - Sedentarismo
# - IMC
# - Glicemia
# - Presión arterial

# Reglas:
  
#  Usa entre 3 y 5 flechas
#  No hagas ciclos
#  Intenta que la historia tenga sentido clínico

# Ejemplo de formato:
  
#  sedentarismo → imc
#  imc → glicemia
#  imc → presion

# DAG de sedentarismo, imc,  glicemia y presión arterial
dag <- dagitty("
dag {
  sedemtarismo -> imc
  imc -> glicemia
  imc -> presion
}
")
#plot(dag)

# Después responde:
  
#  ¿Por qué elegiste esas flechas y no otras? Porque quiero llegar
#  a plantear que la glicemia y la presión sí dependen en parte del 
#  peso de una persona

#######################
##
## Ejercicio 5
##
#######################

# Ejercicio 5 — Encontrar un error causal

# Observa este DAG:
  
# imc → glicemia
# glicemia → actividad_fisica
# actividad_fisica → imc

#  ¿Por qué este DAG no es válido? Porque termina donde empezó en imc
#  ¿Qué significa que tenga un ciclo? Que termina dónde empezó
#  ¿Cómo podrías corregirlo? Creo que esto podría ser una opción:
# imc → glicemia
# glicemia → actividad_fisica

#######################
##
## Ejercicio 6
##
#######################

# Ejercicio 6 — Traducir consulta médica a DAG

# Imagina este paciente:
  
# - Tiene obesidad
# - Tiene presión arterial elevada
# - Tiene glicemia alta
# - Refiere dormir poco

# Pregunta:
  
#  ¿Cómo podrías representar esta historia usando un DAG?

# sueño -> glicemia
# glicemia -> imc
# imc -> presión
 
#  Pista:
  
#  Tal vez el sueño influye sobre más de una variable.

#######################
##
## Ejercicio 7
##
#######################

# Ejercicio 7 — Pensar como Jason Fung

# Construye un DAG inspirado en la idea central de El 
# Código de la Obesidad.

# Debe incluir al menos estas variables:
  
# - Insulina
# - Obesidad
# - Inflamación
# - Glicemia

dag <- dagitty("
{
  Insulina -> Obesidad
  Obesidad -> inflamacion
  Obesidad -> glicemia
}
" 
)
#plot(dag)

# Después responde:
  
#  ¿Cuál parece ser la variable más “importante” en tu DAG?
#  - Insulina
#  ¿Qué variable parece estar más “al final” de la cadena?
#  - Inflamación
#  ¿Cómo cambiaría tu tratamiento si crees que la insulina está 
#   “arriba” en la historia causal? Me preocupo de restringir los 
#   alimentos que elevan más la insulina 

#######################
##
## Ejercicio 8
##
#######################

# Ejercicio 8 — DAG con NHANES

# Usa estas variables reales:
  
# -  BMXBMI
# -  LBXSGL
# -  BPXSY_mean
# -  PAD680

# Escribe un DAG sencillo usando los nombres reales de NHANES.

# Por ejemplo:
  
# PAD680 → BMXBMI
# BMXBMI → LBXSGL
# BMXBMI → BPXSY_mean

dag <- dagitty(
  "
  {
  PAD680 -> BMXBMI
  BMXBMI -> LBXSGL
  BMXBMI -> LBXSGL
  }
  "
)

# Después responde:
  
# -  ¿Qué variable parece ser la más “causal”? 
#    Índice de masa corporal
# -  ¿Qué variable parece ser más “resultado”?
#    Presión arterial
# -  ¿Cuál sería la variable más interesante para intervenir 
#    clínicamente? Obesidad

#######################
##
## Ejercicio 9
##
#######################

# Ejercicio 9 — Dibujar un DAG con dagitty

# Intenta escribir este código y modificarlo:

mi_dag <- dagitty("
dag {
  actividad -> imc
  imc -> glicemia
  imc -> presion
}
")
#plot(mi_dag)

# Ahora agrega una nueva variable, por ejemplo:
  
# - sueño
# - estrés
# - inflamación
# - insulina

mi_dag <- dagitty("
dag {
  insulina -> imc
  estres -> imc
  suenio -> imc
  actividad -> imc
  imc -> glicemia
  imc -> presion
  imc -> inflamacion
}
")
#plot(mi_dag)

# Pregunta:
  
#  ¿Cómo cambia la historia cuando agregas una nueva variable?
#  Se vuelve multicausal y multiconsecuencias

#######################
##
## Ejercicio 10
##
#######################

# Ejercicio 10 — El más importante de todos

# Escoge cualquier asociación que hayas visto en NHANES y responde:
  
#  ¿Qué variables están asociadas? imc y glicemia
#  ¿Cuál sería la explicación causal más obvia?
#  Los altos niveles de glicemia causan altos niveles de insulina
#  y esto se ve como una relación entre glicemia e imc
#  ¿Qué otras explicaciones podrían existir?
#  Que la obesidad trae consigo resistencia a la insulina y un
#  cuerpo resistente a la insulina no disminuye la glicemia
#  ¿Qué DAG resumiría mejor esa historia?

dag <- dagitty(
  "dag {
    resistencia_a_la_insulina -> obesidad
    resistencia_a_la_insulina -> glicemia
  }
  "
)
#plot(dag)
  
#  Objetivo final:
  
#  Empezar a pensar menos como “alguien que mira correlaciones” y más 
#  como “alguien que intenta entender causas”.
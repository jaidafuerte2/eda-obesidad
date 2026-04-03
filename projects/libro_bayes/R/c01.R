########################
##                    ##
## Capítulo 1         ## 
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

# Simulamos 200 pacientes
n <- 200

datos <- tibble(
  # Se crean 200 observaciones con una media de 105 y una desviación
  # estándar de 15
  glucosa = rnorm(n, mean = 105, sd = 15),
  # "verdadero estado" (1 = alteración metabólica, 0 = normal)
  # Se categoriza
  estado_real = if_else(glucosa > 110, 1, 0)
)

# Agregamos ruido (simulando incertidumbre real)
datos <- datos %>%
  mutate(
    # Un 10% se invierte simulando errores posibles de la vida real
    # como: errores diagnósticos, medición imperfecta o variabilidad
    # biológica
    estado_observado = if_else(runif(n) < 0.1, 1 - estado_real, estado_real)
  )

# Gráfico en el que se observarán barras azules por debajo de 110 y
# barras rosadas por encima de 110 lo que corresponde al 10% del
# error de la simulación anterior con runif
ggplot(datos, aes(x = glucosa, fill = factor(estado_observado))) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(
    fill = "Estado observado",
    title = "Glucosa vs estado metabólico (con incertidumbre)"
  )

# Histograma que puede mostrar la distribución de glucosa, hemoglobina 
# glicosilada  e índice de masa corporal en participantes del nhanes
df_obesidad %>%
  select(LBXSGL, LBXGH, BMXBMI) %>%
  drop_na() %>%
  ggplot(aes(x = BMXBMI)) +
  geom_histogram(binwidth = 1)


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

# Ejercicio 1 — El diagnóstico incómodo

# Un paciente tiene:
  
# Glucosa: 125 mg/dL
# HbA1c: 6.4%
# IMC: 31

# -> Pregunta:
  
# Según criterios clásicos, ¿es diabético? NO
# ¿Qué tan seguro estás de esa respuesta? (alto, medio, bajo)
# Medio
# Reformula tu respuesta en términos probabilísticos
# Hay una probabilidad alta de no ser aún diabético pero hay que estar
# atento, mejor repetir el examen 
# ->  Objetivo: romper el pensamiento binario

################
##
## Ejercicio 2
##
################

# Dos pacientes casi iguales

# Paciente A:
  
#  HbA1c: 6.5%

# Paciente B:
  
#  HbA1c: 6.4%

# -> Pregunta:
  
#  ¿Son clínicamente diferentes? NO
#  ¿El sistema de clasificación los trata diferente? Sí
#  ¿Qué problema conceptual estás viendo aquí?
#  - Que en la práctica, un paciente casi diabético, ya debería
#    tomar ciertas precauciones en su hábito, sobre todo. 
  
# -> Objetivo: detectar arbitrariedad de los puntos de corte

################
##
## Ejercicio 3
##
################

# Ejercicio 3 — IMC como medida imperfecta

# Dos pacientes:
  
# Ambos con IMC = 30
# Uno con alta grasa visceral
# Otro con masa muscular alta

# -> Pregunta:
  
#  ¿Tienen el mismo riesgo cardiometabólico? No
#  ¿Qué tipo de incertidumbre estás enfrentando? Alta
#  ¿Qué información te falta? La grasa visceral que es la más inflamatoria
#  y la que nos interesa tratar.
  
# -> Objetivo: entender limitaciones de los datos

################
##
## Ejercicio 4
##
################

# Ejercicio 4 — Pensamiento bayesiano básico

# Un paciente tiene:
  
# Obesidad
# Sedentarismo
# Historia familiar de diabetes

# -> Pregunta:
  
#  Antes de ver exámenes, ¿crees que la probabilidad de diabetes es alta o baja?
#  Es alta
#  ¿Qué estás usando aquí: datos o creencias previas? Creencias
#  previas
#  ¿Por qué esto NO es “mala medicina”? Porque muchos problemas
#  médicos y metabólicos son silenciosos, no presentan síntomas, pero
#  son peligrosos. Las dislipidemias no duelen, igual que las
#  hiperglicemias y muchas veces la presión alta, pero todas son
#  peligrosas y se podrían mejorar con pequeños cambios de estilo
#  de vida
  
# -> Objetivo: introducir la idea de prior (creencia previa)

################
##
## Ejercicio 5
##
################

# Ejercicio 5 — Crear tu propia incertidumbre
# -> Modifica el código de simulación:
  
#Cambia el punto de corte de glucosa (ej: 100 en lugar de 110)
#Cambia el error (ej: 20% en lugar de 10%)
# cambia estos valores
#glucosa > 100
#runif(n) < 0.2

# Crear 200 observaciones
datos <- tibble(
  glucosa = rnorm(n, mean = 105, sd = 15),
  estado_real = if_else(glucosa > 100, 1, 0)
)

# Agregamos ruido (simulando incertidumbre real)
datos <- datos %>%
  mutate(
    estado_observado = if_else(runif(n) < 0.2, 1 - estado_real, estado_real)
  )

# Histograma de glucosa coloreado por estado observado
ggplot(datos, aes(x = glucosa, fill = factor(estado_observado))) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(
    fill = "Estado observado",
    title = "Glucosa vs estado metabólico (con incertidumbre)"
  )

# -> Pregunta:
  
#  ¿Qué pasa con el solapamiento?
#  - El solapamiento aumenta
#  ¿Aumenta o disminuye la incertidumbre?
#  - Aumenta la incertidumbre
#  ¿Qué escenario se parece más a la vida real?
#  - No sé creo que un 10% de error por toma de muestra es suficiente,
#    o por variabilidad biológica o por error diagnóstico, creo que
#    20% es demasiado
#  -> Objetivo: ver cómo nace la incertidumbre

################
##
## Ejercicio 6
##
################

# Ejercicio 6 — El falso diagnóstico perfecto

# -> Elimina el error:
  
#  estado_observado = estado_real

# Agregamos ruido (simulando incertidumbre real)
datos <- datos %>%
  mutate(
    estado_observado = estado_real
  )

# Histograma de glucosa coloreado por estado observado
ggplot(datos, aes(x = glucosa, fill = factor(estado_observado))) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(
    fill = "Estado observado",
    title = "Glucosa vs estado metabólico (con incertidumbre)"
  )

# -> Pregunta:
  
#  ¿Qué pasa con el gráfico?
#  - Hay mucho menor solapamiento
#  ¿Existe aún incertidumbre?
#  - No , ya no hay incertidumbre, el solapamiento es por los rangos
#    que grafica el histograma
#  ¿Este escenario es realista?
#  - Creo que no
  
#  -> Objetivo: entender que la perfección no existe en medicina

################
##
## Ejercicio 7
##
################

# Ejercicio 7 — Juega con la variabilidad

# -> Cambia la desviación estándar:
  
#rnorm(n, mean = 105, sd = 5)
#rnorm(n, mean = 105, sd = 25)

# Crear 200 observaciones
datos <- tibble(
  glucosa = rnorm(n, mean = 105, sd = 25),
  estado_real = if_else(glucosa > 100, 1, 0)
)

# Agregamos ruido (simulando incertidumbre real)
datos <- datos %>%
  mutate(
    estado_observado = estado_real
)

# Histograma de glucosa coloreado por estado observado
ggplot(datos, aes(x = glucosa, fill = factor(estado_observado))) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(
    fill = "Estado observado",
    title = "Glucosa vs estado metabólico (con incertidumbre)"
  )

# -> Pregunta:
  
#  ¿Cómo cambia la superposición?
#  -  Cuando la desviación estándar es mayor, disminuye la 
#     superposición 
#  ¿Qué significa esto clínicamente?
#  Que cuando la desviación estándar es mayor, disminuye la 
#  incertidumbre
# ->   Objetivo: entender variabilidad biológica

################
##
## Ejercicio 8
##
################

#Ejercicio 8 — ¿Hay dos poblaciones?
  
#  -> Grafica la glucosa:
ggplot(df_obesidad, aes(x = LBXSGL)) +
  geom_histogram(binwidth = 5)

# -> Pregunta:
  
#  ¿Ves dos grupos claramente separados?
#  - No
#  ¿Dónde pondrías el punto de corte?
#  - En 125
#  ¿Qué tan arbitrario es?
#  - Tal vez bastante arbitrario, o tal vez no tanto  
#  -> Objetivo: ver la continuidad de la realidad

################
##
## Ejercicio 9
##
################

# Ejercicio 9 — Variable categórica real

#Supongamos:
  
# Hipertensión (sí/no)
# Obesidad (sí/no)

# -> Pregunta (sin modelo aún):
  
#  ¿Crees que están relacionadas?
#  -Sí están relacionadas, mucho.

#  ¿Cómo lo expresarías en términos de probabilidad?
#  Ejemplo esperado:
#  P(obesidad | hipertensión)
#  - Podría decir que las personas obesas tienen más probabilidades
#    de tener hipertensión que las personas no obesas
# -> Objetivo: empezar a pensar en probabilidades condicionales

################
##
## Ejercicio 10
##
################

# Ejercicio 10 — Traducción mental

# Convierte estas frases:
  
# -> “El paciente es diabético”

# -> En versión bayesiana: Los exámenes indican que hay más probabilidad
#    de una glucosa en sangre excesiva
  
# ->“El paciente está sano”

# -> En versión bayesiana: Es más probable que el paciente esté
#    sano
  
# -> “El IMC indica obesidad”

# -> En versión bayesiana: El imc indica la probabilidad de que
#    tengas exceso de pso 
  
# Objetivo: reprogramar tu lenguaje clínico

################
##
## Ejercicio 11
##
################

# Ejercicio 11 — Detecta el error

# Un colega dice:
  
#  “Si la HbA1c es menor a 6.5%, no hay diabetes”

# -> Pregunta:
  
#  ¿Qué está mal en esa afirmación?
#  Ese valor está al límite cuantitativo y pudo producirse por
#  errores o variabilidad bológica. En la práctica ya se le pediría
#  que haga cambios en su vida
#  ¿Qué tipo de pensamiento es? Frecuentista. Todo o nada
#  ¿Cómo lo corregirías?
#  Es improbable que tenga diabetes pero es muy probable que llegue
#  a ella. En la práctica, se pediría que, al menos, haga cambios
# en su estilo de vida.

################
##
## Ejercicio 12
##
################

# Ejercicio 12 — Tu propia práctica clínica

# Piensa en un paciente real que hayas visto recientemente.
# Me pasa un poco con los pacientes que presentan un tsh un poco
# elevado pero no presentan clínica de hipotiroidismo

# -> Responde:
  
#  ¿Qué decisión tomaste?
#  - Volver a hacerse el examen posteriormente. Porque puede ser un error
#    en la medición y porque adelganzando suele disminuir el tsh
#  ¿Qué incertidumbre había?
#  - Alta porque no coincide el laboratorio con la clínica
#  ¿La expresaste o la ignoraste?
#  - La expresé
#  ¿Cómo la expresarías ahora en términos probabilísticos?
#  - Hay una alta probabilidad de que el resultado sea por un error
#    de laboratorio pues es improbable que haya hipotiroidismo 
#    sin clínica.

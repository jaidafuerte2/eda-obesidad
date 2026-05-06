library(dplyr)
library(tidyverse)
library(rstanarm)
library(bayesplot)

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

# Datos reales simulados
imc_real <- rnorm(100, mean = 28, sd = 4)

# Modelo (supuesto): IMC ~ Normal(28, 4)
# Generamos datos simulados desde el modelo
imc_sim <- replicate(100, rnorm(100, mean = 28, sd = 4))

# Comparar visualmente el modelos con los datos reales
ppc_dens_overlay(y = imc_real, yrep = imc_sim)
# La línea recta representa a los datos reales, las líneas celestes
# representan los datos simulados por el modelo

# Modelo incorrecto
imc_sim_mal <- replicate(100, rnorm(100, mean = 24, sd = 2))

# Comparar visualmente el modelos con los datos reales. 
ppc_dens_overlay(y = imc_real, yrep = imc_sim_mal)
# Se observa claramente que los datos simulados por el modelo generan
# un gráfico muy diferente al gráfico generado por los datos reales

###########################
##
## Ejemplos con NHANES
##
###########################

############ Con aspirina ##############

# Supongamos que ya tienes los datos limpios
datos <- df_obesidad %>%
  select(BMXBMI, RXD530, RIDAGEYR, PAD680) %>%
  drop_na()

# Filtrar los pacientes con por imc, dosis habitual de aspirina, edad,
# y minutos diarios de actividad sedentaria
datos <- datos %>%
  filter(
    BMXBMI <= 100,
    RXD530 <= 500,
    RIDAGEYR <= 100,
    PAD680 < 2000
  )

# Crear modelo base
modelo <- stan_glm(
  BMXBMI ~ RXD530 + RIDAGEYR + PAD680,
  data = datos,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Generar datos simulados
yrep <- posterior_predict(modelo)

# Chequeo visual
ppc_dens_overlay(
  y = datos$BMXBMI,
  yrep = yrep[1:50, ]
)

# Alternativa con el histograma
ppc_hist(
  y = datos$BMXBMI,
  yrep = yrep[1:5, ]
)

################ Con insulina #####################

# Supongamos que ya tienes los datos limpios
datos <- df_obesidad %>%
  select(BMXBMI, LBXIN, RIDAGEYR, PAD680) %>%
  drop_na()

# Filtrar los pacientes con por imc, dosis habitual de aspirina, edad,
# y minutos diarios de actividad sedentaria
datos <- datos %>%
  filter(
    BMXBMI <= 100,
    LBXIN <= 100,
    RIDAGEYR <= 100,
    PAD680 < 2000
  )

# Crear modelo base
modelo <- stan_glm(
  BMXBMI ~ LBXIN + I(LBXIN^2) + RIDAGEYR + PAD680,
  data = datos,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Generar datos simulados
yrep <- posterior_predict(modelo)

# Chequeo visual
ppc_dens_overlay(
  y = datos$BMXBMI,
  yrep = yrep[1:50, ]
)

# Alternativa con el histograma
ppc_hist(
  y = datos$BMXBMI,
  yrep = yrep[1:5, ]
)

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

# Ejercicio 1 — Detectar el problema

# Toma tu modelo actual.

# 👉 Pregunta:
  
#  ¿La media está bien?
#  Sobreestima el promedio de imc
#  ¿La dispersión está bien?
#  Hay un poco más de dispersión en el modelo
#  ¿Las colas están bien?
#  Sobreestima los valores bajor de imc y subestima ligeramente
#  los valores bajos
  
#  👉 Escribe 3 frases clínicas:
  
#  “El modelo sobreestima el IMC promedio…”
#  "El modelo es más disperso con respecto a la realidad de imc"
#  "El modelo sobreestima los niveles más bajos de imc"

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — Comparación de modelos

#Construye dos modelos:

  
modelo1 <- stan_glm(
  BMXBMI ~ LBXIN + RIDAGEYR,
  data = datos,
  chains = 2, iter = 1000, refresh = 0
)

modelo2 <- stan_glm(
  BMXBMI ~ LBXIN + I(LBXIN^2) + RIDAGEYR,
  data = datos,
  chains = 2, iter = 1000, refresh = 0
)

# 👉 Genera yrep para ambos

yrep1 <- posterior_predict(modelo1)
yrep2 <- posterior_predict(modelo2)

# 👉 Haz PPC
ppc_dens_overlay(
  y = datos$BMXBMI,
  yrep = yrep1[1:50, ]
)

ppc_dens_overlay(
  y = datos$BMXBMI,
  yrep = yrep2[1:50, ]
)

#Pregunta:
  
#  👉 ¿Cuál reproduce mejor:
  
#  la media? El segundo modelo sobreestima un poco menos la media
#  la forma? Creo que los dos tienen una forma similar (pero no 
# corrige el problema de las colas)



#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Intuición sobre distribución

# Sin cambiar el modelo:
  
#  👉 observa el histograma real de IMC

ggplot(datos, aes(x = BMXBMI)) +
  geom_histogram(bins = 30)

# Pregunta:
  
#  👉 ¿Es simétrico? No completamente

#  👉 ¿Tiene cola derecha? Sí

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — Pensamiento clínico

# Responde:
  
#  👉 ¿Por qué el IMC NO debería ser perfectamente normal 
#  en la vida real?
  
#  Pista:
  
#  obesidad
#  heterogeneidad
#  estilos de vida

# Respuesta: Porque es mucho más fácil variar hacia arriba que hacia
# abajo. Es muy difícil seguir vivo por debajo de ciertos grados de
# desnutrición o de diabetes tipo 1 no controlada o cáncer. Pero
# en cambio las superobesidades sí son compatibles con la vida.

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Nivel clave

#Completa esta frase:
  
#  “Aunque el modelo no reproduce perfectamente la distribución,
# sigue siendo útil para ________,:
# - Entender tendencias generales: relación imc-inuslina, dirección
#   del efecto, orden de magnitud
# - Generar hipótiesis clínicas: parece que la insulina sí importa,
#   la actividad sola no explica mucho.
# - Aprender del sistema

# pero no para ________.”:
# - Predicción individual precisa: este paciente tendrá imc = 32.1
# - Decisiones clínicas finas: ajustar tratamiento basado en ese modelo
# - Interpretar extremos: porque el modelo falla en colas 

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6 — Insight profundo

#Responde:
  
#  👉 ¿Qué te enseñó el PPC sobre el modelo que no podrías ver 
#     solo con coeficientes? Me enseñó cómo varía el modelo de los
#     datos reales en la media, la dispersión (altura de la 
#     distribución) y las colas (valores extremos). Esto no veo
#     sólo observando los intervalos posteriores

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7 — ¿Dónde falla más el modelo?
  
#  Ya viste que el modelo no encaja perfecto.

# 👉 Ahora sé más específico:
  
#  Divide mentalmente la distribución en 3 zonas:
  
#  baja (IMC < 25)
#  media (25–30)
#  alta (> 30)

# Pregunta:
  
#  👉 ¿En cuál zona el modelo falla más?
# El modelo falla subestimando el sobrepeso, falla 
# sobreestimando a los desnutridos, falla sobrestimando a los
# obesos no mórbidos. Pero acierta bastante bien a los obesos 
# mórbidos.

#  👉 ¿Qué implicación clínica tendría eso?
# No se debería usar el modelo para predecir con exactitud el 
# grado de obesidad a partir de insulina
#  (Pista: no es lo mismo fallar en obesidad que en normopeso)

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8 — Chequeo de extremos

# Sin cambiar el modelo:
  
#  👉 compara visualmente o con quantiles:
  
#  mínimo real vs mínimo simulado
#  máximo real vs máximo simulado
#  Pregunta:
  
#  👉 ¿El modelo genera pacientes “imposibles”?
  
#  Ejemplo:
  
#  IMC demasiado bajo (< 15)
#  IMC extremadamente alto sin control

# Respuesta: Sí , hacia la izaquierda hay pacientes con imc menor a
# 15 (en el modelo) lo que es un poco absurdo, la cola derecha del
# modelo es bastante precisa.

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9 — Sensibilidad a variables

# Construye este modelo:

modelo_simple <- stan_glm(
  BMXBMI ~ RIDAGEYR,
  data = datos,
  chains = 2, iter = 1000, refresh = 0
)

# 👉 haz PPC

yrep1 <- posterior_predict(modelo_simple)

# 👉 Haz PPC
ppc_dens_overlay(
  y = datos$BMXBMI,
  yrep = yrep1[1:50, ]
)

# Pregunta:
  
#  👉 ¿Es peor, igual o mejor que el modelo con insulina?
# Peor que el modelo con insulina
  
#   Objetivo

# Entender:
  
#  👉 cuánto aporta realmente la insulina al modelo
#  La media del modelo con edad se eleja incluso más que la media
# que el modelo con insulina y parece que la cola izquierda se desvía 
# un poco más hacia valores excesivamente bajos de imc. La insulina
# si ayuda a estabilizar la media del modelo.

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10 — Modelo absurdo (muy importante)

#Construye un modelo deliberadamente malo:
  
modelo_absurdo <- stan_glm(
  BMXBMI ~ 1,
  data = datos,
  chains = 2, iter = 1000, refresh = 0
)
  
#  👉 PPC

yrep1 <- posterior_predict(modelo_absurdo)

ppc_dens_overlay(
  y = datos$BMXBMI,
  yrep = yrep1[1:50, ]
)

# Pregunta:
  
#  👉 ¿Qué patrones desaparecen completamente? no es muy distinto
# al de la edad. El modelo de insulina es más parecido, al menos,
# en la media. Cola y altura del modelo absurdo son parecidas al
# modelo de edad e  insulina
  
#   Insight esperado

# 👉 entender qué captura el modelo… y qué no

#####################
##
## Ejercicio 11
##
#####################

# Ejercicio 11 — Comparación mental

#Sin correr código nuevo:
  
#  Ordena estos modelos de mejor a peor (según PPC):
  
#1. IMC ~ 1
#2. IMC ~ edad 
#3. IMC ~ insulina + edad 
#4. IMC ~ insulina + insulina² + edad 

#Pregunta:
  
#👉 ¿Por qué ese orden?

# Respuesta: 4, 3, 2, 1: Los 4 son muy parecidos pero el modelo
# con insulina se acerca un poco más a la media mientras que el
# modelo con edad y el absurdo se alejan un poco más de la media.

#####################
##
## Ejercicio 12
##
#####################

#Ejercicio 12 — Forma de la relación

#Haz un gráfico:
  
ggplot(datos, aes(x = LBXIN, y = BMXBMI)) +
  geom_point(alpha = 0.3) + 
  geom_smooth()

#Pregunta:
  
#  👉 ¿La relación parece lineal? Sí, pero no perfecta, es una curva
  
#  👉 ¿Dónde falla la linealidad? En los valores de insulina e imc
#      demasiado altos, donde ya no hay tantos pacientes.

#####################
##
## Ejercicio 13
##
#####################

# Ejercicio 13 — Pensamiento causal

# Responde:
  
#  👉 ¿Tiene sentido clínico modelar:
  
#  IMC ~ insulina

#o

# insulina ~ IMC

# Objetivo

# Diferenciar:
  
# predicción
# vs
# causalidad

# Desde el punto de vista fisiopatólógico, la hormona encargada
# de conservar la grasa corporal es la insulina. Entonces la 
# insulina es la que aumenta el imc. Sin embargo por esta relación,
# el imc, ocasionalmente, podría predecir el nivel de insulina.

#####################
##
## Ejercicio 14
##
#####################

# Ejercicio 14 — Interpretación crítica

# Completa:
  
# “El modelo reproduce razonablemente ________,
# La media de la población

# pero falla un poco en ________,
# que la población está más dispersa, sobre todo hacia la izquierda,
# hacia los valores bajos de imc.

# lo que implica que ________.” 
# se está subestimando la cantidad de valores promedio en favor
# de valores de imc demasiado bajos


#####################
##
## Ejercicio 15
##
#####################

# Ejercicio 15 — Umbral de aceptación

# Define tu criterio personal:
  
#  👉 ¿Cuándo considerarías que un modelo es “suficientemente bueno”?
  
#  Ejemplo:
  
# diferencia de media < X
# colas razonables
# forma aproximada

# Este ejercicio es clave

# 👉 estás definiendo tu estándar como analista

# Respuesta: Creo que una forma aproximada es importante, sin embargo
# valores razonables también deberían ser una condición importante
# para aceptar un modelo. Valores de imc menores a 15, cercanos
# a 10 son un poco inverosímiles.

#####################
##
## Ejercicio 16
##
#####################

# Ejercicio 16 — Traducción clínica real

# Imagina que presentas esto a un colega médico.

# 👉 Explica en lenguaje clínico:
  
# qué hace el modelo
# qué hace bien
# qué hace mal

# Respuesta: El modelo representa la distribución de imc tomando
# en la insulina. La forma y la media de la distribución se acercan
# un poco a los datos reales, pero el modelo está más disperso hacia
# la cola izquierda, hacia los datos más bajos de imc.

#####################
##
## Ejercicio 17
##
#####################

# Ejercicio 17 — Pregunta incómoda

# Responde con honestidad:
  
#  👉 ¿Te sentirías cómodo tomando decisiones clínicas con este modelo?
  
# sí
# no
# depende

# Y explica por qué

# Respuesta: Depende, lo probaría con pacientes reales y observaría
# si los resultados del modelo varían demasiado cuando los comparo 
# con pacientes reales. Es decir probaría si el modelo es útil 
# clínicamente y tiene  sentido con lo que ya se sabe. Sí le daría
# una oportunidad al modelo porque no es completamente diferente
# a la distribución de los datos reales, sólo está más dispersa
# hacia los pacientes con imc más bajos

#####################
##
## Ejercicio 18
##
#####################

# Ejercicio 18 — Insight final del capítulo

# Completa:
  
# “El posterior predictive check me permitió ver que ________,
# El modelo está un poco disperso hacia los pacientes con imc más
# bajo
# algo que no habría detectado mirando solo ________.”
# El intervalo posterior.
library(dplyr)
library(tidyverse)
library(rstanarm)
library(bayesplot)
options(scipen = 999) # evitar la notación científica

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

# Simular actividad física y glicemia
datos <- tibble(
  actividad = rnorm(120, 3, 1),
  glicemia = 95 - 4 * actividad + rnorm(120, 0, 8)
)

# Modelar el efecto de la actividad física en la glicemia
modelo <- stan_glm(
  glicemia ~ actividad,
  data = datos,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Extraer la distribución posterior
posterior <- as.matrix(modelo)
# Ver la probabilidad de que mayor actividad física reduzca 
# la glicemia.
mean(posterior[, "actividad"] < 0) # produce: 1

# Visualizar la incertidumbre
mcmc_areas(
  posterior,
  pars = "actividad"
)

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar las variables que voy a usar
nhanes_modelo <- df_obesidad %>%
  select(LBXSGL, # glicemia 
         BMXBMI,# índice de masa corporal
         BMXWAIST, # cintura
         PAD680, # minutos al día de actividad sedentaria
         LBXIN # insulina
  ) %>%
  drop_na() %>%
  filter(
    LBXSGL <= 600,
    BMXBMI < 100,
    BMXWAIST < 200,
    PAD680 < 1000,
    LBXIN < 70
  )

# Modelar el efecto de imc, sedentarismo e insulina sobre la glicemia
modelo_nhanes <- stan_glm(
  LBXSGL ~ BMXBMI + PAD680 + LBXIN,
  data = nhanes_modelo,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Extraer la distribución posterior
posterior <- as.matrix(modelo_nhanes)

# Ver la probabilidad de que imc se asocie con mayor glicemia:
mean(posterior[, "BMXBMI"] > 0) # produce: 1
# Interprestación: Existe una alta probabilidad de que un mayor
# imc se asocie con mayor glicemia

# Ver la probabilidad de que el sedentarismo se asocie con mayor
# glicemia
mean(posterior[, "PAD680"] > 0) # produce: 0,73
# Interpretación: Existe un 73% de probabilidad de que el sedentarismo
# se relacione con mayor glicemia

# Ver la probabilidad de que la insulina se asocie con mayor glicemia
mean(posterior[, "LBXIN"] > 0) # produce: 1
# Interpretación: Existe una alta probabilidad de que la insulina
# se relaciones con mayor glicemia


#####################
##
## Ejercicios
##
#####################

#####################
##
## Ejercicio 1
##
#####################

# Ejercicio 1 — Certeza vs decisión

# Imagina un paciente con:
  
#  IMC = 34
# glicemia = 108
# actividad física baja
# insulina elevada

# Un modelo bayesiano sugiere:
  
#  mean(posterior[, "LBXIN"] > 0)

# Resultado:
  
#  0.91
# Preguntas
# ¿Qué significa clínicamente ese 0.91? Significa que hay una 
#  probabilidad de 91% de que la insulina se asocie con más glicemia
#  ¿Eso implica certeza absoluta? No, porque hay un 9% de probabilidad
#   de lo contrario
#  ¿Crees que ya existe suficiente evidencia para intervenir? Sí
#  ¿Qué riesgos existen si decides esperar? Que la glicemia empeore

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — No decidir también es decidir

# Un médico decide:
  
#  “Todavía no voy a intervenir porque el paciente aún no tiene 
#   diabetes.”

# Preguntas
# ¿Por qué esa decisión también implica riesgo? Porque hay un 91%
#  de probabilidad de que si sigue aumentando el imc, también
#  aumentará la glicemia
#  ¿Qué procesos fisiopatológicos podrían seguir avanzando 
#   silenciosamente? Resistencia a la insulina y obesidad abdominal
#  ¿Qué consecuencias podría tener intervenir demasiado tarde?
#   El problema es que un problema hormonal que dura más tiempo es 
#   más difícil de corregir

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Umbrales personales

# Dos médicos interpretan el mismo resultado:
  
#  mean(posterior[, "BMXBMI"] > 0)

# Resultado:
  
#  0.78

# Médico A

# “Eso es suficiente para actuar.”

# Médico B

# “Todavía prefiero observar.”

# Preguntas
# ¿Cuál de los dos está “correcto”? Depende del tipo de intervención
#  ¿Por qué los umbrales clínicos pueden variar? Porque para las 
#   distintas intervenciones hay un umbral. Y cada intervención
#   depende también de las características de un paciente
#  ¿Qué factores podrían influir en esa diferencia? Factores 
#   económicos, sociales, idiosincrasia, etc. Tipo de intervención 
#  nutricional, farmacológica, quirúrgica, etc.

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — Traducir probabilidades a lenguaje clínico

# Interpreta clínicamente cada uno de estos resultados:
  
#  a)
# mean(posterior[, "PAD680"] > 0)

# Resultado:
  
#  0.62

# b)
# mean(posterior[, "LBXIN"] > 0)

# Resultado:
  
#   0.97

# c)

# mean(posterior[, "BMXBMI"] > 0)

# Resultado:
  
#  0.51

# Preguntas

# Para cada caso responde:
  
#  ¿Qué tan fuerte es la evidencia? Para el sedentarismo y el imc
#   hay poca evidencia, la insulina tiene mejor evidencia
#  ¿Intervendrías clínicamente? Sí
#  ¿Preferirías observar más datos? No
#  ¿Qué tipo de intervención tendría más sentido? nutricional y
#   moverse más (no ejercicio)

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Comparando riesgos

# Supón dos posibles intervenciones:
  
# Intervención	             Beneficio esperado	     Riesgo
# Cambios de estilo de vida	 Moderado-alto	         Muy bajo
# Medicación intensiva	     Alto	                   Moderado

# El modelo muestra:
  
#  0.74

# de probabilidad de empeoramiento metabólico.

# Preguntas
#  ¿Qué intervención parece más razonable inicialmente?
#   El de menor riesgo
#  ¿Por qué el riesgo de la intervención importa tanto?
#   Porque trato de causar el menor daño posible al paciente
#  ¿Necesitamos el mismo nivel de certeza para todas las 
#  intervenciones? No, depende del contexto del paciente

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6 — Caso clínico integrado

# Paciente:
  
#  Variable	            Resultado
#  IMC	                37
#  Glicemia	            115
#  Actividad física	    Muy baja
#  Insulina	            Elevada

# Resultados del modelo:
  
#  mean(posterior[, "BMXBMI"] > 0)
# 0.95

# mean(posterior[, "PAD680"] > 0)
# 0.88

# mean(posterior[, "LBXIN"] > 0)
# 0.98

# Preguntas
#  ¿Qué patrón fisiopatológico observas? Resistencia a la insulina
#  ¿Qué te preocupa más clínicamente? Glicemia
#  ¿Intervendrías ahora o esperarías? Hoy
#  ¿Qué tipo de intervención priorizarías? Nutrición
#  ¿Cómo explicarías la incertidumbre al paciente? Hay una alta 
#   probabilidad de que bajar de peso disminuya la glicemia


#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7 — Comunicación clínica

# Imagina que debes explicarle al paciente este resultado:
  
#  0.89

# de probabilidad de que su baja actividad física esté empeorando su 
# control glucémico.

# Tu objetivo

# Explicarlo:
  
#  sin lenguaje estadístico complejo
#  sin generar miedo innecesario
#  siendo honesto sobre la incertidumbre

# Existe una alta probabilidad de que el sedentarismo esté aumentando
# su glucosa

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8 — La ilusión de certeza

# Reflexiona sobre esta frase:
  
#  “La medicina moderna muchas veces actúa como si necesitara 
#    certeza absoluta para prevenir enfermedades.”

# Preguntas
# ¿Estás de acuerdo? No pues, si vas a prevenir es que no tienes
#  certeza de la enfermedad
# ¿Qué problemas puede generar esperar demasiada certeza?
#  que un cuadro fácil de manejar empeore.
# ¿Cómo ayuda el pensamiento bayesiano a prevenir antes? Ayudándome
#  a observar los niveles de certeza e incertidumbre

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9 — Obesidad y acumulación de riesgo

# Explica con tus propias palabras:
  
#  👉 ¿Por qué la obesidad puede entenderse como un proceso de 
#      acumulación probabilística de riesgo?
  
#  Intenta relacionar:
  
#  inflamación
#  hiperinsulinemia
#  grasa visceral
#  glicemia
#  progresión lenta

# Porque la obesidad está relacionada con muchas enfermedades. Esto
# pasa porque el problema central de la obesidad es la resistencia a la
# insulina donde hay mucha glucosa y mucha insulina. El exceso 
# de inuslina causa acumulación de grasa sobre todo abdomiinal, la
# que es muy inflamatoria. Sin embargo esta resistencia a la insulina
# no suele darse en cortos periódos de tiempo sino a largo plazo

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10 — Tu filosofía clínica

# Imagina que eres el médico responsable de una clínica metabólica.

# Preguntas

#  ¿Qué nivel de probabilidad te haría intervenir temprano? Depende
#   de cada paciente
#  ¿Prefieres prevenir agresivamente o esperar más evidencia?
#   Esperar evidencia
#  ¿Qué riesgos tiene cada enfoque? Hay métodos poco invasivos
#   que se pueden usar con evidencia limitada
#  ¿Cómo equilibrarías:
#    beneficio
#    costo
#    adherencia
#    incertidumbre?
#  Creo que ese es el problema que se toman decisiones nutricionales
#  con poca evidencia. 

#####################
##
## Ejercicio 11
##
#####################

# Ejercicio 11 — Interpretación rápida

# ¿Qué significa este código?
  
#  mean(posterior[, "BMXBMI"] > 0)

# Explícalo:
  
#  estadísticamente
#  Conocer la probabilidad de que el intervalo posterior del modelo
#  del efecto del imc sobre la glicemia sea positivo

#  clínicamente
#  Conocer si el imc se asocia con un aumento de la glicemia

#####################
##
## Ejercicio 12
##
#####################

#Ejercicio 12 — Cambiando el umbral

#Supón:
  
#  mean(posterior[, "PAD680"] < 0)
# 0.72

#Completa la tabla:
  
#         Umbral personal	        Acción
#         60%	                    ?
#         75%	                    ?
#         90%	                    ?
  
#####################
##
## Ejercicio 13
##
#####################

# Ejercicio 13 — Distribuciones, no números únicos

# Explica por qué esta idea es tan importante:
  
# “El objetivo no es encontrar el valor verdadero exacto, 
#  sino un rango plausible de efectos.”

# Porque un paciente no es un promedio, uno no puede predecir a
# ciencia cierta los valores de un examen o incluso el resultado 
# de una intervención pero predecir un rango plausible me
# ayuda a ubicarme mentalmente en el estado de un paciente y con eso
# tomar un decisión

#####################
##
## Ejercicio 14
##
#####################

# Ejercicio 14 — Pensamiento bayesiano completo

# Escribe un pequeño análisis clínico (1–2 páginas) para un 
#  paciente con:
  
#  obesidad
#  glicemia alterada
#  sedentarismo
#  hiperinsulinemia

# Tu análisis debe incluir:
  
#  1. Interpretación fisiopatológica

# Relaciona:
  
#  resistencia a la insulina
#  inflamación
#  riesgo cardiometabólico

# 2. Interpretación probabilística

# Explica:
  
#  por qué no necesitas certeza absoluta
#  cómo usarías probabilidades para actuar

# 3. Plan de decisión clínica

# Discute:
  
#  cuándo intervenir
#  qué intervención elegir
#  riesgos y beneficios
#  incertidumbre residual
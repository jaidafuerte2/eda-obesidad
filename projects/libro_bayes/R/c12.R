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

# 200 pacientes
n <- 200

# Simular la edad, la dosis de aspirina y el imc de 200 pacientes
datos_sim <- tibble(
  edad = rnorm(n, 50, 10),
  aspirina = 0.05 * edad + rnorm(n, 0, 1),
  imc = 20 + 0.3 * edad + rnorm(n, 0, 2)
)

# Crear un modelos simple ("incorrecto")
modelo_simple <- stan_glm(
  imc ~ aspirina,
  data = datos_sim,
  chains = 2,
  iter = 1000,
  refresh = 0
)
#print(modelo_simple) # produce:
#            Median MAD_SD
#(Intercept) 32.5    0.5  
#aspirina     1.1    0.2  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 3.5    0.2   

posterior_interval(modelo_simple) # produce:
#                    5%       95%
#(Intercept) 31.5552435 33.358330
#aspirina     0.7905714  1.473874
#sigma        3.2061448  3.765340

# Modelo ajustado
modelo_ajustado <- stan_glm(
  imc ~ aspirina + edad,
  data = datos_sim,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_ajustado) # produce:
#          Median MAD_SD
#(Intercept) 20.3    0.8  
#aspirina     0.1    0.2  
#edad         0.3    0.0  
#
#Auxiliary parameter(s):
#   Median MAD_SD
#sigma 2.2    0.1   

# Interpretación: la mediana de imc es 20.3 cuando apirina y edad son
# iguales a cero; aumentar en 1 unidad la aspirina cambia el imc en
# 0.1 pero con una incertidumbre de 0.2 (mayor al cambio de imc) por
# lo que el efecto de la aspirina es débil e incierto; cada año 
# adicional aumenta el imc 0.3 puntos con una incertidumbre de cero,
# entonces la edad es un predictor fuerte y claro y el modelo está
# muy seguro de esto.

posterior_interval(modelo_ajustado) # produce:
#                    5%        95%
#(Intercept) 19.0038601 21.5695667
#aspirina    -0.1615683  0.3029590
#edad         0.2697486  0.3231022
#sigma        2.0145175  2.3864663

# Interprestación: el efecto de la aspirina (de cada unidad) sobre el 
# imc varía entre
# -0.16 y 0.3 puntos de imc , es decir que incluso puede ser que no 
# tenga un efecto; el efecto de la edad (de cada unidad o año) sobre 
# el imc es entre 0.27
# y 0.32 o que tiene un efecto positivo, consistente y preciso; La 
# variación del promedio de  imc es entre 2.01 y 2.38. En resumen ,
# despues de considerar la edad, no hay evidencia de que la aspirina
# cambie el imc, en cambio la edad sí tiene  un efecto consistente.

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar sólo las variables que voy a usar
datos <- df_obesidad %>%
  select(BMXBMI, RXD530, RIDAGEYR, PAD680, RIAGENDR) %>%
  drop_na()
datos <- datos %>%
  filter(
    RXD530 <= 2000,
    BMXBMI <= 100,
    RIDAGEYR <= 100, 
    PAD680 <= 2000
  )

# Crear el modelo simple
modelo_simple <- stan_glm(
  BMXBMI ~ RXD530,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_simple) # produce:
#          Median MAD_SD
#(Intercept) 30.0    0.2  
#RXD530       0.0    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 6.8    0.2   
posterior_interval(modelo_simple)
#                      5%          95%
#(Intercept) 29.653977466 30.771530045
#RXD530      -0.006031818  0.001966065
#sigma        6.547515870  7.032060519

# Modelo ajustado
modelo_ajustado <- stan_glm(
  BMXBMI ~ RXD530 + RIDAGEYR + PAD680,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_ajustado) # produce:
#            Median MAD_SD
#(Intercept) 35.3    1.2  
#RXD530       0.0    0.0  
#RIDAGEYR    -0.1    0.0  
#PAD680       0.0    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 6.5    0.1  
#
# Interprtación: El modelo cree que el efecto de la aspirina sobre  el
# imc es cero y está muy seguro de eso.
# Sin embargo por cada año adicional, el imc disminuye ligeraramente
# -0.1. 
# Adicionalmente los minutos de actividad sedentaria no se relacionan 
# mucho con el peso
# La mediana de imc tiene una variabilidad de 6.5 en los pacientes 

posterior_interval(modelo_ajustado)
#                      5%          95%
#(Intercept) 33.141881636 37.347304756
#RXD530      -0.006577923  0.001133576
#RIDAGEYR    -0.151810431 -0.095624651
#PAD680       0.005241313  0.008665878
#sigma        6.258139654  6.760844131
# 
# Interpretación: Cada gramo de aspirina puede causar una disminución 
# ligera de imc o cero o un aumento ligero, es muy probable que la
# aspirina tenga un efecto irrelevante sobre el imc. 
# El modelo está bastante seguro de que a mayor edad, menor imc;
# es decir cada año una persona puede perder entre 0.15 y 0.09 
# puntos de imc
# Cada minuto de sedentarismo aumenta el peso entre 0.005 y 0.008
# puntos de imc. Parece irrelevante pero qué pasa en 100 minutos?
# Pues hay un aumento de medio punto o casi un punto de imc. Muy 
# interesante por eso se aconseja tratar de mantenerse activo 
# aunque no necesariamente ejercicio.
#
# En resumen:
#
# Después de ajustar por edad y sedentarismo, la aspirina no muestra
# relación con el IMC.
# En cambio, el sedentarismo sí se asocia consistentemente con 
# mayor IMC, mientras que la edad muestra una relación inversa 
# que probablemente refleja cambios fisiológicos no lineales como 
# la pérdida de masa muscular en edades avanzadas.

ggplot(datos, aes(x = RXD530 , y = BMXBMI )) +
  geom_point() +
  geom_smooth()

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

# Ejercicio 1 — Detectando confusión
# 🎯 Objetivo

# Ver cómo cambia una relación al ajustar.

# 🔧 Instrucciones
# 1. Ajusta un modelo simple:
my_model <- stan_glm(BMXBMI ~ RXD530, 
         data = datos,
         chains = 2,
         iter = 1000,
         refresh = 0) 
my_model # produce:
#            Median MAD_SD
#(Intercept) 30.2    0.4  
#RXD530       0.0    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 6.7    0.2 
posterior_interval(my_model) # produce:
#                      5%          95%
#(Intercept) 29.670880490 30.779812156
#RXD530      -0.006455201  0.001167056
#sigma        6.469398616  6.979437610

# 2. Ajusta el modelo completo:
my_model <- stan_glm(
  BMXBMI ~ RXD530 + RIDAGEYR + PAD680, 
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0) 
my_model # produce: 
#            Median MAD_SD
#(Intercept) 35.2    1.4  
#RXD530       0.0    0.0  
#RIDAGEYR    -0.1    0.0  
#PAD680       0.0    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 6.5    0.1   
posterior_interval(my_model) # produce:
#                      5%          95%
#(Intercept) 33.169809208 37.426479119
#RXD530      -0.006678998  0.001169306
#RIDAGEYR    -0.154763813 -0.093418063
#PAD680       0.005253648  0.008488101
#sigma        6.265649536  6.715645420

# ❓ Preguntas
#  ¿Cómo cambia el intervalo de RXD530? Cambia poco, casi nada
#  ¿Se acerca más a 0? No, se aleja de cero
#  ¿Se vuelve más estrecho o más ancho? Más ancho

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — Interpretación clínica profunda
# 🎯 Objetivo

#Traducir coeficientes a lenguaje médico

# 🔧 Instrucciones

# Usa el modelo ajustado y responde:
  
#❓ Preguntas
#¿Qué significa el coeficiente de PAD680 en términos clínicos?
# Significa que cada minunto de actividad sedentaria aumenta el
# imc entre 0.0052 y 0.0084
#  👉 Tradúcelo a:
  
#  +30 minutos sedentarios
#  Cada 30 minutos de actividad sedentaria aumenta entre 0.156 
#  y 0.252 puntos de imc
#  +120 minutos sedentarios
#  Cada 30 minutos de actividad sedentaria aumenta entre 0.624 
#  y 1.008 puntos de imc
#  ¿El efecto es clínicamente relevante o pequeño? Es pequeño
#  pero hay que tomar en cuenta.
#  ¿Coincide con tu experiencia clínica? Con la indicación que suelo
#  dar

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Escala e interpretación
# 🎯 Objetivo

# Entender el impacto de las unidades

# 🔧 Instrucciones

# Calcula mentalmente:
  
#  Si PAD680 tiene un coeficiente ≈ 0.006

# ❓ Preguntas
# ¿Qué pasa con el IMC si una persona pasa:
#  1 hora más sedentaria 
# Con 60 minutos de actividad sedentaria el imc aumenta 0.360 puntos 
#  3 horas más sedentaria
# Con 180 minutos de actividad sedentaria el imc aumenta 1.1 puntos

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — ¿Qué falta en el modelo?
# 🎯 Objetivo

# Pensar como clínico

# ❓ Pregunta abierta

# Tu modelo actual tiene:
  
#  aspirina
#  edad
#  sedentarismo

# 👉 ¿Qué variables importantes faltan para explicar IMC?
  
#   Escribe al menos 5.

# 💡 Pistas
# nutrición
# metabolismo
# comportamiento

# Respuesta
# nutrición : carbos, grasas, proteínas, calorías, fibra,
# metabolismo: glucosa, insulina, resistencia a la insulina
# conportamiento: Actividad física

# 👉 ¿Cuál de esas variables podría estar confundiendo la relación entre 
# sedentarismo e IMC? A mi parecer, todas son más importantes
# excepto actividad física.

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Cambiando la historia del modelo
# 🎯 Objetivo

# Entender que el modelo cuenta historias distintas

# 🔧 Instrucciones

# Compara:
  
#  Modelo simple
#  Modelo ajustado
# ❓ Preguntas
# ¿Qué historia cuenta cada uno? El modelo simple cuenta que
# no hay una relación débil entre aspirina e imc; adicionalmente
# el modelo completo nos cuenta que hay una relación moderada entre
# menor edad e imc y una relación fuerte entre sedentarismo e imc
# ¿Cuál se parece más a la realidad clínica? La segunda
# ¿Cuál sería peligrosa si la usaras para tomar decisiones?
# Sólo me parece interesante la relación entre sedentarismo e imc

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6 — Pensamiento causal (clave)
# 🎯 Objetivo

# Ir más allá de correlaciones

# ❓ Pregunta

# Responde con palabras, no código:
  
#  👉 ¿La actividad física afecta el IMC directamente…
#  o a través de otros mecanismos?
  
# 💡 Ejemplos
# apetito
# insulina
# masa muscular
# Reflexión profunda

# La actividad física es "un tema" porque tiene relación con el
# gasto de calorías, sin embargo no sólo gastamos calorías cuando
# hacemos ejercicio sino también en condiciones de reposo por el
# metabolismo basal. El trabajo muscular debería reducir la glicemia
#  y esto mejorar la sensibilidad a la insulina, pero el efecto 
# real no se conoce bien, sería bueno medirlo. El problema grave
# es que el ejercicio puede aumentar el apetito y eso se podría
# ver reflejado en el imc comparado con el ejercicio intenso y
# moderado.

# 👉 ¿Tu modelo está capturando eso… o lo está simplificando 
# demasiado? Simplifica demasiado el tema del ejercicio.

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7 — Detectando no linealidad
# 🎯 Objetivo

# Conectar gráficos con modelos

# 🔧 Instrucciones

# Haz un gráfico:
ggplot(datos, aes(x = RIDAGEYR, y = BMXBMI)) +
  geom_point(alpha = 0.2) +
  geom_smooth()

# ❓ Preguntas
# ¿La relación parece lineal? Sí
# ¿Ves un “arco”? Sí
# ¿El modelo lineal está capturando bien esa forma? No, la edad
# aumenta un poco el imc hasta los 55 años, se estabiliza y empieza
# a caer desde los 60 años. Es interesante esta relación. El modelo
# me hace pensar que la tendencia es una línea inclinada, que va cayendo
# porque los dos valores del intervalo posterior son negativos, pero
# al ver la tendencia veo más bien un arco

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8 — Interpretación bayesiana pura
# 🎯 Objetivo

# Hablar como bayesiano

# ❓ Pregunta

# Completa esta frase con tus palabras:
  
#  “Dado los datos y el modelo, es plausible que…”

# Para:
  
# aspirina: Dado los datos y el modelo, es plausible que la aspirina
# no tenga ninguna relación con el imc
# edad: Dado los datos y el modelo, es plausible que la edad tenga
# una relación moderada-fuerte con la disminución de peso
# sedentarismo: Dado los datos y el modelo, es plausible que el 
# sedentarismo tenga una relación fuerte con el aumento del imc

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9 — La lección del capítulo

# Responde:
  
#  👉 ¿Por qué un modelo simple puede engañarte? Porque las 
# enfermedades son múltifactoriales y por eso pueden haber factores
# de confusión
  
#  👉 ¿Qué cambió en tu forma de interpretar los datos después de 
#  este capítulo? Una variable puede estar influenciada por otras

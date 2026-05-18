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

# Simular el imc y la glicemia de 80 pacientes
datos <- tibble(
  imc = rnorm(80, mean = 30, sd = 5)
) %>%
  mutate(
    glicemia = 70 + 0.9 * imc + rnorm(80, 0, 8)
  )

# Diagrama de dispersión y tendencia que relaciona el imc con la 
# glicemia 
ggplot(datos, aes(imc, glicemia)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "IMC y glicemia simulada",
    x = "IMC",
    y = "Glicemia"
  )

# Modelar el efecto de el imc a la glicemia
modelo <- stan_glm(
  glicemia ~ imc,
  data = datos,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)
# Imprimir resultado del modelo
print(modelo, digits = 2) # produce:
#(Intercept) 63.11   5.56 
#imc          1.11   0.18 
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 7.61   0.61  
# Interpretación: Significa que por cada aumento de 1 unidad de IMC,
# la glicemia aumenta alrededor de 0.92 mg/dL 

# Imprimir intervalo posterior
posterior_interval(modelo, prob = 0.95) # produce:
#                  2.5%     97.5%
#(Intercept) 52.6631751 75.326697
#imc          0.7191751  1.461611
#sigma        6.5422657  9.065549
# Interpretación: Hay un 95% de probabilidad de que el verdadero 
# efecto del IMC sobre la glicemia esté entre 0.71 y 1.46 mg/dL.

# Conocer la probabilidad de que el efecto sea positivo
posterior <- as.matrix(modelo)
mean(posterior[, "imc"] > 0) # produce: [1] 1
# Interpretación: Existe una probabilidad extremadamente alta de que 
# un mayor IMC se asocie con mayor glicemia.
# Otra interpretación: Existe aproximadamente un 100% de probabilidad 
# de que el efecto del IMC sobre la glicemia sea positivo. 

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
         PAD680 # minutos al día de actividad sedentaria
         ) %>%
  drop_na() %>%
  filter(
    LBXSGL <= 600,
    BMXBMI < 100,
    BMXWAIST < 200,
    PAD680 < 1000
  )

################## índice de masa corporal ######################

# Diagrama de dispersión y tendencia de la relación entre IMC y
# glicemia
ggplot(nhanes_modelo, aes(BMXBMI, LBXSGL)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "IMC y glicemia en NHANES",
    x = "IMC",
    y = "Glicemia"
  )

# Modelar el efecto de el índice de masa corporal sobre la glicemia
modelo_imc <- stan_glm(
  LBXSGL ~ BMXBMI,
  data = nhanes_modelo,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Imprimir el modelo modelo_imc
print(modelo_imc) # produce:
#            Median MAD_SD
#(Intercept) 74.2    2.1  
#BMXBMI       1.0    0.1  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 40.1    0.4  

posterior_interval(modelo_imc,) # produce:
#                    5%       95%
#(Intercept) 71.0195776 77.808027
#BMXBMI       0.9150901  1.147657
#sigma       39.5104199 40.728242

# Calcular la probabilidad de efecto positivo:
posterior <- as.matrix(modelo_imc)
mean(posterior[, "BMXBMI"] > 0) # produce: 1
# Interpretación: Existe una probabilidad extremadamente alta de que 
# un mayor IMC se asocie con mayor glicemia.

################ Cintura ##################

# Modelar el efecto de la cintura sobre la glicemia 
modelo_cintura <- stan_glm(
  LBXSGL ~ BMXWAIST,
  data = nhanes_modelo,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_cintura) # produce:
#                    5%        95%
#(Intercept) 40.8347499 51.3032983
#BMXWAIST     0.5344307  0.6400847
#sigma       38.6249741 39.7948358
# Interpretación: Existe una alta probabilidad de que mayor 
# circunferencia de cintura se asocie con mayor glicemia.
# Otra interpretación: Los valores más plausibles del efecto están 
# entre 0.45 y 0.71 mg/dL por cada cm adicional de cintura.

# Calcular la probabilidad de efecto positivo:
posterior <- as.matrix(modelo_cintura)
mean(posterior[, "BMXWAIST"] > 0) # produce: 1

############### Actividad sedentaria ##################

# Modelo para conocer cómo influye el sedentarismo sobre
# la glicemia
modelo_sedentarismo <- stan_glm(
  LBXSGL ~ PAD680,
  data = nhanes_modelo,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_sedentarismo) # produce:
#                        5%           95%
#(Intercept) 100.0766517393 104.203007769
#PAD680       -0.0007332123   0.008572547
#sigma        39.6230151808  40.999116079
# interpretación: como el intervalo incluye valores negativos, 
# Existe incertidumbre considerable sobre el efecto del sedentarismo 
# en este modelo.

# Calcular la probabilidad de efecto positivo:
posterior <- as.matrix(modelo_sedentarismo)
mean(posterior[, "PAD680"] > 0) # produce: [1] 0.925
# Interpretación: Existe aproximadamente un 92.5% de probabilidad 
# de que el efecto del sedentarismo sobre la glicemia sea positivo.

# Resumir los valores de sedentarismo
summary(df_obesidad$PAD680) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
# 0.0   240.0   480.0   455.6   540.0  9999.0       5

# Cuánto influyen 10 horas de actividad sedentaria en la glicemia?
#PAD680       -0.0007332123   0.008572547
#-0.0007332123 * 600 # 600 minutos es 10 horas
# produce: [1] -0.4399274
# 0.008572547 * 600
# produce: [1] 5.143528
# Es decir que cada 10 horas de actividad sedentaria afectan 
# a la glicemia entre -0.43 y 5.14 mg/dL


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

# Ejercicio 1 — Traducir notación científica

# Convierte mentalmente estos números:
  
# 3.5e+02 = 350
# 4.2e-03 = 0.0042
# -7.1e-04 = -0.00071
# 1.08e+01 = 10.8

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — Interpretación intuitiva

# Supón este resultado:
  
#  Parámetro	5%	95%
#   BMXBMI	0.40	1.20

# Responde:
  
#  ¿El efecto parece positivo o negativo? Positivo
#  ¿El intervalo incluye cero? No
#  ¿Qué dirías clínicamente? Existe una alta probabilidad de que mayor
#   índice de masa corporal se asocie con mayor glicemia
#  ¿Existe mucha o poca incertidumbre? Hay poca incertidumbre

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Pensar en rangos plausibles

# Supón este intervalo:
  
#  Parámetro	5%	95%
#  BMXWAIST	-0.05	0.60

# Pregunta:
  
#  ¿Podemos estar seguros de que el efecto es positivo? No
#  ¿Qué significa que el intervalo incluya valores negativos? 
#  Significa que más cintura incluso puede disminuir la glicemia
#  ¿Cómo interpretarías esto en lenguaje clínico?
#  Significa que existe incertidumbre considerable del efecto de
#  la cintura sobre la glicemia

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — Interpretar probabilidades directas

# Supón que obtienes:
  
#  mean(posterior[, "BMXBMI"] > 0)

# [1] 0.97

# Responde:
  
#  ¿Qué significa exactamente 0.97? Significa que el 97% de simulaciones
# posteriores tienen un efecto positivo
#  Escríbelo en lenguaje clínico. Hay una alta probabilidad de que 
#  el imc tenga un efecto positivo sobre la glicemia.
#  ¿Eso implica certeza absoluta? No certeza absoluta, sólo una alta
#  probabilidad.

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Comparar dos escenarios

# Escenario A
# Probabilidad efecto positivo = 0.99

# Intervalo:
  
#  0.80 a 1.20

# Escenario B
# Probabilidad efecto positivo = 0.60

# Intervalo:
  
#  -0.30 a 0.90

# Preguntas:
  
#  ¿Cuál tiene evidencia más fuerte? El escenario A
#  ¿Cuál tiene más incertidumbre? El escenario B
#  ¿Cuál usarías con más confianza clínicamente? El escenario A
#  ¿Cuál parece más compatible con “efecto pequeño o inexistente”?
#  El escenario B porque el intervalo incluye cero o efecto 
# inexistente.

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6 — Construir interpretación clínica

# Supón este resultado:
  
#  Parámetro	media	5%	95%
#     PAD680	0.004	-0.001	0.009

# Ahora escribe:
  
#  una interpretación estadística simple: Cada minuto de sedentarismo
#  se asocia con un aumento de 0.004 mg/dL de glucosa
#  una interpretación clínica: El sedentarismo tiene un efecto 
#  positivo sobre la glucosa
#  una interpretación bayesiana usando probabilidades: Existe un 90%
#  de probabilidades de que el sedentarismo se asocie con una 
#  variación de la glicemia de entre -0.001 y 0.009 mg/dL
#  una frase sobre incertidumbre: Existe incertidumbre considerable
#  sobre el efecto del sedentarismo en este modelo.

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7 — Magnitud vs probabilidad

# Supón dos modelos.

#Modelo A
#Parámetro	5%	95%
#efecto	0.001	0.003

#Probabilidad positiva:
  
#  0.99

#Modelo B
#Parámetro	5%	95%
#   efecto	2.0	8.0

#Probabilidad positiva:
  
#  0.90

#Preguntas:
  
#  ¿Cuál tiene mayor magnitud clínica? El modelo B
#  ¿Cuál tiene mayor certeza estadística? El modelo A
#  ¿Cuál podría ser más importante para pacientes? El modelo B
#   Porque el efecto parece más amplio
#  ¿Qué enseña esto sobre “probabilidad” vs “relevancia clínica”?
#   Que a veces la magnitud del efecto es más trascendente que
#   la certeza de una magnitud que puede ser marginal

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8 — Interpretar sigma

# Supón este output:
  
#  Parámetro	5%	95%
#      sigma	38	42

# Preguntas:
  
#  ¿Qué representa sigma? Representa la cantidad de glicemia que no
#   puede explicar el modelo
#  ¿Qué significa clínicamente un sigma alto? Significa que el modelo
#   solo explica una parte pequeña del efecto, todavía hay mucho que
#   el modelo no puede explicar
#  ¿Qué podría indicar sobre glicemia? Significa que todavía el modelo
#   no puede explicar una gran parte de la glicemia que las variables
#   imc, citura y sedentarismo que el modelo no puede explicar
#  ¿Un sigma grande invalida automáticamente el modelo? Claro que
#  no porque si el promedio que da el modelo es 70 y sigma es 40,
#  esto indica que el modelo sí puede explicar 30 puntos de esos 70
#  lo que no es poco.

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9 — Traducir para un médico

# Supón este resultado:
  
#  Parámetro	5%	95%
#  BMXWAIST	0.45	0.70

# Y:
  
#  Probabilidad efecto positivo = 0.999

# Ahora escribe la interpretación:
  
# 1. como estadístico: Existe un 90% de probabilidad de que cada cm
#    de cintura se asocie con entre 0.45 y 0.70 mg/dL de más glicemia
# 2. como médico clínico: Hay una alta probabilidad de que la cintura
#    afecte los niveles de glucosa
# 3. como si explicaras a un residente: Con una certeza del 90% 
#    se puede decir que por cada cm de cintura aumenta la glicemia
#    entre 0.45 y 70  mg/dL
# 4. como si lo escribieras en el libro: Hay auna alta probabilidad
#    de que el tamaño de la cintura de asocia a niveles mayores de
#    glicemia

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10 — La incertidumbre como información

# Supón este intervalo:
  
#  Parámetro	5%	95%
#   PAD680	-1.2	1.4

# Preguntas:
  
#  ¿Por qué este resultado NO significa “modelo inútil”? porque
#   Sería interesante interpretar por qué el sedentarismo podría
#   disminuir un poco la glicemia
#  ¿Qué podría estar causando tanta incertidumbre? Tal vez los más
#   sedentarios están tienen menos apetito
#  ¿Qué hipótesis clínicas podrías plantear? Es posible que el 
#   sedentarismo mantenga estables las hormonas del hambre y la 
#   saciedad: grelina, colecistoqinina, peptido yy y amilina
#  ¿Qué harías después como investigador? Creo que modelar el
#   sedentarismo por actividad física y calorías puede ser conveniente.

#####################
##
## Ejercicio 11
##
#####################

# Mini ejercicio conceptual (MUY IMPORTANTE)

# Completa la frase:
  
#  “En Bayes, el objetivo no es eliminar la incertidumbre, sino…”
#  ofrecer intervalos plausibles

#####################
##
## Ejercicio 12
##
#####################

#Ejercicio final integrador

#Usa uno de tus modelos reales de NHANES y responde:

# Imprimir el modelo
print(modelo_sedentarismo) # produce:
#           Median MAD_SD
#(Intercept) 102.2    1.2 
#PAD680        0.0    0.0 
#
#Auxiliary parameter(s):
#    Median MAD_SD
#sigma 40.3    0.4  

posterior_interval(modelo_sedentarismo) # produce:
#                        5%           95%
#(Intercept) 100.0766517393 104.203007769
#PAD680       -0.0007332123   0.008572547
#sigma        39.6230151808  40.999116079

# Calcular la probabilidad de efecto positivo:
posterior <- as.matrix(modelo_sedentarismo)
mean(posterior[, "PAD680"] > 0) # produce: 0.925

# 1.   ¿Cuál es el efecto medio? 
#0.0085 - 0.0007 # produce: 0.0078
#0.0078/2 # produce: 0.0039 
# 2.   ¿Cuál es el intervalo creíble?
# -0.0007332123   0.008572547
# 3.  ¿Incluye cero? Sí
# 4.  ¿Qué tan probable parece el efecto? 90% de probabilidad
# 5.  ¿Qué tan grande parece clínicamente?
#0.0039 * 600 # produce: 2.34
# Significa que cada 10 horas de actividad sedentaria aumenta 2.34
# mg/dL la glucosa. No es algo clínicamente importante
# 6.  ¿Existe mucha incertidumbre? Sí, mucha
# 7.  ¿Cómo se lo explicarías a otro médico? Existe incertidumbre 
#      considerable sobre el efecto del sedentarismo con la glicemia
#       en este modelo
# 8.  ¿Cómo cambiaría tu interpretación si el intervalo fuera el 
#      doble de ancho? que hay más incertidumbre.

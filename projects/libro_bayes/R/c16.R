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

# Crear 120 paciente
n <- 120

# Simular la presión sistólica basados en imc, triglicéridos y hdl
# también simulados
datos_sim <- tibble(
  imc = rnorm(n, 30, 5),
  trigliceridos = rnorm(n, 170, 40),
  hdl = rnorm(n, 45, 10)
) |>
  mutate(
    presion_sistolica =
      90 +
      0.7 * imc +
      0.05 * trigliceridos -
      0.4 * hdl +
      rnorm(n, 0, 8)
  )

# Visualización inicial
ggplot(datos_sim,
       aes(imc, presion_sistolica)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red") +
  labs(
    title = "IMC y presión arterial sistólica",
    x = "IMC",
    y = "Presión sistólica"
  )
 
# Crear modelo bayesiano del efecto del imc, los triglicéridos y el 
# colesterol hdl sobre la presión sistólica
modelo_sim <- stan_glm(
  presion_sistolica ~ imc + trigliceridos + hdl,
  data = datos_sim,
  chains = 2,
  iter = 1000,
  seed = 123,
  refresh = 0
)

# Resumen del modelo
print(modelo_sim) # produce:
#              Median MAD_SD
#(Intercept)   105.0    6.4 
#imc             0.4    0.1 
#trigliceridos   0.1    0.0 
#hdl            -0.5    0.1 
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 8.1    0.5 

# Imprimir intervalo posterior
posterior_interval(modelo_sim,
                   prob = 0.95) # produce:
#                    2.5%        97.5%
#(Intercept)   92.11766252 117.28784453
#imc            0.13396296   0.67115957
#trigliceridos  0.01397267   0.09351491
#hdl           -0.68580881  -0.39466712
#sigma          7.10883646   9.14408336

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar presión sistólica, imc, trigliceridemia y colesterolemia
# hdl
cardio <- df_obesidad |> 
  select(
    BPXSY_mean,
    BMXBMI,
    LBXTR,
    LBDHDD
  ) |> 
  drop_na()
# Filtrar los datos por glicemia e índice de masa corporal
cardio <- cardio %>%
  filter(
    BMXBMI <= 100,
    LBXTR <= 1500
  )

# Visualizar la tabla
glimpse(cardio) # produce:
#Rows: 2,575
#Columns: 4
#$ BPXSY_mean <dbl> 142.0000, 137.3333, 122.6667, 104.6667, 121.3333, 11…
#$ BMXBMI     <dbl> 28.9, 19.7, 35.7, 20.3, 22.8, 28.9, 35.9, 23.6, 38.3…
#$ LBXTR      <dbl> 51, 75, 64, 24, 14, 148, 57, 93, 87, 328, 168, 139, …
#$ LBDHDD     <dbl> 60, 85, 58, 96, 53, 33, 55, 78, 56, 43, 42, 30, 49, …

# Distribución de la presión sistólica
ggplot(cardio,
       aes(BPXSY_mean)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución de presión sistólica",
    x = "Presión sistólica",
    y = "Frecuencia"
  )

# Diagrama de dispersión que relaciona imc con presión sistólica
ggplot(cardio,
       aes(BMXBMI, BPXSY_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              color = "red",
              se = FALSE) +
  labs(
    title = "IMC y presión sistólica",
    x = "IMC",
    y = "Presión sistólica"
  )

# Diagrama de dispersión que relaciona el imc con los triglicéridos
ggplot(cardio,
       aes(LBXTR, BPXSY_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              color = "darkgreen",
              se = FALSE) +
  labs(
    title = "Triglicéridos y presión sistólica",
    x = "Triglicéridos",
    y = "Presión sistólica"
  )

# Diagrama de dispersión que relaciona la presión sistólica con el
# colesterol hdl
ggplot(cardio,
       aes(LBDHDD, BPXSY_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              color = "purple",
              se = FALSE) +
  labs(
    title = "HDL y presión sistólica",
    x = "HDL",
    y = "Presión sistólica"
  )

# Construir modelo bayesiano que muestra como influyen el imc,
# los triglicéridos y el hdl sobre la presión sistólica
modelo_cardio <- stan_glm(
  BPXSY_mean ~ BMXBMI + LBXTR + LBDHDD,
  data = cardio,
  chains = 2,
  iter = 1000,
  seed = 123,
  refresh = 0
)

# Resumen del modelo
print(modelo_cardio) # produce:
#           Median MAD_SD
#(Intercept) 101.1    2.5 
#BMXBMI        0.4    0.1 
#LBXTR         0.0    0.0 
#LBDHDD        0.1    0.0 
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 17.5    0.2  

# Imprimir el intervalo posterior
posterior_interval(modelo_cardio,
                   prob = 0.95)
#                   2.5%        97.5%
#(Intercept) 96.46277989 105.79771432
#BMXBMI       0.33303423   0.52736367
#LBXTR        0.01875994   0.03499764
#LBDHDD       0.05430737   0.15067188
#sigma       17.00060752  17.97561440

# Visualizar incertidumbre
mcmc_areas(
  as.matrix(modelo_cardio),
  pars = c("BMXBMI",
           "LBXTR",
           "LBDHDD")
)

########## Con ldl ###############

# Seleccionar presión sistólica, imc, trigliceridemia y colesterolemia
# hdl
cardio_ldl <- df_obesidad |> 
  select(
    BPXSY_mean,
    BMXBMI,
    LBXTR,
    LBDHDD,
    LBDLDL,
    RIDAGEYR,
    LBXSGL
  ) |> 
  drop_na()
# Filtrar los datos por glicemia e índice de masa corporal
cardio_ldl <- cardio_ldl %>%
  filter(
    BMXBMI <= 100,
    LBXTR <= 1500
  )

# Ajustar el modelo
modelo_cardio_ldl <- stan_glm(
  BPXSY_mean ~ BMXBMI + LBXTR + LBDHDD + LBDLDL,
  data = cardio_ldl,
  chains = 2,
  iter = 1000,
  seed = 123,
  refresh = 0
)

# Imprimir resumen del modelo
print(modelo_cardio_ldl) # produce:
#          Median MAD_SD
#(Intercept) 96.7    2.4  
#BMXBMI       0.4    0.1  
#LBXTR        0.0    0.0  
#LBDHDD       0.1    0.0  
#LBDLDL       0.0    0.0  
#
#Auxiliary parameter(s):
#    Median MAD_SD
#sigma 17.4    0.2  

# Imprimir el intervalo posterior
posterior_interval(
  modelo_cardio_ldl,
  prob = 0.95
)
#                   2.5%        97.5%
#(Intercept) 92.01461970 101.43185829
#BMXBMI       0.32428423   0.51640780
#LBXTR        0.02753173   0.05021541
#LBDHDD       0.06214817   0.15692244
#LBDLDL       0.01134122   0.05131124
#sigma       16.88526493  17.87842928

# Visulaización de incertidumbre
mcmc_areas(
  as.matrix(modelo_cardio_ldl),
  pars = c(
    "BMXBMI",
    "LBXTR",
    "LBDHDD",
    "LBDLDL"
  )
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

# Ejercicio 1

#Explica con tus propias palabras:
  
#  ¿Por qué el riesgo cardiovascular se considera multifactorial?
  
# Intenta conectar al menos 3 de estas variables:
  
# obesidad
# triglicéridos
# HDL
# presión arterial
# resistencia a la insulina
# inflamación

# Creo que es por su  relación tan íntima con la inflamación y la
# presión arterial. y Aquí es donde se conecta el problema del
# tejido adiposo con la presión arterial a través de la resistencia
# a la insulina. 

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2

# ¿Por qué un coeficiente puede cambiar de signo cuando agregamos nuevas 
#  variables al modelo?
  
#  Por ejemplo:
  
#  HDL negativo en un gráfico simple
#  HDL positivo en un modelo ajustado

# 👉 Explica la intuición clínica y estadística.

# Porque es posible que parte del efecto negativo se deba a la
# variable que se ajusta. Tal vez por eso

# Ajustar el modelo
modelo_cardio_ldl <- stan_glm(
  BPXSY_mean ~ LBDHDD,
  data = cardio_ldl,
  chains = 2,
  iter = 1000,
  seed = 123,
  refresh = 0
)
print(modelo_cardio_ldl) # produce:
#            Median MAD_SD
#(Intercept) 122.7    1.3 
#LBDHDD        0.0    0.0 
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 17.9    0.2  

posterior_interval(
  modelo_cardio_ldl,
  prob = 0.95) # produce:
#                   2.5%        97.5%
#(Intercept) 119.9846210 125.25726815
#LBDHDD       -0.0545733   0.03486778
#sigma        17.4033610  18.34613965

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3

# ¿Cuál es la diferencia entre:
  
#  BPXSY_mean ~ LBDHDD

# y:
  
#  BPXSY_mean ~ BMXBMI + LBXTR + LBDHDD

# ¿Qué pregunta responde cada modelo?

# La diferencia es que BPXSY_mean ~ LBDHDD responde cuantos puntos
# de presión cambia cada unidad de hdl; mientras que 
# BPXSY_mean ~ BMXBMI + LBXTR + LBDHDD responde cuantas unidades
# de presión cambia cada unidad de hdl cuando los triglicéridos y
# el imc se mantienen estables.

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4

# Explica con tus propias palabras:
  
#  ¿Qué significa “ajustar por otras variables”?
  
#  Usa un ejemplo clínico sencillo.

# Es estabilizar las variables en un valor preciso. Es evitar los
# efectos de otras variables
# por ejemplo: glicemia ~ imc + cintura : se ajusta imc por cintura

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5

# Construye un gráfico entre:
  
# LDL y presión arterial
# ggplot(...)

# Diagrama de dispersión que relaciona la presión sistólica con el
# colesterol ldl
ggplot(cardio_ldl,
       aes(LBDLDL, BPXSY_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              color = "purple",
              se = FALSE) +
  labs(
    title = "LDL y presión sistólica",
    x = "LDL",
    y = "Presión sistólica"
  )

# Preguntas:

#  ¿La pendiente parece positiva o negativa? Positiva
#  ¿La relación parece fuerte o débil? Fuerte
#  ¿Existe mucha dispersión? Sí

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6

# Haz un gráfico entre:
  
#  triglicéridos y HDL

# Diagrama de dispersión que relaciona los triglicéridos con el
# colesterol hdl
ggplot(cardio_ldl,
       aes(LBDHDD, LBXTR)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              color = "purple",
              se = FALSE) +
  labs(
    title = "HDL y triglicéridos",
    x = "HDL",
    y = "Triglicéridos"
  )

# Preguntas:

# Preguntas:
  
#  ¿Qué relación observas? Inversa
#  ¿Eso podría explicar parte de la multicolinealidad? Sí porque
#   ambas tienen información metabólica relacionada

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7

# Calcula la matriz de correlaciones:
  
cardio_ldl |>
  select(
    BMXBMI,
    LBXTR,
    LBDHDD,
    LBDLDL,
    LBXSGL
  ) |>
  cor(use = "complete.obs") # produce:
#            BMXBMI      LBXTR      LBDHDD      LBDLDL
#BMXBMI  1.00000000  0.2014430 -0.26669184  0.07895419
#LBXTR   0.20144295  1.0000000 -0.42770327  0.17745851
#LBDHDD -0.26669184 -0.4277033  1.00000000 -0.02712537
#LBDLDL  0.07895419  0.1774585 -0.02712537  1.00000000

# Preguntas:
  
#  ¿Qué variables parecen más relacionadas?
#   Relación inversa: es muy alta entre hdl y triglicéridos, también
#   entre hdl e imc
#   Relación directa: es alta entre triglicéridos e imc y entre ldl
#   y triglicéridos
#  ¿Qué implicaciones tiene esto para el modelo? Me parece que gran
#  parte de los triglicéridos se explica por hdl

#  En general el modelo tiene dificultad para separar completamente
#  efectos individuales porque varias variables reflejan parcialmente
#  el mismo fenómeno fisiopatológico (la resistencia a la insulina)

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8

# Construye este modelo:
  
modelo_1 <- stan_glm(
  BPXSY_mean ~ BMXBMI,
  data = cardio_ldl,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_1) # produce:
#                     5%         95%
#(Intercept) 107.2798891 111.9284715
#BMXBMI        0.3565314   0.5148186
#sigma        17.2339165  18.0305571

#Preguntas:
  
#  ¿Cómo cambia el coeficiente de IMC comparado con el modelo 
#   completo?
#  ¿El intervalo posterior se vuelve más ancho o más estrecho?
#  Este era el intervalo posterior del modelo completo:
#  BMXBMI       0.32428423   0.51640780  
#  Entonces con el modelo completo el intervalo se vuelve más estrecho
#  porque imc ahora captura parte de la información metabólica que 
#  antes compartía con: triglicéridos, hdl y ldl

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9

# Construye este modelo:

modelo_2 <- stan_glm(
  BPXSY_mean ~ LBXTR + LBDHDD,
  data = cardio_ldl,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_2) # produce:

#                     5%          95%
#(Intercept) 110.19471043 115.96470470
#LBXTR         0.03663925   0.05640413
#LBDHDD        0.03396859   0.11268458
#sigma        17.19448074  18.04709300

#Preguntas:
  
#  ¿HDL sigue positivo? Si
#  ¿Triglicéridos cambian? No
#  ¿Qué aprendemos sobre el papel del IMC? Influye bastante en la 
#   presión arterial, también aprendemos que el imc estaba capturando 
#   gran parte del entorno metabólico global 

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10

# Agrega edad al modelo:

modelo_edad <- stan_glm(
  BPXSY_mean ~ BMXBMI +
    LBXTR +
    LBDHDD +
    LBDLDL +
    RIDAGEYR,
  data = cardio_ldl,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_edad) # produce:
#            Median MAD_SD
#(Intercept) 88.6    2.3  
#BMXBMI       0.3    0.0  
#LBXTR        0.0    0.0  
#LBDHDD       0.0    0.0  
#LBDLDL       0.0    0.0  
#RIDAGEYR     0.4    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 15.8    0.2  

posterior_interval(modelo_edad) # produce:
#                      5%         95%
#(Intercept) 84.584300632 92.45827828
#BMXBMI       0.255218079  0.40266916
#LBXTR        0.007670429  0.02501095
#LBDHDD      -0.025666710  0.05364749
#LBDLDL       0.004380286  0.03486143
#RIDAGEYR     0.372753159  0.42927396
#sigma       15.470231067 16.18862931

# Preguntas:
  
#  ¿Qué ocurre con HDL? El inicio del intervalo cambia de signo
#  ¿Qué ocurre con sigma? sigma disminuye, lo que significa que 
#   en parte la variación de la presión sí se debe a la edad
#  ¿La edad parece importante? Según sigma, Sí. Porque la edad
#   afecta muchísimo la presión arterial

#####################
##
## Ejercicio 11
##
#####################

# Ejercicio 11

# Supón que un paciente presenta:
  
# IMC elevado
# triglicéridos altos
# HDL bajo
# presión arterial elevada

# Pregunta:
  
#  👉 ¿Cómo conectarías fisiopatológicamente estas variables?
  
#  Intenta usar conceptos como:
  
# resistencia a la insulina
# inflamación
# disfunción endotelial
# obesidad visceral

# La resistencia a la insulina es la fuente de varios de estos males
# por eso también se le conoce como sindrome metabólico y se relaciona
# con dislipidemia, hipertensión y también  con aumneto de peso.
# Esto pasa porque la resistencia a la insulina se define por altos
# niveles de insulina y glucosa y estos dos componentes pueden
# aumentar la grasa del cuerpo, esta genera inflamación y el 
# exceso de glucosa causa disfunción endotelial.

#####################
##
## Ejercicio 12
##
#####################

# Ejercicio 12

# ¿Por qué el modelo NO demuestra causalidad? 
#  Explica usando un ejemplo clínico.
# Porque aunque 
# trigliceridemia, colesterolemia hdl e imc se relacionan con
# la presión sistólica, hay todavía un sigma bastante elevado,
# lo que significa que buena parte de la presión no se debe
# sólo a imc, triglicéridos y hdl
# Un ejemplo: no sucede que las características únicas y necesarias
# para tener hipertensión sean tener altos triglicéridos, ser robusto
# y tener poco hdl. Hay muchos pacientes con hipertensión que no 
# tienen ninguna de estas características.

#####################
##
## Ejercicio 13
##
#####################

# Ejercicio 13

# Explica esta frase:
  
#  “Los coeficientes del modelo representan asociaciones condicionadas.”

# Significa que los coeficientes de cada variable individualmente
# en el modelo dependen de que los valores de las otras variables
# se estabilicen en un valor preciso.

#####################
##
## Ejercicio 14
##
#####################

# Ejercicio 14

# ¿Por qué el enfoque bayesiano resulta útil en medicina real?
  
#  Intenta conectar con:
  
#  incertidumbre
#  pacientes reales
#  variabilidad biológica
#  decisiones clínicas

# Porque bayes no te da un sólo valor aceptable sino un rango de 
# posibilidades plausible lo que es basatnte coherente con la 
# naturaleza humana que es muy variable. Y basados en esta 
# variabilidad tomamos decisiones clínicas

#####################
##
## Ejercicio 15
##
#####################

# Ejercicio 15

# Explica esta idea con tus propias palabras:
  
#  “No existe una única causa dominante del riesgo cardiovascular.”

# La hipertensión se puede explicar por múltiples factores y
# no hay una causa única y necesaria para que una persona tenga
# hipertensión

#####################
##
## Ejercicio 16
##
#####################

# Ejercicio 16

# Construye este modelo:

modelo_glucosa <- stan_glm(
  BPXSY_mean ~ BMXBMI +
    LBXSGL +
    LBXTR +
    LBDHDD,
  data = cardio_ldl,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_glucosa) # produce:

#            Median MAD_SD
#(Intercept) 92.2    2.6  
#BMXBMI       0.4    0.1  
#LBXSGL       0.1    0.0  
#LBXTR        0.0    0.0  
#LBDHDD       0.1    0.0  

#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 17.1    0.2  

posterior_interval(modelo_glucosa) # produce:
#                     5%         95%
#(Intercept) 88.16093357 96.62137853
#BMXBMI       0.27226550  0.43366562
#LBXSGL       0.07452006  0.10986114
#LBXTR        0.02481527  0.04331275
#LBDHDD       0.07741656  0.16316003
#sigma       16.74485787 17.58550985


# Preguntas:
  
#  ¿Qué ocurre cuando agregas glicemia? NO ocurre mucho, sólo 
#   aumenta una variable
#  ¿Qué variable parece más estable? Creo que imc
#  ¿Qué cambia más? Todos los rangos disminuyen de manera parecida
#  ¿Qué nos enseña esto sobre metabolismo? Que la glucosa también
#  se relaciona con la hipertensión, lo que si tiene sentido por
#  el daño arterial que producen los metabolitos de la glucosa


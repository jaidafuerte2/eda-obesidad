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

# Crear 200 pacientes
n <- 200

# Simular una dataset con las variables sexo, cintura y glicemia.
datos_sim <- tibble(
  sexo = rep(c("Male", "Female"), each = n/2),
  
  cintura = c(
    rnorm(100, mean = 102, sd = 10),
    rnorm(100, mean = 92, sd = 9)
  )
) %>%
  mutate(
    glicemia = if_else(
      sexo == "Male",
      70 + cintura * 0.55 + rnorm(n, 0, 8),
      72 + cintura * 0.35 + rnorm(n, 0, 8)
    )
  )

# Visualizar datos:
ggplot(datos_sim,
       aes(x = cintura,
           y = glicemia,
           color = sexo)) +
  
  geom_point(alpha = 0.7) +
  
  geom_smooth(method = "lm",
              se = FALSE) +
  
  labs(
    title = "Relación entre cintura y glicemia",
    x = "Cintura abdominal",
    y = "Glicemia"
  )

# Crear modelo global sin tomar en cuenta el sexo
modelo_global <- stan_glm(
  glicemia ~ cintura,
  data = datos_sim,
  chains = 2,
  iter = 1000,
  refresh = 0
)
# Pendiente del modelo global
posterior_interval(modelo_global) # produce:
#5%        95%
#(Intercept) 20.3094216 42.9663607
#cintura      0.7491978  0.9831496
#sigma        9.7772324 11.6141876

# Modelos separados por sexo
modelo_hombres <- stan_glm(
  glicemia ~ cintura,
  data = datos_sim %>%
    filter(sexo == "Male"),
  chains = 2,
  iter = 1000,
  refresh = 0
)
modelo_mujeres <- stan_glm(
  glicemia ~ cintura,
  data = datos_sim %>%
    filter(sexo == "Female"),
  chains = 2,
  iter = 1000,
  refresh = 0
)
# Comparar pendientes
posterior_interval(modelo_hombres) # produce:
#                   5%        95%
#(Intercept) 50.383751 76.6883227
#cintura      0.478332  0.7422517
#sigma        7.354724  9.2324668
posterior_interval(modelo_mujeres) # produce:
#                    5%        95%
#(Intercept) 51.2145847 80.4227811
#cintura      0.2554258  0.5789589
#sigma        6.8405365  8.7735099

###########################
##
## Ejemplos con NHANES
##
###########################

summary(df_obesidad$RIAGENDR) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   2.000   1.517   2.000   2.000 

# Seleccionar las variables que usaré
datos <- df_obesidad %>%
  select(
    LBXSGL, # Glicemia
    BMXWAIST, # Cintura
    RIAGENDR, # Género
    RIDAGEYR, # Edad
  ) %>%
  drop_na() %>%
  filter( # Filtrar por valores coherentes
    LBXSGL < 1000,
    BMXWAIST < 200,
    #RIAGENDR < 2
  ) %>%
  mutate( # Cambiar el tipo la variable de género a factor
    RIAGENDR = factor(RIAGENDR)
  )

# Modelo global
modelo_global <- stan_glm(
  LBXSGL ~ BMXWAIST,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_global) # produce:
#                    5%        95%
#(Intercept) 28.3099733 44.6222014
#BMXWAIST     0.6086867  0.7712493
#sigma       38.2374025 40.0487506

# Modelo incluyendo sexo
modelo_sexo <- stan_glm(
  LBXSGL ~ BMXWAIST + RIAGENDR,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_sexo) # produce:
#                    5%        95%
#(Intercept) 41.1736561 52.8571792
#BMXWAIST     0.5287472  0.6416889
#RIAGENDR2   -3.2305384  0.3701474
#sigma       38.5655897 39.9156901

# Modelo con interacción
modelo_interaccion <- stan_glm(
  LBXSGL ~ BMXWAIST * RIAGENDR,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_interaccion) # produce:
#                           5%         95%
#(Intercept)        29.1039758 44.78794589
#BMXWAIST            0.6081524  0.76247325
#RIAGENDR2           7.1428407 27.25348262
#BMXWAIST:RIAGENDR2 -0.2881582 -0.08591395
#sigma              38.5973859 39.78518555
#
# Interpretación: Intercept es el promedio de glicemia del grupo
# base que en nhanes suele ser hombres (1 = male, 2 = female) cuando
# la cintura = 0; BMXWAIST dice que por cada cm de cintura, la 
# glicemia aumenta entre 0.60 y 0.76 ; BMXWAIST:RIAGENDR2 es la interacción
# que nos dice cual es la variación de la cintura en la glicemia
# de las mujeres en relación con los hombres, es decir si el 
# centro de BMXWAIST es +-0.68 y el centro de BMXWAIST:RIAGENDR2 es
# +- -0.18 el efecto de cada cm de cintura sobre la glicemia está
# alrededor de 50(0.68 - 0.18). Sigma es la cantidad de glicemia 
# que el modelo no puede explicar.
# RIAGENDR2 es cuanto cambia el intercepto de mujeres respecto a 
# hombres, es decir el intercepto de mujeres sería igual al 
# intercepto de hombres + RIAGENDR2.


# Diagrama de dispersión que relaciona cintura con glicemia y 
# coloreado por género dónde 1 es hombre y 2 mujer 
ggplot(datos,
       aes(x = BMXWAIST,
           y = LBXSGL,
           color = RIAGENDR)) +
  
  geom_point(alpha = 0.3) +
  
  geom_smooth(method = "lm",
              se = FALSE) +
  
  labs(
    title = "Cintura y glicemia según sexo",
    x = "Cintura abdominal",
    y = "Glicemia"
  )
############### Interacción con edad ################

# Categorizar la edad 
datos <- datos %>%
  mutate(
    grupo_edad = case_when(
      RIDAGEYR < 40 ~ "Joven",
      RIDAGEYR < 60 ~ "Mediana edad",
      TRUE ~ "Mayor"
    )
  )

# Modelo incluyendo edad
modelo_edad <- stan_glm(
  LBXSGL ~ BMXWAIST + grupo_edad,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_edad) # produce:
#                               5%        95%
#(Intercept)            40.2718564 51.0163845
#BMXWAIST                0.4365105  0.5482645
#grupo_edadMayor        15.9157338 20.0999472
#grupo_edadMediana edad 11.4571779 15.7249984
#sigma                  37.8645146 39.1209243

# Modelo con interacción
modelo_interaccion <- stan_glm(
  LBXSGL ~ BMXWAIST * grupo_edad,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_interaccion) # produce:
#                                         5%         95%
#(Intercept)                      53.8224518  70.0708042
#BMXWAIST                          0.2375380   0.4055496
#grupo_edadMayor                 -12.2806848  15.9342269
#grupo_edadMediana edad          -36.5254669 -11.9902661
#BMXWAIST:grupo_edadMayor          0.0296201   0.3136139
#BMXWAIST:grupo_edadMediana edad   0.2621546   0.5105246
#sigma                            37.7827145  39.0238252

# Diagrama de dispersión que relaciona la cintura con la glicemia
# coloreado por grupo de edad
ggplot(datos,
       aes(x = BMXWAIST,
           y = LBXSGL,
           color = grupo_edad)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Cintura y glicemia según edad",
    x = "Cintura abdominal",
    y = "Glicemia"
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

# 1. El problema del promedio

# Imagina que analizas:
  
# - cintura abdominal
# - glicemia

# en toda la población mezclada.

# Preguntas
# ¿Qué información clínica podría ocultarse? Todo lo relacionado
#  con edad, sexo y embarazo: 
# ¿Por qué hombres y mujeres podrían comportarse distinto?
#  Porque a nivel abdominal la grasa subcutánea puede ser mayor en
#  mujeres que en hombres y el estrógeno puede tener ciertos efectos
#  hipoglicemiantes sobre el hígado
# ¿Por qué la edad podría modificar la relación? Por algunas efectos
# hormonales que suceden en la vejez como: la resistencia a la insulina
# empora o aumenta, también las hormonas sexuales disminuyen lo
# que puede hacer que se acumule más grasa a nivel abdominal y haya
# menos musculatura. También puede ser que en la vejez las personas 
# empiecen a cuidar más su salud y esto disminuya su cintura y su
# glicemia como en el caso de medicarse con hipoglicemiantes

#####################
##
## Ejercicio 2
##
#####################

# 2. Variables proxy

# Reflexiona sobre esta frase:
  
#  “La cintura no mide exclusivamente grasa visceral”.

# Preguntas
# ¿Qué otras cosas podría estar capturando la cintura?
#  Musculatura y grasa subcutánea
# ¿Por qué la misma cintura podría representar distinto riesgo según 
# sexo? Pienso que debe ser por la musculatura de la cintura
# ¿Qué limitaciones clínicas tiene usar IMC o cintura como proxy 
#  metabólico? Que cintura e IMC no sólo miden grasa visceral sino
#  también musculatura y grasa subcutánea, tejidos que no son tan
# activos metabólicamente como la grasa visceral

#####################
##
## Ejercicio 3
##
#####################

# 3. Pensamiento clínico sobre interacciones

# Explica con palabras simples:
  
#  👉 ¿Qué significa una interacción positiva? Significa que 
#      hay que sumar el valor de la interacción al valor de base
#      para saber cuanto afecta.
  
#  👉 ¿Qué significa una interacción negativa? Significa que 
#      hay que restar el valor de la interacción al valor de base
#      para saber cuanto afecta.

#####################
##
## Ejercicio 4
##
#####################

# 4. Analogía clínica

# Piensa en un medicamento antihipertensivo.

# Pregunta

# ¿Por qué sería razonable esperar que el efecto del medicamento cambie según:
  
# edad
# sexo
# obesidad
# actividad física

# ?
  
# Describe esto usando la idea intuitiva de interacción.
  
# Me gustaría pensasr en un diurético porque el filtrado glomerular
# disminuye con la edad, el estrógeno retiene líquidos y la insulina
# retiene líquidos 

#####################
##
## Ejercicio 5
##
#####################

# 5. Modelo global vs modelos separados

# Construye:

# Modelo global
modelo_global <- stan_glm(
  LBXSGL ~ BMXWAIST,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_global) # produce:
#                   5%        95%
#(Intercept) 40.964222 51.6437878
#BMXWAIST     0.531691  0.6382714
#sigma       38.531569 39.8208818

# Luego crea modelos separados por sexo.
modelo_hombres <- stan_glm(
  LBXSGL ~ BMXWAIST,
  data = datos %>%
    filter(RIAGENDR == "1"),
  chains = 2,
  iter = 1000,
  refresh = 0
)
modelo_mujeres <- stan_glm(
  LBXSGL ~ BMXWAIST,
  data = datos %>%
    filter(RIAGENDR == "2"),
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_hombres) # produce:
#                   5%        95%
#(Intercept) 29.256014 44.5512991
#BMXWAIST     0.609346  0.7620806
#sigma       38.250352 39.9507715
posterior_interval(modelo_mujeres) # produce:
#                    5%        95%
#(Intercept) 46.9955012 61.0283826
#BMXWAIST     0.4259051  0.5697777
#sigma       38.4319708 40.1800199

# Preguntas
# ¿Las pendientes son parecidas? No
# ¿La incertidumbre cambia? No tanto
# ¿Qué grupo tiene mayor variabilidad? Un poco más hombres

#####################
##
## Ejercicio 6
##
#####################

# 6. Modelo con interacción por sexo

# Construye:

# Modelo con interacción
modelo_interaccion <- stan_glm(
  LBXSGL ~ BMXWAIST * RIAGENDR,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_interaccion) # produce:
#                           5%         95%
#(Intercept)        29.0826575 45.15109287
#BMXWAIST            0.6059912  0.76283327
#RIAGENDR2           5.9933348 27.02207622
#BMXWAIST:RIAGENDR2 -0.2867048 -0.07696574
#sigma              38.5526432 39.85108369

# Preguntas
# ¿La interacción parece positiva o negativa? Creo que negativa
# ¿Qué significa clínicamente? Significa que el efecto de la cintura
#  sobre la glicemia en mujeres es menor que el efecto de la cintura 
#  sobre la glicemia en hombres
# ¿La cintura parece afectar más a hombres o mujeres? A hombres

#####################
##
## Ejercicio 7
##
#####################

# 7. Visualización de interacción

# Haz un gráfico con:
  
# cintura
# glicemia
# color por sexo

# y agrega líneas de regresión.

# Diagrama de dispersión que relaciona cintura con glicemia y 
# coloreado por género dónde 1 es hombre y 2 mujer 
ggplot(datos,
       aes(x = BMXWAIST,
           y = LBXSGL,
           color = RIAGENDR)) +
  
  geom_point(alpha = 0.3) +
  
  geom_smooth(method = "lm",
              se = FALSE) +
  
  labs(
    title = "Cintura y glicemia según sexo",
    x = "Cintura abdominal",
    y = "Glicemia"
  )

# Preguntas
# ¿Las pendientes parecen distintas? Son muy parecidas
# ¿Las líneas son paralelas? No tanto
# ¿El gráfico coincide con el modelo? Sí

#####################
##
## Ejercicio 8
##
#####################

# 8. Interacción con edad

# Crea grupos:

# Categorizar la edad 
datos <- datos %>%
  mutate(
    grupo_edad = case_when(
      RIDAGEYR < 40 ~ "Joven",
      RIDAGEYR < 60 ~ "Mediana edad",
      TRUE ~ "Mayor"
    )
  )

# Luego:
# Modelo con interacción
modelo_interaccion <- stan_glm(
  LBXSGL ~ BMXWAIST * grupo_edad,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
posterior_interval(modelo_interaccion) # produce:
#                                         5%         95%
#(Intercept)                      53.8224518  70.0708042
#BMXWAIST                          0.2375380   0.4055496
#grupo_edadMayor                 -12.2806848  15.9342269
#grupo_edadMediana edad          -36.5254669 -11.9902661
#BMXWAIST:grupo_edadMayor          0.0296201   0.3136139
#BMXWAIST:grupo_edadMediana edad   0.2621546   0.5105246
#sigma                            37.7827145  39.0238252

# Diagrama de dispersión que relaciona la cintura con la glicemia
# coloreado por grupo de edad
ggplot(datos,
       aes(x = BMXWAIST,
           y = LBXSGL,
           color = grupo_edad)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Cintura y glicemia según edad",
    x = "Cintura abdominal",
    y = "Glicemia"
  )

# Preguntas
# ¿Qué grupo tiene mayor pendiente? Mediana edad
# ¿El resultado coincide con tu intuición clínica? Nope
# ¿Qué explicaciones fisiopatológicas podrían existir?
#  Fisiopatológica no se me ocurre nada, pero sí creo que por ejemplo
#  las personas de más edad estén tomando hipoglicémicos o cuidándose
#  un poco más en su salud, es difícil saber.

#####################
##
## Ejercicio 9
##
#####################

# 9. Pensar como epidemiólogo

# Supón que los adultos mayores tienen menor pendiente que mediana edad.

# Preguntas
# ¿Podría existir sesgo de supervivencia? Sí , talvez
# ¿Qué papel podrían tener los medicamentos? Un papel importante, ya
#  que a cierta edad es más frecuente usar hipoglicemiantes
# ¿Por qué los adultos mayores de NHANES no representan 
#  necesariamente a todos los adultos mayores? Porque NHANES no es
#  una muestra completamente aleatoria

#####################
##
## Ejercicio 10
##
#####################

# 10. Inflamación y distribución de grasa

# Reflexiona sobre esta idea:
  
#  “La grasa subcutánea puede ser metabólicamente menos dañina que 
#   la visceral”.

# Preguntas
# ¿Por qué? Porque el adiposito de la grasa visceral tiene la 
# capacidad de hipertrofiarse mucho
# ¿Cómo se relaciona esto con resistencia a la insulina? Cuando
#  el adiposito visceral se hipertrofia, no aumentan los receptores
#  de insulina, por eso este adiposito visceral es más resistente
#  a la insulina que el adiposito subcutáneo que no se hipertrofia
# ¿Cómo podría afectar la interpretación de la cintura abdominal?
#  Una buena parte de la cintura se debe al tejido subcutáneo. Sin 
#  embargo creo que el efecto del estrógeno sobre el hígado puede
#  ser más importante para prevenir hiperglicemia -tal vez-

#####################
##
## Ejercicio 11
##
#####################

# 11. Pensar en incertidumbre

# Observa un intervalo posterior amplio.

# Preguntas
# ¿Qué significa clínicamente? Que hay una amplia variabilidad de
#  valores, es decir mucha incertidumbre
# ¿Implica que “no hay efecto”? No, sólo que el efecto es un poco
#  incierto
# ¿Qué tipos de incertidumbre pueden existir en medicina?
# Protección, sin efecto y riesgo.

#####################
##
## Ejercicio 12
##
#####################

# 12. Comparar modelos

# Compara mentalmente:
  
#  Modelo A
# LBXSGL ~ BMXWAIST

# Modelo B
# LBXSGL ~ BMXWAIST * RIAGENDR

# Preguntas
# ¿Qué complejidad agrega el modelo B? Sexo
# ¿Qué información clínica extra obtiene? La disposición típica
#  de la grasa corporal por sexo y el efecto de las hormonas sexuales
#  sobre la distribución de la grasa y el efecto sobre la glicemia
# ¿Qué riesgos existen al usar solo el modelo A? Que podría usar
#  los mismos datos para hombres y mujeres, lo que sería una error.

#####################
##
## Ejercicio 13
##
#####################

# 13. Buscar otras interacciones plausibles

# Piensa en variables NHANES donde podría existir interacción.

# Ejemplos:
  
# actividad física × obesidad
# edad × HDL
# sexo × triglicéridos
# cintura × aspirina
# diabetes × inflamación
# Pregunta

# 👉 ¿Por qué sería razonable esperar que el efecto cambie 
#    según el grupo?
# En el caso de obesidad porque los grados más severos impiden la
# movilidad de las personas. En el caso de edad, esto puede influenciar
# sobre las resistencia a la insulina y esto sobre el hdl. En el caso
# del sexo, este también puede influenciar el grado de resistencia a
# la insulina y esto repercutir en los triglicéridos. La cintura
# y el tav aumentan la inflamación, es posible que algunos pacientes
# usen aspirina en dosis antiinflamatorias como profilaxis. La 
# arteriopatía diabética podría impedir que ciertos tejidos sanen 
# y se mantengan en un estado inflamatorio perenne.

#####################
##
## Ejercicio 14
##
#####################

# 14. Tu propia hipótesis clínica

# Propón una hipótesis basada en obesidad.

# Ejemplo:
  
#  “La actividad física podría reducir más la glicemia en personas 
#   jóvenes que en adultos mayores”.

# Luego:
  
# plantea el modelo
# glicemia ~ ejercicio * edad

# explica la interacción esperada
# Esperaría que el ejercicio disminuya más la glicemia en jóvenes
# y en adultos mayores un poco menos. Es decir esperaría valores
# negativos en edadAdultosMayores en el intervalo posterior

# justifica fisiopatológicamente
# Yo esperaría que el ejercicio mejore la glicemia de una persona
# vía hipertrofia muscular que, en edades avanzadas, no suele ser
# tan evidente. Por eso no vemos campeones mundiales de 100m de
# 60, 50 o incluso 40 años.

#####################
##
## Ejercicio 15
##
#####################

# 15. Reflexión final

# Explica con tus propias palabras:
  
#  👉 ¿Por qué “promediar pacientes” puede ser peligroso en medicina?
#      Porque tomar decisiones sin conocer a un paciente, sólo basado
#      en promedios, quita contexto sobre el paciente, que suele ser
#      muy importante para tomar decisiones, a veces en el diagnóstico,
#      o  a veces en el tratamiento

#  Y luego responde:
  
#   👉 ¿Cómo ayudan las interacciones a respetar la heterogeneidad 
#       clínica? Simplemente te percatas de situaciones un poco
#       más específicas de cada paciente.

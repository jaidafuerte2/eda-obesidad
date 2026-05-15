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

# Crear 200 pacientes
n <- 200

# Simular glicemmia a partir de valores simulados de imc y cintura 
datos_sim <- tibble(
  imc = rnorm(n, 27, 4),
  cintura = rnorm(n, 95, 12)
) %>%
  mutate(
    glicemia = 70 + 
      0.6 * imc +          # efecto moderado
      0.8 * cintura +      # efecto más fuerte
      rnorm(n, 0, 10)
  )

# Visualizar la relación entre imc y glicemia
ggplot(datos_sim, aes(x = imc, y = glicemia)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# Visualizar la relación entre cintura y glicemia
ggplot(datos_sim, aes(x = cintura, y = glicemia)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)


###########################
##
## Ejemplos con NHANES
##
###########################

# Preparar los datos
datos <- df_obesidad %>%
  select(LBXSGL, LBXIN, BMXBMI, BMXWAIST) %>%
  drop_na()
# Resumir cintura para saber si debo filtrar algunos datos
summary(datos$BMXWAIST) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#55.50   86.50   97.00   98.52  108.20  177.90 
# Filtrar los datos por glicemia e índice de masa corporal
datos <- datos %>%
  filter(
    LBXSGL <= 1000,
    BMXBMI <= 100,
  )

# Crear un modelo simple
modelo_imc <- stan_glm(
  LBXSGL ~ BMXBMI,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_imc) # produce:
#            Median MAD_SD
#(Intercept) 74.4    2.3  
#BMXBMI       1.0    0.1  
#
#Auxiliary parameter(s):
#    Median MAD_SD
#sigma 39.8    0.4  
#
# Interpretación: Cada punto de imc aumenta 1 mg/dl de glicemia. La
# variabilidad del promedio de glicemia que el modelo no logra expli-
# car es 39.8


posterior_interval(modelo_imc) # produce:
#                    5%       95%
#(Intercept) 70.4774536 78.291490
#BMXBMI       0.8964584  1.150145
#sigma       39.1263377 40.456909

# Cada punto de imc aumenta entre 0.89 y 1.15 mg/dl la glicemia. Y la
# variabilidad del promedio de glicemia que el modelo no logra explicar
# es entre 39.12 y 40.4

# Crear modelo de relación de glicemia con imc + citura
modelo_completo <- stan_glm(
  LBXSGL ~ BMXBMI + BMXWAIST,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_completo) # produce:
#            Median MAD_SD
#(Intercept) 32.5    3.5  
#BMXBMI      -1.6    0.2  
#BMXWAIST     1.2    0.1  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 39.0    0.4  
#
# Interprestación: cuando el imc es el mismo por cada cm de
# cintura, se aumenta 1mg/dl de glucosa. Sin embargo cuando la cintura
# es la misma, por cada punto de imc que aumenta, disminuye la
# glicemia. Fisiopatológicamente puede significar que si un paciente
# tiene la misma grasa en la cintura pero más peso, ese peso 
# probablemente es de tejido magro, tal vez musculatura, músculo
# que es muy sensible a la insulina, capta glucosa y almacena 
# glucógeno.

posterior_interval(modelo_completo) # produce:
#                   5%       95%
#(Intercept) 26.596344 38.598014
#BMXBMI      -1.902192 -1.292372
#BMXWAIST     1.067458  1.321663
#sigma       38.350156 39.603469


############ Gráfico ############

# Filtrar por pacientes cercanos a un imc de 30
datos_imc30 <- datos %>%
  filter(BMXBMI >= 29,
         BMXBMI <= 31)

# Diagrama de dispersión de cintura en relación con la glicemia cuando
# los pacientes tienen más o menos 30 de imc 
ggplot(datos_imc30,
       aes(x = BMXWAIST,
           y = LBXSGL)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Glicemia y cintura",
    subtitle = "Pacientes con IMC aproximado de 30",
    x = "Circunferencia de cintura (cm)",
    y = "Glicemia (mg/dL)"
  ) +
  theme_minimal()

# filtrar por observaciones con cintura similar a 100cm o casi 100 cm
datos_cintura <- datos %>%
  filter(BMXWAIST >= 98,
         BMXWAIST <= 102)

# Diagrama de dispersión que relaciona el imc con la glicemia en las
# observaciones cercanas a 100cm de cintura
ggplot(datos_cintura,
       aes(x = BMXBMI,
           y = LBXSGL)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Glicemia e IMC",
    subtitle = "Pacientes con cintura aproximada de 100 cm",
    x = "IMC",
    y = "Glicemia (mg/dL)"
  ) +
  theme_minimal()


# Diagrama de dispersión que relaciona el imc con la glicemia 
# facetado por nivel de cintura.
datos %>%
  mutate(
    cintura_cat = case_when(
      BMXWAIST < 90 ~ "Cintura baja",
      BMXWAIST < 110 ~ "Cintura media",
      TRUE ~ "Cintura alta"
    )
  ) %>%
  ggplot(aes(x = BMXBMI,
             y = LBXSGL)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~ cintura_cat) +
  theme_minimal()

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

# Ejercicio 1 — Interpretación clínica básica

# Tu modelo ajustado mostró:

# | Variable | Mediana |
# | -------- | ------- |
# | IMC      | -1.6    |
# | cintura  | 1.2     |
  
# Explica con tus propias palabras:
  
#  ¿Qué significa el coeficiente positivo de cintura?
#   Significa que cuando el imc es el mismo, por cada cm de cintura
#   aumentan 1.2 mg/dl de glicemia
#  ¿Por qué el coeficiente negativo del IMC NO significa 
#  necesariamente que el IMC “proteja”?
#  Porque el IMC no necesariamente representa un peso neto de
#  tejido magro saludable y graso que aumenta la glicemia
#  ¿Qué posible interpretación fisiopatológica podría tener ese 
#  coeficiente negativo?
#  Que cuando se tiene el mismo tamaño de cintura, pero mayor imc,
#  ese peso extra no es de tejido graso visceral sino que puede
#  ser peso extra de tejido magro como el  músculo; músculo que es
#  muy sensible a la insulina y disminuye la glicemia.

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — Pensamiento clínico

# Imagina dos pacientes:

# | Paciente | IMC | Cintura |
# | -------- | --- | ------- |
# | A        | 30  | 90      |
# | B        | 30  | 110     |
  
#Usando los resultados del modelo:
  
# 1. ¿Cuál paciente esperarías que tenga mayor glicemia?
#    El de mayor cintura
# 2. ¿Qué concepto fisiopatológico está representando la cintura?
#    Resistencia a la insulina
# 3. ¿Por qué el IMC por sí solo podría ser insuficiente?
#    Porque el IMC no diferencia grasa visceral de hueso o tejido 
#    magro o grasa subcutánea.

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Interpretando incertidumbre

# El intervalo posterior para cintura fue aproximadamente:
  
#  1.07 a 1.32

# Responde:
  
# 1.  ¿Qué significa este intervalo en lenguaje clínico?
#     Significa que por cada cm de cintura aumenta 1mg/dl la glucosa
# 2.  ¿Por qué este intervalo da más información que un único 
#     coeficiente?
#     Porque me da una mejor idea de la fuerza de la relación. Al
#     tener dos valores positivos concluyo que la relación es
#     fuerte o al menos moderada.
# 3.  ¿Qué implicaría si el intervalo incluyera valores cercanos 
#     a 0? Significa que casi no hay relación entre cintura y 
#     glicemia.

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — Pensamiento bayesiano

# Completa la idea:
  
#  “El modelo no produce un único efecto verdadero del IMC.
# Produce _____________________.”
# Respuesta: Produce un rango de valores posibles que va desde 
# -1.9 a -1.2

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Sigma y biología

# Tu sigma fue aproximadamente:
  
#  σ≈39

# Explica:
  
# 1.  ¿Qué representa sigma intuitivamente? La variación del promedio
#     en el modelo o lo que no puede explicar el modelo.
# 2.  ¿Por qué sigma sigue siendo relativamente grande incluso 
#      después de ajustar por cintura? Porque hay muchos factores
#      que intervienen en el nivel de glicemia. 
# 3.  Menciona al menos 4 factores biológicos que podrían explicar 
#     parte de esa variabilidad residual. alimentos, actividad 
#     física, insulina, sueño, etc.

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6 — Limitaciones del IMC

# Menciona al menos 3 limitaciones del IMC como marcador metabólico.
# 1. Porque no describe qué proporción de músculo - grasa tiene
#    una persona
# 2. 

# Luego responde:
  
#  👉 ¿Por qué dos personas con el mismo IMC pueden tener perfiles 
#  metabólicos distintos? Porque hay 3 grandes almacenes de glucosa
#  en el cuerpo: el hígado, el músculo y el tejido adiposo y el imc
# pierde información de estos 3 almacenes

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7 — Interpretación visual

# Observaste:
  
# - pendiente positiva entre cintura y glicemia
# - pendiente negativa entre IMC y glicemia (a cintura constante)

# Explica fisiopatológicamente:
  
# -  ¿Por qué la grasa visceral podría elevar glicemia?
#    Porque la grasa visceral es resistente a la insulina y libera
#    mucha grasa a la circulación portal que llega al hígado y
#    estimula la producción de glucosa
# -  ¿Cómo podría la masa muscular influir sobre glicemia?
#    El músculo es un tejido muy sensible a la insulina que capta 
#    mucha glucosa y así disminuye la glicemia
# -  ¿Qué está intentando separar el modelo ajustado?
#    Creo que el tejido magro del tejido graso visceral

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8 — Colinealidad clínica

# Explica con tus palabras:
  
# 1.  ¿Por qué IMC y cintura están correlacionados?
#      Porque imc incluye la grasa visceral
# 2.  ¿Por qué esto dificulta separar efectos?
#      Porque en el cálculo, parte del efecto del imc sobre la 
#      glicemia sí corresponde al tejido graso visceral
# 3.  ¿Por qué en este capítulo la colinealidad tiene significado 
#      fisiopatológico y no solo matemático?
#      Porque el momento en que separas la grasa visceral del imc,
#      queda hueso y tejido magro, que tiene características 
#      fisiológicas distintas a las del tejido visceral.

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9 — Exploración adicional con NHANES

# Crea un nuevo gráfico usando:
  
#  HbA1c (LBXGH)
# o
# insulina (LBXIN)

# como resultado.

# Puedes usar:
  
#  LBXGH ~ BMXBMI + BMXWAIST

# o:
  
#  LBXIN ~ BMXBMI + BMXWAIST

# Luego responde:
  
#  ¿La cintura sigue mostrando una asociación fuerte?
#  ¿El IMC vuelve a cambiar de signo?
#  ¿Los resultados son fisiopatológicamente plausibles?

#############  Con inuslina ###############

# Preparar los datos
datos <- df_obesidad %>%
  select(LBXIN, BMXBMI, BMXWAIST) %>%
  drop_na()
# Resumir la variable insulina
summary(df_obesidad$LBXIN) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#0.14    5.79    9.13   13.29   14.95  682.48    3169 
# Filtrar los datos por glicemia e índice de masa corporal
datos <- datos %>%
  filter(
    LBXIN <= 100,
    BMXBMI <= 100,
  )
# Histograma de la insulina
ggplot(datos, aes(x = LBXIN)) +
  geom_histogram(bins = 60)

# Crear un modelo simple
modelo_imc <- stan_glm(
  LBXIN ~ BMXBMI,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_imc) # produce:
#            Median MAD_SD
#(Intercept) -9.6    0.8  
#BMXBMI       0.8    0.0  
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 10.3    0.1  

# Interpretación:

posterior_interval(modelo_imc) # produce:
#                     5%        95%
#(Intercept) -10.9721230 -8.2422854
#BMXBMI        0.7219324  0.8133086

# Crear modelo de relación de insulina con imc + citura
modelo_completo <- stan_glm(
  LBXIN ~ BMXBMI + BMXWAIST,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_completo) # produce:
#            Median MAD_SD
#(Intercept) -17.9    1.3 
#BMXBMI        0.3    0.1 
#BMXWAIST      0.2    0.0 
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 10.2    0.1 
#
# Interpretación: 

posterior_interval(modelo_completo) # produce:
#                     5%         95%
#(Intercept) -20.0525450 -15.6247266
#BMXBMI        0.1792142   0.4028262
#BMXWAIST      0.1754673   0.2699823
#sigma        10.0032987  10.4601144

# Diagrama de dispersión de cintura en relación con la insulina cuando
# los pacientes tienen más o menos 30 de imc 
ggplot(datos_imc30,
       aes(x = BMXWAIST,
           y = LBXIN)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Inuslinemia y cintura",
    subtitle = "Pacientes con IMC aproximado de 30",
    x = "Circunferencia de cintura (cm)",
    y = "Insulinemia"
  ) +
  theme_minimal()

# Diagrama de dispersión que relaciona el imc con la insulina en las
# observaciones cercanas a 100cm de cintura
ggplot(datos_cintura,
       aes(x = BMXBMI,
           y = LBXIN)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(
    title = "Insulina e IMC",
    subtitle = "Pacientes con cintura aproximada de 100 cm",
    x = "IMC",
    y = "Insulina (mg/dL)"
  ) +
  theme_minimal()

# Diagrama de dispersión que relaciona el imc con la glicemia 
# facetado por nivel de cintura.
datos %>%
  mutate(
    cintura_cat = case_when(
      BMXWAIST < 90 ~ "Cintura baja",
      BMXWAIST < 110 ~ "Cintura media",
      TRUE ~ "Cintura alta"
    )
  ) %>%
  ggplot(aes(x = BMXBMI,
             y = LBXIN)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~ cintura_cat) +
  theme_minimal()

############# Con glicohemoglobina ##############

# Preparar los datos
datos <- df_obesidad %>%
  select(LBXGH, BMXBMI, BMXWAIST) %>%
  drop_na()
# Resumir la variable glicohemoglobina
summary(df_obesidad$LBXGH) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#3.500   5.200   5.500   5.715   5.900  17.500     221 
# Filtrar los datos por glicemia e índice de masa corporal
datos <- datos %>%
  filter(
    LBXGH <= 100,
    BMXBMI <= 100,
    BMXWAIST <= 400
  )
# Histograma de la insulina
ggplot(datos, aes(x = LBXGH)) +
  geom_histogram(bins = 60)
# Crear un modelo simple
modelo_imc <- stan_glm(
  LBXGH ~ BMXBMI,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
#            Median MAD_SD
#(Intercept) 4.8    0.1   
#BMXBMI      0.0    0.0   
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 1.0    0.0  

# Interpretación:

posterior_interval(modelo_imc) # produce:
#                    5%        95%
#(Intercept) 4.67464822 4.86821412
#BMXBMI      0.02950456 0.03590523
#sigma       1.01580441 1.04897365

# Crear modelo de relación de glicohemoglobina con imc + cintura
modelo_completo <- stan_glm(
  LBXGH ~ BMXBMI + BMXWAIST,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_completo) # produce:
#            Median MAD_SD
#(Intercept) 3.6    0.1   
#BMXBMI      0.0    0.0   
#BMXWAIST    0.0    0.0   
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 1.0    0.0  
#
# Interpretación: 

posterior_interval(modelo_completo) # produce:
#                     5%         95%
#(Intercept)  3.48522720  3.79456941
#BMXBMI      -0.04571848 -0.03015440
#BMXWAIST     0.02893992  0.03542957
#sigma        0.99186693  1.02416875

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10 — Reflexión final (el más importante)

# Antes de este capítulo, muchas personas pensarían:
  
#  “La obesidad depende principalmente del peso.”

# Después de trabajar con estos modelos:
  
#  👉 ¿Cómo cambiarías esa afirmación?
  
#  Intenta responder en:
  
#  lenguaje clínico
#  no estadístico
#  como si se lo explicaras a un paciente o a otro médico.

# Respuesta: Es cierto que el ejercicio es muy saludable porque
# regula muy bien el exceso de glucosa de la sangre y demasiada 
# glucosa no es saludable. Y sin duda el ejercicio mejora la musculatura
# y al parecer miesntras más musculatura, tambien hay menores
# niveles de glicemia. Sin embargo el efecto del ejercicio para 
# adelgazar es cuestionable pues no se observa que logra reducir 
# los niveles de inuslina de la sangre, un componente fundamental
# del exceso de grasa.

#####################
##
## Ejercicio 11
##
#####################

# Ejercicio BONUS (MUY recomendado)

#Haz este modelo:
  
#  stan_glm(
#    LBXSGL ~ BMXWAIST,
#    data = datos
#  )

# Y compáralo con:
  
#  stan_glm(
#    LBXSGL ~ BMXBMI,
#    data = datos
#  )

# Luego responde:
  
#  ¿Cuál predictor parece más fuerte por sí solo?
#  ¿Cuál parece más coherente fisiopatológicamente?
#  ¿Qué aprendiste sobre adiposidad visceral?

# Crear modelo de relación de glicohemoglobina con  cintura
modelo_cintura <- stan_glm(
  LBXGH ~ BMXWAIST,
  data = datos,
  chains = 2,
  iter = 1000,
  refresh = 0
)
print(modelo_cintura) # produce:
#            Median MAD_SD
#(Intercept) 4.0    0.1   
#BMXBMI      0.0    0.0   
#BMXWAIST    0.0    0.0   
#
#Auxiliary parameter(s):
#      Median MAD_SD
#sigma 1.0    0.0  
#
# Interpretación: 

posterior_interval(modelo_cintura)
#                    5%        95%
#(Intercept) 3.83050556 4.09593653
#BMXWAIST    0.01640998 0.01911339
#sigma       0.99760646 1.03043159
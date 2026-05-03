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

# Crear 80 pacientes 
n <- 80

# Simaular imc e insulina (en relación con imc)
datos_sim <- tibble(
  imc = rnorm(n, mean = 28, sd = 4),
  insulina = 5 + 0.6 * imc + rnorm(n, 0, 3)
)

# Crear modelo débil
modelo_debil <- stan_glm(
  insulina ~ imc,
  data = datos_sim,
  prior = normal(0, 10), # prior es cuánto espero que cambie la 
                         # insulina por cada punto de imc 
  prior_intercept = normal(0, 20), # prior_intercept es la insulina
                         # base cuando imc es igual a cero
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Crear modelo informativo
modelo_informativo <- stan_glm(
  insulina ~ imc,
  data = datos_sim,
  prior = normal(0.5, 0.2), # Espero que la insulina aumente 0.5 por
                            # cada punto de imc. Incertidumbre = 0.2
  prior_intercept = normal(5, 2), # La insulina de base es 5 cuando
                                  # imc es cero. Con incertidumbre = 2
  chains = 2,
  iter = 1000,
  refresh = 0
)


posterior_interval(modelo_debil) # produce:
#                    5%       95%
#(Intercept) -1.8625325 6.2914680
#imc          0.5534275 0.8369485
#sigma        2.5238069 3.3006238

# Por cada punto de imc, la insulina aumenta entre 0.55 y 0.83 puntos
# En este rango la media de insulina varía entre -1.8 y 6.2

posterior_interval(modelo_informativo) # produce:
#                    5%       95%
#(Intercept) -1.2191106 6.1309477
#imc          0.5334967 0.7981484
#sigma        2.5219588 3.3335806

# Ajustado con un prior informativo (no prior débil), el intervalo
# se achica un poco

###########################
##
## Ejemplos con NHANES
##
###########################

############# Con glucosa ############

# Seleccionar sólo los datos que voy a usar
datos <- df_obesidad %>%
  select(BMXBMI, LBXSGL) %>%
  drop_na() %>%
  rename(
    imc = BMXBMI,
    glicemia = LBXSGL
  )
# Filtrar los pacientes con imc monor de 100 y glicemia menor de 1000
datos <- datos %>%
  filter(
    imc <= 100,
    glicemia <= 500
  )

# Exploración rápida de imc vs insulina
ggplot(datos, aes(x = imc, y = glicemia)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Relación entre IMC y glicemia",
    x = "IMC",
    y = "glicemia"
  )


# Crear un modelo con prior débil
modelo_nhanes_debil <- stan_glm(
  glicemia ~ imc,
  data = datos,
  prior = normal(0, 10),
  prior_intercept = normal(90, 20),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Crear un modelo con prior informativo
modelo_nhanes_info <- stan_glm(
  glicemia ~ imc,
  data = datos,
  prior = normal(0.8, 0.3),
  prior_intercept = normal(90, 10),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Intervalo posterior débil
posterior_interval(modelo_nhanes_debil) # produce:
#                    5%       95%
#(Intercept) 70.7988723 77.723260
#imc          0.9130388  1.156862
#sigma       39.4669466 40.697100

# Intervalo posterior informativo
posterior_interval(modelo_nhanes_info) # produce:
#                    5%       95%
#(Intercept) 71.0304618 78.154686
#imc          0.9032765  1.141132
#sigma       39.4348780 40.745331

# Interprestación: El prior casi no cambió nada porque los datos
# son fuertes. Sigma me indica que dos personas con el mismo 
# imc pueden tener diferencias de glicemia de más o menos 40mg/dl
# pr ejemplo 70mg/dl y 110mg/dl 

# Visualizar con prior débil
mcmc_areas(
  as.matrix(modelo_nhanes_debil),
  pars = "imc"
)

# Visualizar con prior informativo
mcmc_areas(
  as.matrix(modelo_nhanes_info),
  pars = "imc"
)

# La línea vertical azul indica el promedio o la mediana de la
# variación de glicemia por cada punto de imc que es más o menos
# 1.05 y las francas azules suelen indicar dónde está el 50% de la
# población.

############## Con insulina ##############

datos <- df_obesidad %>%
  select(BMXBMI, LBXIN) %>%
  drop_na() %>%
  rename(
    imc = BMXBMI,
    insulina = LBXIN
  )
# Filtrar los pacientes con imc monor de 100 y glicemia menor de 1000
datos <- datos %>%
  filter(
    imc <= 100,
    insulina <= 100
  )

# Exploración rápida de imc vs insulina
ggplot(datos, aes(x = imc, y = insulina)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Relación entre IMC e insulina",
    x = "IMC",
    y = "Insulina"
  )

# Crear modelo débil
modelo_debil <- stan_glm(
  insulina ~ imc,
  data = datos,
  prior = normal(0, 10),
  prior_intercept = normal(10, 10),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Crear modelo informativo
modelo_info <- stan_glm(
  insulina ~ imc,
  data = datos,
  prior = normal(0.5, 0.2),
  prior_intercept = normal(10, 5),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Intervalo posterior débil
posterior_interval(modelo_debil) # produce:
#                     5%        95%
#(Intercept) -10.3710132 -7.5943856
#imc           0.6998839  0.7903278
#sigma        10.0612540 10.5319168

posterior_interval(modelo_info) # produce:
#                     5%        95%
#(Intercept) -10.2033456 -7.5923428
#imc           0.6973874  0.7851875
#sigma        10.0676907 10.5142701

# Visualizar con prior débil:
mcmc_areas(
  as.matrix(modelo_debil),
  pars = "imc"
)

# Visualizar con prior fuerte
mcmc_areas(
  as.matrix(modelo_info),
  pars = "imc"
)

############## Con insulina como causal ##############

# Exploración rápida de imc vs insulina
ggplot(datos, aes(x = insulina, y = imc)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Relación entre IMC e insulina",
    x = "Insulina",
    y = "IMC"
  )

# Modelar con prior débil
modelo_imc_debil <- stan_glm(
  imc ~ insulina,
  data = datos,
  prior = normal(0, 1),
  prior_intercept = normal(25, 10),
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Modelar con prior informativo
modelo_imc_info <- stan_glm(
  imc ~ insulina,
  data = datos,
  prior = normal(0.2, 0.1),
  prior_intercept = normal(25, 5),
  chains = 2,
  iter = 1000,
  refresh = 0
)
# Mostrar modelo débil
posterior_interval(modelo_imc_debil) # produce:
#                    5%        95%
#(Intercept) 24.8814331 25.4908797
#insulina     0.2721796  0.3062718
#sigma        6.2777036  6.5741216

# Mostrar modelo informativo 
posterior_interval(modelo_imc_info) # produce:
#                    5%        95%
#(Intercept) 24.9193301 25.4842656
#insulina     0.2734358  0.3073012
#sigma        6.2882336  6.5759666

# Sigma significa la desviación estándar de la media. Es decir para
# cierta insulina hay una media y alrededor de esa media hay 
# una desviación de +-6 puntos de imc. Por ejemplo una media de
# 28 tiene pacientes alrededor de entre 22 y 34 de imc.

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

#Sin usar R, responde:
  
#  Tienes esta creencia clínica:
  
#  “La insulina aumenta con el IMC, y el efecto es moderado, 
# pero no estoy completamente seguro”

#👉 Propón un prior para la pendiente.=> normal()

#normal(0.5, 0.3)

#Preguntas:
  
#  ¿media?
#media = 0.5 → efecto positivo moderado

#  ¿desviación?
#sd = 0.3 → incertidumbre relativamente amplia

#  ¿por qué?
# “Creo que sube, pero podría ser un poco más o menos”
 

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2

# Ahora cambia el escenario:
  
#  “Estoy MUY seguro de que el efecto es positivo y 
#  relativamente fuerte”

# 👉 Ajusta el prior. => normal()

#normal(0.8, 0.1)


# Pregunta clave:
  
#  ¿qué cambia: la media, la desviación o ambas?
# media ↑ → efecto más fuerte
# sd ↓ → más certezao 
# la seguridad se refleja en la desviación estándar

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 (clave conceptual)

#Explica con tus palabras:
  
#  👉 ¿Qué pasaría si usas este prior?
  
#  normal(-0.5, 0.1)

# en el modelo:
  
#  insulina ~ imc


# Estoy diciendo: Estoy seguro de que a mayor IMC, menor insulina

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4

# Simula datos donde:
  
#  👉 NO hay relación entre IMC e insulina

# Luego:
  
# ajusta modelo con prior débil
# ajusta modelo con prior informativo positivo

# 👉 Compara resultados

# Pregunta clave

#👉 ¿El prior puede “inventar” una relación?

# Simaular imc e insulina (en relación con imc)
datos_sim <- tibble(
  imc = rnorm(n, mean = 28, sd = 4),
  insulina = 5 + 0.6 * 27 + rnorm(n, 0, 3)
)

# Crear modelo débil
modelo_debil2 <- stan_glm(
  insulina ~ imc,
  data = datos_sim,
  prior = normal(0, 10), # prior es cuánto espero que cambie la 
  # insulina por cada punto de imc 
  prior_intercept = normal(0, 20), # prior_intercept es la insulina
  # base cuando imc es igual a cero
  chains = 2,
  iter = 1000,
  refresh = 0
)

# Crear modelo informativo
modelo_informativo2 <- stan_glm(
  insulina ~ imc,
  data = datos_sim,
  prior = normal(0.5, 0.2), # Espero que la insulina aumente 0.5 por
  # cada punto de imc. Incertidumbre = 0.2
  prior_intercept = normal(5, 2), # La insulina de base es 5 cuando
  # imc es cero. Con incertidumbre = 2
  chains = 2,
  iter = 1000,
  refresh = 0
)

posterior_interval(modelo_debil2) # produce:
#                    5%        95%
#(Intercept) 17.2170375 25.4685529
#imc         -0.1603095  0.1313035
#sigma        2.7731916  3.5941837

posterior_interval(modelo_informativo2) # produce:
#                     5%        95%
#(Intercept) 15.04202729 22.4587371
#imc         -0.07021134  0.1969383
#sigma        2.78018509  3.6813201

# Respuesta: Creo que el prior no puede "inventar una relación"
# sólo ajusta un poco la pendiente. es un efecto pequeño, sin 
# embargo, aunque es improbable, no es imposible que un prior
# "invente" una relación

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5

# Usa:
  
#  insulina ~ imc

# Construye:
  
# modelo con prior débil
# modelo con prior informativo

# 👉 compara:
  
#  posterior_interval()

# Intervalo posterior débil
posterior_interval(modelo_debil) # produce:
#                     5%        95%
#(Intercept) -10.3710132 -7.5943856
#imc           0.6998839  0.7903278
#sigma        10.0612540 10.5319168

# Intervalo posterior informativo
posterior_interval(modelo_info) # produce:
#                     5%        95%
#(Intercept) -10.2033456 -7.5923428
#imc           0.6973874  0.7851875
#sigma        10.0676907 10.5142701

# Preguntas

# ¿cambian los intervalos? Sí
# ¿cuánto cambian? Muy poco
# ¿quién domina: datos o prior? Datos

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6

#Ejercicio 6 (muy importante)

#Ahora usa:
  
#  imc ~ insulina

# Mostrar modelo débil
posterior_interval(modelo_imc_debil) # produce:
#                    5%        95%
#(Intercept) 24.8814331 25.4908797
#insulina     0.2721796  0.3062718
#sigma        6.2777036  6.5741216

# Mostrar modelo informativo 
posterior_interval(modelo_imc_info) # produce:
#                    5%        95%
#(Intercept) 24.9193301 25.4842656
#insulina     0.2734358  0.3073012
#sigma        6.2882336  6.5759666

# 👉 y responde:
  
#  ¿cambia el sigma? Poco
#  ¿en qué unidades está sigma? kg/m2
#  ¿cuál modelo tiene más variabilidad? Parece que el informativo
#  varía un poco más 

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7

# Responde sin código:
  
#  👉 ¿Cuándo usarías un prior débil en medicina? Cuando no tengo
#  muchas observaciones ni muchos datos previos  
  
#  👉 ¿Cuándo usarías un prior informativo? Cuando tengo muchos
#  datos y/o fuerte evidencia sobre algún suceso

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8

# Tienes este escenario:
  
#  Paciente:
  
#  IMC = 35
# insulina elevada
# en tratamiento con aspirina

# 👉 Pregunta:
  
#  ¿Cómo usarías un prior en un modelo que incluya:
  
#  insulina ~ imc + aspirina

# 👉 Específicamente:
  
#  ¿qué prior pondrías para IMC?
#  usaría priors informativos:
#  prior = normal(0.5, 0.2)
#  prior_intercept = normal(10, 5)
#  ¿qué prior pondrías para aspirina?
#  prior = normal(0, 10),
#  prior_intercept = normal(100, 10), o lo que me de summary
#  ¿por qué?
#  Porque la relación entre imc e insulina está bien documentada
#  En cambio la relación entre aspirina e insulina NO.

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9 (el más importante)

# Explica con tus palabras:
  
#  👉 ¿Por qué el prior casi no cambió tus resultados en NHANES?
#  Porque los datos de NHANES son buenos y abundantes

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10 (nivel avanzado pero clave)

# Imagina:
  
#  tienes pocos datos
#  mucho ruido
#  relación poco clara

# 👉 Pregunta:
  
#  ¿Qué pasaría si usas un prior muy fuerte? Asumiré como hechos
#  creencias que realmente no exiten lo que generará un error
#  "invisible" en los análisis
 
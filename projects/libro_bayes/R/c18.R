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
#                       5%          95%
#(Intercept)  1.000767e+02 1.042030e+02
#PAD680      -7.332123e-04 8.572547e-03
#sigma        3.962302e+01 4.099912e+01
# interpretación: como el intervalo incluye valores negativos, 
# Existe incertidumbre considerable sobre el efecto del sedentarismo 
# en este modelo.

# Calcular la probabilidad de efecto positivo:
posterior <- as.matrix(modelo_sedentarismo)
mean(posterior[, "PAD680"] > 0) # produce: [1] 0.925
# Interpretación: Existe aproximadamente un 92.5% de probabilidad 
# de que el efecto del sedentarismo sobre la glicemia sea positivo.
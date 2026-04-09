########################
##                    ##
## Capítulo 4         ## 
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

# Simular pacientes con imc y glicemia
datos_sim <- tibble(
  imc = rnorm(200, mean = 28, sd = 5),
  glicemia = 80 + imc * 1.5 + rnorm(200, 0, 10)
)

# Histograma de la variable imc
ggplot(datos_sim, aes(x = imc)) +
  geom_histogram(bins = 30)

# Diagrama de dispersión que relaciona el imc con la glicemia
ggplot(datos_sim, aes(x = imc, y = glicemia)) +
  geom_point()

############################
##
## Ejemplo con NHANES
##
############################

# Seleccionar las variables importantes
df_obesidad_sub <- df_obesidad %>%
  select(
    RIDAGEYR,   # edad
    RIAGENDR,   # sexo
    BMXBMI,     # IMC
    LBXSGL,     # glicemia
    BPXSY_mean, # presión sistólica
    BPXDI_mean, # presión diastólica
    RXD530,     # dosis de aspirina
    PAD680      # sedentarismo
  )

# Histograma de la variable de índice de masa corporal
ggplot(df_obesidad_sub, aes(x = BMXBMI)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución del IMC",
    x = "IMC",
    y = "Frecuencia"
  )

# Histograma de la variable de glicemia
ggplot(df_obesidad_sub, aes(x = LBXSGL)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución de glicemia",
    x = "Glicemia (mg/dL)",
    y = "Frecuencia"
  )

# Diagrama de dispersión que relaciona el índice de masa corporal con
# la glicemia
ggplot(df_obesidad_sub, aes(x = BMXBMI, y = LBXSGL)) +
  geom_point(alpha = 0.1) +
  labs(
    title = "IMC vs Glicemia",
    x = "IMC",
    y = "Glicemia"
  )

# Histograma de minutos de sedentarismo
ggplot(df_obesidad_sub, aes(x = PAD680)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Minutos sedentarios",
    x = "Minutos sedentarios",
    y = "Frecuencia"
  )

# Diagrama de dispersión que relaciona el sedentarismo con el índice de
# masa corporal
ggplot(df_obesidad_sub, aes(x = BMXBMI, y = PAD680)) +
  geom_point(alpha = 0.1) +
  labs(
    title = "IMC vs Sedentarismo",
    x = "IMC",
    y = "Sedentarismo"
  )

# Pasar a factor la variable de género
df_obesidad_sub <- df_obesidad_sub %>%
  mutate(
    RIAGENDR = case_when(
      RIAGENDR == 1 ~ "male",
      RIAGENDR == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    RIAGENDR = as.factor(RIAGENDR)
  )

# Gráfico de caja que relaciona el género con el índce de masa 
# corporal
ggplot(df_obesidad_sub, aes(x = RIAGENDR, y = BMXBMI)) +
  geom_boxplot() +
  labs(
    title = "IMC por sexo",
    x = "Sexo",
    y = "IMC"
  )

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

# Grafica la distribución de edad (RIDAGEYR)

ggplot(df_obesidad_sub, aes(x = RIDAGEYR)) +
  geom_histogram(bins = 80)

# ¿Es homogénea? No

################
##
## Ejercicio 2
##
################

# Haz un scatter de edad vs IMC

df_obesidad_sub %>%
  ggplot(aes(x = RIDAGEYR, y = BMXBMI)) +
  geom_point(alpha = 0.1) +
  geom_smooth()

# ¿ves relación? Casi nada

################
##
## Ejercicio 3
##
################

# Filtra solo pacientes con IMC > 30

df_obesidad_sub %>%
  filter(BMXBMI > 30) %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 50)

# ¿cómo cambia la glicemia? Muy poco:
df_obesidad_sub %>%
  ggplot(aes(x = LBXSGL)) +
  geom_histogram(bins = 50)

################
##
## Ejercicio 4
##
################

# Divide por sexo y compara glicemia (boxplot): ligeramente más alta
# en mujeres
df_obesidad_sub %>%
  ggplot(aes(x = RIAGENDR, y = LBXSGL)) +
  geom_boxplot()

################
##
## Ejercicio 5
##
################

#Busca valores extremos en glicemia

df_obesidad_sub %>%
  filter(LBXSGL > 140) %>%
  select(LBXSGL, BMXBMI, RIDAGEYR, RIAGENDR)
# A tibble: 447 × 4
#  LBXSGL BMXBMI RIDAGEYR RIAGENDR
#   <dbl>  <dbl>    <dbl> <fct>   
#1    554   26.7       69 male    
#2    219   28.6       54 male    
#3    183   28.9       72 male    
#4    174   31.2       58 male 

#¿qué tan frecuentes son? hay 447 observaciones

################
##
## Ejercicio 6
##
################

# Ver glicemia por categorías de IMC

df_obesidad_sub %>%
  mutate(
    categoria_imc = case_when(
      BMXBMI < 25 ~ "normal",
      BMXBMI < 30 ~ "sobrepeso",
      TRUE ~ "obesidad"
    )
  ) %>%
  ggplot(aes(x = categoria_imc, y = LBXSGL)) +
  geom_boxplot()

# ¿Realmente cambia la glicemia entre grupos? Muy , muy poco.

################
##
## Ejercicio 7
##
################

# Scatter con suavizado (clave)

ggplot(df_obesidad_sub, aes(x = BMXBMI, y = LBXSGL)) +
  geom_point(alpha = 0.1) +
  geom_smooth()

# Esto revela tendencias débiles: Sí ligera tendencia ascendente

################
##
## Ejercicio 8
##
################

# Distribución de IMC solo en glicemia alta

df_obesidad_sub %>%
  filter(LBXSGL > 110) %>%
  ggplot(aes(x = BMXBMI)) +
  geom_histogram(bins = 30)

# ¿predomina obesidad? No , todavía predomina sobrepeso

################
##
## Ejercicio 9
##
################

# Edad vs glicemia

ggplot(df_obesidad_sub, aes(x = RIDAGEYR, y = LBXSGL)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

# ¿la glicemia aumenta con edad? Ligeramente

################
##
## Ejercicio 10
##
################

ggplot(df_obesidad_sub, aes(x = PAD680, y = BMXBMI)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

# ¿más sedentarismo → más IMC? Aquí hay una buena relación, súper
# buena, de hecho se pasa de sobrepeso a obesidad
########################
##                    ##
## Capítulo 3         ## 
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

# Simulamos datos "desordenados"
datos_sucios <- tibble(
  edad = c(45, 52, NA, 60, 38),
  sexo = c(1, 2, 1, 2, NA),
  glucosa = c(90, 110, 105, NA, 95)
)
datos_sucios # produce:
# A tibble: 5 × 3
#   edad  sexo glucosa
#  <dbl> <dbl>   <dbl>
#1    45     1      90
#2    52     2     110
#3    NA     1     105
#4    60     2      NA
#5    38    NA      95

# Renombrar las variables para que se entiendan mejor
datos_limpios <- datos_sucios %>%
  rename(
    edad_anios = edad,
    glicemia = glucosa
  )
glimpse(datos_limpios) # produce: 
#$ edad_anios <dbl> 45, 52, NA, 60, 38
#$ sexo       <dbl> 1, 2, 1, 2, NA
#$ glicemia   <dbl> 90, 110, 105, NA, 95

# Recodificar los valores de la variable sexo
datos_limpios <- datos_limpios %>%
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ NA_character_
    )
  )
unique(datos_limpios$sexo) # produce: [1]  1  2 NA

# Eliminar los vaores faltantes aunque no siempre se debe hacer así
datos_limpios <- datos_limpios %>%
  filter(!is.na(edad_anios), !is.na(glicemia))

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
    RXD530      # dosis de aspirina
  )
glimpse(df_obesidad_sub) # produce:
#Rows: 5,782
#Columns: 7
#$ RIDAGEYR   <dbl> 69, 54, 72, 73, 56, 61, 56, 65, 26, 76, 33, 32, 18, …
#$ RIAGENDR   <dbl> 1, 1, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 1…
#$ BMXBMI     <dbl> 26.7, 28.6, 28.9, 19.7, 41.7, 35.7, 26.5, 22.0, 20.3…
#$ LBXSGL     <dbl> 554, 219, 183, 104, 104, 107, 108, 104, 81, 126, 89,…
#$ BPXSY_mean <dbl> 112.6667, 157.3333, 142.0000, 137.3333, 157.3333, 12…
#$ BPXDI_mean <dbl> 74.00000, 61.33333, 82.00000, 86.66667, 82.00000, 80…
#$ RXD530     <dbl> 81, 81, NA, 81, 81, 325, NA, NA, NA, 81, NA, NA, NA,…

# Renombrar variables
df_obesidad_sub <- df_obesidad_sub %>%
  rename(
    edad = RIDAGEYR,
    sexo = RIAGENDR,
    imc = BMXBMI,
    glicemia = LBXSGL,
    pas = BPXSY_mean,
    pad = BPXDI_mean,
    aspirina = RXD530
  )
glimpse(df_obesidad_sub) # produce:
#Rows: 5,782
#Columns: 7
#$ edad     <dbl> 69, 54, 72, 73, 56, 61, 56, 65, 26, 76, 33, 32, 18, 38…
#$ sexo     <dbl> 1, 1, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 1, …
#$ imc      <dbl> 26.7, 28.6, 28.9, 19.7, 41.7, 35.7, 26.5, 22.0, 20.3, …
#$ glicemia <dbl> 554, 219, 183, 104, 104, 107, 108, 104, 81, 126, 89, 9…
#$ pas      <dbl> 112.6667, 157.3333, 142.0000, 137.3333, 157.3333, 122.…
#$ pad      <dbl> 74.00000, 61.33333, 82.00000, 86.66667, 82.00000, 80.6…
#$ aspirina <dbl> 81, 81, NA, 81, 81, 325, NA, NA, NA, 81, NA, NA, NA, N…

df_obesidad_sub <- df_obesidad_sub %>%
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ NA_character_
    )
  )
unique(df_obesidad_sub$sexo) # produce: [1] "Hombre" "Mujer" 

# Conocer los valores faltantes de cada variable
df_obesidad_sub %>%
  summarise(across(everything(), ~ sum(is.na(.)))) # produce:
# A tibble: 1 × 7
#   edad  sexo   imc glicemia   pas   pad aspirina
#  <int> <int> <int>    <int> <int> <int>    <int>
#1     0     0     0      286   186   186     4699

# Filtrar datos útiles, eliminar los valores faltantes de edad, imc
# y glicemia
df_obesidad_clean <- df_obesidad_sub %>%
  filter(
    !is.na(edad),
    !is.na(imc),
    !is.na(glicemia)
  )

# Crear variables útiles
df_obesidad_clean <- df_obesidad_clean %>%
  mutate(
    obesidad = if_else(imc >= 30, "Sí", "No"),
    glicemia_alta = if_else(glicemia > 100, "Sí", "No")
  )
glimpse(df_obesidad_clean) # produce:
#Rows: 5,496
#Columns: 9
#$ edad          <dbl> 69, 54, 72, 73, 56, 61, 56, 65, 26, 76, 33, 32, 3…
#$ sexo          <chr> "Hombre", "Hombre", "Hombre", "Mujer", "Hombre", …
#$ imc           <dbl> 26.7, 28.6, 28.9, 19.7, 41.7, 35.7, 26.5, 22.0, 2…
#$ glicemia      <dbl> 554, 219, 183, 104, 104, 107, 108, 104, 81, 126, …
#$ pas           <dbl> 112.6667, 157.3333, 142.0000, 137.3333, 157.3333,…
#$ pad           <dbl> 74.00000, 61.33333, 82.00000, 86.66667, 82.00000,…
#$ aspirina      <dbl> 81, 81, NA, 81, 81, 325, NA, NA, NA, 81, NA, NA, …
#$ obesidad      <chr> "No", "No", "No", "No", "Sí", "Sí", "No", "No", "…
#$ glicemia_alta <chr> "Sí", "Sí", "Sí", "Sí", "Sí", "Sí", "Sí", "Sí", "…

summary(df_obesidad_clean) # produce:

# Visualización simple
ggplot(df_obesidad_clean, aes(x = imc)) +
  geom_histogram(bins = 30)


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

# Ejercicio 1

# Cuenta los NA de solo estas variables:
  
# glicemia
# imc

# ->  pista:
  
#  across(c(glicemia, imc), ...)

df_obesidad_sub %>%
  summarise(across(c(glicemia, imc), 
                   ~ sum(is.na(c(glicemia, imc))))) # produce:
# A tibble: 1 × 2
#glicemia   imc
#  <int> <int>
#  1      286   286

################
##
## Ejercicio 2
##
################

# Ejercicio 2

# Cuenta cuántos pacientes hay en total

# -> usa:
  
#  summarise(n = n())

df_obesidad_sub %>%
  summarise(
    n_pacientes = n()
  ) # produce:
# A tibble: 1 × 1
#    n_pacientes
#          <int>
#  1        5782

################
##
## Ejercicio 3
##
################

# Ejercicio 3

# Calcula la edad promedio

# pista:
  
#  mean(edad, na.rm = TRUE)

df_obesidad_sub %>%
  summarise(
    avg_edad = mean(edad, na.rm = TRUE)
  )
# A tibble: 1 × 1
#    avg_edad
#       <dbl>
#  1     47.5

################
##
## Ejercicio 4
##
################

# Ejercicio 4

# Filtra solo pacientes con:
  
#  IMC ≥ 30

# -> pregunta clínica:
#  ¿Cuántos pacientes obesos tienes? Tengo 2129 

df_obesidad_sub %>%
  filter(imc >= 30)
# A tibble: 2,129 × 7
#   edad sexo     imc glicemia   pas   pad aspirina
#  <dbl> <chr>  <dbl>    <dbl> <dbl> <dbl>    <dbl>
#1    56 Hombre  41.7      104  157.  82         81
#2    61 Mujer   35.7      107  123.  80.7      325
#3    76 Hombre  34.4      126  127.  66.7       81
#4    18 Hombre  35.6       NA  123.  64         NA
#5    38 Mujer   35.9       94  119.  69.3       NA

################
##
## Ejercicio 5
##
################

#Ejercicio 5

#Filtra pacientes con:
  
#  glicemia > 100

# -> interpreta:
#  ¿posible alteración metabólica? Sí, posible

df_obesidad_sub %>%
  filter(
    glicemia > 100
  ) # produce:
# A tibble: 1,850 × 7
#   edad sexo     imc glicemia   pas   pad aspirina
#  <dbl> <chr>  <dbl>    <dbl> <dbl> <dbl>    <dbl>
#1    69 Hombre  26.7      554  113.  74         81
#2    54 Hombre  28.6      219  157.  61.3       81
#3    72 Hombre  28.9      183  142   82         NA
#4    73 Mujer   19.7      104  137.  86.7       81
#5    56 Hombre  41.7      104  157.  82         81

################
##
## Ejercicio 6
##
################

# Ejercicio 6

# Filtra pacientes que:
  
# NO tienen NA en glicemia

# -> compara tamaño del dataset antes vs después
#    El dataset antes tenía 5782 observaciones y ahora tiene 5496
df_obesidad_sub %>%
  filter(
    !is.na(glicemia)
  )
# A tibble: 5,496 × 7
#   edad sexo     imc glicemia   pas   pad aspirina
#  <dbl> <chr>  <dbl>    <dbl> <dbl> <dbl>    <dbl>
#1    69 Hombre  26.7      554  113.  74         81
#2    54 Hombre  28.6      219  157.  61.3       81
#3    72 Hombre  28.9      183  142   82         NA
#4    73 Mujer   19.7      104  137.  86.7       81
#5    56 Hombre  41.7      104  157.  82         81
#6    61 Mujer   35.7      107  123.  80.7      325

################
##
## Ejercicio 7
##
################

# Ejercicio 7

# Crea una variable:
  
#  riesgo_metabolico:
#  "Alto" si glicemia > 110
# "Normal" si no

df_obesidad_sub <- df_obesidad_sub %>%
  mutate(
    riesgo_metabolico = case_when(
      glicemia > 110 ~ "alto",
      glicemia <= 100 ~ "normal",
      TRUE ~ NA_character_
    )
  )
class(df_obesidad_sub$riesgo_metabolico) # produce: character
unique(df_obesidad_sub$riesgo_metabolico) # produce:
# [1] "alto"   NA       "normal"
table(df_obesidad_sub$riesgo_metabolico) # produce:
#alto normal 
#1102   3646 

################
##
## Ejercicio 8
##
################

# Ejercicio 8

# Crea una variable:
  
# grupo_imc:
# "Normal" (<25)
# "Sobrepeso" (25–29.9)
# "Obesidad" (≥30)

df_obesidad_sub <- df_obesidad_sub %>%
  mutate(
    grupo_imc = case_when(
      imc < 25 ~ "normal",
      imc < 30 ~ "sobrepeso",
      imc >= 30 ~ "obesidad",
      TRUE ~ NA_character_
    )
  )
unique(df_obesidad_sub$grupo_imc) # produce:
#[1] "sobrepeso" "normal"    "obesidad" 
class(df_obesidad_sub$grupo_imc) # produce: character
table(df_obesidad_sub$grupo_imc) # produce:
#normal  obesidad sobrepeso 
#1828      2129      1825 

################
##
## Ejercicio 9
##
################

# Ejercicio 9

# Calcula:
  
# -> glicemia promedio por sexo

# (pista: group_by(sexo))

df_obesidad_sub %>%
  group_by(sexo) %>%
  summarise(
    avg_glucosa = mean(glicemia, na.rm = TRUE)
  ) # produce:
# A tibble: 2 × 2
#  sexo   avg_glucosa
#  <chr>        <dbl>
#1 Hombre        106.
#2 Mujer         103.

################
##
## Ejercicio 10
##
################

# Ejercicio 10

# Calcula:
  
#  -> IMC promedio en pacientes con glicemia alta vs normal

df_obesidad_sub %>%
  group_by(riesgo_metabolico) %>%
  summarise(
    avg_imc = mean(imc, na.rm = TRUE)
  ) # produce:
# A tibble: 3 × 2
#riesgo_metabolico avg_imc
#  <chr>               <dbl>
#1 alto                 31.6
#2 normal               27.8
#3 NA                   29.9

################
##
## Ejercicio 11
##
################

# Ejercicio 11

# Responde:
  
#  -> ¿Los pacientes con obesidad tienen mayor glicemia promedio?
#     Sí los pacientes con mayor glicemia tienen un imc mayor  
#  (no necesitas modelo aún, solo resumen)

################
##
## Ejercicio 12
##
################

# Ejercicio 12

# Explora:
  
#  -> ¿Cuántos pacientes con obesidad tienen datos faltantes de 
#     glicemia?
#     104 pacientes
#  (pista: combina filter() + is.na())

df_obesidad_sub %>%
  filter(grupo_imc == "obesidad") %>%
  filter(is.na(glicemia)) %>%
  summarise(
    n = n()
  ) # produce:
# A tibble: 1 × 1
#        n
#    <int>
#  1   104

################
##
## Ejercicio 13
##
################

# Ejercicio 13

# Crea una tabla:
  
#  -> proporción de pacientes con:
  
# obesidad
# glicemia alta

# (pista: count() o summarise())

df_obesidad_clean %>%
  summarise(
    prop_obesidad = mean(obesidad == "Sí"),
    prop_glicemia_alta = mean(glicemia_alta == "Sí")
  )
# A tibble: 1 × 2
#  prop_obesidad prop_glicemia_alta
#  <dbl>              <dbl>
#  1         0.368              0.337


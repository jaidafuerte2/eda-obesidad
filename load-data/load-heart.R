
#################################
##                             ##
## Transformaciones Básicas    ##
## de enfermedad cardíaca      ##
##                             ##
#################################

library(readr)
library(dplyr)
library(tidyverse)

# Importar desde el IDE la tabla obesity latin y cuidar de llamarla 
# df_corazon

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del heart disease para que su nombre se
# entienda mejor en español

df_corazon <- df_corazon |>
  rename(
    edad = Age,
    genero = Gender,
    presion_arterial = Blood.Pressure,
    nivel_colesterol = Cholesterol.Level,
    hace_ejercicio = Exercise.Habits,
    fuma = Smoking,
    tiene_familiar_cardiaco = Family.Heart.Disease,
    tiene_diabetes = Diabetes,
    imc = BMI,
    tiene_presion_alta = High.Blood.Pressure,
    bajo_colesterol_hdl = Low.HDL.Cholesterol,
    alto_colesterol_ldl = High.LDL.Cholesterol,
    toma_alcohol = Alcohol.Consumption,
    tiene_estres = Stress.Level,
    horas_sueno = Sleep.Hours,
    come_azucar = Sugar.Consumption,
    trigliceridos = Triglyceride.Level,
    glucosa_sangre = Fasting.Blood.Sugar,
    pcr = CRP.Level,
    homocisteina = Homocysteine.Level,
    tiene_enfermedad_corazon = Heart.Disease.Status
  )
glimpse(df_corazon[1:4,]) # produce:
#Rows: 4
#Columns: 21
#$ edad                     <dbl> 56, 69, 46, 32
#$ genero                   <chr> "Male", "Female", "Male", "Female"
#$ presion_arterial         <dbl> 153, 146, 126, 122
#$ nivel_colesterol         <dbl> 155, 286, 216, 293
#$ hace_ejercicio           <chr> "High", "High", "Low", "High"
#$ fuma                     <chr> "Yes", "No", "No", "Yes"
#$ tiene_familiar_cardiaco  <chr> "Yes", "Yes", "No", "Yes"
#$ Diabetes                 <chr> "No", "Yes", "No", "No"
#$ imc                      <dbl> 24.99159, 25.22180, 29.85545, 24.13048
#$ tiene_presion_alta       <chr> "Yes", "No", "No", "Yes"
#$ bajo_colesterol_hdl      <chr> "Yes", "Yes", "Yes", "No"
#$ alto_colesterol_ldl      <chr> "No", "No", "Yes", "Yes"
#$ toma_alcohol             <chr> "High", "Medium", "Low", "Low"
#$ tiene_estres             <chr> "Medium", "High", "Low", "High"
#$ horas_sueno              <dbl> 7.633228, 8.744034, 4.440440, 5.249405
#$ come_azucar              <chr> "Medium", "Medium", "Low", "High"
#$ trigliceridos            <dbl> 342, 133, 393, 293
#$ glucosa_sangre           <dbl> NA, 157, 92, 94
#$ pcr                      <dbl> 12.969246, 9.355389, 12.709873, 12.509…
#$ homocisteina             <dbl> 12.387250, 19.298875, 11.230926, 5.961…
#$ tiene_enfermedad_corazon <chr> "No", "No", "No", "No"

########################################################
##
## Crear la variable tipo de obesidad a partir del imc 
##
########################################################

# Conocer el tipo de la variable imc
class(df_corazon$imc) # produce: "numeric"
# Conocer los valores de la variable imc
unique(df_corazon$imc)[1:20] # produce:
#[1] 24.99159 25.22180 29.85545 24.13048 20.48629 28.14468 18.04233
#[8] 34.73668 34.49311 30.14215 34.44762 31.73962 33.34402 19.42243
#[15] 37.38784 32.16649 31.43388 26.51930 18.61898 37.83216
# Valor mínimo del imc
min(df_corazon$imc) # produce: NA
# Valor máximo del imc
max(df_corazon$imc) # produce:NA
# Conocer cuantos valores faltantes hay
sum(is.na(df_corazon$imc)) # produce: 22
min(df_corazon$imc, na.rm = TRUE) # produce: [1] 18.00284
max(df_corazon$imc, na.rm = TRUE) # produce: [1] 39.99695

# Crear una nueva variable de tipo de obesidad con los parámetros
# de la OMS
df_corazon <- df_corazon |>
  mutate(
    tipo_obesidad = case_when(
      imc < 18.5  ~ "desnutricion",
      imc < 25  ~ "peso_normal",
      imc < 30 ~ "sobrepeso",
      imc < 35 ~ "obesidad_1",
      imc < 40 ~ "obesidad_2",
      imc >= 40 ~ "obesidad_3",
      TRUE ~ NA_character_
    )
  )
# Conocer el tipo de la variable de tipo de obesidad
class(df_corazon$tipo_obesidad) # produce: "character"
# Conocer los valores de tipo de obesidad
unique(df_corazon$tipo_obesidad) # produce:
#[1] "peso_normal"  "sobrepeso"    "desnutricion" "obesidad_1"  
#[5] "obesidad_2"   NA    

df_corazon <- df_corazon |>
  mutate(
    tipo_obesidad = factor(
      tipo_obesidad,
      levels = c(
        "desnutricion",
        "peso_normal",
        "sobrepeso",
        "obesidad_1",
        "obesidad_2",
        "obesidad_3"
      )
    )
  )
unique(df_corazon$tipo_obesidad) # produce: 
#[1] peso_normal  sobrepeso    desnutricion obesidad_1   obesidad_2  
#[6] <NA>        
#6 Levels: desnutricion peso_normal sobrepeso obesidad_1 ... obesidad_3
# Conocer cuantos valores faltantes tiene tipo de obesidad
sum(is.na(df_corazon$imc)) # produce: 22

#################################
##
## Transformaciones de Género
##
#################################

# Conocer el tipo de la variable género 
class(df_corazon$genero) # produce: character
# Conocer los valores de la variable género
unique(df_corazon$genero) # produce: 
#[1] "Male"   "Female" "" 
sum(df_corazon$genero == "") # produce: 19

# Pasar las cadenas vacías a NA
df_corazon <- df_corazon |>
  mutate(
    genero = na_if(genero, "")
  )
sum(is.na(df_corazon$genero)) # produce: 19

# Recodificar la variable género para que male sea masculino y female 
# sea femenino
df_corazon <- df_corazon |>
  mutate(
    genero = recode(
      genero,
      "Male" = "masculino",
      "Female" = "femenino"
    )
  )
unique(df_corazon$genero) # produce:
#[1] "masculino" "femenino"  NA  

df_corazon <- df_corazon |>
  mutate(
    genero = factor(genero)
  )
class(df_corazon$genero) # produce: factor
unique(df_corazon$genero) # produce: 
#[1] masculino femenino  <NA>     
#Levels: femenino masculino

####################################
##
## Transformaciones de Ejercicio
##
####################################

# Conocer el tipo de la variable ejercicio
class(df_corazon$hace_ejercicio) # produce: character
# Conocer los valores de la variable ejercicio
unique(df_corazon$hace_ejercicio) # produce:
#[1] "High"   "Low"    "Medium" "" 
# Conocer cuantas cadenas vacás hay en la variable ejercicio
sum(df_corazon$hace_ejercicio == "") # produce: 25

# Pasar las cadenas vacías a valores faltantes
df_corazon <- df_corazon |>
  mutate(
    hace_ejercicio = na_if(hace_ejercicio, "")
  )
sum(is.na(df_corazon$hace_ejercicio)) # produce: 25

# Pasar a español los valores de la variable de ejercicio
df_corazon <- df_corazon |>
  mutate(
    hace_ejercicio = recode(
      hace_ejercicio,
      "Low" = "poco",
      "Medium" = "moderado",
      "High" = "mucho"
    )
  )
unique(df_corazon$hace_ejercicio) #produce:
# [1] mucho    poco     moderado <NA>    

# Convertir los valores character de ejercicio a factor ordenado
df_corazon <- df_corazon |>
  mutate(
    hace_ejercicio = factor(
      hace_ejercicio,
      levels = c("poco", "moderado", "mucho")
    )
  ) 
unique(df_corazon$hace_ejercicio) #produce:
#[1] mucho    poco     moderado <NA>    
#Levels: poco moderado mucho

################################
##
##
## Transformaciones de fumar
##
################################

# Conocer el tipo de la variable fumar
class(df_corazon$fuma) # produce: character
# Conocer los valores de la variable fumar
unique(df_corazon$fuma) # produce: [1] "Yes" "No"  "" 
# Conocer cuantas cadenas vacías hay en la variable de fumar
sum(df_corazon$fuma == "") # produce: 25

# Pasar las cadenas vacías a valores faltantes
df_corazon <- df_corazon |>
  mutate(
    fuma = na_if(fuma, "")
  )
sum(is.na(df_corazon$fuma)) # produce: 25

# Cambiar a español los valores de la variable de fumar
df_corazon <- df_corazon |>
  mutate(
    fuma = recode(
      fuma,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$fuma) # produce: [1] "si" "no" NA  

# Cambiar el tipo de la variable fumar de character a factor
df_corazon <- df_corazon |>
  mutate(
    fuma = factor(fuma)
  )
unique(df_corazon$fuma) # produce:
#[1] si   no   <NA>
#Levels: no si

#############################################
##
## Transformación de familiar cardíaco
##
#############################################

# Conocer el tipo de la variable de familiar cardíaco
class(df_corazon$tiene_familiar_cardiaco) # produce: character
# Conocer los valores de la variable de familiar cardíaco
unique(df_corazon$tiene_familiar_cardiaco) # produce: [1] "Yes" "No"  "" 
# Conocer cuantas cadenas vacías tiene la variable de familiar
# cardíaco
sum(df_corazon$tiene_familiar_cardiaco == "") # produce: 21

# Cambiar las cadenas vacías de familiar cardíaco a valores faltantes
df_corazon <- df_corazon |>
  mutate(
    tiene_familiar_cardiaco = na_if(tiene_familiar_cardiaco, "")
  )
sum(is.na(df_corazon$tiene_familiar_cardiaco)) # produce: 21

# Comabiar al español los valores de la variable de familiar cardíaco
df_corazon <- df_corazon |>
  mutate(
    tiene_familiar_cardiaco = recode(
      tiene_familiar_cardiaco,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$tiene_familiar_cardiaco) # produce:
# [1] "si" "no" NA 

# Cambiar el tipo de la variable de familiar cardíaco de character
# a factor
df_corazon <- df_corazon |>
  mutate(
    tiene_familiar_cardiaco = factor(tiene_familiar_cardiaco)
  )
unique(df_corazon$tiene_familiar_cardiaco) # produce:
#[1] si   no   <NA>
#Levels: no si

####################################################
##
## Transformaciones de la variable de diabetes
##
####################################################

# Conocer el tipo de la variable diabetes 
class(df_corazon$tiene_diabetes) # produce: character
# Conocer los valores de la variable diabetes
unique(df_corazon$tiene_diabetes) # produce: [1] "No"  "Yes" "" 
# Conocer la cantidad de cadenas vacías en la variable de diabetes
sum(df_corazon$tiene_diabetes == "") # produce: 30

# Cambiar las cadenas vacías de la variable de diabetes a valores
# faltantes
df_corazon <- df_corazon |>
  mutate(
    tiene_diabetes = na_if(tiene_diabetes, "")
  )
sum(is.na(df_corazon$tiene_diabetes)) # produce: 30

# Cambiar a español los valores de la variable de diabetes
df_corazon <- df_corazon |>
  mutate(
    tiene_diabetes = recode(
      tiene_diabetes,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$tiene_diabetes) # produce: [1] "no" "si" NA  

# Cambiar el tipo de la variable de diabetes de character a factor
df_corazon <- df_corazon |>
  mutate(
    tiene_diabetes = factor(tiene_diabetes)
  )
class(df_corazon$tiene_diabetes) # produce: "factor"
unique(df_corazon$tiene_diabetes) # produce: 
#[1] no   si   <NA>
#Levels: no si

########################################
##
## Transformaciones de Presión Alta
##
########################################

# Conocer el tipo de la variable de presión alta
class(df_corazon$tiene_presion_alta) # produce: character
# Conocer los valores de la variable de presión alta
unique(df_corazon$tiene_presion_alta) # produce: 
#[1] "Yes" "No"  ""  

# Conocer la cantidad de cadenas vacías en la variable de presión alta
sum(df_corazon$tiene_presion_alta == "") # produce: 26

# Cambiar las cadenas vacías en la variable de presión alta por
# valores faltantes
df_corazon <- df_corazon |>
  mutate(
    tiene_presion_alta = na_if(tiene_presion_alta, "")
  )
sum(is.na(df_corazon$tiene_presion_alta)) # produce: 26

# Cambiar los valores de la varibale de presión alta a español
df_corazon <- df_corazon |>
  mutate(
    tiene_presion_alta = recode(
      tiene_presion_alta,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$tiene_presion_alta) # produce:
#[1] no   si   <NA>
#Levels: no si

# Cambiar a factor la variable de presión alta
df_corazon <- df_corazon |>
  mutate(
    tiene_presion_alta = factor(tiene_presion_alta)
  )
class(df_corazon$tiene_presion_alta) # produce: "factor"
unique(df_corazon$tiene_presion_alta) # produce:
#[1] si   no   <NA>
#Levels: no si
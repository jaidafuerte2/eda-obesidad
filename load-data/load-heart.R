
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
    tiene_bajo_hdl = Low.HDL.Cholesterol,
    tiene_alto_ldl = High.LDL.Cholesterol,
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
## Transformaciones de edad
##
#################################

# Conocer el tipo de la variable edad
class(df_corazon$edad) # produce:  numeric
# Conocer los valores de la variable edad
unique(df_corazon$edad) # produce:
#[1] 56 69 46 32 60 25 78 38 75 36 40 28 41 70 53 57 20 39 19 61 47 55 77
#[24] 50 29 42 66 44 76 80 59 45 33 79 64 68 72 74 54 24 26 35 21 31 67 43
#[47] 37 52 34 23 71 51 27 48 65 62 58 18 22 30 49 73 63 NA

sum(is.na(df_corazon$edad)) # produce: 29

min(df_corazon$edad, na.rm = TRUE) # produce: 18
max(df_corazon$edad, na.rm = TRUE) # produce: 80

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

########################################
##
## Tranformaciones de presión arterial
##
########################################

# Conocer el tipo de la variable de presión arterial
class(df_corazon$presion_arterial) # produce: numeric
# Conocer los valores de la variable de presión arterial
unique(df_corazon$presion_arterial) # produce:
#[1] 153 146 126 122 166 152 121 161 135 144 179 134 143 150 133 173 125
#[18] 136 137 139 170 159 158 171 151 163 128 165 129 168 155 142 127 132
#[35] 176 141 160 154 164 138 149 178 140 147  NA 175 162 157 174 123 145
#[52] 124 148 169 172 167 131 120 177 130 180 156

# Conocer la cuántos valores faltantes tiene la variable de presión
# arterial
sum(is.na(df_corazon$presion_arterial)) # produce: 19

# Valor mínimo de la variable de presión arterial
min(df_corazon$presion_arterial, na.rm = TRUE) # produce: 120

# Valor máximo de la variable de presión arterial
max(df_corazon$presion_arterial, na.rm = TRUE) # produce: 180

###################################
##
## Tranformaciones de nivel de
## colesterol
##
###################################

# Conocer el tipo de la variable de nivel de colesterol
class(df_corazon$nivel_colesterol) # produce: numeric
# Conocer los valores de la variable de nivel de colesterol
unique(df_corazon$nivel_colesterol)[1:20] # produce: 
#[1] 155 286 216 293 242 257 175 187 291 252 191 296 215 290 255 245 219
#[18] 246 268 300

# Conocer cuanto valores faltantes hay en la variable nivel de 
# colesterol
sum(is.na(df_corazon$nivel_colesterol)) # produce: 30

# Valor mínimo de nivel de colesterol
min(df_corazon$nivel_colesterol, na.rm = TRUE) # produce: 150
# Valor máximo de nivel de colesterol
max(df_corazon$nivel_colesterol, na.rm = TRUE) # produce: 300



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

##############################################
##
## Tranformaciones de bajo colesterol hdl
##
##############################################

# Conocer el tipo de la variable de bajo colesterol hdl
class(df_corazon$tiene_bajo_hdl) # produce:  character
# Conocer los valores de la variable de bajo colesterol hdl
unique(df_corazon$tiene_bajo_hdl) # produce:
#[1] "Yes" "No"  ""   
sum(df_corazon$tiene_bajo_hdl == "") # produce: 25

# Cambiar las cadenas vacías de bajo colesterol hdl a valores 
# faltantes
df_corazon <- df_corazon |>
  mutate(
    tiene_bajo_hdl = na_if(tiene_bajo_hdl, "")
  )
sum(is.na(df_corazon$tiene_bajo_hdl)) # produce: 25

# Cambiar a español los valores de bajo colesterol hdl
df_corazon <- df_corazon |>
  mutate(
    tiene_bajo_hdl = recode(
      tiene_bajo_hdl,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$tiene_bajo_hdl) # produce: [1] "si" "no" NA 

# Cambiar el tipo de la variable de bajo colesterol hdl de 
# character a factor
df_corazon <- df_corazon |>
  mutate(
    tiene_bajo_hdl = factor(tiene_bajo_hdl)
  )
class(df_corazon$tiene_bajo_hdl) # produce: "factor"
unique(df_corazon$tiene_bajo_hdl) # produce:
#[1] si   no   <NA>
#Levels: no si

#############################################
##
## Transformaciones de alto colesterol ldl
##
#############################################

# Conocer el tipo de la variable de alto colesterol ldl
class(df_corazon$tiene_alto_ldl) # produce: character
# Conocer los valores de la variable de alto colesterol ldl
unique(df_corazon$tiene_alto_ldl) # produce: [1] "No"  "Yes" ""
sum(df_corazon$tiene_alto_ldl == "") # produce: 26

# Cambiar las cadenas vacías de alto colesterol ldl a valores
# faltantes
df_corazon <- df_corazon |>
  mutate(
    tiene_alto_ldl = na_if(tiene_alto_ldl, "")
  )
sum(is.na(df_corazon$tiene_alto_ldl)) # produce: 26

# Cambiar a español los valores de la variable de alto colesterol ldl
df_corazon <- df_corazon |>
  mutate(
    tiene_alto_ldl = recode(
      tiene_alto_ldl,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$tiene_alto_ldl) # produce: [1] "no" "si" NA  

# Cambiar el tipo de la variable de alto colesterol ldl de character
# a factor
df_corazon <- df_corazon |>
  mutate(
    tiene_alto_ldl = factor(tiene_alto_ldl)
  )
class(df_corazon$tiene_alto_ldl) # produce: "factor"
unique(df_corazon$tiene_alto_ldl) # produce:
#[1] no   si   <NA>
#Levels: no si

###########################################
##
## Transformaciones de consumo de alcohol
##
###########################################

class(df_corazon$toma_alcohol) #produce: [1] "character"
unique(df_corazon$toma_alcohol) # produce:
#[1] "High"   "Medium" "Low"    "None"   ""  
sum(df_corazon$toma_alcohol == "") # produce: 32

# Transformar las cadenas vacías de consumo de alcohol a valores
# faltantes
df_corazon <- df_corazon |>
  mutate(
    toma_alcohol = na_if(toma_alcohol, "")
  )
sum(is.na(df_corazon$toma_alcohol)) # produce: 32

# Pasar a español los valores de la variable de consumo de alcohol
df_corazon <-df_corazon |>
  mutate(
    toma_alcohol = recode(
      toma_alcohol,
      "None" = "nada",
      "Low" = "poco",
      "Medium" = "moderado",
      "High" = "mucho"
    )
  )
unique(df_corazon$toma_alcohol) # produce:
#[1] "mucho"    "moderado" "poco"     "nada"     NA  

# Cambiar el tipo de la variable de consumo de alcohol de character
# a factor
df_corazon <- df_corazon |>
  mutate(
    toma_alcohol = factor(
      toma_alcohol,
      levels = c("mucho", "moderado", "poco", "nada")
    )
  )
class(df_corazon$toma_alcohol) # produce: factor
unique(df_corazon$toma_alcohol) # produce:
#[1] mucho    moderado poco     nada     <NA>    
#Levels: mucho moderado poco nada

##############################################
##
## Transformaciones de la variable estrés
##
##############################################

# Conocer el tipo de la variable estrés
class(df_corazon$tiene_estres) # produce: character
# Conocer los valores de la varaible estrés
unique(df_corazon$tiene_estres) # produce:
#[1] "Medium" "High"   "Low"    ""
# Conocer la cantidad de cadenas vacías de la variable de estrés
sum(df_corazon$tiene_estres == "" ) # produce: 22

# Transformar las cadenas vacías de la variable estrés a valores
# faltantes
df_corazon <- df_corazon |>
  mutate(
    tiene_estres = na_if(tiene_estres, "")
  )
sum(is.na(df_corazon$tiene_estres)) # produce: 22

# Transformar a español los valores de la variable de estrés
df_corazon <- df_corazon |>
  mutate(
    tiene_estres = recode(
      tiene_estres,
      "Low" = "poco",
      "Medium" = "moderado",
      "High" = "mucho"
    )
  )
unique(df_corazon$tiene_estres) # produce:
#[1] "moderado" "mucho"    "poco"     NA    

# Tranformar el tipo de la variable de estrés de character a factor
df_corazon <- df_corazon |>
  mutate(
    tiene_estres = factor(
      tiene_estres,
      levels = c("poco", "moderado", "mucho")
    )
  )
class(df_corazon$tiene_estres) # produce: "factor"
unique(df_corazon$tiene_estres) # produce:
#[1] moderado mucho    poco     <NA>    
#Levels: poco moderado mucho

####################################
##
## Transformaciones de horas sueño
##
####################################

# Conocer el tipo de la variable de horas sueño
class(df_corazon$horas_sueno) # produce: numeric
# Conocer los valores de la variable de horas sueño
unique(df_corazon$horas_sueno)[1:20] # produce:
#[1] 7.633228 8.744034 4.440440 5.249405 7.030971 5.504876 9.240911
#[8] 7.841008 6.941403 4.002662 9.151889 6.765162 7.188455 8.189629
#[15] 9.624732 4.949260 4.427105 4.731453 8.444345 4.775667

# Conocer los valores faltantes de la variable de horas sueño
sum(is.na(df_corazon$horas_sueno)) # produce: 25

# Valor mínimo de la variable de horas sueño
min(df_corazon$horas_sueno, na.rm = TRUE) # produce: [1] 4.000605

# Valor máximo de la variable de horas sueño
max(df_corazon$horas_sueno, na.rm = TRUE) # produce: [1] 9.999952

##########################################
##
## Transformaciones de Consumo de Azúcar
## 
##########################################

# Conocer el tipo de la variable de consumo de azúcar
class(df_corazon$come_azucar) # produce: "character"
# Conocer los valores de la variable de consumo de azúcar 
unique(df_corazon$come_azucar) # produce: 
#[1] "Medium" "Low"    "High"   "" 
# Conocer cuántas cadenas vacías tiene la variable de consumo de azúcar
sum(df_corazon$come_azucar == "") # produce: 30

# Tranformar las cadenas vacías de la variable de consumo de azúcar
# a valores faltantes
df_corazon <- df_corazon |>
  mutate(
    come_azucar = na_if(come_azucar, "")
  )
sum(is.na(df_corazon$come_azucar)) # produce: 30

# Cambiar a español los valores de la variable de consumo de azúcar
df_corazon <- df_corazon |>
  mutate(
    come_azucar = recode(
      come_azucar,
      "Low" = "poco",
      "Medium" = "moderado",
      "High" = "alto"
    )
  )
unique(df_corazon$come_azucar) # produce:
#[1] "moderado" "poco"     "alto"     NA 

# Cambiar el tipo de la variable de consumo de azúcar de character
# a factor ordenado
df_corazon <- df_corazon |>
  mutate(
    come_azucar = factor(
      come_azucar,
      levels = c("poco", "moderado", "alto")
    )
  )
class(df_corazon$come_azucar) # produce: "factor"
unique(df_corazon$come_azucar) # produce:
#[1] moderado poco     alto     <NA>    
#Levels: poco moderado alto

#######################################
##
## Transformaciones de triglicéridos
##
#######################################

# Conocer el tipo de la variable de triglicéridos
class(df_corazon$trigliceridos) # produce:  numeric

# Conocer los valores de la variable de triglicéridos
unique(df_corazon$trigliceridos)[1:20] # produce:
#[1] 342 133 393 293 263 126 107 228 317 199 231 109 196 178 343 397 212
#[18] 384 346 290

# Conocer cuántos valores faltantes tiene la variable triglicéridos
sum(is.na(df_corazon$trigliceridos)) # produce: 26

# Valor mínimo de la variable triglicéridos
min(df_corazon$trigliceridos, na.rm = TRUE) # produce: 100
# Valor máximo de la variable triglicéridos
max(df_corazon$trigliceridos, na.rm = TRUE) # produce: 400

########################################
##
## Tranformaciones de glucosa en sangre
##
########################################

# Conocer el tipo de la variable de glucosa en sangre
class(df_corazon$glucosa_sangre) # produce: numeric

# Conocer los valores de la variable de glucosa en sangre
unique(df_corazon$glucosa_sangre)[1:20] # produce:
#[1]  NA 157  92  94 154  91  85 111 103  96 139 142 102 124 155 160 132
#[18] 106 144 135  80 116 122 146 126 101  95 151  90 104 129  86  89  84
#[35]  82 110 112 127 134  99 131  98  97 159 153 149  87 147 114 119 100
#[52] 128 130 109 125 121 118 138 140 156 133 117 141 108 136 107 143 145
#[69] 152  88 113 148 120 150 137  83  93 105 123 115 158  81

# Conocer cuántos valores faltantes tiene la variable de glucosa en 
# sangre
sum(is.na(df_corazon$glucosa_sangre)) # produce: 22

# Valor mínimo de la variable de glucosa en sangre:
min(df_corazon$glucosa_sangre, na.rm = TRUE) # produce: 80

# Valor máximo de la variable de glucosa en sangre:
max(df_corazon$glucosa_sangre, na.rm = TRUE) # produce: 160

#########################################
##
## Tranformaciones de PCR
## 
#########################################

# Conocer el tipo de la variable de PCR
class(df_corazon$pcr) # produce: numeric

# Conocer los valores de la variable de PCR
unique(df_corazon$pcr)[1:20] # produce: 
#[1] 12.96924569  9.35538940 12.70987253 12.50904619 10.38125923
#[6]  4.29757473 11.58298299  4.92938115  5.11901481 10.00569840
#[11] 13.58334685  6.69478476  8.42672501  3.38083906 10.05179068
#[16]  4.62001186 13.30906912  0.02843593  7.17629144 12.47282074

# Conocer cuántos valores faltantes tiene la variable de PCR
sum(is.na(df_corazon$pcr)) # produce: 26

# Valor mínimo de la variable de pcr
min(df_corazon$pcr, na.rm = TRUE) # produce: [1] 0.003646712

# Valor máximo de la variable de pcr
max(df_corazon$pcr, na.rm = TRUE) # produce: 14.99

########################################
##
## Trandformaciones de Homocisteína
##
########################################

# Conocer el tipo de la variable de homociteína
class(df_corazon$homocisteina) # produce: "numeric"

# Conocer los valores de la variable de homociteína
unique(df_corazon$homocisteina)[1:20] # produce:
#[1] 12.387250 19.298875 11.230926  5.961958  8.153887 10.815983
#[7] 19.659461 17.146599  6.051129  7.604357 13.783722 19.440650
#[13] 11.146408 17.478149  9.897205  7.919947  9.784993 14.306619
#[19] 13.868994 11.703830

# Saber cuántos valores faltantes hay en la variable de homocisteína
sum(is.na(df_corazon$homocisteina)) # produce: 20

# Valor mínimo de la variable homocisteína
min(df_corazon$homocisteina, na.rm = TRUE) # produce: [1] 5.000236

# Valor máximo de la variable homocisteína
max(df_corazon$homocisteina, na.rm = TRUE) # produce: [1] [1] 19.99904


##########################################
##
## Transformaciones de enfermedad
##              cardíaca
##
##########################################


# Conocer el tipo de la variable de enfermedad cardíaca 
class(df_corazon$tiene_enfermedad_corazon) # produce: "character"
# Conocer los valores de la variable de enfermedad cardíaca
unique(df_corazon$tiene_enfermedad_corazon) # produce:
#[1] "No"  "Yes"

# Cambiar a español los valores de la variable de enfermedad
# cardíaca
df_corazon <- df_corazon |>
  mutate(
    tiene_enfermedad_corazon = recode(
      tiene_enfermedad_corazon,
      "Yes" = "si",
      "No" = "no"
    )
  )
unique(df_corazon$tiene_enfermedad_corazon) # produce:
#[1] "no" "si"

# Cambiar el tipo de la variable de enfermedad cardíaca de character
# a factor
df_corazon <- df_corazon |>
  mutate(
    tiene_enfermedad_corazon = factor(tiene_enfermedad_corazon)
  )
class(df_corazon$tiene_enfermedad_corazon) # produce: factor
unique(df_corazon$tiene_enfermedad_corazon) # produce:
#[1] no si
#Levels: no si


#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset examina         ##
##                             ##
#################################

# Nota: No olvidar que se debe la variable de tipo de obesidad a 
# partir del imc

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset de obesidad
df_obesidad <- read_csv(
  "eda-obesidad/data/nhanes/obesidad_nhanes.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables de examina para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad |>
  rename(
    presion_sistolica = BPXSY_mean,
    presion_diastolica = BPXDI_mean,
    imc = BMXBMI,
    circunferencia_cintura = BMXWAIST,
    diametro_abdominal = BMDAVSAD,
    peso = BMXWT,
    talla = BMXHT
  )

########################
##
## Presión sistólica
##
########################

class(df_obesidad$presion_sistolica) # produce: numeric

unique(df_obesidad$presion_sistolica)[1:20] # produce:
#[1] 112.6667 157.3333 142.0000 137.3333 122.6667 122.0000 141.3333
#[8] 104.6667 126.6667 121.3333 119.3333 118.6667 133.3333 104.0000
#[15] 120.0000 120.6667 142.6667 128.6667       NA 112.0000

summary(df_obesidad$presion_sistolica) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#64.67  110.00  119.33  122.77  132.00  228.67     186 

########################
##
## Presión diastólica
##
########################

class(df_obesidad$presion_diastolica) # produce: numeric

unique(df_obesidad$presion_diastolica)[1:20] # produce:
#[1] 74.00000 61.33333 82.00000 86.66667 80.66667 72.66667 77.33333
#[8] 62.66667 66.66667 62.00000 75.33333 64.00000 69.33333 79.33333
#[15] 70.00000 72.00000 80.00000 83.33333 73.33333       NA

summary(df_obesidad$presion_diastolica) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   62.67   70.00   69.27   76.67  128.00     186 

########################
##
## Índice de masa
## corporal
##
########################

class(df_obesidad$imc) # produce: numeric

unique(df_obesidad$imc)[1:20] # produce:
#[1] 26.7 28.6 28.9 19.7 41.7 35.7 26.5 22.0 20.3 34.4 22.8 35.6 35.9
#[14] 23.6 30.1 26.2 31.0 24.7 31.2 38.3

summary(df_obesidad$imc) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14.10   23.90   27.70   28.92   32.40   82.90 

########################
##
## Circunferencia de la
##    Cintura
##
########################

class(df_obesidad$circunferencia_cintura) # produce: numeric

unique(df_obesidad$circunferencia_cintura)[1:20] # produce:
#[1] 100.0 107.6 109.2    NA 123.1 110.8  85.5  93.7  73.7 122.1  78.7
#[12] 110.2 107.4  99.3  90.3  94.6 107.7  88.6 114.8 117.8

summary(df_obesidad$circunferencia_cintura) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#55.50   86.40   97.00   98.41  108.10  177.90     269 

########################
##
## Diámetro Abdominal
##
########################

class(df_obesidad$diametro_abdominal) # produce: numeric

unique(df_obesidad$diametro_abdominal)[1:20] # produce:
#[1] 20.6 24.4 25.6   NA 29.1 26.7 19.9 20.0 14.5 29.7 15.1 22.8 25.8
#[14] 26.5 22.3 18.3 21.3 25.5 19.5 27.7

summary(df_obesidad$diametro_abdominal) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#12.50   19.10   22.10   22.51   25.40   40.10     378 

########################
##
##        Peso
##
########################

class(df_obesidad$peso) # produce: numeric
 
unique(df_obesidad$peso)[1:20] # produce:
#[1]  78.3  89.5  88.9  52.0 105.0  93.4  61.8  65.3  47.1 102.4  56.8
#[12]  79.7 109.4  93.5  80.9  92.2  92.9  62.7  96.0 104.0

summary(df_obesidad$peso) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#32.30   65.50   77.70   81.04   92.40  222.60

########################
##
##        Talla
##
########################

class(df_obesidad$talla) # produce: numeric

unique(df_obesidad$talla)[1:20] # produce:
#[1] 171.3 176.8 175.3 162.4 158.7 161.8 152.8 172.4 152.5 172.5 158.0
#[12] 166.2 175.2 161.4 185.0 175.1 172.9 173.1 159.4 164.7

summary(df_obesidad$talla) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#135.4   159.7   166.8   167.1   174.3   202.6 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn, el género, la edad y estado de embarazo en el
# examen
obesidad_examina <- df_obesidad %>%
  select(SEQN, presion_sistolica, presion_diastolica, imc,
         circunferencia_cintura, diametro_abdominal, peso, talla)
#glimpse(obesidad_examina[1:4,]) # produce:
#Rows: 4
#Columns: 8
#$ SEQN                   <dbl> 73557, 73558, 73559, 73561
#$ presion_sistolica      <dbl> 112.6667, 157.3333, 142.0000, 137.3333
#$ presion_diastolica     <dbl> 74.00000, 61.33333, 82.00000, 86.66667
#$ imc                    <dbl> 26.7, 28.6, 28.9, 19.7
#$ circunferencia_cintura <dbl> 100.0, 107.6, 109.2, NA

# Crear un archivo .cvs con la dataset de examina
write.csv(obesidad_examina,
          "eda-obesidad/data/obesidad/2_obesidad_examina.csv",
          row.names = FALSE)

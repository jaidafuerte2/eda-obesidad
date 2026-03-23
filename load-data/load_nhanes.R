######################################
##                                  ##
## Cargar las variables importantes ##
## del NHANES 2013-2014             ##
##                                  ##
######################################

# (Nota: no  olvidarse de filtrar al final por mayores de 18 años,
# mujeres embarazadas, el tiempo total del ayuno y valores faltantes 
# de imc. También incluir DIQ060U que es la unidad de tiempo que 
# está tomando insulina. También se podría incluir PAQ645 que es la 
# cantidad de minutos que se usa para desplzarse al trabajo o a la
# escuaela o de compras a pie o  bicicleta en un día típico

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

#################################
##
## Demographic
##
#################################

demographic <- read_csv(
  "eda-obesidad/data/demographic.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

######### SEQN ############

class(demographic$SEQN) # produce: [1] "integer"

unique(demographic$SEQN)[1:20] # produce:
#[1] 73557 73558 73559 73560 73561 73562 73563 73564 73565 73566 73567
#[12] 73568 73569 73570 73571 73572 73573 73574 73575 73576

#########  Género ##########

# RIAGENDR = Género
summary(demographic$RIAGENDR) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   2.000   1.505   2.000   2.000 


unique(demographic$RIAGENDR)[1:20] # produce:
# [1]  1  2 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

#1 = Male
#2 = Female

########## Edad ############

# RIDAGEYR = Edad (Años)

summary(demographic$RIDAGEYR) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    9.25   26.00   31.51   52.00   80.00 

unique(demographic$RIDAGEYR) # produce:
#[1] 69 54 72  9 73 56  0 61 42 65 26 76 10 33  1 16 32 18 12 38 50 23  7
#[24] 13 28  4 14 11 35  2  6 29  3 58 57 20 37 75 43 60 55 62 36 80 31 63
#[47] 71 67 64  8 46 44 19 70 59 25 39  5 24 30 49 45 51 77 78 66 79 47 48
#[70] 53 15 17 21 68 52 34 40 27 41 22 74


########## Estado de embarazo en el examen ############

# RIDEXPRG = Estado de embarazo en el examen
 
summary(demographic$RIDEXPRG) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.000   2.000   2.000   2.076   2.000   3.000    8866 

unique(demographic$RIDEXPRG) # produce: 
# [1] NA  2  1  3 # Donde 1 es embarazada, 2 es no embarazada, 3 es
# desconocido


########## SELECCIONAR ##########

# Seleccionar el código seqn, el género, la edad y estado de embarazo en el
# examen
demografico <- demographic |>
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG) 
head(demografico) # produce:
#   SEQN RIAGENDR RIDAGEYR
#1 73557        1       69
#2 73558        1       54
#3 73559        1       72
#4 73560        1        9
#5 73561        2       73
#6 73562        1       56

#################################
##
## Examination
##
#################################

examination <- read_csv("eda-obesidad/data/examination.csv",
                        guess_max = 10000,
                        show_col_types = FALSE)

########### Presión Arterial #############

# BPXDI = Presión sistólica (mm hg)
# BPXDI = Presión diátolica (mm hg)

# Presión sistólica (tiene 4 mediciones):

class(examination$BPXSY1) # produce:  integer
unique(examination$BPXSY1) # produce:
# [1] 122 156 140 108 136 160  NA 118 128 106 102 124  88  94 120 138  98
class(examination$BPXSY2) # produce:  integer
unique(examination$BPXSY2) # produce:
#[1] 114 160 140 102 134 158  NA 124 142 100 104  88  94 110 118 116 132
class(examination$BPXSY3) # produce:  integer
unique(examination$BPXSY3) # produce:
# [1] 102 156 146 104 142 154  NA 126 114 108 128  94  92 118 110 122 124
class(examination$BPXSY4) # produce: integer
unique(examination$BPXSY4) # produce:
#  [1]  NA 128 114  96 120 152 118  94  84 148 138 110 136 142 160 108  92

# Presión diastólica

class(examination$BPXDI1) # produce:  integer
unique(examination$BPXDI1) # produce:
#[1]  72  62  90  38  86  84  NA  80  74  78  60  44  68  54  56  58  70
class(examination$BPXDI2) # produce:  integer
unique(examination$BPXDI2) # produce:
#[1]  76  80  34  88  82  NA  72  78  62  42  54  60  66  74  64  70  48
class(examination$BPXDI3) # produce:  integer
unique(examination$BPXDI3) # produce:
# [1]  74  42  80  38  86  NA  82  72  76  66  40  68  58  54  64  70  78
class(examination$BPXDI4) # produce:  integer
unique(examination$BPXDI4) # produce:
#[1]  NA  64  58  60  82  70  72  50  86  96  74  68  66  94  56  62  80


examination <- examination %>%
  mutate(
    BPXSY_mean = rowMeans(select(., BPXSY1:BPXSY4), na.rm = TRUE),
    BPXDI_mean = rowMeans(select(., BPXDI1:BPXDI4), na.rm = TRUE)
  )
class(examination$BPXSY_mean) # produce: numeric
class(examination$BPXDI_mean) # produce: numeric

########## Índice de masa corporal ############

# BMXBMI = Índice de masa corporal (kg/m2)

summary(examination$BMXBMI) # produce: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#12.10   19.70   24.70   25.68   30.20   82.90     758

unique(examination$BMXBMI)[1:20] # produce:
#[1] 26.7 28.6 28.9 17.1 19.7 41.7   NA 35.7 26.5 22.0 20.3 17.4 34.4
#[14] 20.0 22.8 23.2 35.6 15.5 35.9 23.6

######## Circunferencia de la cintura (perímetro abdominal) #######

# BMXWAIST = Circunferencia de la cintura (cm)

summary(examination$BMXWAIST) # produce: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#40.20   71.20   87.80   87.27  102.80  177.90    1152 

unique(examination$BMXWAIST)[1:20] # produce:
# [1] 100.0 107.6 109.2  61.0    NA 123.1 110.8  85.5  93.7  73.7  65.6


######## Diámetro abdominal sagital #############

# BMDAVSAD = Diámetro abdominal sagital (cm)

summary(examination$BMDAVSAD) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.10   17.30   20.70   21.11   24.40   40.10    2595

unique(examination$BMDAVSAD)[1:20] # produce:
# [1] 20.6 24.4 25.6 14.9   NA 29.1 26.7 19.9 20.0 14.5 15.4 29.7 15.3

############## Peso ##############

# BMXWT = Peso (kg)

summary(examination$BMXWT) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#3.10   37.95   65.30   62.60   83.50  222.60      90 

unique(examination$BMXWT)[1:20] # produce:
#[1]  78.3  89.5  88.9  32.2  52.0 105.0   7.4  93.4  61.8  65.3  47.1
#[12]  31.9 102.4  41.7  50.1  56.8   9.4  67.3  79.7 109.4

######### Talla ##########

# BMXHT = Altura de pie (cm)

summary(examination$BMXHT) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#79.7   149.5   162.0   155.9   171.1   202.6     746 

unique(examination$BMXHT)[1:20] # produce: 
#[1] 171.30 176.80 175.30 137.30 162.40 158.70     NA 161.80 152.80
#[10] 172.40 152.50 135.50 172.50 145.40 158.38 158.00 170.40 166.20
#[19] 175.20 161.00

############# Seleccionar ###############

# Seleccionar presión sistólica y diastólica, índice de masa corporal,
# diámetro abdominal sagital, peso y talla y perímetro abdominal

examina <- examination |>
  select(SEQN, BMXWT, BMXHT, BMXBMI, BMXWAIST, BMDAVSAD, BPXSY_mean, 
         BPXDI_mean) 
head(examina) # produce:
#   SEQN BMXWT BMXHT BMXBMI BMXWAIST BMDAVSAD BPXSY_mean BPXDI_mean
#1 73557  78.3 171.3   26.7    100.0     20.6   62.88889     55.500
#2 73558  89.5 176.8   28.6    107.6     24.4   73.55556     63.250
#3 73559  88.9 175.3   28.9    109.2     25.6   75.33333     67.250
#4 73560  32.2 137.3   17.1     61.0     14.9   47.77778     40.250
#5 73561  52.0 162.4   19.7       NA       NA   75.22222     67.625
#6 73562 105.0 158.7   41.7    123.1     29.1   80.44444     70.500

#################################
##
## Diet
##
#################################

diet <- read_csv("eda-obesidad/data/diet.csv",
                 guess_max = 10000,
                 show_col_types = FALSE)

############ Sigues alguna dieta #################

# DRQSDIET - ¿Sigues una dieta especial?

summary(diet$DRQSDIET) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.919   2.000   9.000    1030 

unique(diet$DRQSDIET) # produce: [1]  2  1 NA  9
# Donde: 1 = si, 2 = no, 9 = no sé

############### Uso de sal ##################

# DBD100 - Con qué frecuencia se añade sal a la comida en la mesa

summary(diet$DBD100) # produce: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.919   2.000   9.000    1030 

unique(diet$DBD100) # produce: [1]  2  1 NA  3  9
# Donde: 1 = casi nunca, 2 = ocasionalmente, 3 = muy a menudo,
# 9 = no lo sé

############ Dieta baja en calorías ###############

# DRQSDT1 - Dieta para bajar de peso/Dieta baja en calorías

summary(diet$DRQSDT1) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1       1       1       1       1       1    9290

unique(diet$DRQSDT1) # produce: [1] NA  1

table(diet$DRQSDT1, useNA = "ifany") # produce:
#  1 <NA> 
#523 9290

# NOTA: Es mejor -aparentemente- no transformar los NA a cero. Parece
# que es mejor dejarlos como NA's

########## Dieta baja en grasas y colesterol #############

# DRQSDT2 - Dieta baja en grasas y colesterol

summary(diet$DRQSDT2) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2       2       2       2       2       2    9682 

unique(diet$DRQSDT2) # produce: [1] NA  2

table(diet$DRQSDT2, useNA = "ifany") # produce:
#  2 <NA> 
#131 9682

############## Dieta baja en sal #################

# DRQSDT3 - Dieta baja en sal

summary(diet$DRQSDT3) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3       3       3       3       3       3    9692 

unique(diet$DRQSDT3) # produce: [1] NA  3

########### Dieta sin azúcar/baja en azúcar ################

# DRQSDT4 = Dieta sin azúcar/baja en azúcar

summary(diet$DRQSDT4) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   4       4       4       4       4       4    9776

unique(diet$DRQSDT4) # produce: [1] NA  4

table(diet$DRQSDT4, useNA = "ifany") # produce:
# 4 <NA> 
#37 9776 

############ Dieta rica en fibra ##############

# DRQSDT6 = Dieta rica en fibra

summary(diet$DRQSDT6) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  6       6       6       6       6       6    9809  

unique(diet$DRQSDT6) # produce: [1] NA  6

table(diet$DRQSDT6, useNA = "ifany") # produce:
#6 <NA> 
#4 9809 
######## Dieta para diabéticos  ###########

# DRQSDT7 = Dieta para diabéticos

summary(diet$DRQSDT6) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   6       6       6       6       6       6    9809 

unique(diet$DRQSDT6) # produce: [1] NA  6

table(diet$DRQSDT6, useNA = "ifany") # produce:
#6 <NA> 
#4 9809

########## Dieta baja en carbohidratos ##############

#DRQSDT9 = Dieta baja en carbohidratos

summary(diet$DRQSDT9) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   9       9       9       9       9       9    9746 

unique(diet$DRQSDT9) # produce: [1] NA  9

table(diet$DRQSDT9, useNA = "ifany") # produce:
# 9 <NA> 
#67 9746 

############## Dieta alta en proteínas ###############

# DRQSDT10 = Dieta alta en proteínas

summary(diet$DRQSDT10) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   10      10      10      10      10      10    9791 

unique(diet$DRQSDT10) # produce: [1] NA 10

table(diet$DRQSDT10, useNA = "ifany") # produce:
#10 <NA> 
#22 9791 

############# Dieta sin gluten/para celíacos ###############

# DRQSDT11 - Dieta sin gluten/para celíacos

summary(diet$DRQSDT11) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11      11      11      11      11      11    9782 

unique(diet$DRQSDT11) # produce: [1] NA 11

table(diet$DRQSDT11, useNA = "ifany") # produce:
#11 <NA> 
#31 9782 

############ Calorías ##############

# DR1TKCAL = Energy (kcal)

summary(diet$DR1TKCAL) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 117    1310    1794    1965    2430   12108    1282 

unique(diet$DR1TKCAL)[1:20] # produce:
#[1] 1574 5062 1743 1490 1421 1785   NA 2585 1580 2021 3145 2220 1076
#[14] 1546 5621  868 1012 3194  955 1711

###############  Proteína ##############

# DR1TPROT = Proteína (g)

summary(diet$DR1TPROT) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   45.78   66.05   74.54   93.86  869.49    1282

unique(diet$DR1TPROT)[1:20] # produce:
#[1]  43.63 338.13  64.61  77.75  55.24  55.11     NA  91.15  42.26
#[10]  38.09 139.21  76.40  39.40  30.65 274.72  21.60  48.91 144.92
#[19]  81.61  81.54

sum(is.na(diet$DR1TPROT)) # produce: 1282

########### Carbohidratos ############

# DR1TCARB = Carbohidratos (g)

summary(diet$DR1TCARB) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#8.67  158.72  223.30  242.74  301.17 1423.87    1282

unique(diet$DR1TCARB)[1:20] # produce: 
#[1] 239.59 423.78 224.39 162.92 178.20 189.59     NA 300.16 226.32
#[10] 216.59 227.63 299.48 150.93 178.12 575.61 124.92 145.46 385.05
#[19] 113.89 211.42

sum(is.na(diet$DR1TCARB)) # produce: 1282

########### Azúcares totales #############

# DR1TSUGR = Azúcares totales (g)

summary(diet$DR1TSUGR) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.13   61.18   93.98  109.30  138.72 1115.50    1282 

unique(diet$DR1TSUGR)[1:20] # produce:
#[1] 176.47  44.99 102.90  80.58  87.78  81.75     NA 128.23  85.92
#[10]  98.01  83.91 146.21  61.42 115.01 215.83  76.49  44.67 158.45
#[19]   5.31  82.71

sum(is.na(diet$DR1TSUGR)) # produce: 1282

################# Fibra dietética ###################

# DR1TFIBE = Fibra dietética (g)

summary(diet$DR1TFIBE) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    8.50   13.10   15.28   19.80  136.30    1282 

unique(diet$DR1TFIBE)[1:20] # produce:
#[1] 10.8 16.7  9.9 10.6 12.3 22.6   NA 24.8 13.1  7.0 37.6 10.7 14.9
#[14]  5.8   NA 36.0  3.9  4.0 33.4 15.6

sum(is.na(diet$DR1TFIBE)) # produce: 1282

################### Grasa total #####################

# DR1TTFAT - Grasa total (g)

summary(diet$DR1TTFAT) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   43.72   66.23   75.10   94.99  548.38    1282 
   
unique(diet$DR1TTFAT)[1:20] # produce:
#[1]  52.81 124.29  65.97  58.27  55.36  93.92     NA  91.03  41.31
#[10]  82.95 172.21  81.19  39.68  78.54 249.49  32.08  25.14 121.74
#[19]  18.24  60.80

sum(is.na(diet$DR1TTFAT)) # produce: 1282

############## Ácidos grasos saturados totales ################

# DR1TSFAT - Ácidos grasos saturados totales (g)

summary(diet$DR1TSFAT) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   13.82   21.22   24.76   31.93  177.47    1282

unique(diet$DR1TSFAT)[1:20] # produce:
#[1] 17.819 53.408 25.263 23.511  4.479 22.155     NA 25.989 10.911
#[10] 28.438 66.642 26.667 14.153 19.069 80.879 15.026  7.143 48.486
#[19]  6.481 25.869

sum(is.na(diet$DR1TSFAT)) # produce: 1282

############## Ácidos grasos monoinsaturados totales #############

# DR1TMFAT - Ácidos grasos monoinsaturados totales (g)

# Son los que están en el aguacate y el aceite de oliva

summary(diet$DR1TMFAT) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   14.49   22.30   25.83   32.85  221.67    1282 

unique(diet$DR1TMFAT)[1:20] # produce:
#[1] 18.493 35.481 20.902 16.986 26.216 40.013     NA 20.970 14.878
#[10] 24.300 57.929 26.892 14.336 18.599 92.405  9.990  8.835 42.117
#[19]  4.534 19.464

sum(is.na(diet$DR1TMFAT)) # produce: 1282

########### Ácidos grasos poliinsaturados totales ############

# DR1TPFAT - Ácidos grasos poliinsaturados totales (g)

# Son los que están en los aceites de cocina y los omega 3

summary(diet$DR1TPFAT) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    9.05   14.44   17.39   22.39  182.46    1282 

unique(diet$DR1TPFAT)[1:20] # produce:
#[1]  8.829 20.505 12.953  9.617  1.263 23.550     NA 37.652 13.600
#[10] 24.586 32.571 17.215  8.762 35.671     NA 50.794  3.955  6.313
#[19] 16.147  3.264

sum(is.na(diet$DR1TPFAT)) # produce: 1282

############### Colesterol #################

# DR1TCHOL - Colesterol (mg)

summary(diet$DR1TCHOL) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0   107.0   193.0   262.4   352.0  3515.0    1282 

unique(diet$DR1TCHOL)[1:20] # produce:
#[1]  209 2584   88  187   41  534   NA  152   39   81  629  408   80
#[14]   97  687   49  111  361  170  149

sum(is.na(diet$DR1TCHOL)) # produce: 1282

############### OMEGA 6 Ácido linoleico #################

# DR1TP182 - PFA 18:2 (Octadecadienoico) (g)

# ácido graso poliinsaturado Presente en los aceites de cocina, es el
# principal omega 6 de la dieta

summary(diet$DR1TP182) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    7.91   12.74   15.39   19.87  170.28    1282 

unique(diet$DR1TP182)[1:20] # produce:
#[1]  7.932 15.483 11.705  8.466  1.048 21.036     NA 33.709 11.887
#[10] 21.955 27.340 15.391  7.752 31.042 45.852  3.460  5.481 14.023
#[19]  2.618  9.591

############# OMEGA 3 EPA ##############

# DR1TP205 - PFA 20:5 (ácido eicosapentaenoico) (g)

# Presente en los pesacados azules: salmón, atún, sardinas. Es muy
# antiinflamatorio

summary(diet$DR1TP205) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00000 0.00200 0.00600 0.02621 0.01400 2.68700    1282 

unique(diet$DR1TP205)[1:20] # produce:
#[1] 0.001 1.003 0.007 0.000 0.005    NA 0.004 0.012 0.002 0.015 0.006
#[12] 0.008 0.071 0.045 0.009 0.011 0.003 0.010 0.024 0.297

############### OMEGA 3 DHA ####################

# DR1TP226 - PFA 22:6 (ácido docosahexaenoico) (g)

# Presente en los pescados azules: sardina, atún y salmón

summary(diet$DR1TP226) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00000 0.00200 0.00900 0.05674 0.05000 4.33700    1282 

unique(diet$DR1TP226)[1:20] # produce:
#[1] 0.010 1.139 0.002 0.004 0.000 0.063    NA 0.011 0.056 0.031 0.005
#[12] 0.012 0.001 0.009 0.014 0.181 0.049 0.040 0.030 0.028

############# Seleccionar ###############

# Seleccionar los distintos tipos de dietas y nutrientes
dieta <- diet |>
  select(DRQSDT1, DRQSDT2, DRQSDT3, DRQSDT4, DRQSDT6, DRQSDT7, DRQSDT9, 
         DRQSDT10, DRQSDT11, DRQSDIET, DBD100, DR1TKCAL, DR1TPROT, DR1TCARB,
         DR1TSUGR, DR1TFIBE, DR1TTFAT, DR1TSFAT, DR1TMFAT,
         DR1TPFAT, DR1TCHOL, DR1TP182, DR1TP205, DR1TP226) 
head(dieta) # produce:
#  DRQSDT1 DRQSDT2 DRQSDT3 DRQSDT4 DRQSDT6 DRQSDT7 DRQSDT9 DRQSDT10
#1      NA      NA      NA      NA      NA      NA      NA       NA
#2      NA      NA      NA      NA      NA      NA      NA       NA
#3      NA      NA      NA      NA      NA       7      NA       NA

#################################
##
## Labs
##
#################################

labs <- read_csv("eda-obesidad/data/labs.csv",
                 guess_max = 10000,
                 show_col_types = FALSE)

############# Insulina en ayunas ################

# LBXIN - Insulin (uU/mL)

summary(labs$LBXIN) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.14    6.08    9.47   13.53   15.35  682.48    6720 

unique(labs$LBXIN)[1:20] # produce:  
#[1]    NA  5.83  6.12 14.91  3.85  6.05  6.14 16.15 10.92  6.08 21.11
#[12] 20.93 57.77 68.63 17.47  3.24  7.16  9.86  4.33 12.06

############ Glucosa de bioquímica básica en ayunas ###############

# LBXSGL - Glucosa, suero refrigerado (mg/dL)

summary(labs$LBXSGL) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  49      88      94     101     102     403      41 

unique(labs$LBXSGL)[1:20] # produce:
#[1] 183 104 107  81  83  97  94  92 105 101  84  70 113 128 102 106  87
#[18]  88 130  80

############# Hemoglobina glicosilada #################

# LBXGH - Glicohemoglobina (%)

summary(labs$LBXGH) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#3.50    5.20    5.40    5.64    5.80   15.40       2 
   
unique(labs$LBXGH)[1:20] # produce:
#[1] 8.9 4.9 5.5 5.2 5.1 5.3 5.8 5.0 5.7 5.9 5.6 6.0 7.1 4.7 4.3 5.4 6.6
#[18] 7.6 6.1 8.1

############## Gamma glutamil transferasa GGT #################

# LBXSGTSI - Gamma glutamil transferasa (U/L)

# (Nota: este valor es importante para sacar el índice de hígado 
# graso)
summary(labs$LBXSGTSI) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#4.00   13.00   17.00   25.52   26.00  462.00      42 

unique(labs$LBXSGTSI)[1:20] # produce:
#[1] 13 31 17 15 30 19 20 32 43 50 86 21 46 12 23 38 18 34 16 11

############ Colesterol total ################

# LBXTC - Total Cholesterol( mg/dL)

summary(labs$LBXTC) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 69.0   153.0   179.0   182.3   207.0   612.0      30 

unique(labs$LBXTC)[1:20] # produce:
#[1] 126 201 168 154 182 225 202 159 151 161 200 238 162 195 140 210 165
#[18] 271 127 206

########### Colesterol LDL ################

# LBDLDL - Colesterol LDL (mg/dL)

summary(labs$LBDLDL) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#14.0    82.0   104.0   106.8   128.0   375.0      66

unique(labs$LBDLDL)[1:20] # produce:
#[1]  56 101  97  67  82 119 159 105 110  78 103  89 151  92  70 142  71
#[18] 137  94 183

############ Colesterol HDL directo ##############

# LBDHDD - Colesterol HDL directo (mg/dL)

summary(labs$LBDHDD) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   43.00   51.00   53.66   62.00  173.00      30 

unique(labs$LBDHDD)[1:20] # produce:
#[1] 60 85 58 96 61 33 55 78 34 56 30 49 72 57 75 43 47 42 51 38

############### Triglicéridos ################

# LBXTR - Triglicéridos (mg/dL)

summary(labs$LBXTR) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#13.0    60.0    88.0   111.4   131.0  4233.0      32 

unique(labs$LBXTR)[1:20] # produce:
#[1]  51  75  64  24  57 148  93  73  87 139 312  77  67  78  49 108  62
#[18] 226  88  47

############# Neutrófilos ################

# LBDNENO - Segmented neutrophils num (1000 cell/uL)

summary(labs$LBDNENO) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.400   2.800   3.700   3.971   4.700  22.400      13 
  
unique(labs$LBDNENO)[1:20] # produce:
#[1] 4.9 4.5 3.0 4.2 2.7 3.7 2.8 7.1 3.8 2.4 1.7 5.0 7.8 4.7 3.6 5.4 5.2
#[18] 4.8 5.5 4.4

############### Lonfocitos #################

# LBDLYMNO - Número de linfocitos (1000 células/µL)

summary(labs$LBDLYMNO) # produce: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.400   1.600   2.000   2.088   2.400  12.000      13 

unique(labs$LBDLYMNO)[1:20] # produce:
#[1] 1.0 1.4 1.6 1.3 1.8 3.2 1.7 2.7 2.5 2.3 0.9 0.8 2.1 3.3 2.4 2.2 2.9
#[18] 1.9 2.0 1.5

############### Seleccionar #################

# Seleccionar los exámenes de laboratorio más importantes
# Seleccionar los distintos tipos de dietas y nutrientes
laboratorio <- labs |>
  select(LBXIN, LBXSGL, LBXSGTSI, LBXTC, LBDLDL, LBDHDD, LBXTR, 
         LBDNENO, LBDLYMNO) 
head(laboratorio) # produce:
#   LBXIN LBXSGL LBXSGTSI LBXTC LBDLDL LBDHDD LBXTR LBDNENO LBDLYMNO
#1  5.83    183       13   126     56     60    51     4.9      1.0
#2  6.12    104       31   201    101     85    75     4.5      1.4
#3 14.91    107       17   168     97     58    64     3.0      1.6
#4  3.85     81       13   168     67     96    24     4.2      1.3


#################################
##
## Questionnaire
##
#################################

questionnaire <- read_csv("eda-obesidad/data/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

####### Le dijeron que tenía presión arterial alta #######

# BPQ030 - Le dijeron que tenía presión arterial alta - 2+ veces

summary(questionnaire$BPQ030) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.226   1.000   9.000    8001

unique(questionnaire$BPQ030) # produce: [1]  1 NA  2  9
# Donde: 1 es sí, 2 es no y 9 no lo sé

######### Medicamentos para la presión #############

# BPQ050A - Actualmente estoy tomando medicamentos para presión

summary(questionnaire$BPQ050A) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    1.00    1.00    1.13    1.00    9.00    8360 

unique(questionnaire$BPQ050A) # produce: [1]  2 NA  1  9
# Donde: 1 es sí, 2 es no, 9 es no lo sé 

########### Colesterol alto ############

# BPQ080 - El médico le dijo que tenía el colesterol alto.

summary(questionnaire$BPQ080) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.724   2.000   9.000    3711 

unique(questionnaire$BPQ080) # produce: [1]  1 NA  2  9
# Donde: 1 es sí,  2 es no, 9 es no lo sé

########### Medicamentos colesterol ################

# BPQ100D - Ahora estoy tomando medicamentos recetados

summary(questionnaire$BPQ100D) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.236   1.000   9.000    8725 

unique(questionnaire$BPQ100D) # produce: [1]  1 NA  2  9
# Donde 1 es sí, 2 es no y 9 es no lo sé

############ El médico le dijo que tiene diabetes #############

# DIQ010 - El médico le dijo que tiene diabetes

summary(questionnaire$DIQ010) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.000   2.000   1.948   2.000   9.000     406 

unique(questionnaire$DIQ010) # produce: [1]  1  2 NA  3  9  7
# (Nota: 3, 9 y 7 que son borderline, refused y don't know quedan 
# como datos faltantes). 1 es sí, 2 es no.

##### ¿Alguna vez te han dicho que tienes prediabetes? ####

# DIQ160 - ¿Alguna vez te han dicho que tienes prediabetes?

summary(questionnaire$DIQ160) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.965   2.000   9.000    3888

unique(questionnaire$DIQ160) # produce: [1] NA  2  1  9
# Donde 1 es sí, 2 es no , 9 es no sé

################ Usa insulina #######################

# DIQ050 - Tomando insulina ahora

summary(questionnaire$DIQ050) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.979   2.000   9.000     407 

unique(questionnaire$DIQ050) # produce: [1]  1  2 NA  7  9
# Donde: 1 es sí, 2 es no, 7 es rechazado y 9 es no lo sé

############## Tiempo usando insulina #################

# DID060 - ¿Cuánto tiempo lleva tomando insulina?

summary(questionnaire$DID060) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    3.00    8.00   20.59   15.00  999.00    9955 

unique(questionnaire$DID060) # produce:
#[1]   5   1  16  NA  17   6  28   8   3  29 666   9  10  25  14  20  15
#[18]  18   2   4  31  12  30   7  13  11  23  32  46  39  21  43 999  26
#[35]  19  40
# Donde: 1-46 : puede ser años o meses, 666 = menos de un mes, 777 =
# rechazado, 999 es no lo sé

############## Consumo de productos lácteos #################

# DBQ197 - Consumo de productos lácteos en los últimos 30 días

summary(questionnaire$DBQ197) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   1.000   3.000   2.111   3.000   7.000     406 

unique(questionnaire$DBQ197) # produce: [1]  1  3  0 NA  2  4  7
# Donde: 0 es nunca, 1 es ocasional, 2 es medio, 3 frecuente, 4 es
# variado y 7 rechazado

############ Consumo de leche entera ################

# DBQ223A - Usted bebe leche entera o regular

summary(questionnaire$DBQ223A) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   10.00   10.00   10.41   10.00   99.00    7379 

unique(questionnaire$DBQ223A) # produce: [1] 10 NA 99 77
# Donde: 10 es entera, 77 es rechazado y 99 es no lo sé

########### Consumo de leche desnatada #############

# DBQ223D - Usted bebe leche desnatada/sin grasa

summary(questionnaire$DBQ223D) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13      13      13      13      13      13    9429 

unique(questionnaire$DBQ223D) # produce: [1] NA 13
# Donde: 13 es sí.

############### Actividad laboral vigorosa ##################

# PAQ605 - Actividad laboral vigorosa

# Se refiere a actividades físicas que provocan grandes aumentos en la
# frecuencia cardíaca o respiratoria y se realiza durante al menos
# 10 minutos de forma continua. (incluye queahaceres domésticos)

summary(questionnaire$PAQ605) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.837   2.000   7.000    3027 

unique(questionnaire$PAQ605) # produce: [1]  2 NA  1  7
# Donde: 1es sí, 2 es no y 7 es rechazado

########### Número de días de trabajo intenso ##############

# PAQ610 - Número de días de trabajo intenso

summary(questionnaire$PAQ610) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   4.000   4.077   5.000  99.000    9003 

unique(questionnaire$PAQ610) # produce:
#[1] NA  5  3  2  1  4  6  7 99
# Donde: los números se refieren a los días de la semana y 99 es 
# no los sé

######### Minutos de trabajo de intensidad vigorosa ################

# PAD615 - Minutos de trabajo de intensidad vigorosa

# Se refiere a minutos al día de actividad vigorosa en un día típico

summary(questionnaire$PAD615) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0    60.0   120.0   187.5   240.0  9999.0    9007 

unique(questionnaire$PAD615) # produce:
#[1]   NA  240   30  480  120  180   60  360  420  540  300   40   10
#[14]   20   90  600   45   75   12  160   35  720   15   25  660  840
#[27] 1080  130 9999   21
# Donde : 9999 es no lo sé

########## Actividad laboral moderada ############

# PAQ620 - Actividad laboral moderada

# Se refiere a actividades físicas que provocan pequeños aumentos en la
# frecuencia cardíaca o respiratoria y se realiza durante al menos
# 10 minutos de forma continua. Por ejemplo: caminar a paso ligero 
# o cargar objetos ligeros

summary(questionnaire$PAQ620) # produce: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    1.00    2.00    1.68    2.00    9.00    3027 

unique(questionnaire$PAQ620) # produce:
# Donde: 1 es sí, 2 es no, 7 es rechazado y 9 es no lo sé

########### Número de días de trabajo moderado ##############

# PAQ625 - Número de días de trabajo moderado

summary(questionnaire$PAQ625) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   3.000   5.000   4.359   5.000  99.000    7868 

(questionnaire$PAQ625)[1:20] # produce:
#[1] NA  1  7 NA  2 NA NA NA NA  5 NA NA NA NA  3 NA NA NA NA NA
# Donde los números son días de la semana, 77 es rechazado y 99 es
# no lo sé

########### Minutos de trabajo de intensidad moderada ############

# PAD630 - Minutos de trabajo de intensidad moderada

# Se refiere a minutos de actividad física moderada en un día 
# típico

summary(questionnaire$PAD630) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0    45.0   120.0   152.8   180.0  9999.0    7876 

unique(questionnaire$PAD630) # produce:
#[1]   NA   10   30  480  120   60  300  240  360   45   20  180   15
#[14]   31   40  420  600   25  720   90   16   55  150  540   70   35
#[27]   50   65 9999  160   29   80   19  660   12  900
# Donde: 7777 es rechazado y 9999 es no lo sé

############# Caminar o andar en bicicleta ################

# PAQ635 - Caminar o andar en bicicleta

# Se refiere a la forma habitual de desplazarse al trabajo o la 
# escuela o de compras durante al menos 10 minutos para ir y venir
# de los lugares.

summary(questionnaire$PAQ635) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   1.000   2.000   1.715   2.000   9.000    3028 

unique(questionnaire$PAQ635) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es  no y 9 es no lo sé

########## Número de días caminando o en bicicleta ###########

# PAQ640 - Número de días caminando o en bicicleta

summary(questionnaire$PAQ640) # produce:
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   3.000   5.000   4.798   7.000   7.000    8128 

unique(questionnaire$PAQ640) # produce: [1] NA  6  7  5  2  3  1  4
# Donde los números de refieren a los números de días

######### Actividades recreativas vigorosas #############

# PAQ650 - Actividades recreativas vigorosas

# Se refiere a hacer deportes (no actividad laboral ni transporte) o
# actividad física de alta intensidad que provoque un gran aumento
# de la frecuencia respiratoria o cardíaca como correr o jugar 
# baloncesto por al menos 10 minutos

summary(questionnaire$PAQ650) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.713   2.000   9.000    3028

unique(questionnaire$PAQ650) # produce: [1]  2 NA  1  9
# Dónde 1 es sí, 2 es no y 9 es no lo sé

######### Días de actividades recreativas vigorosas ############

# PAQ655 - Días de actividades recreativas vigorosas

summary(questionnaire$PAQ655) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   3.000   3.663   5.000  99.000    8117

unique(questionnaire$PAQ655) # produce: 
#[1] NA  6  2  5  4  3  7  1 99
# Donde los números representan días de la semana y 99 es no lo sé

######### Minutos de actividades recreativas vigorosas ###########

# PAD660 - Minutos de actividades recreativas vigorosas

# Se refiere a minutos de actividades recreativas de alta intensidad
# realizadas en un día típico que no son trabajo ni transporte

summary(questionnaire$PAD660) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   40.00   60.00   91.97  120.00 9999.00    8120

unique(questionnaire$PAD660)[1:20] # produce:
# [1]  NA 120  40 180  60  70  20  90 240  30  45  15  10  75  25  35  11
#[18] 150  14 360

########## Actividades recreativas moderadas ############

# PAQ665 - Actividades recreativas moderadas

# Se refiere a hacer deportes (no actividad laboral ni transporte) o
# actividad física de alta intensidad que provoque un aumento ligero
# de la frecuencia respiratoria o cardíaca como andar a paso ligero, 
# ciclear o jugar voleibol por al menos 10 minutos

summary(questionnaire$PAQ665) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.574   2.000   9.000    3030 

unique(questionnaire$PAQ665) # produce: [1]  2  1 NA  9
# Donde 1 es sí, 2 es no y 9 es no lo sé

######### Días de actividades recreativas moderadas ###########

# PAQ670 - Días de actividades recreativas moderadas

summary(questionnaire$PAQ670) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   3.000   3.636   5.000  99.000    7115

unique(questionnaire$PAQ670) # produce:
# [1] NA  1  4  2  7  3  5  6 99
# Donde los números son número de días a la semana y 99 es no lo sé

######### Minutos de actividades recreativas moderadas ###########

# PAD675 - Minutos de actividades recreativas moderadas

# Se refiere a minutos de actividades recreativas de poca intensidad
# realizadas en un día típico que no son trabajo ni transporte

summary(questionnaire$PAD675) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   30.00   60.00   63.19   60.00  900.00    7118 

unique(questionnaire$PAD675)[1:20] # produce:
#[1]  NA 180  30  60  50  20 120  40  45 300  10  25   0  15  39 480  90
#[18] 240 360  75
# Donde 7777 es rechazado y 9999 es no lo sé

############# Minutos de actividad sedentaria ############

# PAD680 - Minutos de actividad sedentaria

# Se refiere al tiempo que una persona pasa sentada en trabajo,
# quehaceres domésticos, bus, etc. No incluye tiempo durmiendo

summary(questionnaire$PAD680) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   300.0   480.0   478.5   600.0  9999.0    3036

unique(questionnaire$PAD680) # produce:
#[1]  600  540  300   NA  480  360   60   30  120  240  720 1080  960
#[14]  420  180  840  780  660   20  900  150 9999    1 1020   15    5
#[27]   90 7777 1140   45   81    0   25  105   10 1200  490
# Donde: 7777 es rechazado y 9999 es no lo sé

######## Días de actividad física de al menos 60 min. #########

# PAQ706 - Días de actividad física de al menos 60 min.

# Actividad física que aumentó el ritmo cardíaco o respiratorio 
# en algún momento de al menos 60 minutos

summary(questionnaire$PAQ706) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   4.000   7.000   5.802   7.000  99.000    7186 

unique(questionnaire$PAQ706) # produce:
# [1] NA  5  4  7  1  3  6  2 99  0
# Donde 99 es no lo sé

########### Horas viendo televisión o videos ############

# PAQ710 - Horas viendo televisión o videos en los últimos 30 días

summary(questionnaire$PAQ710) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   1.000   2.000   2.516   4.000  99.000     727 

unique(questionnaire$PAQ710) # produce:
# [1]  2  4  3  1  5 NA  0  8 99 77
# Donde 0 es menos de una hora, 5 es 5 horas, 8 es no veo tv ni videos,
# 99 es no lo sé y 77 es rechazado

########### Horas de uso de la computadora  ##############

# PAQ715 - Horas de uso de la computadora en los últimos 30 días

# Se refiere al tiempo de uso de computadoras y videojuegos por fuera
# de la escuela

summary(questionnaire$PAQ715) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    0.00    2.00    3.16    8.00    8.00     727 

unique(questionnaire$PAQ715) # produce:
#[1]  8  0  2  1 NA  5  3  4

######## Toma aspirina en dosis baja por prescripción #########

# RXQ515 - ¿Siguió las indicaciones y tomó aspirina en dosis bajas?

summary(questionnaire$RXQ515) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.386   2.000   4.000    8891 

unique(questionnaire$RXQ515) # produce: [1]  1  3 NA  2  4
# Donde 1 es sí, 2 es no , 3 a veces y 4 es dejé de usar por los 
# efectos secundarios

######### Toma aspirina en dosis bajas por su cuenta ############

# RXQ520 - ¿Toma aspirina en dosis bajas por su cuenta?

summary(questionnaire$RXQ520) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.962   2.000   9.000    7644 

unique(questionnaire$RXQ520) # produce: [1] NA  2  1  9  7

########## Dosis de aspirina en miligramos ###############

# RXD530 - Dosis de aspirina en miligramos (mg)

summary(questionnaire$RXD530) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  20      81      81    1172      81   99999    9042 

unique(questionnaire$RXD530) # produce:
#[1]    81    NA   325    20 99999   400   500   150   120    90    75
#[12]   200   250   163
# Donde 99999 es no lo sé

############ ¿Cuántas horas duermes? ################

# SLD010H - ¿Cuántas horas duermes?

summary(questionnaire$SLD010H) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.00    6.00    7.00    7.05    8.00   99.00    3714

unique(questionnaire$SLD010H) # produce:
#[1]  7  9  8 NA  5  6 10  4  3 12 11  2 99
# Donde: 12 es 12 o más horas, y 99 es no lo sé

########### Trastorno del sueño por médico ###############

# SLQ060 - ¿Alguna vez le ha dicho su médico que tiene un trastorno del sueño?

summary(questionnaire$SLQ060) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.925   2.000   9.000    3711 

unique(questionnaire$SLQ060) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no, 9 es no lo sé

############# Estatura Autoinformada #################  

# WHD010 - Estatura actual autoinformada (pulgadas)

summary(questionnaire$WHD010) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#48.0    63.0    66.0   163.2    69.0  9999.0    3736 

unique(questionnaire$WHD010) # produce:
#[1]   69   71   70   NA   67   64   61   60   62   68   66   74   63
#[14]   75   72   65   73   76   77 9999   59   58   57   55   79   54

######## Peso actual autoinformado #############

# WHD020 - Peso actual autoinformado (libras)

summary(questionnaire$WHD020) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   143.0   170.0   254.2   200.0  9999.0    3745 

unique(questionnaire$WHD020)[1:20] # produce:
#[1]  180  200  195   NA  120  235  212  137  165  105  224  128  145
#[14] 9999  175  104  205  170  240  230

############  Peso hace un año ##############
 
# WHD050 - Peso autoinformado - hace 1 año (libras)

summary(questionnaire$WHD050) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#75.0   140.0   170.0   312.5   200.0  9999.0    3753

(questionnaire$WHD050)[1:20] # produce:
#[1] 210 160 195  NA 150 240  NA 212 190 137 165 110  NA  NA 222  NA  NA
#[18] 123  NA 165

############### Cambio de peso intencional ###############

# WHQ060 - Cambio de peso intencional

summary(questionnaire$WHQ060) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    1.00    1.00    1.37    2.00    9.00    8936 

unique(questionnaire$WHQ060) # produce: [1]  1 NA  2  9
# Donde 1 es sí, 2 es no y 9 es no sé

############## Intentó perder peso el año pasado ##############

# WHQ070 - Intentó perder peso el año pasado

summary(questionnaire$WHQ070) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.641   2.000   9.000    4501 

unique(questionnaire$WHQ070) # produce: [1] NA  2  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

############# Comí menos para bajar de peso ################

# WHD080A - Comí menos para bajar de peso

summary(questionnaire$WHD080A) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  10      10      10      10      10      10    8482 
unique(questionnaire$WHD080A) # produce: [1] NA 10
# Donde 10 es sí

########## Cambié a alimentos con menos calorías #############

#WHD080B - Cambié a alimentos con menos calorías.

summary(questionnaire$WHD080B) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11      11      11      11      11      11    9292

unique(questionnaire$WHD080B) # produce: [1] NA 11

########### Comí menos grasa para bajar de peso ###############

# WHD080C - Comí menos grasa para bajar de peso

summary(questionnaire$WHD080C) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  12      12      12      12      12      12    9373 
     
unique(questionnaire$WHD080C) # produce: [1] NA 12

############### Hacer ejercicio para bajar de peso ##############

# WHD080D - Hacer ejercicio para bajar de peso

summary(questionnaire$WHD080D) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13      13      13      13      13      13    8345 

unique(questionnaire$WHD080D) # produce: [1] NA 13
# Donde 13 es sí

############### Comidas Omitidas ##################

# WHD080E - Comidas omitidas

summary(questionnaire$WHD080E) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  14      14      14      14      14      14    9776

unique(questionnaire$WHD080E) # produce: [1] NA 14

####### Consumió alimentos o productos dietéticos. ######## 

# WHD080F - Consumió alimentos o productos dietéticos.

summary(questionnaire$WHD080F) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  15      15      15      15      15      15    9942 

unique(questionnaire$WHD080F) # produce: [1] NA 15

######## Se unió a un programa de pérdida de peso ########

# WHD080H - Se unió a un programa de pérdida de peso.

summary(questionnaire$WHD080H) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  17      17      17      17      17      17   10061 

unique(questionnaire$WHD080H) # produce: [1] NA 17

######## Tomar agua para adelgazar ########## 

# WHD080M - Bebió mucha agua 

summary(questionnaire$WHD080M) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  34      34      34      34      34      34    9142 

unique(questionnaire$WHD080M) # produce: [1] NA 34

######## Comer menos carbohidratos para adelgazar ##########

# WHD080O - Consumió menos carbohidratos

summary(questionnaire$WHD080O) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  41      41      41      41      41      41    9486 

unique(questionnaire$WHD080O) # produce: [1] NA 41

########## Fumar para adelgazar ###########

# WHD080P - Empezó a echar humo o volvió a echar humo

summary(questionnaire$WHD080P) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  42      42      42      42      42      42   10156 

unique(questionnaire$WHD080P) # produce: [1] NA 42

######### Comer vegetales para adelgazar #############

# WHD080Q - Comió más frutas, verduras y ensaladas.

summary(questionnaire$WHD080Q) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#43      43      43      43      43      43    9054 

unique(questionnaire$WHD080Q) # produce: [1] NA 43

########### Comer menos azúcar para adelgazar #############

# WHD080S - Comió menos azúcar, caramelos y dulces.

summary(questionnaire$WHD080S) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  45      45      45      45      45      45    9232 

unique(questionnaire$WHD080S) # produce: [1] NA 45

######## Comer menos comida chatarra para adelgazar ############

# WHD080T - Comió menos comida chatarra o comida rápida.

summary(questionnaire$WHD080T) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  46      46      46      46      46      46    9179 

unique(questionnaire$WHD080T) # produce: [1] NA 46

##### ¿Alguna vez el médico le dijo que tenía sobrepeso? #####

# MCQ080 - ¿Alguna vez el médico le dijo que tenía sobrepeso?

summary(questionnaire$MCQ080) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.669   2.000   9.000    3711

unique(questionnaire$MCQ080) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé

##### ¿Alguna vez te han dicho que tienes celiaquía? #######

# MCQ082 - ¿Alguna vez te han dicho que tienes celiaquía?

summary(questionnaire$MCQ082) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1       2       2       2       2       9    1603 

unique(questionnaire$MCQ082) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

######## MCQ086 - ¿Sigues una dieta sin gluten? #########

summary(questionnaire$MCQ086) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.988   2.000   9.000    1603 

unique(questionnaire$MCQ086) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

###### ¿Alguna vez el médico le dijo que tenía artritis? ######

# MCQ160a - ¿Alguna vez el médico le dijo que tenía artritis?

summary(questionnaire$MCQ160A) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   1.754   2.000   9.000    4406 

unique(questionnaire$MCQ160A) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé

########## Edad a la que le diagnosticaron artritis ##########

# MCQ180a - Edad a la que le diagnosticaron artritis

summary(questionnaire$MCQ180A) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1      39      50    2038      62   99999    8667 

unique(questionnaire$MCQ180A)[1:20] # produce:
#  [1] 62 NA 70 45 50 42 40 19 59 79 39 52 60 38  1 78 25 68 22 80

################## ¿Qué tipo de artritis era? ####################

# MCQ195 - ¿Qué tipo de artritis era?

summary(questionnaire$MCQ195) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   3.597   9.000   9.000    8667 

unique(questionnaire$MCQ195) # produce: [1]  9 NA  2  4  1  3  7
# Donde 1 es osteoartitis o artritis degenerativa, 2 es artritis
# reumatoide, 3 es artritis psoriasica, 4 es otro tipo de artritis
# 7 es rechazado y 9 es no lo sé

############ Insuficiencia cardíaca ##############

# MCQ160b - ¿Alguna vez le han dicho que tiene insuficiencia cardíaca?

summary(questionnaire$MCQ160B) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.978   2.000   9.000    4406 

unique(questionnaire$MCQ160B) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

###### Edad a la que le diagnosticaron insuficiencia ######

# MCQ180b - Edad a la que le diagnosticaron insuficiencia cardíaca

summary(questionnaire$MCQ180B) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    46.0    58.0   605.4    68.0 99999.0    9993 

unique(questionnaire$MCQ180B)[1:20] # produce:
#[1] NA 78 57 37 54 55 45 71 30 76 50 65 38 49 40 64 59 43 56 79

########### Enfermedad Coronaria #############

# MCQ160c - ¿Alguna vez te han dicho que tienes una enfermedad coronaria?

summary(questionnaire$MCQ160C) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.982   2.000   9.000    4406 

unique(questionnaire$MCQ160C) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

############## Edad Enfermedad Coronaria ################

# MCQ180c - Edad a la que le diagnosticaron enfermedad coronaria

summary(questionnaire$MCQ180C) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    50.0    59.0   488.4    67.0 99999.0    9943 

unique(questionnaire$MCQ180C)[1:20] # produce:
#[1] NA 54 53 51 57 45 66 80 49 71 65 76 46 40 59 48 73 64 52 50

########### Infarto Cardíaco #############

# MCQ160e - ¿Alguna vez te han dicho que tuviste un ataque al corazón?

summary(questionnaire$MCQ160E) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.964   2.000   9.000    4406 
  
unique(questionnaire$MCQ160E) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

######### Edad Infarto cardíaco ############

# MCQ180e - Edad a la que le dijeron que había sufrido un infarto

summary(questionnaire$MCQ180E) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#16.0    45.0    57.0   491.5    65.0 99999.0    9946 

unique(questionnaire$MCQ180E)[1:20] # produce:
# [1] NA 55 72 53 51 57 32 45 47 48 40 64 70 23 80 49 65 52 56 46

############ Derrame cerebral ###############

# MCQ160f - ¿Alguna vez te han dicho que tuviste un derrame cerebral?

summary(questionnaire$MCQ160F) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.971   2.000   9.000    4406

unique(questionnaire$MCQ160F) # produce: [1]  1  2 NA  9
# Donde 1 es sí, 2 es no y 9 es no sé

############# Edad Derrame cerebral ###############

#MCQ180f - Edad a la que le dijeron que había sufrido un derrame cerebral

summary(questionnaire$MCQ180F) # produce:
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#7.00    50.00    60.50  1048.01    69.75 99999.00     9973 
    
unique(questionnaire$MCQ180F)[1:20] # produce:
# [1] 62 NA 21 41 38 43 61 60 50 75 66 45 80 59 40 32 64 65 49 79

############## Hipotiroidismo #################

# MCQ160m - ¿Alguna vez te han dicho que tienes un problema de tiroides?
  
summary(questionnaire$MCQ160M) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   2.000   1.909   2.000   9.000    4406 

unique(questionnaire$MCQ160M) # produce: [1]  2 NA  1  9
# Donde 1 es sí, 2 es no y 9 es no sé

######## ¿Sigues teniendo problemas de tiroides? ########

# MCQ170m - ¿Sigues teniendo problemas de tiroides?

summary(questionnaire$MCQ170M) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   1.000   1.514   2.000   9.000    9574 

unique(questionnaire$MCQ170M) # produce: [1] NA  1  2  9
# Donde 1 es sí, 2 es no y 9 es no sé

############ Edad de hipotiroidismo ##############

# MCQ180m - Edad a la que le diagnosticaron un problema de tiroides

summary(questionnaire$MCQ180M) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1      30      43    1706      56   99999    9574 

unique(questionnaire$MCQ180M)[1:20] # produce:
#  [1] NA 50 53 20 72 46 51 60 65 38 14 15 25 55 30 26 24 49 70 59


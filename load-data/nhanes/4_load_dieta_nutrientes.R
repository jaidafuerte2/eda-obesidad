#################################
##
## Diet - tipo de dieta
##
#################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset diet
diet <- read_csv("eda-obesidad/data/nhanes/diet.csv",
                 guess_max = 10000,
                 show_col_types = FALSE)

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
dieta_nutrientes <- diet |>
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE,
         DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, 
         DR1TP182, DR1TP205, DR1TP226) 
head(dieta) # produce:
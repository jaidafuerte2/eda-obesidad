#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset dieta -         ##
## nutrientes                  ##
##                             ##
#################################

# Nota: No olvidar que hay que sacar algunos valores totales en las
# grasas

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

# Seleccionar sólo las variables que voy a usar
df_obesidad <- df_obesidad %>%
  select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, 
         DR1TTFAT, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, 
         DR1TP182, DR1TP205, DR1TP226)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables de nutrientes para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    calorias = DR1TKCAL,
    proteinas = DR1TPROT,
    carbohidratos = DR1TCARB,
    azucares = DR1TSUGR,
    fibra = DR1TFIBE,
    grasas = DR1TTFAT,
    grasas_saturadas = DR1TSFAT,
    grasas_monoinsaturadas = DR1TMFAT,
    grasas_poliinsaturadas = DR1TPFAT,
    colesterol = DR1TCHOL,
    omega_6 = DR1TP182,
    omega_3_epa = DR1TP205,
    omega_3_dha = DR1TP226
  )

########################
##
## Calorías totales
##
########################

class(df_obesidad$calorias) # produce: numeric

summary(df_obesidad$calorias) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 117    1441    1942    2124    2595   12108     533 

unique(df_obesidad$calorias)[1:20] # produce:
#[1] 1574 5062 1743 1421 1785 2585 1580 2021 3145 1076 5621 3194  955
#[14] 2421 3053 2513 1497 3655 1590 1823

########################
##
## Proteínas (g)
##
########################

summary(df_obesidad$proteinas) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   52.23   74.21   82.84  102.17  656.53     533 

unique(df_obesidad$proteinas)[1:20] # produce:
#[1]  43.63 338.13  64.61  55.24  55.11  91.15  42.26  38.09 139.21
#[10]  39.40 274.72 144.92  81.61  87.39  96.42 285.83  70.54 106.97
#[19]  41.45  95.23

########################
##
## Carbohidratos (g)
##
########################

summary(df_obesidad$carbohidratos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#8.67  165.66  231.56  253.91  313.80 1362.55     533

unique(df_obesidad$carbohidratos)[1:20] # produce:
#[1] 239.59 423.78 224.39 178.20 189.59 300.16 226.32 216.59 227.63
#[10] 150.93 575.61 385.05 113.89 305.69 534.92 175.42 232.55 414.17
#[19] 272.17 151.06

########################
##
## Azucares (g)
##
########################

summary(df_obesidad$azucares) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.13   57.76   93.92  111.16  143.28  979.39     533 

unique(df_obesidad$azucares)[1:20] # produce:
#[1] 176.47  44.99 102.90  87.78  81.75 128.23  85.92  98.01  83.91
#[10]  61.42 215.83 158.45   5.31 153.65 161.91  52.01 137.39 285.23
#[19]  80.32  57.46

########################
##
## Fibra (g)
##
########################

summary(df_obesidad$fibra) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    9.50   14.60   16.91   21.70  136.30     533 

unique(df_obesidad$fibra)[1:20] # produce:
#[1] 10.8 16.7  9.9 12.3 22.6 24.8 13.1  7.0 37.6 14.9 36.0 33.4 15.6
#[14] 14.3 50.3 48.7 16.4  8.5 27.9 10.1

########################
##
## grasas (g)
##
########################

summary(df_obesidad$grasas) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   48.94   72.67   81.58  103.36  548.38     533 

unique(df_obesidad$grasas)[1:20] # produce:
#[1]  52.81 124.29  65.97  55.36  93.92  91.03  41.31  82.95 172.21
#[10]  39.68 249.49 121.74  18.24  97.72  63.95  80.46  34.68 177.38
#[19]  41.89  95.45

########################
##
## grasas saturadas (g)
##
########################

summary(df_obesidad$grasas_saturadas) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   14.35   22.72   26.07   33.72  177.47     533 

unique(df_obesidad$grasas_saturadas)[1:20] # produce:
#[1] 17.819 53.408 25.263  4.479 22.155 25.989 10.911 28.438 66.642
#[10] 14.153 80.879 48.486  6.481 26.584 20.273 21.360 12.229 34.368
#[19] 10.985 32.218

#############################
##
## grasas monoinsaturadas (g)
##
#############################

summary(df_obesidad$grasas_monoinsaturadas) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   16.41   25.07   28.53   36.24  221.67     533 

unique(df_obesidad$grasas_monoinsaturadas)[1:20] # produce:
#[1] 18.493 35.481 20.902 26.216 40.013 20.970 14.878 24.300 57.929
#[10] 14.336 92.405 42.117  4.534 37.877 22.895 27.719 11.980 42.087
#[19] 14.638 39.397

#############################
##
## grasas poliinsaturadas (g)
##
#############################

summary(df_obesidad$grasas_poliinsaturadas) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   10.27   16.26   19.24   24.73  182.46     533 

(df_obesidad$grasas_poliinsaturadas)[1:20] # produce:
#[1]  8.829 20.505 12.953  1.263 23.550 37.652 13.600 24.586 32.571
#[10]  8.762 50.794 16.147  3.264 25.078 13.006 20.488  6.148 83.996
#[19] 11.714 15.793

#############################
##
## Colesterol (mg)
##
#############################

summary(df_obesidad$colesterol) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0   130.0   229.0   299.8   404.0  2584.0     533 

unique(df_obesidad$colesterol)[1:20] # produce:
#[1]  209 2584   88   41  534  152   39   81  629   80  687  361  170
#[14]  256   66  775  153  441  188  410

#############################
##
## Ácidos grasos 
## poliinsaturados omega 6
##
#############################

summary(df_obesidad$omega_6) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    8.96   14.35   16.97   21.91  170.28     533

unique(df_obesidad$omega_6)[1:20] # produce:
#[1]  7.932 15.483 11.705  1.048 21.036 33.709 11.887 21.955 27.340
#[10]  7.752 45.852 14.023  2.618 21.757 11.393 17.455  5.357 72.732
#[19] 10.567 13.537

#############################
##
## Ácidos grasos 
## poliinsaturados omega 3 EPA
##
#############################

summary(df_obesidad$omega_3_epa) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00000 0.00300 0.00800 0.03371 0.01600 2.68700     533
(df_obesidad$omega_3_epa)[1:20] # produce:
#[1] 0.001 1.003 0.001 0.000 0.005 0.007 0.000 0.000 0.004 0.001 0.015
#[12] 0.002 0.005 0.008 0.000 0.071 0.009 0.011 0.002 0.010

#############################
##
## Ácidos grasos 
## poliinsaturados omega 3DHA
##
#############################

summary(df_obesidad$omega_3_dha) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00000 0.00300 0.01300 0.07219 0.06000 4.33700     533

unique(df_obesidad$omega_3_dha)[1:20] # produce:
#[1] 0.010 1.139 0.002 0.000 0.063 0.011 0.004 0.056 0.012 0.009 0.014
#[12] 0.181 0.001 0.040 0.028 0.015 0.128 0.246 0.087 0.035

##########################                              ####################
########################## Crear variables importantes  ####################
##########################                              ####################

####################
##
## Omega 3 total
##
####################

# Crear una nueva variable con el total de omega 3 incluido dha y epa
df_obesidad <- df_obesidad %>%
  mutate(
    omega_3_total = omega_3_dha + omega_3_epa
  )
unique(df_obesidad$omega_3_total)[1:20] # produce:
#[1] 0.011 2.142 0.003 0.000 0.068 0.018 0.004 0.060 0.027 0.002 0.014 0.022 0.252 0.010
#[15] 0.051 0.030 0.025 0.007 0.152 0.543

summary(df_obesidad$omega_3_total) # produce:
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0000  0.0090  0.0240  0.1059  0.0720  6.7650     533 

####################
##
## Relación Omega 6
## con Omega 3
##
####################

# Crear una nueva variable que relacione el consumo de omega 6 con el total de
# omega 3
df_obesidad <- df_obesidad %>%
  mutate(
    ratio_omega = case_when(
      is.na(omega_3_total) | omega_3_total == 0 ~ NA_real_,
      TRUE ~ omega_6 / omega_3_total
    )
  )
unique(df_obesidad$ratio_omega)[1:20] # produce:
#[1]  721.090909    7.228291 3901.666667          NA  309.352941 1872.722222 5488.750000
#[8]  455.666667 2584.000000 1698.222222 7011.500000  187.000000  988.954545   69.265873
#[15]  535.700000 1426.117647  352.233333  541.480000 2713.571429  103.710526

summary(df_obesidad$ratio_omega) # produce:
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#0.687   199.449   541.556  1254.570  1293.867 61795.000       795 

#################################
##
## Transformación log de 
## Relación Omega 6 con Omega 3
##
#################################

# En escala original, esta variable de relación de omegas es difícil de 
# modelar así que hay que hacer una tranformación log
df_obesidad <- df_obesidad %>%
  mutate(
    log_ratio_omega = log(ratio_omega + 1) # + 1: porque log(0) no existe
    # también se podría usar una versión más simple:
    #log_ratio_omega = log(ratio_omega)
  )
summary(df_obesidad$log_ratio_omega) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.523   5.301   6.296   6.183   7.166  11.032     795 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y los nutrientes
obesidad_dieta_nutrientes <- df_obesidad %>%
  select(SEQN, calorias, proteinas, carbohidratos, azucares, 
         fibra, grasas, grasas_saturadas, grasas_monoinsaturadas,
         grasas_poliinsaturadas, colesterol, omega_6, omega_3_epa,
         omega_3_dha, omega_3_total, ratio_omega, log_ratio_omega)

# Crear un archivo .cvs con la dataset de dieta - nutrientes
write.csv(obesidad_dieta_nutrientes,
          "eda-obesidad/data/obesidad/4_obesidad_dieta_nutrientes.csv",
          row.names = FALSE)
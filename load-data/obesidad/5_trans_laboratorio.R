#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset laboratorio     ##
##                             ##
#################################

# Nota: no olvidar crear las variables de homa-ir y nos/mos. Y tampoco
# olvidar que hay que tener una base de datos sin los que no ayunaron
# al menos 8 horas. Tampoco olvidar hígado graso

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

# Renombrar las variables de nutrientes para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    insulinemia = LBXIN,
    glicemia = LBXSGL,
    glicohemoglobina = LBXGH,
    ggt = LBXSGTSI,
    colesterolemia_total = LBXTC,
    colesterolemia_ldl = LBDLDL,
    colesterolemia_hdl = LBDHDD,
    trigliceridemia = LBXTR,
    neutrofilos = LBDNENO,
    linfocitos = LBDLYMNO,
    ayuno_horas = PHAFSTHR_final,
    ayuno_minutos = PHAFSTMN_final
  )



################################
##
## Insulina en Ayunas (uU/mL)
##
################################

summary(df_obesidad$insulinemia) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.14    5.79    9.13   13.29   14.95  682.48    3169 

unique(df_obesidad$insulinemia)[1:20] # produce:
#[1]    NA  5.83  6.12 14.91  3.85  6.05 16.15 10.92  6.08 20.93 57.77
#[12] 68.63 17.47  3.24  7.16  9.86  4.33 12.06  5.91 22.92

###############################
##
## Glucemia en Ayunas (mg/dL)
##
##############################

summary(df_obesidad$glicemia) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#49.0    87.0    94.0   104.3   106.0   577.0     286 

unique(df_obesidad$glicemia)[1:20] # produce:
#[1] 554 219 183 104 107 108  81 126  89  97  NA  94  85  96  77  87 174
#[18] 105 103 119

########################
##
## Glicohemoglobina (%)
##
########################

summary(df_obesidad$glicohemoglobina) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#3.500   5.200   5.500   5.715   5.900  17.500     221

unique(df_obesidad$glicohemoglobina)[1:20] # produce:
#[1] 13.9  9.1  8.9  4.9  5.5  5.4  5.2  6.9  5.0  5.3   NA  5.8  5.7
#[14]  7.7  5.9  6.2  9.4  6.1  6.3  5.6

######################################
##
## Gamma glutamil transferasa (U/L)
##
######################################

summary(df_obesidad$ggt) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#5.00   13.00   19.00   28.17   28.00 1510.00     287 

unique(df_obesidad$ggt)[1:20] # produce:
#[1] 16 15 13 31 22 17 21 24 30 NA 19 20 67 62 29 50 53 43 90 86

#############################
##
## Colesterol total (mg/dL)
##
#############################

summary(df_obesidad$colesterolemia_total) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#69.0   159.0   184.0   187.6   212.0   813.0     270

unique(df_obesidad$colesterolemia_total)[1:20] # produce:
#[1] 167 170 126 201 226 168 278 173 131 182  NA 225 202 198 192 189 165
#[18] 151 177 227

table(df_obesidad$colesterolemia_total) # produce:

# Saber cuántas observaciones llenas y cuántas vacías tiene una
# variable
c(
  llenos = sum(!is.na(df_obesidad$colesterolemia_total)),
  vacios = sum(is.na(df_obesidad$colesterolemia_total))
)
# llenos vacios 
#   5512    270

#############################
##
## Colesterol ldl (mg/dL)
##
#############################

# Nota: triglicéridos y ldl requiere de ayuno por eso hay tantos 
# valores faltantes. Además el colesterol ldl se calcula a partir
# de los triglicéridos.

summary(df_obesidad$colesterolemia_ldl) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#14.0    85.0   107.0   110.0   131.2   375.0    3166 

(df_obesidad$colesterolemia_ldl)[1:20] # produce:
#[1]  NA  NA  56 101  NA  97  NA  NA  67  NA  75 119  NA 159 105  NA  NA
#[18]  NA  NA  NA

# Saber cuántas observaciones llenas y cuántas vacías tiene una
# variable
c(
  llenos = sum(!is.na(df_obesidad$colesterolemia_ldl)),
  vacios = sum(is.na(df_obesidad$colesterolemia_ldl))
)
#llenos vacios 
#  2616   3166 

#############################
##
## Colesterol hdl (mg/dL)
##
#############################

summary(df_obesidad$colesterolemia_hdl) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   42.00   50.00   52.57   61.00  173.00     270 

unique(df_obesidad$colesterolemia_hdl)[1:20] # produce:
#  [1] 65 50 60 85 38 58 59 79 96 53 33 NA 55 78 45 34 62 46 56 43

#############################
##
## trigliceridemia (mg/dL)
##
#############################

# Nota: triglicéridos y ldl requiere de ayuno por eso hay tantos 
# valores faltantes. Además el colesterol ldl se calcula a partir
# de triglicéridos

summary(df_obesidad$trigliceridemia) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#14.0    64.0    93.0   118.7   141.0  4233.0    3126 

unique(df_obesidad$trigliceridemia)[1:20] # produce:
#[1]  NA  51  75  64  24  14 148  57  93  87 328 168 139 312  77  67  78
#[18]  49 108  62

#############################
##
## Neutrófilos (1000 cell/uL)
##
#############################

summary(df_obesidad$neutrofilos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.400   3.150   4.000   4.323   5.200  25.600     235 

unique(df_obesidad$neutrofilos)[1:20] # produce:
#[1]  2.0  7.4  4.9  4.5  6.5  3.0  5.5  4.0  4.2  3.7   NA  2.8  2.7
#[14]  3.5  6.2 11.8  5.9  5.7  3.3  3.8

#############################
##
## Linfocitos (1000 cell/uL)
##
#############################

summary(df_obesidad$linfocitos) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.400   1.700   2.100   2.177   2.600  49.000     235 

unique(df_obesidad$linfocitos)[1:20] # produce:
#[1] 2.0 3.4 1.0 1.4 1.6 3.0 1.3 1.2 3.2  NA 1.7 2.1 2.5 1.9 2.3 1.8 5.3
#[18] 0.9 0.8 2.9

#############################
##
## Duración del ayuno en
## horas
##
#############################

summary(df_obesidad$ayuno_horas) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   2.000   6.000   6.974  11.000  39.000      71 

unique(df_obesidad$ayuno_horas) # produce:
#[1]  3 15 14 11  4 13  1  0 10 12  2 17 NA  7  9  5 18 22  8  6 16 25 24
#[24] 21 26 34 19 27 39 23 28 20 31 32 33 35 29

#############################
##
## Duración del ayuno en
## minutos
##
#############################

summary(df_obesidad$ayuno_minutos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   14.00   30.00   29.46   44.00   59.00      71 

unique(df_obesidad$ayuno_minutos) # produce:
#[1] 47 14 51 42 21 45 57 52  9  2 48  7 36 44 10 46 28 11 NA 50 19 58 38
#[24] 40 27 37 33 15 22 35 41 13 34 39 30  5  1 24 26 12  3  0 55 16 32 31
#[47] 29 53 17 25 56 23  6  8 18 20  4 54 43 49 59

##########################                              ####################
########################## Crear variables importantes  ####################
##########################                              ####################

#############################
##
## HOMA-IR : Índice de 
## resistencia a la insulina
##
#############################

# Sacar el índice de resistencia a la insulina (homa-ir) 
df_obesidad <- df_obesidad %>%
  mutate(
    homa_ir = case_when(
      is.na(glicemia) | is.na(insulinemia) ~ NA_real_,
      TRUE ~ (glicemia * insulinemia) / 405
    )
  )
unique(df_obesidad$homa_ir)[1:20] # produce:
#[1]        NA  2.634296  1.571556  3.939185  0.770000  1.329506  3.868025  2.534519
#[9]  1.561284  5.426296 14.692123 20.165358  4.356716  0.672000  1.626469  1.704198
#[17]  1.037062  2.888444  1.648963  7.243852

summary(df_obesidad$homa_ir) # produce:
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#0.0239   1.3089   2.2250   3.7666   3.8902 266.2515     3169 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y los resultados de laboratorio
obesidad_laboratorio <- df_obesidad %>%
  select(SEQN, insulinemia, glicemia, glicohemoglobina, ggt,
         colesterolemia_total, colesterolemia_ldl, colesterolemia_hdl,
         trigliceridemia, neutrofilos, linfocitos, ayuno_horas,
         ayuno_minutos)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_laboratorio,
          "eda-obesidad/data/obesidad/5_obesidad_laboratorio.csv",
          row.names = FALSE)


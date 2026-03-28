#################################
##
## Labs
##
#################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset labs
labs <- read_csv("eda-obesidad/data/nhanes/labs.csv",
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

############ Duración del ayuno en horas ###############

# PHAFSTHR - Duración total del "ayuno de alimentos", horas

summary(labs$PHAFSTHR) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#8.00   10.00   11.00   11.63   13.00   23.00    6904 

unique(labs$PHAFSTHR) # produce:
#[1] NA 15 14 11 12 10 13  9 20 18 17 16 21  8 22 19 23

############ Duración del ayuno en minutos ################

# PHAFSTMN - Duración total del "ayuno de alimentos", minutos

summary(labs$PHAFSTMN) # produce:   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   15.00   31.00   30.05   44.00   59.00    6904 

unique(labs$PHAFSTMN) # produce:
#[1] NA 51 42 14 13  2  7 36 12 11 58 38 37 47 33 34 39 30  9 35 10 24
#[23] 21 57  0 27 55 16 29  5 17 28 22 45 46 19  6 56 59 52 15 41 32 20
#[45]  4 40 49 31 23 54  8  1 48 44 53 25  3 26 18 43 50

############### Seleccionar #################

# Seleccionar los exámenes de laboratorio más importantes
# Seleccionar los distintos tipos de dietas y nutrientes
laboratorio <- labs |>
  select(SEQN, LBXIN, LBXSGL, LBXGH, LBXSGTSI, LBXTC, LBDLDL, LBDHDD, 
         LBXTR, LBDNENO, LBDLYMNO, PHAFSTHR, PHAFSTMN) 
head(laboratorio) # produce:

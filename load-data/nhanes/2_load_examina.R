#################################
##
## Examination
##
#################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset examination
examination <- read_csv("eda-obesidad/data/nhanes/examination.csv",
                        guess_max = 10000,
                        show_col_types = FALSE)

# Seleccionar sólo las variables que voy a usar
examination <- examination |>
  select(SEQN, BMXWT, BMXHT, BMXBMI, BMXWAIST, BMDAVSAD, BPXSY1,  
         BPXSY2, BPXSY3, BPXSY4, BPXDI1, BPXDI2, BPXDI3, BPXDI4) 
#glimpse(examination) # produce:

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
    BPXSY_mean = rowMeans(select(., BPXSY1, BPXSY2, BPXSY3, BPXSY4), 
                          na.rm = TRUE),
    BPXDI_mean = rowMeans(select(., BPXDI1, BPXDI2, BPXDI3, BPXDI4), 
                          na.rm = TRUE)
  )

summary(examination$BPXSY_mean) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#64.67  106.00  115.33  118.31  128.00  228.67    2282 

summary(examination$BPXDI_mean) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   58.00   66.67   65.44   74.67  128.00    2282 

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


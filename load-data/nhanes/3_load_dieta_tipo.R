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

# Seleccionar sólo las variables que voy a usar
diet <- diet |>
  select(SEQN, DRQSDIET, DBD100, DRQSDT1, DRQSDT2, DRQSDT3, DRQSDT4,
         DRQSDT6, DRQSDT7, DRQSDT9, DRQSDT10, DRQSDT11)
#glimpse(diet) # produce:

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

# DRQSDT5 = Dieta rica en fibra

summary(diet$DRQSDT6) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  6       6       6       6       6       6    9809  

unique(diet$DRQSDT6) # produce: [1] NA  6

table(diet$DRQSDT6, useNA = "ifany") # produce:
#6 <NA> 
#4 9809 

######## Dieta para diabéticos  ###########

# DRQSDT7 = Dieta para diabéticos

summary(diet$DRQSDT7) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   7       7       7       7       7       7    9667  

unique(diet$DRQSDT7) # produce: [1] NA  7

table(diet$DRQSDT7, useNA = "ifany") # produce:
#  7 <NA> 
#146 9667 

########## Dieta baja en carbohidratos ##############

# DRQSDT9 = Dieta baja en carbohidratos

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

############# Seleccionar ###############

# Seleccionar los distintos tipos de dietas y nutrientes
dieta_tipo <- diet |>
  select(SEQN, DRQSDIET, DBD100, DRQSDT1, DRQSDT2, DRQSDT3, DRQSDT4,
         DRQSDT6, DRQSDT7, DRQSDT9, DRQSDT10, DRQSDT11) 
head(dieta_tipo) # produce:



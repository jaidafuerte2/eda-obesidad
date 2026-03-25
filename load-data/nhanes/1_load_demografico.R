#################################
##
## Demographic
##
#################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

# Cargar la dataset demographic
demographic <- read_csv(
  "eda-obesidad/data/nhanes/demographic.csv",
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
###################################
##
## Questionnaire - ejercicio
##
###################################

# Introducción

library(readr)
library(dplyr)
library(tidyverse)

questionnaire <- read_csv("eda-obesidad/data/nhanes/questionnaire.csv",
                          guess_max = 10000,
                          show_col_types = FALSE)

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

########### Minutos en un día a pie o bicicleta #################

# PAD645 - Minutos a pie/en bicicleta para transporte

# Se refiere a tiempo para transportarse a pie o en bicicleta

summary(questionnaire$PAD645) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   20.00   30.00   71.96   60.00 9999.00    8132 

unique(questionnaire$PAD645) # produce:
#[1]   NA  120   30  720   20   37  180   45   15   10   40   60  360
#[14]   90  240  300  420   25   27   35   13   50  480   12   11 9999
#[27]   14   46   16   22  130  840  600  115   95   18

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

# Se refiere al tiempo diario que una persona pasa sentada en trabajo,
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

table(questionnaire$PAQ706) # produce:
#0    1    2    3    4    5    6    7   99 
#131   90  159  194  183  314  111 1799    8 
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

############### Seleccionar #################

# Seleccionar los exámenes las variables del cuestionario más 
# importantes
cuestionario_ejercicio <- questionnaire |>
  select(SEQN, PAQ605, PAQ610, PAD615, PAQ620, PAQ625, PAD630,
         PAQ635, PAQ640, PAD645, PAQ650, PAQ655, PAD660, PAQ665,
         PAQ670, PAD675, PAD680, PAQ706, PAQ710, PAQ715) 

head(cuestionario_ejercicio) # produce:

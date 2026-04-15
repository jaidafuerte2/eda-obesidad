#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset cuestionario    ##
## de dieta - lácteos          ##
##                             ##
#################################

# Nota: Hacer el cálculo total de minutos por semana de actividad
# y sopesar si es conveniente tener los totales de actividad 
# moderada y vogorosa

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

# Seleccionar las variables más importantes
df_obesidad <- df_obesidad %>%
  select(SEQN, PAQ605, PAQ610, PAD615, PAQ620, PAQ625, PAD630,
         PAQ635, PAQ640, PAD645, PAQ650, PAQ655, PAD660, PAQ665,
         PAQ670, PAD675, PAD680, PAQ706, PAQ710, PAQ715)

###################################
##
## Renombrar variables a español
##
###################################

# Renombrar las variables del cuestionario para que su nombre se
# entienda mejor en español
df_obesidad <- df_obesidad %>%
  rename(
    trabajo_vigoroso = PAQ605,
    dias_trabajo_vigoroso = PAQ610,
    minutos_trabajo_vigoroso = PAD615,
    trabajo_moderado = PAQ620,
    dias_trabajo_moderado = PAQ625,
    minutos_trabajo_moderado = PAD630,
    caminar_ciclear = PAQ635,
    dias_caminar_ciclear = PAQ640,
    minutos_caminar_ciclear = PAD645,
    ejercicio_vigoroso = PAQ650,
    dias_ejercicio_vigoroso = PAQ655,
    minutos_ejercicio_vigoroso = PAD660,
    ejercicio_moderado = PAQ665,
    dias_ejercicio_moderado = PAQ670,
    minutos_ejercicio_moderado = PAD675,
    minutos_actividad_sedentaria = PAD680,
    dias_ejercicio_60 = PAQ706,
    horas_tv_videos = PAQ710,
    horas_computadora = PAQ715
  )


############################
##
## Trabajo vigoroso
##
############################

unique(df_obesidad$trabajo_vigoroso) # produce: [1] 2 1 7

table(df_obesidad$trabajo_vigoroso) # produce:
#   1    2    7 
#1049 4732    1 

# Pasar a los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad %>%
  mutate(
    trabajo_vigoroso = case_when(
      trabajo_vigoroso == 1 ~ "si",
      trabajo_vigoroso == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    trabajo_vigoroso = as.factor(trabajo_vigoroso) # transformar en 
    # factor
  )

class(df_obesidad$trabajo_vigoroso) # produce:factor

unique(df_obesidad$trabajo_vigoroso) # produce: 
#[1] no   si   <NA>
#Levels: no si

table(df_obesidad$trabajo_vigoroso) # produce:
#  no   si 
#4732 1049 

############################
##
## Días de trabajo vigoroso
##
############################

summary(df_obesidad$dias_trabajo_vigoroso) # produce: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   3.000   5.000   4.056   5.000   7.000    4733 

unique(df_obesidad$dias_trabajo_vigoroso) # produce:
#[1] NA  5  3  2  1  4  6  7

table(df_obesidad$dias_trabajo_vigoroso) # produce:
#  1   2   3   4   5   6   7 
#109 146 155  98 351  88 102 

#################################
##
## Minutos de trabajo vigoroso
##
#################################

unique(df_obesidad$minutos_trabajo_vigoroso) # produce:
#[1]   NA  240  480   30  120  180   60  360  420  540  300   10  600
#[14]   45   20   75  160   90   35  720   15   25   12  660  840 1080
#[27]  130 9999   40   21

summary(df_obesidad$minutos_trabajo_vigoroso) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0      60     120     193     240    9999    4736 

# Limpiar la variable de minutos de trabajo vigoroso para pasar los
# valores 7777 y 9999 a NA
df_obesidad <- df_obesidad %>%
  mutate(
    minutos_trabajo_vigoroso = 
      ifelse(minutos_trabajo_vigoroso %in% c(7777, 9999), 
             NA, minutos_trabajo_vigoroso)
  )
summary(df_obesidad$minutos_trabajo_vigoroso) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0    60.0   120.0   174.2   240.0  1080.0    4738 

#########################
##
## Trabajo moderado
##
#########################

unique(df_obesidad$trabajo_moderado) # produce: [1] 2 1 9 7

table(df_obesidad$trabajo_moderado) # produce:
#   1    2    7    9 
#1914 3865    1    2 

# Pasar a los valores numéricos a factores no ordenados
df_obesidad <- df_obesidad %>%
  mutate(
    trabajo_moderado = case_when(
      trabajo_moderado == 1 ~ "si",
      trabajo_moderado == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    trabajo_moderado = as.factor(trabajo_moderado) # transformar en 
    # factor
  )

class(df_obesidad$trabajo_moderado) # produce:factor

unique(df_obesidad$trabajo_moderado) # produce: 
#[1] no   si   <NA>
#Levels: no si

table(df_obesidad$trabajo_moderado) # produce:
#  no   si 
#4732 1049 

###############################
##
## Días de trabajo moderado
##
###############################

unique(df_obesidad$dias_trabajo_moderado) # produce:
#[1] NA  1  7  2  5  3  6  4 99 77

summary(df_obesidad$dias_trabajo_moderado) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   3.000   5.000   4.456   5.000  99.000    3868

# Cambiar los valores 77 y 99 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    dias_trabajo_moderado =
      ifelse(dias_trabajo_moderado %in% c(77, 99), 
             NA, dias_trabajo_moderado)
  )
summary(df_obesidad$dias_trabajo_moderado) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   3.000   5.000   4.319   5.000   7.000    3871

unique(df_obesidad$dias_trabajo_moderado) # produce:
#[1] NA  1  7  2  5  3  6  4

table(df_obesidad$dias_trabajo_moderado) # produce:
#  1   2   3   4   5   6   7 
#140 228 293 182 646 134 288 

###############################
##
## Minutos de trabajo moderado
##
###############################

unique(df_obesidad$minutos_trabajo_moderado) # produce:
#[1]   NA   10   30  480  120   60  300  240  360   20   45  180   15
#[14]   40  420  600   25   90  720  150  540   70   50   29   80   19
#[27] 9999  660  900   16
summary(df_obesidad$minutos_trabajo_moderado) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0    60.0   120.0   160.5   240.0  9999.0    3875

# Limpiar la variable de minutos de trabajo moderado para que los 
# valores raros pasen a ser valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    minutos_trabajo_moderado =
      ifelse(minutos_trabajo_moderado %in% c(9999, 7777), 
             NA, minutos_trabajo_moderado)
    )
summary(df_obesidad$minutos_trabajo_moderado) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0    60.0   120.0   150.2   240.0   900.0    3877

###############################
##
## Caminar ciclear para 
## transportarse
##
###############################

unique(df_obesidad$caminar_ciclear) # produce: [1] 2 1 9

table(df_obesidad$caminar_ciclear) # produce:
#   1    2    9 
#1498 4283    1 

# Transformar la variable de caminar o ciclear para desplazarse de
# numeric a factor
df_obesidad <- df_obesidad %>%
  mutate(
    caminar_ciclear = case_when(
      caminar_ciclear == 1 ~ "si",
      caminar_ciclear == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    caminar_ciclear = as.factor(caminar_ciclear) # transformar en
    # factor
  )
unique(df_obesidad$caminar_ciclear) # produce:
#[1] no   si   <NA>
#Levels: no si

table(df_obesidad$caminar_ciclear) # produce:
#  no   si 
#4283 1498 

###############################
##
## Días de caminar y ciclear  
## para transportarse
##
###############################

unique(df_obesidad$dias_caminar_ciclear) # produce:
#[1] NA  5  6  7  3  2  1  4

summary(df_obesidad$dias_caminar_ciclear) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   3.000   5.000   4.871   7.000   7.000    4284 

table(df_obesidad$dias_caminar_ciclear) # produce:
# 1   2   3   4   5   6   7 
#82 144 197 127 364  80 504

##################################
##
## Minutos de caminar y ciclear  
## para transportarse
##
##################################

unique(df_obesidad$minutos_caminar_ciclear) # produce:
#[1]   NA  720   37  180   30   10   20   60   45  120  360   15   40
#[14]   90  240  300  420   25   27   35   50  480 9999   14   46   16
#[27]   12  130  840  600  115   95   18

summary(df_obesidad$minutos_caminar_ciclear) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   20.00   30.00   84.08   60.00 9999.00    4288 

# Cambiar los valores 9999 y 7777 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    minutos_caminar_ciclear = 
      ifelse(minutos_caminar_ciclear %in% c(9999, 7777), 
             NA, minutos_caminar_ciclear)
  )
summary(df_obesidad$minutos_caminar_ciclear) # produce: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   20.00   30.00   64.13   60.00  840.00    4291 


##################################
##
## Actividades recreativas  
## vigorosas
##
##################################

unique(df_obesidad$ejercicio_vigoroso) # produce: [1] 2 1

table(df_obesidad$ejercicio_vigoroso) # produce:
#   1    2 
#1344 4438 

df_obesidad <- df_obesidad %>%
  mutate(
    ejercicio_vigoroso = case_when(
      ejercicio_vigoroso == 1 ~ "si",
      ejercicio_vigoroso == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    ejercicio_vigoroso = as.factor(ejercicio_vigoroso) # trnsformar en
    # factor
  )
unique(df_obesidad$ejercicio_vigoroso) #produce:
#[1] no si
#Levels: no si
table(df_obesidad$ejercicio_vigoroso) #produce:
#  no   si 
#4438 1344 

##################################
##
## Días de actividades recreativas  
## vigorosas
##
##################################

unique(df_obesidad$dias_ejercicio_vigoroso) # produce:
#[1] NA  2  6  4  3  5  1 99  7

summary(df_obesidad$dias_ejercicio_vigoroso) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   3.000   3.437   5.000  99.000    4438 

# Cambiar los valores 99 y77 de días de ejercicio vigoroso a NA
df_obesidad <- df_obesidad %>%
  mutate(
    dias_ejercicio_vigoroso =
      ifelse(dias_ejercicio_vigoroso %in% c(99, 77), 
             NA, dias_ejercicio_vigoroso)
  )
unique(df_obesidad$dias_ejercicio_vigoroso) # produce:
#[1] NA  2  6  4  3  5  1  7

summary(df_obesidad$dias_ejercicio_vigoroso) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   3.000   3.366   5.000   7.000    4439 

##################################
##
## Minutos de actividades  
## recreativas vigorosas
##
##################################

unique(df_obesidad$minutos_ejercicio_vigoroso) # produce:
#[1]  NA  40 180  60  70 120  30  45  90  15 240  75  20  10  25  35  11
#[18] 360 300  31  47  50 150  55 330 480  80 115

summary(df_obesidad$minutos_ejercicio_vigoroso) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   45.00   60.00   78.79  120.00  480.00    4439 

df_obesidad <- df_obesidad %>%
  mutate(
    minutos_ejercicio_vigoroso =
      ifelse(minutos_ejercicio_vigoroso %in% c(7777, 9999),
             NA, minutos_ejercicio_vigoroso)
  )
unique(df_obesidad$minutos_ejercicio_vigoroso) # produce:
#[1]  NA  40 180  60  70 120  30  45  90  15 240  75  20  10  25  35  11
#[18] 360 300  31  47  50 150  55 330 480  80 115

summary(df_obesidad$minutos_ejercicio_vigoroso) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.00   45.00   60.00   78.79  120.00  480.00    4439 

#############################
##
## Actividades recreativas   
## Moderadas  
##
#############################

unique(df_obesidad$ejercicio_moderado) # produce: [1] 2 1

table(df_obesidad$ejercicio_moderado) # produce:
#   1    2 
#2400 3382 

# Transformar la variable de ejercicio moderado a factor
df_obesidad <- df_obesidad %>%
  mutate(
    ejercicio_moderado  = case_when(
      ejercicio_moderado == 1 ~ "si",
      ejercicio_moderado == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    ejercicio_moderado = as.factor(ejercicio_moderado) # Pasar la
    # variable a factor
  )
unique(df_obesidad$ejercicio_moderado) 
#[1] no si
#Levels: no si

table(df_obesidad$ejercicio_moderado) # produce:
#no   si 
#3382 2400 

#############################
##
## Días de Actividades    
## recreativas Moderadas  
##
#############################

unique(df_obesidad$dias_ejercicio_moderado) # produce:
#[1] NA  1  4  2  7  3  6  5 99

summary(df_obesidad$dias_ejercicio_moderado) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   3.000   3.549   5.000  99.000    3382

df_obesidad <- df_obesidad %>%
  mutate(
    dias_ejercicio_moderado = 
      ifelse(dias_ejercicio_moderado %in% c(99, 77),
             NA, dias_ejercicio_moderado)
  )

unique(df_obesidad$dias_ejercicio_moderado) # produce:
#[1] NA  1  4  2  7  3  6  5

summary(df_obesidad$dias_ejercicio_moderado) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   2.000   3.000   3.509   5.000   7.000    3383 

#############################
##
## Minutos de Actividades    
## recreativas Moderadas  
##
#############################

unique(df_obesidad$minutos_ejercicio_moderado) # produce:
#[1]  NA 180  30  60  20 120  45 300  10  25  40  50   0  15  39 480  90
#[18] 360 240  70  12  35 150 600  75  22 135  65  80 100 420 115  16  55
#[35] 900

summary(df_obesidad$minutos_ejercicio_moderado) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   30.00   60.00   63.95   60.00  900.00    3383

# Cambiar los valores raros 7777 y 9999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    minutos_ejercicio_moderado = 
      ifelse(minutos_ejercicio_moderado %in% c(7777, 9999),
             NA, minutos_ejercicio_moderado)
  )

unique(df_obesidad$minutos_ejercicio_moderado) # produce:
#[1]  NA 180  30  60  20 120  45 300  10  25  40  50   0  15  39 480  90
#[18] 360 240  70  12  35 150 600  75  22 135  65  80 100 420 115  16  55
#[35] 900

summary(df_obesidad$minutos_ejercicio_moderado) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   30.00   60.00   63.95   60.00  900.00    3383

###################################
##
## Minutos de actividad     
## física sedentaria (en un día
## normal)
##
###################################

unique(df_obesidad$minutos_actividad_sedentaria) # produce:
#[1]  600  540  300  480  360   60   30  120  240  720 1080  420  180
#[14]  840  660  780   20  960  900  150 9999   NA 1020   15   90 7777
#[27] 1140    1   81    0   25  105   10 1200  490

summary(df_obesidad$minutos_actividad_sedentaria) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   240.0   480.0   455.6   540.0  9999.0       5 

# Cambiar los valores raros 7777 y 9999 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    minutos_actividad_sedentaria = 
      ifelse(minutos_actividad_sedentaria %in% c(7777, 9999),
             NA, minutos_actividad_sedentaria)
  )

unique(df_obesidad$minutos_actividad_sedentaria) # produce:
#[1]  600  540  300  480  360   60   30  120  240  720 1080  420  180
#[14]  840  660  780   20  960  900  150   NA 1020   15   90 1140    1
#[27]   81    0   25  105   10 1200  490

summary(df_obesidad$minutos_actividad_sedentaria) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   240.0   420.0   421.2   540.0  1200.0      26  

#############################
##
## Dias de actividad física    
## mayor a 60 minutos
##
#############################

unique(df_obesidad$dias_ejercicio_60) # produce: [1] NA

summary(df_obesidad$dias_ejercicio_60) # produce:
#Mode    NA's 
#logical    5782

table(df_obesidad$dias_ejercicio_60) # produce:
#< table of extent 0 >
  
#############################
##
## Horas viendo televisión     
## o videos (últimos 30 días)
##
#############################

unique(df_obesidad$horas_tv_videos) # produce:
#[1]  2  4  1  5  0  3  8 99 77

summary(df_obesidad$horas_tv_videos) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   2.696   4.000  99.000 

# Cambiar los valores raros 77 y 99 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    horas_tv_videos = 
      ifelse(horas_tv_videos %in% c(77, 99),
             NA, horas_tv_videos)
  )
unique(df_obesidad$horas_tv_videos) # produce:
#[1]  2  4  1  5  0  3  8 NA

summary(df_obesidad$horas_tv_videos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    1.00    2.00    2.65    4.00    8.00       3

#################################
##
## Horas uso de la coputadora      
## (últimos 30 días)
##
#################################

unique(df_obesidad$horas_computadora) # produce: [1] 8 0 1 5 3 2 4

summary(df_obesidad$horas_computadora) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   2.000   3.352   8.000   8.000 

# Cambiar los valores raros 77 y 99 a valores faltantes
df_obesidad <- df_obesidad %>%
  mutate(
    horas_computadora = 
      ifelse(horas_computadora %in% c(77, 99),
             NA, horas_computadora)
  )
unique(df_obesidad$horas_computadora) # produce:
#[1]  2  4  1  5  0  3  8 NA

summary(df_obesidad$horas_computadora) # produce:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   2.000   3.352   8.000   8.000 

#######################                              #############
####################### Transformaciones importantes #############
#######################                              #############

############################
##
## Minutos de actividades 
## laborales vigorosas
## por semana
##
############################

# Conocer cuantos minutos de actividad física vigorosa laboral 
# realiza una persona por semana
df_obesidad <- df_obesidad %>%
  mutate(
    min_vigorosa_laboral_semana = PAQ610 * PAD615
  )
unique(df_obesidad$min_vigorosa_laboral_semana)[1:20] # produce: 
#[1]   NA 1200 2400   90  480   60  120  720 1080  210  180  360  600
#[14]  150  840 1260 1680 1440  240  540

summary(df_obesidad$min_vigorosa_laboral_semana)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0   150.0   480.0   881.7  1200.0 69993.0    4736 

############################
##
## Minutos de actividades 
## laborales moderdas
## por semana
##
############################

# Conocer cuantos minutos de actividad física vigorosa laboral 
# realiza una persona por semana
df_obesidad <- df_obesidad %>%
  mutate(
    min_moderada_laboral_semana = PAQ625 * PAD630
  )
unique(df_obesidad$min_moderada_laboral_semana)[1:20] # produce: 
#[1]   NA   10  210   60 2400  360  240  600 1800 1200 1440  120  150
#[14]  100  180  480  300 1260   45  900

summary(df_obesidad$min_moderada_laboral_semana)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0   150.0   420.0   757.8  1065.0 59994.0    3875 

############################
##
## Minutos de actividades 
## recreativas vigorosas
## por semana
##
############################

# Conocer cuantos minutos de actividad física vigorosa recrativa 
# realiza una persona por semana
df_obesidad <- df_obesidad %>%
  mutate(
    min_vigorosa_recrea_semana = PAQ655 * PAD660
  )
unique(df_obesidad$min_vigorosa_recrea_semana)[1:20] # produce: 
#[1]   NA   80 1080  240  420  360  300   60  200  180  600   90   75
#[14]  720  225  120   40  450  210   30

summary(df_obesidad$min_vigorosa_recrea_semana) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#10.0   120.0   180.0   265.9   339.5  2400.0    4439 

############################
##
## Minutos de actividades 
## recreativas moderadas
## por semana
##
############################

# Conocer cuantos minutos de actividad física vigorosa recrativa 
# realiza una persona por semana
df_obesidad <- df_obesidad %>%
  mutate(
    min_moderada_recrea_semana = PAQ670 * PAD675
  )
unique(df_obesidad$min_moderada_recrea_semana)[1:20] # produce: 
#[1]  NA 180 120  60 420 540 240 840  40 360 150  30  90 600  50 100 210
#[18]  25 160 200

summary(df_obesidad$min_moderada_recrea_semana) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0    90.0   135.0   217.7   240.0  4500.0    3383

#############################
##
## Minutos totales de 
## actividades  moderadas
##
#############################

# Conocer cuantos minutos de actividad física moderada total 
# realiza una persona por semana
df_obesidad <- df_obesidad %>%
  mutate(
    min_moderada_total = 
      # Se usa coalesce para evitar perder observaciones por NA's en
      # alguna de las dos variables
      coalesce(min_moderada_laboral_semana, 0) + 
      coalesce(min_moderada_recrea_semana, 0)
  )
unique(df_obesidad$min_moderada_total)[1:20] # produce: 
#[1]    0   10  390   60 2400  120  420  780  600  240  900 1800 1200
#[14]   40 1680  360  300  100   30  180

summary(df_obesidad$min_moderada_total) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     0.0    80.0   340.2   360.0 60834.0 

#############################
##
## Minutos totales de 
## actividades  vigorosas
##
#############################

# Conocer cuantos minutos de actividad física vigorosa total 
# realiza una persona por semana
df_obesidad <- df_obesidad %>%
  mutate(
    min_vigorosa_total = 
      # Se usa coalesce para evitar perder observaciones por NA's en
      # alguna de las dos variables
      coalesce(min_vigorosa_laboral_semana, 0) + 
      coalesce(min_vigorosa_recrea_semana, 0)
  )
unique(df_obesidad$min_vigorosa_total)[1:20] # produce: 
#[1]    0 1200 2400   90   80 1560   60  240  420  120  360  720  300
#[14] 1080  200  210  180  600   75  450

summary(df_obesidad$min_vigorosa_total) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   221.3   150.0 69993.0 

###########################
##
## Seleccionar y crear 
## nuevo dataset
##
###########################

# Seleccionar el código seqn y las variables del cuestionario de
# actividad física
obesidad_cuestionario_ejercicio <- df_obesidad %>%
  select(SEQN, trabajo_vigoroso, dias_trabajo_vigoroso, 
         minutos_trabajo_vigoroso, trabajo_moderado, dias_trabajo_moderado,
         minutos_trabajo_moderado, caminar_ciclear, dias_caminar_ciclear,
         minutos_caminar_ciclear, ejercicio_vigoroso,
         dias_ejercicio_vigoroso, minutos_ejercicio_vigoroso,
         ejercicio_moderado, dias_ejercicio_moderado,
         minutos_ejercicio_moderado, minutos_actividad_sedentaria,
         dias_ejercicio_60, horas_tv_videos, horas_computadora)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_cuestionario_dieta_lacteos,
          "eda-obesidad/data/obesidad/9_obesidad_cuestionario_ejercicio.csv",
          row.names = FALSE)
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

# Seleccionar las variables más importantes
df_obesidad <- df_obesidad %>%
  select(SEQN, LBXIN, LBXSGL, LBXGH, LBXSGTSI, LBXTC, LBDLDL,
         LBDHDD, LBXTR, LBXNEPCT, LBXLYPCT, PHAFSTHR_final,
         PHAFSTMN_final, BMXBMI, BMXWAIST)

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
    neutrofilos = LBXNEPCT,
    linfocitos = LBXLYPCT,
    ayuno_horas = PHAFSTHR_final,
    ayuno_minutos = PHAFSTMN_final,
    imc = BMXBMI,
    circunferencia_cintura = BMXWAIST
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
## Neutrófilos %
##
#############################

summary(df_obesidad$neutrofilos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#8.80   51.90   58.40   57.87   64.10   90.80     235  

unique(df_obesidad$neutrofilos)[1:20] # produce:
#[1] 42.3 58.4 68.2 68.7 69.2 58.3 57.5 64.0 70.1 56.0 65.2 46.7   NA
#[14] 49.3 60.7 57.8 60.8 79.7 74.8 58.8

#############################
##
## Linfocitos %
##
#############################

summary(df_obesidad$linfocitos) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#3.40   24.80   30.00   30.47   35.70   88.00     235 

unique(df_obesidad$linfocitos)[1:20] # produce:
#[1] 42.2 27.3 13.9 20.5 17.4 30.2 31.3 22.0 20.9 25.6 40.6   NA 30.8
#[14] 31.6 28.8 29.8 13.7 13.1 30.5 19.5

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

#############################
##
## Transformación log de   
## HOMA-IR 
##
#############################

# Crear la versión log de homa-ir
df_obesidad <- df_obesidad %>%
  mutate(
    log_homa_ir = log(homa_ir + 0.1) # ya que log(0) no existe
  )
unique(df_obesidad$log_homa_ir)[1:20] # produce:
#[1]         NA  1.0058741  0.5137547  1.3960430 -0.1392621  0.3573291
#[7]  1.3782684  0.9687004  0.5075908  1.7095178  2.6940948  3.0089129
#[13]  1.4944122 -0.2587707  0.5460784  0.5901159  0.1284475  1.0947530
#[19]  0.5590230  1.9938635

summary(df_obesidad$log_homa_ir) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-2.0887  0.3428  0.8437  0.9029  1.3838  5.5848    3169 

#############################
##
## HOMA-IR   
## (categórica)
##
#############################

# NOTA: se debe usar la versión normal de homa-ir NO la versión log
# ya que se pierde la escala de categorización

df_obesidad <- df_obesidad %>%
  mutate(
    homa_ir_cat = case_when(
      is.na(homa_ir) ~ NA_character_,
      homa_ir < 2 ~ "normal",
      homa_ir < 3 ~ "resistencia_leve",
      homa_ir >= 3 ~ "resistencia_alta"
    ),
    
    homa_ir_cat = factor(
      homa_ir_cat,
      levels = c("normal", "resistencia_leve", "resistencia_alta"),
      ordered = TRUE
    )
  )
unique(df_obesidad$homa_ir_cat) # produce:
#[1] <NA>             resistencia_leve normal           resistencia_alta
#Levels: normal < resistencia_leve < resistencia_alta

table(df_obesidad$homa_ir_cat) # produce:
#normal resistencia_leve resistencia_alta 
#  1185              504              924 

#############################
##
## Indice inflamatorio NLR 
## neutrófilos / linfocitos
##
#############################

# Crear una nueva variable de índice inflamatorio relacionando
# los neutrófilos con los linfocitos
df_obesidad <- df_obesidad %>%
  mutate(
    nlr = neutrofilos / linfocitos,
    nlr = if_else(linfocitos == 0, NA_real_, nlr)
  )
unique(df_obesidad$nlr)[1:20] # produce:
#[1] 1.002370 2.139194 4.906475 3.351220 3.977011 1.930464 1.837061
#[8] 2.909091 3.354067 1.854305 2.546875 1.150246       NA 1.600649
#[15] 1.920886 1.862620 2.006944 2.040268 5.817518 5.709924
summary(df_obesidad$nlr) # produce:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.100   1.461   1.942   2.179   2.590  25.735     235 

#############################
##
## Indice inflamatorio NLR 
## (transformación log)
##
#############################

# Transformación log del índice inflamatorio
df_obesidad <- df_obesidad %>%
  mutate(
    log_nlr = log(nlr)
  )
unique(df_obesidad$log_nlr)[1:20] # produce
#[1] 0.002366865 0.760429188 1.590555725 1.209324313 1.380530656
#[6] 0.657760169 0.608166850 1.067840630 1.210173635 0.617509766
#[11] 0.934867117 0.139976098          NA 0.470409391 0.652786577
#[16] 0.621983996 0.696613389 0.713081395 1.760873753 1.742205655

summary(df_obesidad$log_nlr) # produce
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-2.3026  0.3793  0.6637  0.6689  0.9515  3.2479     235 

#############################
##
## Indice inflamatorio NLR 
## (categórico)
##
#############################

# Categorizar el índice inflamatorio NLR
df_obesidad <- df_obesidad %>%
  mutate(
    nlr_cat = case_when(
      nlr < 1.5 ~ "baja",
      nlr >= 1.5 & nlr < 3 ~ "moderada",
      nlr >= 3 ~ "alta",
      TRUE ~ NA_character_
    ),
    nlr_cat = factor(nlr_cat, ordered = TRUE,
                     levels = c("baja", "moderada", "alta"))
  )
unique(df_obesidad$nlr_cat) # produce:
#[1] baja     moderada alta     <NA>    
#Levels: baja < moderada < alta

table(df_obesidad$nlr_cat) # produce:
#baja moderada     alta 
#1488     3195      864 

#############################
##
## Indice de hígado graso
## FLI (fatty Liver Index)
##
#############################

# Evitar que trigliceridemia y ggt sean iguales a cero porque estas
# dos variables serán argumentos de log() y log(0) no existe
df_obesidad <- df_obesidad %>%
  mutate(
    trigliceridemia2 = if_else(trigliceridemia <= 0, 
                              NA_real_, trigliceridemia),
    ggt2 = if_else(ggt <= 0, NA_real_, ggt)
  )

# Sacar el índice de hígado graso a partir de ggt, triglicéridos,
# circunferencia de cintura e imc
df_obesidad <- df_obesidad %>%
  mutate(
    fli_logit = 0.953 * log(trigliceridemia2) +
                0.139 * imc +
                0.718 * log(ggt2) +
                0.053 * circunferencia_cintura -
                15.745,
    
    fli = (exp(fli_logit) / (1 + exp(fli_logit))) * 100
  )
unique(df_obesidad$fli)[1:20] # produce:
#[1]        NA 41.298562 74.788479  1.556748  2.403979 68.492595
#[7] 71.186005 32.493213 89.209848 82.286050 97.277111 96.178393
#[13] 86.801534 81.186573 52.081133 28.195003 42.684673 33.240027
#[19] 71.622722  6.875189

summary(df_obesidad$fli) # produce:
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#0.5662  15.8408  45.7135  48.1498  79.1301  99.9976     3236 

#############################
##
## Versión log de fli
## Indice de hígado graso
##
#############################

unique(df_obesidad$fli_logit)[1:20] # produce:
#[1]          NA -0.35163653  1.08736276 -4.14688106 -3.70371125
#[6]  0.77650302  0.90443505 -0.73119691  2.11235762  1.53584916
#[11]  3.57587019  3.22553370  1.88352372  1.46217908  0.08329342
#[16] -0.93480931 -0.29472815 -0.69734890  0.92582359 -2.60602145

summary(df_obesidad$fli_logit) # produce:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-5.1684 -1.6701 -0.1719  0.0055  1.3328 10.6378    3236 

#############################
##
## Indice de hígado graso
## Categórico
##
#############################

# Categorizar el índice de hígado graso pasándolo a factor y 
# ordenándolo
df_obesidad <- df_obesidad %>%
  mutate(
    fli_cat = case_when(
      fli < 30 ~ "bajo",
      fli >= 30 & fli < 60 ~ "intermedio",
      fli >= 60 ~ "alto",
      TRUE ~ NA_character_
    ),
    fli_cat = factor(fli_cat, ordered = TRUE,
                     levels = c("bajo", "intermedio", "alto"))
  )
unique(df_obesidad$fli_cat) # produce:
#[1] <NA>       intermedio alto       bajo      
#Levels: bajo < intermedio < alto

table(df_obesidad$fli_cat) # produce:
#bajo intermedio       alto 
# 972        554       1020 

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
         ayuno_minutos, homa_ir, log_homa_ir, homa_ir_cat, nlr,
         log_nlr, nlr_cat, fli, fli_logit, fli_cat)

# Crear un archivo .cvs con la dataset de laboratorio
write.csv(obesidad_laboratorio,
          "eda-obesidad/data/obesidad/5_obesidad_laboratorio.csv",
          row.names = FALSE)


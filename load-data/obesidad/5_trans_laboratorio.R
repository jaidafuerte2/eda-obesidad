#################################
##                             ##
## Transformaciones Básicas    ##
## del dataset laboratorio     ##
##                             ##
#################################

# Nota: no olvidar crear las variables de homa-ir y nos/mos

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
    ayuno_horas = PHAFSTHR,
    ayuno_minutos = PHAFSTMN
  )



########################
##
## Insulina en Ayunas
##
########################


library(dplyr)
library(tidyverse)
library(rstanarm)
library(bayesplot)
options(scipen = 999) # evitar la notación científica

# Cargar la dataset de obesidad
df_obesidad <- read_csv(
  "eda-obesidad/projects/libro_bayes/data/obesidad_nhanes.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

#######################
##
## Simulación
##
#######################

set.seed(123)

# Simular 100 pacientes con las variables de cintura y glicemia
datos <- tibble(
  cintura = rnorm(100, mean = 100, sd = 10),
  glicemia = 70 + cintura * 0.6 + rnorm(100, 0, 10)
)

# Diagrama de dispersión que relaciona la cintura y la glicemia
ggplot(datos, aes(cintura, glicemia)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación simulada entre cintura y glicemia",
    x = "Cintura (cm)",
    y = "Glicemia"
  )

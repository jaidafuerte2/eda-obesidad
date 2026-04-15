library(dplyr)
library(tidyverse)
library(dagitty)

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

# Simular 1000 pacientes
n <- 1000

# Crear simulación con las variables actividad física, obesidad y
# glicemia
df_sim <- tibble(
  # Simular actividad física
  actividad = rnorm(n, mean = 0, sd = 1),
  # Simular obesidad con una media de cero y una desviación de uno
  obesidad = 0.7 * (-actividad) + rnorm(n, 0, 1),
  # Simular glicemia con una media de cero y una desviación de uno
  glicemia = 0.8 * obesidad + rnorm(n, 0, 1)
)

# Diagrama de dispersión que relaciona la obesidad con la glicemia
ggplot(df_sim, aes(x = obesidad, y = glicemia)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Obesidad vs Glicemia")

# Diagrama de dispersión que relaciona la actividad física con la 
# glicemia
ggplot(df_sim, aes(x = actividad, y = glicemia)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Actividad vs Glicemia")

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionar sólo las variables que voy a usar
df_joint <- df_obesidad %>%
  select(BMXBMI, LBXSGL, BPXSY_mean, PAD680, PAQ655, PAD660) %>%
  drop_na()

# Diagrama de dispersión que relaciona el índice de masa corporal
# con la glicemia
df_joint %>%
  filter(
    LBXSGL <= 300,
    BMXBMI <= 50
  ) %>%
  ggplot(aes(x = BMXBMI, y = LBXSGL)) +
    geom_point(alpha = 0.3) +
    geom_smooth() +
    labs(
      title = "IMC vs Glicemia",
      x = "IMC",
      y = "Glicemia"
    )

# Diagrama de dispersión que relaciona el índice de masa corporal 
# con la glicemia
df_joint %>%
  filter(
    BPXSY_mean <= 200,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = BPXSY_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(
    title = "IMC vs Presión Arterial",
    x = "IMC",
    y = "Presión sistólica"
  )

# Diagrama de dispersión que relaciona el sedentarismo con el índice
# de masa corporal
df_joint %>%
  filter(
    PAD680 <= 1250,
    BMXBMI <= 60
  ) %>%
  ggplot(aes(x = BMXBMI, y = PAD680)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(
    title = "IMC vs Sedentarismo",
    x = "IMC",
    y = "Sedentarismo"
  )

# Dag de sedentarismo, imc, glicemia y presión
dag <- dagitty("
dag {
  sedentarismo -> imc
  imc -> glicemia
  imc -> presion
}
")
#plot(dag)

# Crear la variable de minutos de actividad física vigorosa 
# recreativa por semana
df_joint <- df_joint %>%
  mutate(
    PAQ655 = if_else(PAQ655 %in% c(77, 99), NA_real_, as.numeric(PAQ655)),
    PAD660 = if_else(PAD660 %in% c(7777, 9999), NA_real_, as.numeric(PAD660))
  ) %>%
  mutate(
    min_vigorosa_semana = PAQ655 * PAD660
  )

# Diagrama de dispersión que relaciona la actividad recreativa 
# vigorosa con el índice de masa corporal
df_joint %>%
  filter(
    min_vigorosa_semana <= 500,
    BMXBMI <= 45
  ) %>%
  ggplot(aes(x = BMXBMI, y = min_vigorosa_semana)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "IMC vs Ejercicio",
    x = "IMC",
    y = "Ejercicio"
  )

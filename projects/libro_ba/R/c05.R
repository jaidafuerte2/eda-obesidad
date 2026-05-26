########################
##                    ##
## Capítulo 5         ## 
##                    ##
########################

library(readr)
library(dplyr)
library(tidyverse)
library(skimr)

# Cargar la dataset de marketing
marketing <- read_delim(
  "eda-obesidad/projects/libro_ba/data/marketing_campaign.csv",
  delim = ";"
  #guess_max = 10000,
  #show_col_types = FALSE
)
#glimpse(marketing) # produce:
skim(marketing) # produce:

# Calcular la proporción general
marketing %>%
  summarise(
    conversion_rate = mean(Response)
  ) # produce:
# A tibble: 1 × 1
#    conversion_rate
#              <dbl>
#  1           0.149

# Gráfico de barras de Conteo básico de la variable Response
marketing %>%
  ggplot(aes(x = Response)) +
  geom_bar() +
  labs(
    title = "Respuesta a la campaña",
    x = "Respuesta",
    y = "Número de clientes"
  )

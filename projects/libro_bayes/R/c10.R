library(dplyr)
library(tidyverse)
library(dagitty)
library(ggdag)

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

# Crear 300 pacientes
n <- 300

# Crear una simulación de neutrófilos, glucosa y aspirina a partir
# de una simulación de inflamación
datos_proxy <- tibble(
  inflamacion_real = rnorm(n, mean = 0, sd = 1),
  neutrofilos = 4 + inflamacion_real * 1.2 + rnorm(n, 0, 0.8),
  glucosa = 90 + inflamacion_real * 8 + rnorm(n, 0, 10),
  aspirina = 50 + inflamacion_real * 30 + rnorm(n, 0, 20)
)

# Diagrama de dispersión y línea suavizada que compara aspirina con
# neutrófilos
ggplot(datos_proxy, aes(x = aspirina, y = neutrofilos)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Aspirina como proxy de inflamación",
    x = "Dosis de aspirina",
    y = "Neutrófilos"
  )

# Agregar edad como factor de confusión
datos_proxy <- datos_proxy %>%
  mutate(
    edad = rnorm(n, mean = 55, sd = 12),
    
    aspirina = aspirina + edad * 0.8
  )

# Diagrama de dispersión y línea suavizada que compara aspirina con
# neutrófilos
ggplot(datos_proxy, aes(x = aspirina, y = neutrofilos)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Aspirina como proxy de inflamación",
    x = "Dosis de aspirina",
    y = "Neutrófilos"
  )

###########################
##
## Ejemplos con NHANES
##
###########################

# Seleccionamos sólo las variables que vamos a usar
df_aspirina <- df_obesidad %>%
  select(
    RXD530,
    RXQ515,
    RXQ520,
    LBDNENO,
    LBDLYMNO,
    LBXSGTSI,
    BMXBMI,
    LBXSGL
  ) %>%
  filter(
    !is.na(RXD530),
    !is.na(LBDNENO),
    !is.na(BMXBMI),
    !is.na(LBXSGL)
  )

# Diagrama de dispersión y línea suavizada que relacionan la dosis
# de aspirina con índice de masa corporal
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
ggplot(my_df_aspirina, aes(x = RXD530, y = BMXBMI)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dosis de aspirina e IMC",
    x = "Dosis de aspirina",
    y = "IMC"
  )

# Diagrama de dispersión que relaciona la dosis de aspirina con 
# netrófilos
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
ggplot(my_df_aspirina, aes(x = RXD530, y = LBDNENO)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dosis de aspirina y neutrófilos",
    x = "Dosis de aspirina",
    y = "Neutrofilos"
  )

# Diagrama de dispersión que relaciona la dosis de aspirina con 
# glicemia
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
ggplot(my_df_aspirina, aes(x = RXD530, y = LBXSGL)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dosis de aspirina y glucosa",
    x = "Dosis de aspirina",
    y = "Glucosa"
  )

# Crear una variable binaria simple
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
my_df_aspirina <- my_df_aspirina %>%
  mutate(
    aspirina_cat = case_when(
      RXD530 <= 100 ~ "sí",
      TRUE ~ "no"
    )
  )
# Resumir la aspirina antiinflamatoria por imc, glucosa, neutrófilos
# y frecuencia absoluta
my_df_aspirina %>%
  group_by(aspirina_cat) %>%
  summarize(
    imc_promedio = mean(BMXBMI, na.rm = TRUE),
    glucosa_promedio = mean(LBXSGL, na.rm = TRUE),
    nos_los_ratio = mean(LBDNENO/LBDLYMNO, na.rm = TRUE),
    n = n()
  )

# Diagrama de caja que relaciona el uso de aspirina por causas
# inflamatorias (<= 100mg) con neutrófilos
ggplot(my_df_aspirina, aes(x = aspirina_cat, y = LBDNENO)) +
  geom_boxplot() +
  labs(
    title = "Neutrófilos según uso de aspirina",
    x = "Uso de aspirina",
    y = "Neutrófilos"
  )

# Crear un dag simple de aspirina
dag_aspirina <- dagitty("
dag {
  obesidad -> inflamacion
  edad -> aspirina
  edad -> glucosa
  inflamacion -> aspirina
  inflamacion -> neutrofilos
  inflamacion -> glucosa
  obesidad -> glucosa
  obesidad -> aspirina
}
")
# Visualizar el dag
plot(dag_aspirina)

# Crear un dag de aspirina con la aspirina no observada
dag_aspirina2 <- dagitty("
dag {
  obesidad -> inflamacion
  edad -> aspirina
  edad -> glucosa
  
  inflamacion [unobserved]
  
  inflamacion -> aspirina
  inflamacion -> neutrofilos
  inflamacion -> glucosa
  obesidad -> glucosa
}
")
# Visualizar el  dag
#plot(dag_aspirina2)

####################
##
## Ejercicios
##
####################

#####################
##
## Ejercicio 1
##
#####################

# Ejercicio 1 — Pensar como clínico

# Sin usar código, responde:
  
#  ¿Por qué la aspirina puede relacionarse con inflamación?
#   Porque se suele usar como profiláctico para prevenir infartos,
#   y los infartos se producen en parte por inflamación y por 
#   formación de coágulos. La aspirina es un antiinflamatorio por
#   excelencia y un antiagregante plaquetario
#  ¿Por qué la aspirina también puede relacionarse con edad?
#   Con la edad aumenta el riesgo cardiovascular y el daño del
#   endotelio que puede terminar en un coágulo. La aspirina al ser
#   un antiinflamatorio podría prevenir el daño del endotelio 
#   y la formación de coágulos
#  ¿Por qué dos personas con la misma dosis de aspirina podrían 
#   tener niveles muy diferentes de inflamación?
#   Creo que por la profilaxis podrías tener ese caso. Por ejemplo
#   se podría usar el mismo caso de la edad que es un factor de 
#   riesgo cardiovascular independientemente del laboratorio, pero
#   podría ser un paciente realmente poco inflamado. Hay que tomar 
#   en cuenta que las dosis menores de 100mg suelen ser seguras
#  ¿Por qué una relación plana entre aspirina y neutrófilos no significa 
#   necesariamente que la aspirina “no sirve”?
#  Porque no sabemos relamente cual es el nivel basal de inflamación

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2 — Comparar proxies

# Ordena estas variables desde la que crees que podría ser un proxy más fuerte 
# de inflamación hasta la más débil:
  
# - neutrófilos
# - GGT
# - glucosa
# - IMC
# - dosis de aspirina
# - linfocitos

# Luego explica por qué pusiste la aspirina en esa posición.
# Pienso que ese mismo orden es adecuado. La dosis de aspirina
# no parece darnos una idea certera del nivel de inflamación

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3 — Crear una variable binaria de aspirina

# Crea una variable llamada usa_aspirina usando RXD530.

# Crear una variable binaria simple
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
my_df_aspirina <- my_df_aspirina %>%
  mutate(
    aspirina_cat = case_when(
      RXD530 <= 100 ~ "sí",
      TRUE ~ "no"
    )
  )

# Después calcula:
  
# - IMC promedio
# - glucosa promedio
# - neutrófilos promedio

# para los grupos "sí" y "no"

# Resumir la aspirina antiinflamatoria por imc, glucosa, neutrófilos
# y frecuencia absoluta
my_df_aspirina %>%
  group_by(aspirina_cat) %>%
  summarize(
    imc_promedio = mean(BMXBMI, na.rm = TRUE),
    glucosa_promedio = mean(LBXSGL, na.rm = TRUE),
    nos_los_ratio = mean(LBDNENO/LBDLYMNO, na.rm = TRUE),
    n = n()
  ) # produce:
#aspirina_cat imc_promedio glucosa_promedio nos_los_ratio     n
#  <chr>               <dbl>            <dbl>         <dbl> <int>
#1 no                   29.5             111.          2.48   139
#2 sí                   30.1             121.          2.46   884

# Pregunta para interpretar:
  
#  ¿Las personas que usan aspirina parecen tener peor perfil 
#   metabólico? Sí, por la glucosa, los que usan aspirina en menos
#   de 100 mg parecen tener glucosa más alta
#  ¿Crees que eso significa que la aspirina causa ese peor perfil?
#   No, estos promedios me podrían sugerir que las personas
#   con dosis bajas (menos de 100mg ) están tomando medidas de
#   diversos tipos para bajar esa inflamación (aunque no sé, los
#   resultados son confusos).

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4 — Relación visual con neutrófilos

# Haz este gráfico:

# Diagrama de dispersión que relaciona la dosis de aspirina con 
# la relación neutrófilos linfocitos
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
ggplot(my_df_aspirina, aes(x = RXD530, y = LBDNENO/LBDLYMNO)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dosis de aspirina y neutrófilos",
    x = "Dosis de aspirina",
    y = "Neutrofilos/linfocitos"
  )

#Luego responde:
  
#  ¿La pendiente parece positiva, negativa o plana?
#   Es pendiente plana
#  ¿La relación parece fuerte o débil?
#   Es débil
#  ¿Qué otras variables podrían estar ocultando una relación más clara?
#   Tal vez imc, glicemia, glicohemoglobina, cintura, abdomen, etc
#  ¿Qué te enseña esto sobre los proxies imperfectos?
#   Que no necesariamente son muy útiles, llegan a ser muy confusos

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5 — Comparar con otro proxy

# Haz dos gráficos:

# Diagrama de dispersión que relaciona imc con neutrófilos
ggplot(my_df_aspirina, aes(x = BMXBMI, y = LBDNENO)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)

# Diagrama de dispersión que relaciona la dosis de aspirina con los
# neutrófilos
my_df_aspirina <- df_aspirina %>%
  filter(
    RXD530 <= 500
  )
ggplot(my_df_aspirina, aes(x = RXD530, y = LBDNENO)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)

# Después compara:
  
#  ¿Qué variable parece relacionarse más claramente con neutrófilos?
#  El índice de masa corporal
#  ¿IMC parece un proxy más fuerte de inflamación que la aspirina?
#  Sí, mucho más
#  ¿Qué nos dice esto sobre obesidad e inflamación?
#  Que tal vez la obesidad causa inflamación o la inflamación causa
#  obesidad

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6 — DAG conceptual

# Crea este DAG:

# Crear un dag de aspirina
dag_asprina <- dagitty('
dag {
  obesidad -> inflamacion
  edad -> aspirina
  edad -> glucosa
  
  inflamacion [unobserved]
  
  inflamacion -> aspirina
  inflamacion -> neutrofilos
  inflamacion -> glucosa
  obesidad -> glucosa
}
')

# Visualizar el dag
#plot(dag_aspirina)

# Luego responde:
  
# ¿Por qué la edad puede confundir la relación entre aspirina y 
#  glucosa? Porque aspirina se puede dar profilacticamente en edad
#  avanzada y la glicemia puede aumentar con la edad por la
#  resistencia a la insulina
# ¿Por qué la obesidad puede aumentar tanto la inflamación como la 
#  glucosa? La obesidad puede aumentar la inflamación por el tejido 
#  adiposo visceral y la glicemia por la resistencia a la insulina
# ¿Por qué no sería correcto decir “la aspirina aumenta la glucosa” 
#  solamente viendo una asociación? Porque justamente la aspirina
#  se podría dar como profiláctico en problemas metabólicos, lo que
#  no significa que la aspirina cause estos problemas

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7 — Crear una hipótesis bayesiana
# Imagina que observas esto:
  
  
#  IMC alto
#  glucosa alta
#  neutrófilos ligeramente altos
#  uso de aspirina

# Escribe una hipótesis clínica simple de 3–5 líneas sobre qué 
# podría estar pasando en esa persona.

# La idea no es encontrar “la verdad”, sino practicar pensamiento 
# bayesiano:

#integrar varias pistas imperfectas para formar una creencia razonable sobre un proceso no observable como la inflamación crónica.

# La obesidad está muy relacionada con resistencia a la insulina. La
# resistencia a la insulina se caracteriza por niveles altos de glucosa 
# e insulina. Además en la obesidad puede haber tejido adisposo
# visceral acumulado lo que se asocia con hipoxia e inflamación 
# caracterizada por altos niveles de neutrófilos. Todo esto nos
# indica que una persona tiene más riesgo metabólico y cardiovascular
# por lo que se le podría prescribir aspirina como antinflamatorio


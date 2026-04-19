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

# 300 Pacientes
n <- 300

# Simulación de dieta -> insulina -> cintura -> inflamación 
df_sim <- tibble(
  calidad_dieta = rnorm(n, mean = 0, sd = 1),
  # Una mejor dieta (mayor dieta) será igual a una menor insulina
  insulina = 15 - 3 * calidad_dieta + rnorm(n, 0, 2),
  # Más insulina dará lugar a una mayor cintura
  cintura = 80 + 2 * insulina + rnorm(n, 0, 5),
  # Mayor cintura da lugar a mayor inflamación
  inflamacion = 2 + 0.08 * cintura + rnorm(n, 0, 1)
)

# Diagrama de dispersión que relaciona insulina con cintura
ggplot(df_sim, aes(x = insulina, y = cintura)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Mayor insulina, mayor cintura abdominal",
    x = "Insulina",
    y = "Circunferencia de cintura"
  )

# Diagrama de dispersión que relaciona cintura con inflamación
ggplot(df_sim, aes(x = cintura, y = inflamacion)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Mayor cintura abdominal, mayor inflamación",
    x = "Circunferencia de cintura",
    y = "Inflamación"
  )

# Representar el ejemplo de la simulación con un DAG
dag_simple <- dagitty('
dag {
  calidad_dieta -> insulina
  insulina -> cintura
  cintura -> inflamacion
}
')

ggdag(dag_simple, text = FALSE) + # text = False impide que se pongan
                                  # los nombres de los nodos
  theme_dag()

###########################
##
## Ejemplos con NHANES
##
###########################

# Crear una tabla simplificada con las variables seleccionadas
nhanes_dag <- df_obesidad %>%
  transmute(
    imc = BMXBMI,
    cintura = BMXWAIST,
    diametro_abdominal = BMDAVSAD,
    insulina = LBXIN,
    glucosa = LBXSGL,
    hba1c = LBXGH,
    neutrofilos = LBDNENO,
    linfocitos = LBDLYMNO,
    ggt = LBXSGTSI,
    aspirina = RXD530,
    actividad_vigorosa = PAD680
  )

# Crear una primera versión del modelo causal 
dag_obesidad <- dagitty('
dag {

  dieta -> insulina
  dieta -> inflamacion

  actividad_fisica -> insulina
  actividad_fisica -> inflamacion

  sedentarismo -> insulina
  sedentarismo -> inflamacion

  insulina -> cintura
  insulina -> diametro_abdominal
  insulina -> imc
  
  cintura -> inflamacion
  diametro_abdominal -> inflamacion
  imc -> inflamacion

  inflamacion -> resistencia_insulina
  resistencia_insulina -> insulina

  glucosa <- insulina
  hba1c <- glucosa

  aspirina -> inflamacion

  edad -> actividad_fisica
  edad -> imc

}
')

# Visualizar el modelo causal
#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()

########### Relación entre insulina y adiposidad #############

# Diagrama de dispersión que relaciona la insulina con la cintura
my_nhanes_dag <- nhanes_dag %>%
  filter(
    insulina <= 200,
    cintura < 200
  )
ggplot(my_nhanes_dag, aes(x = insulina, y = cintura)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Insulina e incremento de cintura abdominal",
    x = "Insulina",
    y = "Circunferencia de cintura"
  )

# Diagrama de dispersión que relaiona la insulina con el diametro 
# abdominal
my_nhanes_dag <- nhanes_dag %>%
  filter(
    insulina <= 200,
    diametro_abdominal <= 40
  )
ggplot(my_nhanes_dag, aes(x = insulina, y = diametro_abdominal)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Insulina y diámetro abdominal",
    x = "Insulina",
    y = "Diámetro abdominal sagital"
  )

######### Relación entre grasa visceral e inflamación ############

# Diagrama de dispersión que relaciona la cintura con la ggt
ggplot(nhanes_dag, aes(x = cintura, y = ggt)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Cintura abdominal y GGT",
    x = "Circunferencia de cintura",
    y = "GGT"
  )

# Diagrama de dispersión que relaciona el diámetro abdominal con
# los neutrófilos
ggplot(nhanes_dag, aes(x = diametro_abdominal, y = neutrofilos)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Diámetro abdominal y neutrófilos",
    x = "Diámetro abdominal sagital",
    y = "Neutrófilos"
  )

############# ¿Dónde entra la aspirina? ################


# Diagrama de caja que relaciona la aspirina con los neutrófilos
my_nhanes_dag <- nhanes_dag %>%
  filter(
    aspirina <= 500
  )
ggplot(my_nhanes_dag, aes(x = factor(aspirina), y = neutrofilos)) +
  geom_boxplot() +
  labs(
    title = "Neutrófilos según uso de aspirina",
    x = "Uso de aspirina",
    y = "Neutrófilos"
  )

###########################
##
## Ejercicio sugerido
##
###########################

# Construye tu propia versión del DAG de obesidad.

# Intenta agregar: sueño, estrés, alcohol, tabaquismo, depresión,
# edad, sexo, medicamentos

dag_obesidad <- dagitty('
dag {

  alcohol -> dieta

  dieta -> glicemia
  suenio -> glicemia
  estres -> glicemia
  
  dieta -> insulina
  

  glicemia -> resistencia_insulina
  insulina -> resistencia_insulina

  insulina -> cintura
  insulina -> diametro_abdominal
  insulina -> imc
  
  cintura -> inflamacion
  diametro_abdominal -> inflamacion

  inflamacion -> resistencia_insulina
  
}
')

# Visualizar el modelo causal
#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()

# Luego pregúntate:

#  ¿Qué variables parecen causas? Alcohol, dieta, sueño y estrés.
#  ¿Qué variables parecen mediadores? Glicemia e insulinemia y 
#   resistencia a la insulina
#  ¿Qué variables parecen resultados? cintura, abdomen , imc,
#   inflamación
#  ¿Qué variables podrían ser confusores? Alcohol
  
# Ese ejercicio te ayudará a pensar como analista causal.

# Y ese es exactamente el siguiente paso para comenzar a hacer 
# inferencia bayesiana más adelante.

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

# Ejercicio 1

# Para cada una de las siguientes parejas de variables, escribe una posible historia causal.

# Por ejemplo:
  
# -insulina → cintura
# -cintura → inflamación
# -actividad física → insulina
# -sedentarismo → HbA1c
# -dieta → glucosa
# -glucosa → HbA1c

#Para cada una responde:
  
#  ¿Cuál crees que es la causa?
#  ¿Cuál crees que es el resultado?
#  ¿Cuál podría ser el mecanismo intermedio?

# insulina -> cintura : La insulina es una hormona anábólica, 
# que principalmente tiene efecto sobre las grasas en general. Por 
# qué en la cintura? tal vez por factores genéticos que aumentan la
# facilidad de hipertrofia del adiposito visceral

# Cintura -> inflamación: La grasa visceral suele hipertrofiarse
# con facilidad, pero no suele vascularizarse tan rápido por los
# que hay cierto nivel de hipoxia tisular lo que produce cantidades
# considerables de citoquinas inflamatorias. El mecanismo intermedio 
# sería la hipoxia isquémica.

# actividad fisica - insulina -> Los músculos toman glucosa de la
# sangre durante el ejercicio. Y menos glicemia debería disminuir
# el nivel de insulina. El mecanismo intermedio podría ser la 
# disminuición de glicemia.

# sedentarismo -> HbA1c : Las personas sedentarias hacen menos
# uso de su glucosa saguínea. Esto debería aumentar la glicemia a
# largo plazo y entonces la hemoglobina glicosilada.

# dieta -> glucosa : Una dieta alta en carbohidratos procesados
# aumenta los niveles de glicemia directamente.

# glucosa -> HbA1c : Más glicemia, directamente aumenta los niveles
# de glucosa y esto influye directamente en la hemoglobina 
# glicosilada

#####################
##
## Ejercicio 2
##
#####################

# Ejercicio 2

# Decide si cada variable actúa principalmente como:
  
# - causa
# - mediador
# - resultado
# - posible confusor

# Variables:
  
# - edad
# - insulina
# - cintura abdominal
# - actividad física
# - inflamación
# - HbA1c
# - dieta
# - aspirina
# - sedentarismo

# No existe una única respuesta correcta.

# Lo importante es justificar tu razonamiento.

# - Edad: No suele ser la causa ni mediador sino confusor porque
#   la resistencia a la insulina , al ser un problema hormonal,
#   suele empeorar con el tiempo. Entonces a más edad, más tiempo
#   con resistencia a la insulina y más obesidad.

# - Insulina: Suele ser el mediador más importante por ser una hormona
#   anabólica por excelencia sobre todo relacionada con la grasa.

# - Cintura: Suele ser efecto de niveles altos de insulina pues
#   La insulina busca donde almacenar la grasa y el adiposito
#   visceral tiene una gran capacidad de hipertrofia

# - Actiidad física: Puede ser causa, pero para mí es un gran confusor
#   ya que muchas veces aumenta el apetito y esto nos hace ser más
#   obesos

# - Inflamación: Creo que es una gran efecto de un tejido graso
#   visceral demasiado hipertrofiado.

# - Hemoglobina glicosilada: Es efecto de niveles de glucosa 
#   permanentemente elevados. También es mediador pues la glicemia
#   alta es resultado de mala alimentación  que termina afectando
#   aumentando insulina

# - Dieta: Es la causa más importante de altos niveles de glicemia 
#   e insulina.

# - Aspirina: Es el efecto de tener riesgo cardiovascular alto
#   ya que actúa como un antiinflamatorio.

# - Sedentarismo: Es una posible causa que aumente la glicemia pues
#   se usan menos los músculos (que consumen glucosa).

#####################
##
## Ejercicio 3
##
#####################

# Ejercicio 3

# Explica por qué estas dos historias no significan lo mismo:
  
#  Historia A:
  
#  actividad física → insulina → cintura

# Historia B:
  
#  cintura → actividad física

# Pregunta:
  
#   ¿Cómo cambiaría tu interpretación clínica en cada caso?

# En el primer caso la falta de actividad física influye en un 
# aumento de insulina y esto produce aumento de cintura.

# En el segundo caso el aumento del diámetro de la cintura puede
# influir en que a una persona se le dificulte hacer actividad 
# física.

#####################
##
## Ejercicio 4
##
#####################

# Ejercicio 4

# Dibuja un DAG simple con estas variables:
  
# - dieta
# - insulina
# - cintura
# - inflamación

# Debes incluir al menos:
  
# - una causa directa
# - una causa indirecta
# - un mediador

dag_obesidad <- dagitty('
dag {
  dieta -> insulina
  insulina -> cintura
  cintura -> inflamacion
}
')

# Visualizar el modelo causal
#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()

#####################
##
## Ejercicio 5
##
#####################

# Ejercicio 5

# Amplía el DAG anterior agregando:
  
# - actividad física
# - sedentarismo
# - edad
# - HbA1c

dag_obesidad <- dagitty('
dag {
  actividad_fisica -> HbA1c
  sedentarismo -> HbA1c
  
  edad -> HbA1c
  edad -> insulina
  
  HbA1c -> insulina
  
  dieta -> insulina
  
  insulina -> cintura
  
  cintura -> inflamacion
}
                        
') 

#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()

# Pregunta:
  
# - ¿Qué variables parecen más “centrales”? Insulina y Glicemia
# - ¿Qué variables parecen más “finales”? Inflamación
# - ¿Qué variables parecen estar en el medio de la historia?
#   Insulina y glicemia.

#####################
##
## Ejercicio 6
##
#####################

# Ejercicio 6

# Construye un DAG alternativo donde el sueño tenga un papel importante.

# Por ejemplo, puedes pensar algo como:
  
# - mal sueño → más apetito
# - mal sueño → menos actividad física
# - mal sueño → más inflamación
# - mal sueño → peor sensibilidad a la insulina

dag_obesidad <- dagitty('
dag {
  insomnio -> HbA1c
  actividad_fisica -> HbA1c
  sedentarismo -> HbA1c
  
  edad -> HbA1c
  edad -> insulina
  
  HbA1c -> insulina
  
  dieta -> insulina
  
  insulina -> cintura
  
  cintura -> inflamacion
}
')

ggdag(dag_obesidad, use_labels = "name") +
  theme_dag()

# Pregunta:
  
#  ¿Cómo cambia tu modelo causal cuando agregas sueño? No cambia
#  mucho relamente, sólo un nodo más que afecta a la glicemia

#####################
##
## Ejercicio 7
##
#####################

# Ejercicio 7

# Crea una tabla simplificada de NHANES usando transmute() con 
# estas variables:
  
# imc = BMXBMI
# cintura = BMXWAIST
# insulina = LBXIN
# glucosa = LBXSGL
# hba1c = LBXGH
# neutrofilos = LBDNENO
# ggt = LBXSGTSI
# actividad = PAD680

df_obesidad_dag <- df_obesidad %>%
  transmute(
    imc = BMXBMI,
    cintura = BMXWAIST,
    diametro_abdominal = BMDAVSAD,
    insulina = LBXIN,
    glucosa = LBXSGL,
    hba1c = LBXGH,
    neutrofilos = LBDNENO,
    ggt = LBXSGTSI,
    aspirina = RXD530,
    actividad = PAD680
  )

# Pregunta:
  
# ¿Por qué transmute() puede ser útil cuando quieres trabajar
# solo con pocas variables? Porque puedo tener mejor visibilidad
# de las variables que voy a usar, y porque ocupa menos memoria.

#####################
##
## Ejercicio 8
##
#####################

# Ejercicio 8

# Haz un gráfico de dispersión entre:
  
# insulina y cintura
# insulina y diámetro abdominal
# cintura y neutrófilos
# cintura y GGT

# Usa:
  
# geom_point(alpha = 0.3)
# geom_smooth(method = "lm", se = FALSE)

# 1. 

# Filtrar los valores de insulina menores a 200
my_obesidad_dag <- df_obesidad_dag %>%
  filter(
    insulina <= 200
  )
# Diagrama de dispersión que relaciona cintura con insulina
ggplot(my_obesidad_dag, aes(x = cintura, y = insulina)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + 
  labs(
    title = "cintura vs insulina",
    x = "Cintura",
    y = "Insulina"
  )

# 2.

# Filtrar los valores de insulina menores a 200
my_obesidad_dag <- df_obesidad_dag %>%
  filter(
    insulina <= 200
  )
# Diagrama de dispersión que relaciona abdomen con insulina
ggplot(my_obesidad_dag, aes(x = diametro_abdominal, y = insulina)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + 
  labs(
    title = "Diámetro abdominal vs insulina",
    x = "Diámetro abdominal",
    y = "Insulina"
  )

# 3.  

# Filtrar los valores de cintura menores a 200
my_obesidad_dag <- df_obesidad_dag %>%
  filter(
    cintura <= 300
  )
# Diagrama de dispersión que relaciona cintura con neutrófilos
ggplot(my_obesidad_dag, aes(x = cintura, y = neutrofilos)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + 
  labs(
    title = "Cintura vs neutrófilos",
    x = "Cintura",
    y = "Neutrófilos"
  )

# 4.  

# Filtrar los valores de cintura menores a 200
my_obesidad_dag <- df_obesidad_dag %>%
  filter(
    cintura <= 300
  )
# Diagrama de dispersión que relaciona cintura con ggt
ggplot(my_obesidad_dag, aes(x = cintura, y = ggt)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + 
  labs(
    title = "Cintura vs ggt",
    x = "Cintura",
    y = "ggt"
  )

# Pregunta:
  
#  ¿Qué relación parece más fuerte? la relación cintura - insulina 
#  ¿Cuál parece más débil? cintura - ggt
#  ¿Qué variable parece comportarse como mejor proxy de inflamación?
#  Neutrófilos

#####################
##
## Ejercicio 9
##
#####################

# Ejercicio 9

# Haz un boxplot comparando neutrófilos según uso de aspirina.
my_obesidad_dag <- df_obesidad_dag %>%
  filter(
    aspirina <= 500
  ) %>%
  mutate(
    aspirina_cat = case_when(
      is.na(aspirina) ~ "no_usa",
      aspirina <= 100 ~ "baja_dosis",
      aspirina > 100 ~ "alta_dosis"
    )
  )
ggplot(my_obesidad_dag, aes(x = aspirina_cat, y = neutrofilos)) +
  geom_boxplot()

# Pregunta:
  
#  ¿Qué grupo parece tener mayor inflamación? Los que toman dosis
#  mayores a 100mg
#  ¿Crees que la aspirina está disminuyendo inflamación?
#  Al parecer NO. 
#  ¿O crees que las personas que usan aspirina ya tienen más 
#  enfermedad y riesgo metabólico? Los que toman bajas dosis ya
#  tienen inflamación. Sin embargo tienen menos inflamación que
#  los que toman mas de 100mg (tal vez lo toman como analgésico)

#####################
##
## Ejercicio 10
##
#####################

# Ejercicio 10

# Haz un gráfico de HbA1c contra glucosa.

# Filtrar los valores de glucosa menores a 1000
my_obesidad_dag <- df_obesidad_dag %>%
  filter(
    glucosa <= 1000
  )
# Diagrama de dispersión que relaciona glucosa con glicohemoglobina
ggplot(my_obesidad_dag, aes(x = glucosa, y = hba1c)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + 
  labs(
    title = "Glucosa vs Glicohemoglobina",
    x = "Glucosa",
    y = "Glicohemoglobina"
  )

# Pregunta:
  
#  ¿La pendiente es positiva? Sí, muy positiva. Directamente 
#  proporcional
#  ¿Tiene sentido fisiopatológico? Claro, porque la hemglobina
#  Glicosilada es el promedio de las glicemias de los últimos
#  3 meses
#  ¿Cuál de las dos variables parece más “final” dentro del 
#  proceso metabólico? La glicohemoglobina

#####################
##
## Ejercicio 11
##
#####################

# Ejercicio 11

# Imagina dos pacientes:
  
#  Paciente A:
  
# IMC = 32
# cintura muy alta
# insulina alta
# neutrófilos altos
# HbA1c alta

# Paciente B:
  
# IMC = 32
# cintura moderada
# insulina normal
# neutrófilos normales
# HbA1c normal

# Pregunta:
  
#  ¿Cuál parece tener más riesgo metabólico? El paciente A
#  ¿Por qué el IMC por sí solo no es suficiente? Este es un caso 
#  muy, muy atípico; el tipo pesado repleto de músculos. El 
#  musculoso tiene poca grasa visceral entonces no tiene inflamación,
#  como hace mucho ejercicio, tal vez tiene glicemia normal y
#  la insulina es una hormona anabólica pero de lípidos ( una persona)
#  musculosa no debería tener una insulina alta) 
#  ¿Qué papel parece tener la grasa visceral? La grasa visceral
#  es una grasa en hipoxia constantemente inflamada por eso.

#####################
##
## Ejercicio 12
##
#####################

# Ejercicio 12

# Piensa en una persona sedentaria que empieza a caminar 45 minutos al día.

# Describe una posible cadena causal de cambios.

# Por ejemplo:
  
# - más actividad física
# - menor insulina
# - menos grasa visceral
# - menos inflamación
# - mejor glucosa

# Tu respuesta puede escribirse como lista o como DAG.

dag_obesidad <- dagitty('
dag {
  actividad_fisica -> glicemia
  glicemia -> insulina
  insulina -> resistencia_insulina
  resistencia_insulina -> grasa_visceral
  grasa_visceral -> inflamacion
}
')

#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()

#####################
##
## Ejercicio 13
##
#####################

# Ejercicio 13

# Haz tu propia versión completa del modelo causal de obesidad.

# Debes incluir al menos:
  
# - dieta
# - actividad física
# - sedentarismo
# - inflamación
# - insulina
# - cintura
# - HbA1c
# - edad
# - sueño
# - estrés

dag_obesidad <- dagitty('
dag{
  dieta -> glicemia
  ejercicio -> glicemia
  sedentarismo -> glicemia
  suenio -> glicemia
  estres -> glicemia
  
  dieta -> insulina
  glicemia -> insulina
  
  glicemia -> resistencia_insulina
  insulina  -> resistencia_insulina
  
  resistencia_insulina -> imc
  resistencia_insulina -> cintura
}
')

#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()

#ggdag(dag_obesidad, use_labels = "name") +
#  theme_dag()
# Pregunta:
  
#  ¿Qué variable parece iniciar más relaciones? Glicemia
#  ¿Qué variable parece acumular más consecuencias?
#  Cintura
#  ¿Cuál podría ser el mejor objetivo de intervención clínica?
#  Glicemia

#####################
##
## Ejercicio 14
##
#####################

# Ejercicio 14

# Escribe un párrafo corto respondiendo esta pregunta:
  
#  ¿Por qué la obesidad no puede explicarse solamente como 
# “comer mucho y moverse poco”?
  
#  Tu respuesta debe incluir al menos tres de estas ideas:
  
#  insulina
#  inflamación
#  grasa visceral
#  resistencia a la insulina
#  actividad física
#  sueño
#  dieta
#  hormonas
#  sedentarismo

# La meta es que expliques la obesidad como un sistema, 
# no como una sola causa.

# Porque la hormona que aumenta las reservas de grasa es la insulina
# y no todos los alimentos elevan igual esta hormona. Además, el
# ejercicio si puede disminuir los niveles de glucosa de la sangre
# pero también puede aumentar las hormonas del hambre lo que
# puede hacer que incluso engordemos e incluso puede aumentar 
# nuestra grasa visceral.
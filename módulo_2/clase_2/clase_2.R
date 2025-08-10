# cargar paquetes
library(dplyr)
library(readxl)
# recordemos que tienen que haber sido previamente instalados para poder cargarlos

pobreza <- read_xlsx("datos/pobreza_comunas_2022.xlsx")

# si exploramos el dataframe cargado, vemos que se carga de forma incorrecta
pobreza
# los nombres de la columna están en una de las filas del dataframe!

# con slice podemos eliminar una fila del dataframe, pero no es suficiente
pobreza |> slice(-1)

# instalar paquete {janitor}, que contiene funciones que nos ayudan a arreglar esto
install.packages("janitor")
library(janitor)

# con janitor le decimos que una de las filas del dataframe se transforme en los nombres de las columnas
pobreza <- pobreza |> 
  janitor::row_to_names(2)

pobreza

# otra opción sería usar el argumento skip de read_xlsx para que se salte n cantidad de filas
read_xlsx("datos/pobreza_comunas_2022.xlsx", skip = 2)

# explorar los datos
glimpse(pobreza)

# seleccionar dos columnas
pobreza |> 
  select(Región, `Número de personas según proyecciones de población (*)`)
# los nombres de columna son muy largos!

# renombrar columnas
pobreza |> 
  rename(region = Región,
         poblacion = `Número de personas según proyecciones de población (*)`,
         pobreza_multi = `Número de personas en situación de pobreza multidimensional (**)`)

# renombrar columnas por su posición
pobreza |> 
  rename(region = 2,
         poblacion = 4,
         pobreza_multi = 5)

# Limpiar los nombres de las columnas
pobreza |> 
  janitor::clean_names()

# Mezclada la limpieza de nombres de columnas con renombrar algunas otras que siguen siendo largas
pobreza_2 <- pobreza |> 
  janitor::clean_names() |>
  rename(poblacion = 4,
         pobreza_personas = 5,
         pobreza_porcentaje = 6,
         casen = presencia_de_la_comuna_en_la_muestra_casen,
         tipo = tipo_de_estimacion_sae)

# ver los nuevos nombres de las columnas
names(pobreza_2)

pobreza_2

pobreza_2 |> 
  print(n = 100)

# revisar el tipo de una variable
class(pobreza_2$pobreza_personas)

# sumar todos los valores de una variable
pobreza_2 |> 
  # convertir a numérico primero
  mutate(pobreza_personas = as.numeric(pobreza_personas)) |> 
  # sumar por medio de la función de resumen
  summarize(sum(pobreza_personas))
# la suma no funciona porque hay datos periddos en esta variable

# filtrar datos perdidos en una variable
pobreza_2 |> 
  filter(is.na(pobreza_personas))

# encontrar el problema
pobreza_2 |> tail() # ver el final de los datos

pobreza_2 |> 
  filter(is.na(pobreza_personas)) |> 
  select(codigo)

# eliminar los datos perdidos de esa variable
pobreza_3 <- pobreza_2 |> 
  filter(!is.na(pobreza_personas))

# confirmar el final de los datos
pobreza_3 |> tail()

# obtener varios resumenes estadísticos de la variable limpiada
pobreza_3 |> 
  mutate(pobreza_personas = as.numeric(pobreza_personas)) |> 
  summarize(sum(pobreza_personas),
            max(pobreza_personas),
            mean(pobreza_personas))

# paquete para ensuciar datos
install.packages("messy")
library(messy)

# agregar datos perdidos a la variable, para probar limpieza de datos
pobreza_3b <- pobreza_3 |> 
  make_missing(cols = "pobreza_personas")

# ahora hay más datos perdidos en la variable
pobreza_3b |> 
  filter(is.na(pobreza_personas))

# contar datos perdidos en una o más columnas
pobreza_3b |> 
  summarize(sum(is.na(pobreza_personas)),
            sum(is.na(pobreza_porcentaje)))

# podemos ignorar los dados perdidos con el argumento na.rm = TRUE
pobreza_3b |> 
  # convertir a numérico
  mutate(pobreza_personas = as.numeric(pobreza_personas)) |> 
  # contar missing y además hacer la suma ignorando missings
  summarize(n_missing = sum(is.na(pobreza_personas)),
            suma_pobreza_personas = sum(pobreza_personas, na.rm = TRUE))

# crear codebook o libro de variables, usando los nombres de columna originales y los actuales
names(pobreza)
names(pobreza_3b)

# crear una tabla que una los dos vectores
tibble(
  etiqueta = names(pobreza),
  variable = names(pobreza_3b)
)

# crear una tabla manualmente
tribble(~etiqueta, ~variable,
        "Años",    "anio",
        "Edades",  "edad_cat"
        )

# convertir varias variables a numérico
pobreza_3b |> 
  mutate(pobreza_personas = as.numeric(pobreza_personas),
         pobreza_porcentaje = as.numeric(pobreza_porcentaje),
         limite_inferior = as.numeric(limite_inferior)
  )

# convertir varias variables a numérico con un solo uso de mutate()
# usando el verbo across() (a través de)
# al usar mutate(across()) aplicamos una misma operación a varias columnas al mismo tiempo
pobreza_4 <- pobreza_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(pobreza_personas,
        pobreza_porcentaje,
        limite_inferior,
        limite_superior),
      # función a aplicar
      as.numeric))

pobreza_4

# otro ejemplo: se especifican primero las columnas que vamos a afectar, 
# y luego la función que les vamos a aplicar
pobreza_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(pobreza_personas,
        pobreza_porcentaje,
        limite_inferior,
        limite_superior),
      # función a aplicar
      ~as.numeric(.x))
  )

# también se pueden seleccionar las columnas usando números (posición de las columnas)
pobreza_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(4:8),
      # función a aplicar
      as.numeric))

pobreza_3b |> 
  mutate(
    across(
      # columnas a transformar
      starts_with("pobreza"),
      # función a aplicar
      as.numeric))

# transformar todas las columnas que sean de un cierto tipo
pobreza_3b |> 
  mutate(
    across(
      # columnas a transformar
      where(is.character), # todas las variables de texto
      # función a aplicar
      as.numeric))

# mezclar formas de selección: todas las de texto, menos dos específicas
pobreza_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(where(is.character), -region, -nombre_comuna),
      # función a aplicar
      as.numeric))

# contar todos los datos perdidos de varias columnas
pobreza_4 |> 
  summarise(
    across(
      # a estas columnas
      4:8,
      # aplicarle esto
      ~sum(is.na(.x)) # función lambda
    )
  )

# calcular la suma de varias columnas, especificando el arugmento para saltarse los datos perdidos
pobreza_4 |> 
  summarise(
    across(
      # a estas columnas
      5:8,
      # aplicarle esto
      ~sum(.x, na.rm = TRUE) # función lambda
    )
  )

pobreza_4

# aumentar los datos perdidos
pobreza_4b <- pobreza_4 |> 
  make_missing(cols = "pobreza_personas", messiness = 0.3)

pobreza_4b

# rellenar datos perdidos con ceros
pobreza_4b |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), 
                                   0, 
                                   pobreza_personas))

# rellenar datos perdidos con un texto
pobreza_4b |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), "(Sin datos)", pobreza_personas))

# arreglar los decimales antes de rellenar datos perdidos
pobreza_4b |> 
  mutate(pobreza_personas = round(pobreza_personas, 0),
         pobreza_personas = ifelse(is.na(pobreza_personas), "(Sin datos)", pobreza_personas))


# instalar paquete que nos ayuda con la limpieza de los datos
install.packages("tidyr")
library(tidyr)

# reemplazar perdidos con la función replace_na()
pobreza_4 |> 
  mutate(pobreza_personas = replace_na(pobreza_personas, 0))

# rellenar datos perdidos con el promedio
pobreza_4 |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), 
                                   mean(pobreza_personas, na.rm = T), 
                                   pobreza_personas))

pobreza_4 |> 
  group_by(region) |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), 
                                   mean(pobreza_personas, na.rm = T), 
                                   pobreza_personas))

pobreza_4 |> 
  # select(region, nombre_comuna, pobreza_personas) |> 
  group_by(region) |> 
  mutate(pobreza_personas_2 = ifelse(is.na(pobreza_personas), 
                                     mean(pobreza_personas, na.rm = T), 
                                     pobreza_personas))


pobreza_4 <- pobreza_4

# ----

# cargar un conjunto de datos distinto
censo <- read_xlsx("datos/estimaciones-y-proyecciones-2002-2035-comunas.xlsx")

# podemos cargarlo y realizar la limpieza inmediatamente
censo <- read_xlsx("datos/estimaciones-y-proyecciones-2002-2035-comunas.xlsx") |> 
  janitor::clean_names() |> 
  rename(sexo = 7)

censo

# Intentemos encontrar un dato específico
censo |> 
  filter(nombre_comuna == "La Florida") |> 
  select(poblacion_2010)
# No resulta muy conveniente por la forma en que está construida esta tabla

censo |> 
  glimpse()
# en esa tabla, existe una variable, los años, que en vez de estar en una sola columna, están distribuidos en múltiples columnas
# esto es típico en tablas hechas para ser leídas por personas, pero no son muy convenientes para realizar análisis sobre ellas

library(tidyr)

# pivotar las columnas a una sola columna que contenga los nombres de las columnas, y otra que contenga el valor de las columnas
censo_2 <- censo |> 
  pivot_longer(cols = starts_with("poblacion"),
               names_to = "año",
               values_to = "poblacion")

censo_2

# Separar las columnas que contienen los años de su enunciado
censo_3 <- censo_2 |> 
  separate(col = año, sep = "_", into = c("tipo", "año")) |> 
  mutate(año = as.numeric(año))
# puede tardarse unos segundos

# ahora sí podemos filtrar los datos como corresponde
censo_3 |> 
  filter(nombre_comuna == "La Florida",
         año == 2025)

censo_3 |> 
  filter(nombre_comuna == "La Florida",
         año == 2010)

# calcular la suma de los datos por región y año
censo_region <- censo_3 |> 
  group_by(nombre_region, año) |> 
  summarise(poblacion = sum(poblacion))

censo_region

censo_3 |> 
  group_by(nombre_region, nombre_comuna, año) |> 
  summarise(poblacion = sum(poblacion)) |> 
  print(n=100)

# pivotar a ancho (la operación inversa a la que hicimos recién), dónde los valores de una columna se usan como los nombres de las nuevas columnas,
# específica de qué otra columna sacamos los valores que van a contenerse en estas columnas nuevas
censo_region_2 <- censo_region |> 
  pivot_wider(names_from = año,
              values_from = poblacion)

censo_region

# guaradr el resultado como excel
censo_region_2 |> 
  writexl::write_xlsx("datos/estimaciones-regiones.xlsx")

# —----
# leer otro conjunto de datos del censo
read_xlsx("datos/P5_Genero.xlsx")
#  este archivo viene en múltiples ho

# especificar la hoja que vamos a leer
genero <- read_xlsx("datos/P5_Genero.xlsx", sheet = 2)

# también puede ser por el nombre exacto de la hoja
genero <- read_xlsx("datos/P5_Genero.xlsx", sheet = "1")

genero

library(janitor)

# limpieza
genero |> 
  row_to_names(3) |> 
  clean_names() |> 
  mutate(across(3:10, as.numeric)) |> 
  filter(!is.na(region))

# limpiar y pivotar
genero_2 <- genero |> 
  row_to_names(3) |> 
  # clean_names() |> 
  mutate(across(3:10, as.numeric)) |> 
  filter(!is.na(Región)) |> 
  pivot_longer(where(is.numeric),
               names_to = "género", values_to = "población")

genero_2

# —----

# cargar otro conjunto
eleccion <- readr::read_csv2("datos/presidenciales_2021_comuna.csv")

# explorarlo
eleccion |> 
  count(eleccion)

eleccion |> 
  distinct(eleccion)

eleccion |> 
  distinct(eleccion, candidatura)

eleccion_2 <- eleccion |> 
  filter(eleccion == "Segunda vuelta")

eleccion_2 |> 
  arrange(desc(votos)) |> 
  filter(candidatura == "Gabriel Boric")

eleccion_2 |> 
  slice_max(votos)

eleccion_2 |> 
  slice_max(votos, n = 3)

eleccion_2 |> 
  filter(candidatura == "Gabriel Boric") |> 
  group_by(region) |> 
  slice_max(votos, n = 3) |> 
  arrange(region, desc(votos))

eleccion_2 |> 
  filter(candidatura == "Gabriel Boric") |> 
  group_by(region) |> 
  slice_max(votos, n = 1) |> 
  arrange(desc(votos))


eleccion_region <- eleccion_2 |> 
  group_by(region, candidatura) |> 
  summarise(votos = sum(votos)) |> 
  filter(candidatura %in% c("Gabriel Boric", "Jose Antonio Kast"))

eleccion_region

eleccion_region_2 <- eleccion_region |> 
  pivot_wider(names_from = candidatura, values_from = votos)

eleccion_region_2

# pivotar a ancho
eleccion_3 <- eleccion_2 |> 
  filter(candidatura %in% c("Gabriel Boric", "Jose Antonio Kast")) |> 
  select(2:7) |> 
  pivot_wider(names_from = candidatura, values_from = votos)

eleccion_3

# —----

# unir conjuntos de datos

# quedamos con dos conjuntos de datos que son coincidentes:
# ambos conjuntos tienen una observación por comuna

eleccion_3
pobreza_4

# podemos unir ambas tablas para que las filas que sean de una columna X coincidan con las filas de la otra tabla de la misma comuna

# unir por la variable comuna
left_join(eleccion_3,
          pobreza_4, 
          by = "comuna")
# no se puede! en la segunda tabla (tabla Y) no hay una variable que se llame comuna

# especificar que la unión es por columnas que se llaman distinto
left_join(eleccion_3,
          pobreza_4, 
          by = c("comuna" = "nombre_comuna"))
# El resultado es inexacto, porque quedan varias comunas sin datos
# esto significa la coincidencia entre columnas no es perfecta, porque las comunas tienen nombres levemente distintos en ambas tablas

# Por suerte tenemos el código único territorial de las comunas, que permite una unión de los datos con menos errores
left_join(eleccion_3,
          pobreza_4, 
          by = c("cut_comuna" = "codigo"))
# pero no permite ser la unión porque son de distinto tipo!

# entonces nos dedicamos a preparar los datos para hacer una unión que funcione mejor

# primero renombramos las columnas de unión para que se llamen igual
eleccion_4 <- eleccion_3 |> 
  rename(codigo_comuna = cut_comuna)

# Luego convertimos la columna de unión a un valor común, y seleccionamos 
# solamente las columnas nuevas que queremos agregarle a la primera tabla
pobreza_5 <- pobreza_4 |> 
  rename(codigo_comuna = codigo) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  select(codigo_comuna, poblacion, pobreza_porcentaje)

# Ahora que tenemos datos más preparados y limpios para la unión, esta funciona de inmediato
eleccion_pobreza <- left_join(eleccion_4,
                              pobreza_5, 
                              by = "codigo_comuna")

eleccion_pobreza

eleccion_pobreza |> filter(is.na(pobreza_porcentaje))


eleccion_pobreza |> 
  arrange(desc(pobreza_porcentaje))

library(readr)
library(dplyr)
library(janitor)
# install.packages("tidyverse")
library(dplyr)
library(tidyverse)

# cargar conjunto de datos que viene con una codificación un poco común
fonasa <- read_csv("datos/Beneficiarios Fonasa 2024.csv")

fonasa <- read_csv("datos/Beneficiarios Fonasa 2024.csv",
                    locale = locale(encoding = "latin1")) |> 
  clean_names()

# explorar
fonasa |> count(sexo)
fonasa |> distinct(sexo)
fonasa |> count(edad_tramo)
fonasa |> count(nacionalidad)

fonasa |> 
  glimpse()

fonasa |> count(tramo)

fonasa |> 
  group_by(tramo) |> 
  summarise(sum(beneficiarios))

fonasa |> distinct(edad_tramo)

# primer filtro
fonasa_2 <- fonasa |> 
  filter(sexo == "Mujer",
         edad_tramo == "60 a 64 años",
         tramo == "A") |> 
  tail() |> 
  select(2, 3, 4, 5)

# segundo filtro
fonasa_3 <- fonasa |> 
  filter(sexo == "Hombre",
         edad_tramo == "60 a 64 años",
         tramo == "B") |> 
  tail() |> 
  select(2, 3, 4, 5)

fonasa |> 
  filter(region == "Del Bíobío") |> 
  distinct(comuna)

# agregar filas
fonasa_2 |> 
  add_row(region = "Del Biobío", comuna = "Curanilahue")

altura <- c(180, 190, 170, 150, 140, 120)

# agregar columnas
fonasa_2 |> 
  bind_cols(altura = altura)

# unir dos tablas cuando comparten una misma estructura (columnas, tipo de datos)
bind_rows(fonasa_2, fonasa_3)

# repaso de left_join:
# crear tablas de datos manualmente
animales_1 <- tibble(animal = c("perro", "gato", "pez"),
                     color = c("gris", "negro", "azul"))

animales_1

animales_2 <- tibble(animal = c("perro", "gato", "pez"),
                     patas = c(4, 3, 0))

left_join(animales_1, animales_2)


# segundo ejemplo
animales_1 <- tibble(animal = c("gato", "ratón", "perro", "pez"),
                     color = c("gris", "negro", "blanco", "azul"))

animales_2 <- tibble(animal = c("perro", "gato", "pez"),
                     patas = c(4, 3, 0),
                     edad = c(8, 3, 1))

animales_1
animales_2

left_join(animales_1, animales_2)

animales_3 <- left_join(animales_2, animales_1)

animales_3


# cargar conjunto de datos que vienen en formato Arrow Parquet
install.packages("arrow")
library(arrow)

cead <- read_parquet("datos/cead_delincuencia_chile.parquet")

distinct(cead, delito)
unique(cead$delito)
distinct(cead, delito) |> print(n=Inf)

cead |> 
  filter(delito == "Robo en lugar habitado") |> 
  filter(comuna == "Algarrobo") |> 
  select(comuna, fecha, delito_n)

# este conjunto de datos viene con una columna en formato fecha
cead |> 
  filter(delito == "Robo en lugar habitado") |> 
  filter(comuna == "Algarrobo") |> 
  select(comuna, fecha, delito_n) |> 
  arrange(desc(fecha))

# intentar filtrar datos de un año
cead |> 
  filter(delito == "Robo en lugar habitado") |> 
  filter(comuna == "Algarrobo") |> 
  select(comuna, fecha, delito_n) |> 
  filter(fecha == 2020)

# trabajar con datos en formato fecha
install.packages("lubridate")
library(lubridate)

cead |> 
  filter(delito == "Robo en lugar habitado") |> 
  filter(comuna == "Algarrobo") |> 
  select(comuna, fecha, delito_n) |> 
  mutate(año = year(fecha)) |> # convertir las fechas en años
  filter(año == 2022) # filtrar el año

# crear dataframe filtrado
cead_filtro <- cead |> 
  # filter(delito == "Robo en lugar habitado") |> 
  filter(delito == "Hurtos") |> 
  # filter(comuna == "Algarrobo") |> 
  filter(comuna == "El Quisco") |> 
  select(comuna, fecha, delito_n)

# hacer una versión del dataframe con la fechaen formato caracter
cead_malo <- cead_filtro |> 
  mutate(fecha = as.character(fecha))

# arreglar fecha en formato caracter
cead_malo |> 
  mutate(fecha = lubridate::as_date(fecha))

cead_malo |> 
  mutate(fecha = lubridate::ymd(fecha))
# mutate(fecha = lubridate::dmy(fecha))

cead_filtro |> 
  mutate(año = year(fecha)) |> 
  filter(año == 2023) |> 
  mutate(suma = sum(delito_n),
         suma_acumulada = cumsum(delito_n))

cead_filtro |> 
  mutate(año = year(fecha)) |> 
  filter(año == 2023) |> 
  mutate(suma = sum(delito_n),
         suma_acumulada = cumsum(delito_n)) |> 
  mutate(porcentaje = delito_n/suma,
         porcentaje = porcentaje*100) |> 
  # arrange(desc(porcentaje))
  slice_max(porcentaje)

cead_filtro_2 <- cead_filtro |> 
  mutate(año = year(fecha)) |> 
  filter(año == 2023) |> 
  mutate(suma = sum(delito_n),
         suma_acumulada = cumsum(delito_n)) |> 
  mutate(porcentaje = delito_n/suma,
         porcentaje = porcentaje*100)

cead_filtro_2 |> 
  arrange(fecha) |> 
  mutate(cambio = delito_n/lag(delito_n))

cead_filtro_3 <- cead_filtro_2 |> 
  arrange(fecha) |> 
  mutate(cambio = delito_n/lag(delito_n)) |> 
  mutate(cambio2 = (delito_n/lag(delito_n))-1)

cead_filtro_3

cead_filtro_2 |> 
  arrange(desc(fecha)) |> 
  mutate(cambio = delito_n/lead(delito_n))

cead_filtro_3 |> 
  mutate(clasificacion = ifelse(cambio2 > 0, "subieron", "bajaron"))
# {zoo} {slider}


cead_todo <- cead |> 
  mutate(año = year(fecha)) |> 
  group_by(año, comuna, delito) |> 
  mutate(suma = sum(delito_n),
         suma_acumulada = cumsum(delito_n)) |> 
  mutate(porcentaje = delito_n/suma,
         porcentaje = porcentaje*100) |> 
  mutate(cambio = delito_n/lag(delito_n)) |> 
  mutate(cambio2 = (delito_n/lag(delito_n))-1)

cead_todo |> 
  print(n=400)


# datos de tipo texto o caracter
library(readxl)
camp <- read_xlsx("datos/campamentos_chile_2024.xlsx")

camp |> 
  filter(provincia == "Marga Marga")


camp |> 
  filter(nombre == "Bellavista")

install.packages("stringr")
library(stringr)

nombres <- c("bastián", "paula", "saul", "simón", "yael", "pablo")

nombres == "maría"
nombres == "s"

str_detect(nombres, "s")

str_detect(nombres, "a") |> sum()

camp |> 
  filter(nombre == "Bellavista")

camp |> 
  filter(str_detect(nombre, "Bellavista"))

camp |> 
  filter(str_detect(nombre, "Bella"))

camp |> 
  filter(str_detect(nombre, "Vista"))

camp |> 
  filter(str_detect(nombre, "vista"))

camp |> 
  filter(str_detect(nombre, "vista|Vista"))

camp |> 
  filter(str_detect(tolower(nombre), "vista"))

camp |> 
  select(nombre) |> 
  mutate(nombre2 = tolower(nombre),
         nombre3 = toupper(nombre),
         nombre4 = str_to_title(nombre3),
         nombre5 = str_to_sentence(nombre3),
         )

animales <- c("PERRO", "Gato", "ratita", "rAtOtA")

str_to_title(animales)

animales <- c("Un PERRO", "Perro", "Gato", "gatos", "GATOS", "ratita", "Rata", "rAtOtA", "Pescado")

animales_2 <- tibble(animales)

animales_2 |> 
  count(animales)

animales_2 |> 
  mutate(animales2 = str_to_title(animales)) |> 
  count(animales2)

animales_2 |> 
  mutate(animales2 = str_to_title(animales)) |> 
  mutate(animales3 = str_remove(animales2, "Un ")) |> 
  count(animales3)

animales_2 |> 
  mutate(animales2 = str_to_title(animales)) |> 
  mutate(animales3 = str_remove(animales2, "Un ")) |> 
  # mutate(animales4 = str_remove(animales3, "to"))
  mutate(animales4 = str_replace(animales3, "Ratota", "Rata"),
         animales4 = str_replace(animales4, "Ratita", "Rata"))

animales_2 |> 
  mutate(animales2 = str_to_title(animales)) |> 
  mutate(animales3 = str_remove(animales2, "Un ")) |> 
  # mutate(animales4 = str_remove(animales3, "to"))
  mutate(animales4 = ifelse(str_detect(animales3, "Rat"), 
                            "Rata", animales3))

animales_2 |> 
  mutate(animales2 = str_to_title(animales)) |> 
  mutate(animales3 = str_remove(animales2, "Un ")) |> 
  # mutate(animales4 = str_remove(animales3, "to"))
  mutate(animales4 = str_replace(animales3, "Ratota", "Rata"),
         animales4 = str_replace(animales4, "Ratita", "Rata")) |> 
  mutate(animales5 = str_remove(animales4, "s$"))

camp |> 
  mutate(nombre = str_remove(nombre, "Las "))


# tarea ----
cowsay::say("Tarea para la casa")


temperatura <- read_csv2("datos/temperaturas_chile.csv")

unique(temperatura$nombre)

# ¿cuántas estaciones meteorológicas hay?
# ¿qué años abarca la base de datos?
# en total, ¿cuál fue la mayor temperatura registrada? ¿y la menor?
# ¿cuál es la temperatura máxima (t_max) promedio de cada año, a partir del año 1970?
# para una de las estaciones meteorológicas (a tu elección), y considerando sólo desde 1990 en adelante, calcula la temperatura máxima promedio de cada mes. ¿Cuál es el mes más caluroso?
# queremos saber el rango de temperaturas en cada territorio. ¿qué estación meteorológica tiene el mayor rango o diferencia entre sus temperaturas mínimas y máximas?

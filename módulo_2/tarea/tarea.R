# En base al archivo temperaturas_chile.csv, responder las siguientes preguntas:

library(dplyr)

temp <- readr::read_csv2("temperaturas_chile.csv") |>
  filter(!is.na(nombre))

#   ¿cuántas estaciones meteorológicas hay?

unique(temp$nombre)
length(unique(temp$nombre))
n_distinct(temp$nombre)
temp |> summarize(n_distinct(nombre))

#   ¿qué años abarca la base de datos?

temp$año
class(temp$año)
min(temp$año)
max(temp$año)
unique(temp$año)
sort(unique(temp$año))
paste(min(temp$año), "-", max(temp$año))

años_completos <- 1950:2024 # no recomendado (hardcodear)
años_completos <- min(temp$año):max(temp$año)
años_completos <- seq(min(temp$año), max(temp$año), 1)

length(años_completos) == length(unique(temp$año))

años_completos <- c(1950:1980, 1982:2024) |> as.numeric()

waldo::compare(años_completos, sort(unique(temp$año)))


#   en total, ¿cuál fue la mayor temperatura registrada? ¿y la menor?
max(temp$t_max)
max(temp$t_max, na.rm = T)
min(temp$t_min, na.rm = T)

temp |> 
  summarise(
    mínima = min(t_min, na.rm = TRUE),
    máxima = max(t_max, na.rm = TRUE))
  

#   ¿cuál es la temperatura máxima (t_max) promedio de cada año, a partir del año 1970?
temp |> 
  filter(año >= 1970) |> 
  select(t_max, año) |> 
  print(n = 100)

temp |> 
  select(t_max, año) |> 
  filter(año >= 1970) |> 
  na.omit() |> 
  summarise(promedio = mean(t_max))

temp |> 
  select(t_max, año) |> 
  filter(año >= 1970) |> 
  na.omit() |> 
  group_by(año) |> 
  summarise(promedio = mean(t_max)) |> 
  print(n=Inf)

#   para una de las estaciones meteorológicas (a tu elección), y considerando sólo desde 1990 en adelante, calcula la temperatura máxima promedio de cada mes. ¿Cuál es el mes más caluroso?
estaciones <- unique(temp$nombre)

estacion <- sample(estaciones, 1)

# estacion <- ""
estacion

temp_mes <- temp |> 
  filter(nombre == estacion) |> 
  filter(año >= 1990) |> 
  # na.omit() |> 
  group_by(nombre, mes) |> 
  summarise(promedio = mean(t_max, na.rm = T))

#  ¿Cuál es el mes más caluroso?
temp_mes
max(temp_mes$promedio)

temp_mes |> 
  filter(promedio == max(promedio))

temp_mes |> 
  slice_max(promedio)

temp_mes |> 
  arrange(desc(promedio))

temp_mes |> 
  arrange(desc(promedio)) |> 
  slice(1:3)


#   queremos saber el rango de temperaturas en cada territorio. ¿qué estación meteorológica tiene el mayor rango o diferencia entre sus temperaturas mínimas y máximas?

temp_estacion <- temp |> 
  group_by(nombre) |>
  # group_by(zona_geografica) |>
  summarise(
    mínima = min(t_min, na.rm = TRUE),
    máxima = max(t_max, na.rm = TRUE)) |> 
  mutate(diferencia = máxima - mínima)

temp_estacion |> 
  arrange(desc(diferencia))

temp_estacion_nombre <- temp_estacion |> 
  slice_max(diferencia) |> 
  pull(nombre)

paste("La estación con la mayor variación es", temp_estacion_nombre)

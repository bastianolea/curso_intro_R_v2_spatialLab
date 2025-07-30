# cargar datos

datos <- read.csv2(file = "campamentos_chile_2024.csv")

class(datos)

edades <- c(10, 34, 56, 87, 423, 36, 24)
length(edades)

length(datos) #columnas
nrow(datos)
names(datos)
datos$hogares

View(datos)

# instalar paquetes
# install.packages("dplyr")

library(dplyr)

datos$hogares
mean(datos$hogares)
round(mean(datos$hogares), 1)
paste(round(mean(datos$hogares), 1), "hogares en promedio")

# conector o "pipe"
mean(datos$hogares)
datos$hogares |> mean()
datos$hogares |> mean() |> round(1)
datos$hogares |> mean() |> round(1) |> paste("hogares en promedio")
mean(datos$hogares) |> round(1) |> paste("hogares en promedio")

datos$hogares |> 
  # calcular promedio
  mean() |> 
  # redondear número
  # round(1) |>
  round(2) |>
  format(decimal.mark = ",") |>
  paste("hogares en promedio") # pegarle un texto

# control + comando + m
# control + alt + m
# %>% # requiere dplyr (o magrittr)
# |> 

datos |> tibble()

# install.packages("readr")
library(readr)

datos <- read_csv2(file = "campamentos_chile_2024.csv")
datos <- readr::read_csv2(file = "campamentos_chile_2024.csv")

datos
datos |> head(n = 20)
datos |> head(n = 100) |> print(n = 100)
datos |> print(n = 500)

datos |> print(n = Inf)
datos |> tail()
datos |> slice(54)
datos |> slice(1000)
datos |> slice(50:60)

datos |> names()
datos |> glimpse()
datos
glimpse(datos)
View(datos)

datos_2 <- datos |> 
  select(nombre, comuna, hogares)

datos
datos_2

datos |> pull(hogares)
datos$hogares

datos |> 
  select(nombre, comuna, hogares) |> 
  pull(hogares)

lista_comunas <- datos |> 
  # select(nombre, comuna, hogares) |> 
  pull(comuna) |>
  sort() |>
  unique()

lista_comunas


datos |> 
  filter(hogares > 60)

datos$hogares
datos$hogares > 60

datos |> 
  filter(hogares > 600) |> 
  select(nombre, comuna)

datos |> 
  filter(hogares > 600) |> 
  arrange(hogares) |> 
  select(nombre, comuna, hogares)

datos |> 
  filter(hogares > 600) |> 
  # arrange(-hogares) |> 
  arrange(desc(hogares)) |> 
  select(nombre, comuna, hogares)

datos |> 
  filter(comuna == "La Florida")

datos |> 
  filter(comuna %in% c("La Florida", 
                       "Puente Alto", 
                       "La Granja")
         ) |> 
  select(comuna) |> 
  print(n=Inf)

datos |> 
  filter(comuna %in% c("La Florida", 
                       "Puente Alto", 
                       "Pirque",
                       "La Granja")
  ) |> 
  select(comuna) |> 
  distinct()

datos |> 
  filter(comuna %in% c("La Florida", 
                       "Puente Alto", 
                       "Pirque",
                       "La Granja")
  ) |> 
  select(comuna) |> 
  count(comuna)

datos |> 
  count(comuna) |> 
  arrange(desc(n))

datos |> 
  select(nombre, comuna, hogares, area) |> 
  mutate(densidad = hogares/area,
         area_km = area/1000)

# crear variables
datos_2 <- datos |> 
  # select(nombre, comuna, hogares, area) |> 
  mutate(tipo = ifelse(hogares > 50, 
                       yes = "grandes", no = "normales"))

# filtrar observaciones
datos_3 <- datos_2 |> 
  filter(hogares > 10)

datos_4 <- datos_3 |> 
  select(-cut_r, -cut_p, -cut)

write_csv2(datos_4, file = "campamentos 2.csv")

write_csv2(datos_4, file = "datos/campamentos 2.csv")


lista_regiones <- datos_4 |> pull(region) |> unique()
lista_regiones <- unique(datos_4$region)

datos_4 |> 
  filter(region == lista_regiones[2])




region_filtrar <- lista_regiones[2]

datos_4 |> 
  filter(region == region_filtrar)


for (region_filtrar in lista_regiones) {
 print(region_filtrar)
}


for (region_filtrar in lista_regiones) {
  print(region_filtrar)

datos_4 |> 
  filter(region == region_filtrar) |> 
  print()
}


for (region_filtrar in lista_regiones) {
  print(region_filtrar)
  
  datos_region <- datos_4 |> 
    filter(region == region_filtrar)
  
  write_csv2(datos_region, 
             file = paste("datos/campamentos", region_filtrar, ".csv")
             )
}


for (region_filtrar in lista_regiones) {
  print(region_filtrar)
  
  datos_region <- datos_4 |> 
    filter(region == region_filtrar)
  
  archivo <- paste("datos/campamentos", region_filtrar, ".csv")
  
  print(archivo)
  
  write_csv2(datos_region, 
             file = archivo)
}



for (region_filtrar in lista_regiones) {
  print(region_filtrar)
  
  if (region_filtrar == "Valparaíso") {
    datos_4 <- datos_4 |> 
      # mutate(region = paste("Región de", region))
      filter(hogares > 100)
  }
  
  datos_region <- datos_4 |> 
    filter(region == region_filtrar)
  
  archivo <- paste("datos/campamentos", region_filtrar, ".csv")
  
  print(archivo)
  
  write_csv2(datos_region, 
             file = archivo)
}

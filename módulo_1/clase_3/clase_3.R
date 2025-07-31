# cargar datos

# función para cargar datos en formato csv
datos <- read.csv2(file = "campamentos_chile_2024.csv")
# se asigna al objeto "datos"

# revisemos la clase que tiene la tabla resultante
class(datos)

# nombres de columnas del dataframe
names(datos)

# cantidad de columnas del dataframe
length(datos) 

# ojo, porque al aplicar la función length() a un vector, entrega el largo del vector
edades <- c(10, 34, 56, 87, 423, 36, 24)
length(edades)

# ver candidad de filas de un dataframe
nrow(datos)

# extraer una columna del dataframe
datos$hogares

# ver los datos como una planilla en RStudio
View(datos)



# instalar paquetes ----
# instalamos paquetes para agregar nuevas funcionalidades a R
# sólo necesitamos instalar los paquetes una vez por cada computador
install.packages("dplyr")
# el paquete se descarga desde internet y queda instalado en tu computador

# para abrir o activar el paquete usamos library()
library(dplyr)
# dplyr nos va a ayudar a manejar datos en R de una forma más amigable

## conector ----
# el conector o "pipe" en inglés es un operador que nos permite "pasar" instrucciones paso por paso
# permite ejecutar operaciones encadenadas
# por ejemplo:
nrow(datos)
# es equivalente a 
datos |> nrow()
# porque el operador hace que a "datos" se le aplique nrow()

# ejemplo de un cálculo SIN conector
datos$hogares
mean(datos$hogares)
round(mean(datos$hogares), 1)
paste(round(mean(datos$hogares), 1), "hogares en promedio")

# ejemplo del mismo cálculo con conector o "pipe"
datos$hogares |> mean()
datos$hogares |> mean() |> round(1)
datos$hogares |> mean() |> round(1) |> paste("hogares en promedio")
mean(datos$hogares) |> round(1) |> paste("hogares en promedio")
# el resultado es más legible y amigable, porque las instrucciones van de izquierda a derecha, partiendo con el dato original

# también se pueden escribir hacia abajo
datos$hogares |> 
  # calcular promedio
  mean() |> 
  # redondear número
  # round(1) |>
  round(2) |>
  format(decimal.mark = ",") |>
  paste("hogares en promedio") # pegarle un texto

# control + comando + m en Mac
# control + alt + m
# hay dos tipos: %>% # requiere dplyr (o magrittr)
# y el pipe "nativo": |> 


## tablas ----
# con la función tibble() podemos ver los datos de una forma más cómoda
datos |> tibble()

# si instalamos el paquete {readr} podemos cargar los datos directamente en ese formato
# install.packages("readr")
library(readr)

# cargar con {readr}
datos <- read_csv2(file = "campamentos_chile_2024.csv")
datos <- readr::read_csv2(file = "campamentos_chile_2024.csv")

## previsualizar datos ----

# ver las primeras filas de la tabla
datos

datos |> head(n = 20)

# imprimir la cantidad de filas
datos |> print(n = 500)

# imprimir todas las filas de la tabla
datos |> print(n = Inf)

# ver las ultimas filas
datos |> tail()

# sacar una fila cualquiera 
datos |> slice(54)
datos |> slice(1000)
datos |> slice(50:60)

# explorar con glimpse para ver las filas hacia abajo y las columnas hacia el lado
datos |> names()
datos |> glimpse()
datos

glimpse(datos)
View(datos)

## seleccionar columnas
datos |> 
  select(nombre, comuna, hogares)

# seleccionarlas y crear un nuevo objeto solo con esas columnas
datos_2 <- datos |> 
  select(nombre, comuna, hogares)

datos
datos_2

# extraer una columna
datos |> pull(hogares)
datos$hogares

# encadenar dos operaciones
datos |> 
  select(nombre, comuna, hogares) |> 
  pull(hogares)

# encadenar varias operaciones
lista_comunas <- datos |> 
  # select(nombre, comuna, hogares) |> 
  pull(comuna) |>
  sort() |>
  unique()

lista_comunas

## filtros ----
# usamos una comparación para filtrar las observaciones del dataframe
datos$hogares
datos$hogares > 60

datos |> 
  filter(hogares > 60)

# encadenar un filtro y una selección
datos |> 
  filter(hogares > 600) |> 
  select(nombre, comuna)

# filtrar y ordenar datos
datos |> 
  filter(hogares > 600) |> 
  arrange(hogares)

# ordenar datos en orden descendente
datos |> 
  filter(hogares > 600) |> 
  # arrange(-hogares) |> 
  arrange(desc(hogares)) |> 
  select(nombre, comuna, hogares)

# filtrar una variable categórica o de texto
datos |> 
  filter(comuna == "La Florida")

# filtrar con el operador %in% para dejar las que pertenezcan a varias categorías
datos |> 
  filter(comuna %in% c("La Florida", 
                       "Puente Alto", 
                       "La Granja")
         )

datos |> 
  filter(comuna %in% c("La Florida", 
                       "Puente Alto", 
                       "Pirque",
                       "La Granja")
  ) |> 
  select(comuna) |> 
  distinct()

# contar los resultados
datos |> count(comuna)

# contar los resultados luego de hacer un filtro
datos |> 
  filter(comuna %in% c("La Florida", 
                       "Puente Alto", 
                       "Pirque",
                       "La Granja")
  ) |> 
  select(comuna) |> 
  count(comuna)

# ordenar luego de contar
datos |> 
  count(comuna) |> 
  arrange(desc(n))

# crear nuevas variables ----
datos |> 
  select(nombre, comuna, hogares, area) |> 
  mutate(prueba = "hola")

datos |> 
  select(nombre, comuna, hogares, area) |> 
  mutate(densidad = hogares/area)

# crear dos a la vez
datos |> 
  select(nombre, comuna, hogares, area) |> 
  mutate(densidad = hogares/area,
         area_km = area/1000)

# ejemplo de un flujo de trabajo donde vamos haciendo varios cambios en un dataframe
# y cada cambio lo guardamos en un objeto nuevo

# crear variables con ifelse
datos_2 <- datos |> 
  # select(nombre, comuna, hogares, area) |> 
  mutate(tipo = ifelse(hogares > 50, 
                       yes = "grandes", no = "normales"))

datos_2

# filtrar observaciones
datos_3 <- datos_2 |> 
  filter(hogares > 10)

datos_3

# sacar columnas
datos_4 <- datos_3 |> 
  select(-cut_r, -cut_p, -cut)

datos_4

# guardar los resultados
write_csv2(datos_4, file = "campamentos 2.csv")

write_csv2(datos_4, file = "datos/campamentos 2.csv")


# ejemplo de usar {dplyr} dentro de un loop

# primero creamos una lista de las regiones en el dataframe
lista_regiones <- datos_4 |> pull(region) |> unique()
lista_regiones <- unique(datos_4$region)
# cualquiera de estos métodos funciona

# probamos filtrar los datos con un elemento del vector que creamos
datos_4 |> 
  filter(region == lista_regiones[2])

# también podemos asignar el elemento del vector a un objeto nuevo
region_filtrar <- lista_regiones[2]

# luego usar el objeto nuevo para filtrar
datos_4 |> 
  filter(region == region_filtrar)


# probemos un loop que vaya por las regiones de la lista de regiones
for (region_filtrar in lista_regiones) {
 print(region_filtrar)
}
# imprime el valor de cada uno de los elementos


# siguiendo el loop anterior, que hizo que "region_filtrar" fuera cada valor de "lista_regiones",
# hagamos que el looop filtre cada región por separado
for (region_filtrar in lista_regiones) {
  print(region_filtrar)

datos_4 |> 
  filter(region == region_filtrar) |> 
  print()
}

# ahora hagamos que se filtren los datos, y luego que se guarden en un archivo nuevo
for (region_filtrar in lista_regiones) {
  print(region_filtrar)
  
  datos_region <- datos_4 |> 
    filter(region == region_filtrar)
  
  write_csv2(datos_region, 
             file = paste("datos/campamentos", region_filtrar, ".csv")
             )
}
# cada archivo va a tener el nombre de la región, el mismo que se usó para filtrar

# ahora lo mismo pero creando el nombre del archivo por separado
for (region_filtrar in lista_regiones) {
  print(region_filtrar)
  
  datos_region <- datos_4 |> 
    filter(region == region_filtrar)
  
  archivo <- paste("datos/campamentos", region_filtrar, ".csv")
  
  print(archivo)
  
  write_csv2(datos_region, 
             file = archivo)
}

# agreguemos una condicional, para que sólo en ciertos casos, se haga algo específico
for (region_filtrar in lista_regiones) {
  print(region_filtrar)
  
  # sólo en Valparaíso:
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

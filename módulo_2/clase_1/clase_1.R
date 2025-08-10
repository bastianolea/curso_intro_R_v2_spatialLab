# Cargar datos desde archivos Excel

# instalar el paquete
install.packages("readxl")
# install.packages("openxlsx")

library(readxl)

# Cargar los datos
datos <- read_xlsx("datos/campamentos_chile_2024.xlsx")

datos

# Explorar los datos
library(dplyr)

# Seleccionar columnas
datos |> 
  select(nombre, region)

datos |> 
  select(hogares, hectareas)

# Seleccionar por posición
datos |> 
  select(1, 2, 3, 4, 8, 9)

datos |> 
  select(1:4, 8, 9)

datos |> 
  select(1:10)

# seleccionar las que contienen parcialmente un texto
datos |> 
  select(contains("hect"))

# seleccionar las que empiecen con un texto
datos |> 
  select(starts_with("cut"))

# seleccionar las que no empiezan con un texto
datos |> 
  select(-starts_with("cut"))

# seleccionar las que son de un cierto tipo
datos |> 
  select(nombre, where(is.numeric))

datos |> 
  select(nombre, where(is.numeric), -cut)

datos |> 
  select(nombre, where(is.numeric)) |> 
  select(-cut)

# seleccionar las que terminan con un texto
datos |> 
  select(ends_with("_p"))


# conteos ----
# conteo de valores únicos de una variable
datos |> 
  count(region)

# contar y ordenar
datos |> 
  count(region) |> 
  arrange(n)

# contar y ordenar descendentemente
datos |> 
  count(region) |> 
  arrange(desc(n))

# contar y ordenar dentro de la misma función de conteo
datos |> 
  count(provincia, sort = TRUE)

# contar, ordenar, y renombrar
datos |> 
  count(provincia) |> 
  arrange(desc(n)) |> 
  rename(conteo = n)

# las tres operaciones dentro de la misma función
datos |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo")

# guardar el conteo como un nuevo objeto
datos_conteo <- datos |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo")

datos_conteo

# guardarlo como un excel
install.packages("writexl")
library(writexl)

# guardar el archivo dentro de una carpeta
write_xlsx(datos_conteo, "datos/conteo.xlsx")

# lo mismo pero usando un conector
datos_conteo |> 
  write_xlsx("datos/conteo.xlsx")

# lo mismo, de principio a fin, desde el dato original hasta guardarlo en Excel
datos |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo") |> 
  filter(conteo > 50) |> 
  write_xlsx("datos/conteo.xlsx")

# incluso se puede hacer lo mismo desde la carga del archivo
read_xlsx("datos/campamentos_chile_2024.xlsx") |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo") |> 
  filter(conteo > 50) |> 
  write_xlsx("datos/conteo.xlsx")

# filtrar datos ----

# filtrar observaciones que son mayores a
datos |> 
  filter(hogares > 100)

# filtrar las observaciones que no son mayores a
datos |> 
  filter(!hogares > 100)
# el ! es el operador de negación, que invierte cualquier comparación

datos |> 
  filter(hogares <= 100)

datos |> 
  filter(nombre == "Bellavista")

datos |> 
  filter(nombre != "Bellavista")

datos |> 
  filter(nombre == "Bellavista")

# filtrar los elementos que están dentro de un conjunto
datos |> 
  filter(nombre %in% c("Bellavista", "Los Fleteros", "Manuel Rodríguez"))

# invertirlo anterior: elementos que no están dentro de un conjunto
datos |> 
  filter(!nombre %in% c("Bellavista", "Los Fleteros", "Manuel Rodríguez"))

# crear un vector con los valores
lista_campamentos <- c("Bellavista", "Los Fleteros", "Manuel Rodríguez", "Manzana 33")

# filtrar observaciones que están dentro del vector con los valores
datos |> 
  filter(nombre %in% lista_campamentos)

# por dentro, está ocurriendo esto
datos$nombre %in% lista_campamentos
# se genera un vector de verdaderos y falsos, con los que se filtran los datos


# crear variables ----
datos |> 
  select(1:4) |> 
  mutate(variable = 1)

# Crear una variable que contenga el resultado de una comparación
datos |> 
  select(1:4) |> 
  mutate(seleccion = nombre %in% lista_campamentos)
# La variable estará llena de verdaderos y falsos

datos |> 
  select(1:4) |> 
  mutate(seleccion = nombre %in% c("Bellavista", "Los Fleteros", "Manuel Rodríguez", "Manzana 33"))

datos |> 
  select(1:4, hogares) |> 
  filter(hogares > 60)

datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = hogares > 60)

# Crear una variable con una función condicional, que rellenará con los valores entregados dependiendo de si la observación cumple con la condición
datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, 
                          "sí", "no"))

# Crear tres variables en base de condicionales sobre una columna
datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, "sí", "no")) |> 
  mutate(medianos = ifelse(hogares > 30, "sí", "no")) |> 
  mutate(chicos = ifelse(hogares > 10, "sí", "no"))

# Hacer un filtro después de haber creado una variable con condicional
datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, "grandes", "no")) |> 
  mutate(medianos = ifelse(hogares > 30, "medianos", "no")) |> 
  mutate(chicos = ifelse(hogares > 10, "chicos", "no")) |> 
  filter(medianos == "medianos")

datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, "grandes", "no"),
         medianos = ifelse(hogares > 30, "medianos", "no"),
         chicos = ifelse(hogares > 10, "chicos", "no"))

datos |> 
  filter(hogares > 100) |> 
  filter(hectareas > 4)

datos |> 
  filter(hogares > 100,
         hectareas > 4)

# Crear un subconjunto de los datos a partir de un filtro
datos_2 <- datos |> 
  select(nombre, region, comuna, hogares) |> 
  filter(region != "Valparaíso")

datos_2 |> 
  mutate(grupo = ifelse(hogares > 40, "grupo a", "grupo b"))

# crea una variable más compleja utilizando la función case_when()
# esta función opera como varios ifelse() en serie, permitiendo crear 
# una nueva variable que contenga distintos valores dependiendo de 
# las condiciones que se cumplen
datos_2 |> 
  mutate(tamaños = case_when(hogares > 40 ~ "grandes",
                             hogares <= 40 & hogares > 30 ~ "medianos",
                             hogares <= 30 ~ "pequeños"))

# guardar el resultado como un objeto
datos_conteo_2 <- datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 & hogares <= 80 ~ "grandes",
                             hogares <= 40 & hogares > 20 ~ "medianos",
                             hogares <= 20 ~ "pequeños"))

datos_conteo_2

# ver el resultado de la variable que creamos
datos_conteo_2 |> count(tamaños)

# También se puede ir creando la decodificación por pasos, dado que 
# las observaciones que adquieren un resultado
# no vuelven a ser sobrescritas por las siguientes condiciones
datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes"))

datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 ~ "grandes"))

datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 ~ "grandes",
                             hogares > 30 ~ "medianos"))

datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 ~ "grandes",
                             hogares > 30 ~ "medianos",
                             hogares <= 30 ~ "pequeños")) #|> count(tamaños)

# resúmenes ----
# ¿qué pasa si queremos saber algo sobre una columna completa, como su valor mayor, su promedio, etc.?
datos_2 |> arrange(hogares)
min(datos_2$hogares)
max(datos_2$hogares)

# Podemos crear una columna que contenga una operación que se aplica a la columna entera
datos_2 |> 
  mutate(min_hogares = min(hogares),
         max_hogares = max(hogares))
# Pero el resultado aparece repetido por cada observación, lo que puede ser confuso

# usamos la función summarize() para obtener resúmenes de un dataframe 
# a partir de operaciones que se realizan sobre sus columnas
datos_2 |> 
  summarise(maximo = max(hogares))

datos_2 |> 
  summarise(minimo = min(hogares),
            maximo = max(hogares),
            promedio = mean(hogares),
            mediana = median(hogares),
            percentil_25 = quantile(hogares, .25),
            percentil_75 = quantile(hogares, .75))


datos |> 
  summarise(minimo = min(hogares),
            percentil_25 = quantile(hogares, .25),
            promedio = mean(hogares),
            mediana = median(hogares),
            percentil_75 = quantile(hogares, .75),
            maximo = max(hogares))

# lo mismo aplicado a una variable distinta
datos |> 
  summarise(minimo = min(hectareas),
            percentil_25 = quantile(hectareas, .25),
            promedio = mean(hectareas),
            mediana = median(hectareas),
            percentil_75 = quantile(hectareas, .75),
            maximo = max(hectareas))

# truco para reusar este tipo de expresiones en varias columnas: 
# renombrar la columna primero, para que si quieres aplicarlo a otra columna
# solo necesitas cambiar el nombre en el rename()
datos |> 
  rename(variable = hogares) |> 
  summarise(minimo = min(variable),
            percentil_25 = quantile(variable, .25),
            promedio = mean(variable),
            mediana = median(variable),
            percentil_75 = quantile(variable, .75),
            maximo = max(variable))

# crear una función que hace lo mismo
estadisticos <- function(x) {
  x |> 
    summarise(minimo = min(variable),
              percentil_25 = quantile(variable, .25),
              promedio = mean(variable),
              mediana = median(variable),
              percentil_75 = quantile(variable, .75),
              maximo = max(variable)) 
}

# usar la función creada
datos |> rename(variable = hogares) |> estadisticos()
datos |> rename(variable = hectareas) |> estadisticos()
datos |> rename(variable = area) |> estadisticos()


# operaciones agrupadas ----
# Cualquier operación, ya sea un resumen o la creación de una variable nueva, 
# puede ser realizada en separado por grupos de una variable anteponiento group_by()
datos |> 
  group_by(region) |> 
  summarise(minimo = min(hogares),
            percentil_25 = quantile(hogares, .25),
            promedio = mean(hogares),
            mediana = median(hogares),
            percentil_75 = quantile(hogares, .75),
            maximo = max(hogares))
# En este caso, los resúmenes estadísticos se calculan por los grupos que da la variable región

datos_2 |> 
  mutate(min_hogares = min(hogares),
         max_hogares = max(hogares))

datos_2 |> 
  summarize(min_hogares = min(hogares),
            max_hogares = max(hogares))

datos_2 |> 
  group_by(region) |> 
  mutate(min_hogares = min(hogares),
         max_hogares = max(hogares),
         prom_hogares = mean(hogares)) |> 
  arrange(hogares) |> 
  filter(hogares != 0) |> 
  print(n=50)


datos_2 |> 
  group_by(comuna) |> 
  summarize(min_hogares = min(hogares),
            max_hogares = max(hogares),
            prom_hogares = mean(hogares)) |> 
  arrange(desc(max_hogares))


datos_2 |> 
  group_by(region) |> 
  summarize(hogares = sum(hogares),
            min_hogares = min(hogares),
            max_hogares = max(hogares),
            prom_hogares = mean(hogares))

# Contar la cantidad de observaciones en cada grupo obteniendo un resumen agrupados y usando la función n()
datos_2 |> 
  group_by(region) |> 
  summarise(n())

# se obtiene el mismo resultado que haber aplicado count()
datos_2 |> 
  count(region)


datos_2 |> 
  group_by(region) |> 
  summarize(hogares = sum(hogares),
            campamentos = n(),
            min_hogares = min(hogares),
            max_hogares = max(hogares),
            prom_hogares = mean(hogares))

datos_2 |> 
  # creamos la variable tamaños
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 ~ "grandes",
                             hogares > 30 ~ "medianos",
                             hogares <= 30 ~ "pequeños")) |> 
  # calculamos el resumen
  group_by(tamaños) |> 
  summarize(hogares = sum(hogares),
            campamentos = n(),
            min_hogares = min(hogares),
            max_hogares = max(hogares),
            prom_hogares = mean(hogares))

# función para recodificar variables continuas cortándolas en distintos valores
datos |> 
  select(1:2, comuna, hectareas) |> 
  mutate(grupo = cut(hectareas, c(0, 1, 3, 5)))

datos |> 
  select(1:2, comuna, hectareas) |> 
  mutate(grupo = case_when(hectareas > 5 ~ "alto",
                           hectareas > 3 ~ "medio",
                           hectareas > 1 ~ "bajo",
                           .default = "muy bajo"))

datos |> 
  mutate(prueba = hogares/hectareas)

# recodificación ----
# Reemplazar los valores de una variable por otros en base a coincidencias exactas
datos |> 
  select(1:5) |> 
  # distinct(region) |>
  mutate(region = case_match(region, 
                             "Valparaíso" ~ "Valpo",
                             "Aysén del General Carlos Ibáñez del Campo" ~ "Aysén",
                             "Magallanes y de la Antártica Chilena" ~ "Magallanes",
                             "Libertador General Bernardo O'Higgins" ~ "O'Higgins",
                             .default = region)) |> 
  count(region)

datos |> 
  select(1:5) |> 
  # distinct(region) |>
  mutate(region = case_when(region == "Valparaíso" ~ "Valpo",
                            region == "Aysén del General Carlos Ibáñez del Campo" ~ "Aysén",
                            region == "Magallanes y de la Antártica Chilena" ~ "Magallanes",
                            region == "Libertador General Bernardo O'Higgins" ~ "O'Higgins",
                             .default = region)) |> 
  count(region)

# read.csv2()

library(readr)
# read_csv2() # carga en una tabla tibble

# instalar el paquete

# install.packages("readxl")
# install.packages("openxlsx")

library(readxl)

# readxl::read_xlsx()

datos <- read_xlsx("datos/campamentos_chile_2024.xlsx")

datos

library(dplyr)

datos |> 
  select(nombre, region)

datos |> 
  select(hogares, hectareas)

datos |> 
  select(1, 2, 3, 4, 8, 9)

datos |> 
  select(1:4, 8, 9)

datos |> 
  select(1:10)

datos |> 
  select(contains("hect"))

datos |> 
  select(starts_with("cut"))

datos |> 
  select(-starts_with("cut"))

datos |> 
  select(nombre, where(is.numeric))

datos |> 
  select(nombre, where(is.numeric), -cut)

datos |> 
  select(nombre, where(is.numeric)) |> 
  select(-cut)

datos |> 
  select(ends_with("_p"))



# conteos
datos |> 
  count(region)

datos |> 
  count(region) |> 
  arrange(n)

datos |> 
  count(region) |> 
  arrange(desc(n))

datos |> 
  count(provincia, sort = TRUE)

datos |> 
  count(provincia) |> 
  arrange(desc(n)) |> 
  rename(conteo = n)

datos |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo")

datos_conteo <- datos |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo")

# guardarlo como un excel
library(writexl)
# install.packages("writexl")

datos_conteo

write_xlsx(datos_conteo, "datos/conteo.xlsx")

datos_conteo |> 
  write_xlsx("datos/conteo.xlsx")

datos |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo") |> 
  filter(conteo > 50) |> 
  write_xlsx("datos/conteo.xlsx")


read_xlsx("datos/campamentos_chile_2024.xlsx") |> 
  count(provincia, 
        sort = TRUE, 
        name = "conteo") |> 
  filter(conteo > 50) |> 
  write_xlsx("datos/conteo.xlsx")

datos |> 
  filter(hogares > 100)

datos |> 
  filter(!hogares > 100)

datos |> 
  filter(hogares <= 100)

datos |> 
  filter(nombre == "Bellavista")

datos |> 
  filter(nombre != "Bellavista")

datos |> 
  filter(nombre == "Bellavista")

datos |> 
  filter(nombre %in% c("Bellavista", "Los Fleteros", "Manuel Rodríguez"))

datos |> 
  filter(!nombre %in% c("Bellavista", "Los Fleteros", "Manuel Rodríguez"))



lista_campamentos <- c("Bellavista", "Los Fleteros", "Manuel Rodríguez", "Manzana 33")

datos |> 
  filter(nombre %in% lista_campamentos)

datos$nombre %in% lista_campamentos

datos |> 
  select(1:4) |> 
  mutate(variable = 1)

datos |> 
  select(1:4) |> 
  mutate(seleccion = nombre %in% lista_campamentos)

datos |> 
  select(1:4) |> 
  mutate(seleccion = nombre %in% c("Bellavista", "Los Fleteros", "Manuel Rodríguez", "Manzana 33"))



datos |> 
  select(1:4, hogares) |> 
  filter(hogares > 60)

datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = hogares > 60)

datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, 
                          "sí", "no"))

datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, "sí", "no")) |> 
  mutate(medianos = ifelse(hogares > 30, "sí", "no")) |> 
  mutate(chicos = ifelse(hogares > 10, "sí", "no"))

datos |> 
  select(1:4, hogares) |> 
  mutate(grandes = ifelse(hogares > 60, "grandes", "no")) |> 
  mutate(medianos = ifelse(hogares > 30, "medianos", "no")) |> 
  mutate(chicos = ifelse(hogares > 10, "chicos", "no")) |> 
  filter(medianos == "medianos")

datos |> 
  mutate(a = 1,
         b = 2,
         c = 3)

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


datos_2 <- datos |> 
  select(nombre, region, comuna, hogares) |> 
  filter(region != "Valparaíso")

datos_2 |> 
  mutate(grupo = ifelse(hogares > 40, "grupo a", "grupo b"))

datos_2 |> 
  mutate(tamaños = case_when(hogares > 40 ~ "grandes",
                             hogares <= 40 & hogares > 30 ~ "medianos",
                             hogares <= 30 ~ "pequeños"))

datos_conteo_2 <- datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 & hogares <= 80 ~ "grandes",
                             hogares <= 40 & hogares > 20 ~ "medianos",
                             hogares <= 20 ~ "pequeños"))

datos_conteo_2 |> count(tamaños)

datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 ~ "grandes",
                             hogares > 30 ~ "medianos",
                             hogares <= 30 ~ "pequeños")) #|> count(tamaños)

datos_2 |> arrange(hogares)
min(datos_2$hogares)
max(datos_2$hogares)

datos_2 |> 
  mutate(min_hogares = min(hogares),
         max_hogares = max(hogares))

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

datos |> 
  summarise(minimo = min(hectareas),
            percentil_25 = quantile(hectareas, .25),
            promedio = mean(hectareas),
            mediana = median(hectareas),
            percentil_75 = quantile(hectareas, .75),
            maximo = max(hectareas))

datos |> 
  rename(variable = hogares) |> 
  summarise(minimo = min(variable),
            percentil_25 = quantile(variable, .25),
            promedio = mean(variable),
            mediana = median(variable),
            percentil_75 = quantile(variable, .75),
            maximo = max(variable))

estadisticos <- function(x) {
  x |> 
    summarise(minimo = min(variable),
              percentil_25 = quantile(variable, .25),
              promedio = mean(variable),
              mediana = median(variable),
              percentil_75 = quantile(variable, .75),
              maximo = max(variable)) 
}

datos |> rename(variable = hogares) |> estadisticos()
datos |> rename(variable = hectareas) |> estadisticos()
datos |> rename(variable = area) |> estadisticos()



datos |> 
  group_by(region) |> 
  summarise(minimo = min(hogares),
            percentil_25 = quantile(hogares, .25),
            promedio = mean(hogares),
            mediana = median(hogares),
            percentil_75 = quantile(hogares, .75),
            maximo = max(hogares))


datos_2 |> 
  mutate(tamaños = case_when(hogares > 80 ~ "muy grandes",
                             hogares > 40 ~ "grandes",
                             hogares > 30 ~ "medianos",
                             hogares <= 30 ~ "pequeños"))

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

datos_2 |> 
  group_by(region) |> 
  summarise(n())

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

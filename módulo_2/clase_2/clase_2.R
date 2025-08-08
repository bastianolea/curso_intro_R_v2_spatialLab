library(dplyr)
library(readxl)

pobreza <- read_xlsx("datos/pobreza_comunas_2022.xlsx")

pobreza |> 
  slice(-1)

install.packages("janitor")
library(janitor)

pobreza |> 
  janitor::row_to_names(2)


pobreza <- read_xlsx("datos/pobreza_comunas_2022.xlsx", skip = 2)

pobreza

glimpse(pobreza)

pobreza |> 
  select(Región, `Número de personas según proyecciones de población (*)`)

pobreza |> 
  rename(region = Región,
         poblacion = `Número de personas según proyecciones de población (*)`,
         pobreza_multi = `Número de personas en situación de pobreza multidimensional (**)`)

pobreza |> 
  rename(region = 2,
         poblacion = 4,
         pobreza_multi = 5)

pobreza |> 
  janitor::clean_names()

pobreza_2 <- pobreza |> 
  janitor::clean_names() |>
  rename(poblacion = 4,
         pobreza_personas = 5,
         pobreza_porcentaje = 6)

names(pobreza_2)

pobreza_2


pobreza_malo <- read_xlsx("datos/pobreza_comunas_2022.xlsx")

pobreza_malo_2 <- pobreza_malo |> 
  row_to_names(2) |> 
  janitor::clean_names() |>
  rename(poblacion = 4,
         pobreza_personas = 5,
         pobreza_porcentaje = 6,
         casen = presencia_de_la_comuna_en_la_muestra_casen,
         tipo = tipo_de_estimacion_sae)

pobreza_malo_2 |> 
  print(n = 100)

pobreza_malo_2 |> 
  mutate(pobreza_personas = as.numeric(pobreza_personas)) |> 
  summarize(sum(pobreza_personas))

pobreza_malo_2 |> 
  filter(is.na(pobreza_personas)) |> 
  select(codigo)


pobreza_malo_3 <- pobreza_malo_2 |> 
  filter(!is.na(pobreza_personas))

pobreza_malo_2 |> tail()
pobreza_malo_3 |> tail()

pobreza_malo_3 |> 
  mutate(pobreza_personas = as.numeric(pobreza_personas)) |> 
  summarize(sum(pobreza_personas),
            max(pobreza_personas),
            mean(pobreza_personas))


pobreza_malo_2 |> 
  filter(is.na(pobreza_porcentaje))


library(messy)

pobreza_malo_3b <- pobreza_malo_3 |> 
  make_missing(cols = "pobreza_personas")

pobreza_malo_3b |> 
  filter(is.na(pobreza_personas))

pobreza_malo_3b |> 
  summarize(sum(is.na(pobreza_personas)),
            sum(is.na(pobreza_porcentaje)))


pobreza_malo_3b |> 
  mutate(pobreza_personas = as.numeric(pobreza_personas)) |> 
  summarize(n_missing = sum(is.na(pobreza_personas)),
            suma_pobreza_personas = sum(pobreza_personas, na.rm = TRUE))


names(pobreza)
names(pobreza_malo_3b)

tibble(
  etiqueta = names(pobreza),
  variable = names(pobreza_malo_3b)
)

tribble(~etiqueta, ~variable,
        "Años",        "anio",
        "Edades", NA) |> 
  filter(!is.na(variable))

pobreza_malo_3b |> 
  mutate(pobreza_personas = as.numeric(pobreza_personas),
         pobreza_porcentaje = as.numeric(pobreza_porcentaje),
         limite_inferior = as.numeric(limite_inferior)
  )

pobreza_malo_4 <- pobreza_malo_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(pobreza_personas,
        pobreza_porcentaje,
        limite_inferior,
        limite_superior),
      # función a aplicar
      as.numeric))

pobreza_malo_3b |> 
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

pobreza_malo_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(4:8),
      # función a aplicar
      as.numeric))

pobreza_malo_3b |> 
  mutate(
    across(
      # columnas a transformar
      starts_with("pobreza"),
      # función a aplicar
      as.numeric))

pobreza_malo_3b |> 
  mutate(
    across(
      # columnas a transformar
      where(is.character),
      # función a aplicar
      as.numeric))

pobreza_malo_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(where(is.character), -region, -nombre_comuna),
      # función a aplicar
      as.numeric))

pobreza_malo_3b |> 
  mutate(
    across(
      # columnas a transformar
      c(where(is.character), -region, -nombre_comuna),
      # función a aplicar
      as.numeric))


pobreza_malo_4 |> 
  summarise(
    across(
      # a estas columnas
      4:8,
      # aplicarle esto
      ~sum(is.na(.x)) # función lambda
    )
  )

pobreza_malo_4 |> 
  summarise(
    across(
      # a estas columnas
      5:8,
      # aplicarle esto
      ~sum(.x, na.rm = TRUE) # función lambda
    )
  )


pobreza_malo_4 |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), 0, pobreza_personas))

pobreza_malo_4 |> 
  mutate(pobreza_personas = round(pobreza_personas, 0),
         pobreza_personas = ifelse(is.na(pobreza_personas), "(Sin datos)", pobreza_personas))

install.packages("tidyr")
library(tidyr)


pobreza_malo_4 |> 
  mutate(pobreza_personas = replace_na(pobreza_personas, 0))


pobreza_malo_4 |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), 
                                   mean(pobreza_personas, na.rm = T), 
                                   pobreza_personas))

pobreza_malo_4 |> 
  group_by(region) |> 
  mutate(pobreza_personas = ifelse(is.na(pobreza_personas), 
                                   mean(pobreza_personas, na.rm = T), 
                                   pobreza_personas))

pobreza_malo_4 |> 
  # select(region, nombre_comuna, pobreza_personas) |> 
  group_by(region) |> 
  mutate(pobreza_personas_2 = ifelse(is.na(pobreza_personas), 
                                     mean(pobreza_personas, na.rm = T), 
                                     pobreza_personas))


pobreza_4 <- pobreza_malo_4

# ----

censo <- read_xlsx("datos/estimaciones-y-proyecciones-2002-2035-comunas.xlsx") |> 
  clean_names() |> 
  rename(sexo = 7)

censo |> 
  filter(nombre_comuna == "La Florida") |> 
  select(poblacion_2010)

censo |> 
  glimpse()

library(tidyr)

censo_2 <- censo |> 
  pivot_longer(cols = starts_with("poblacion"),
               names_to = "año",
               values_to = "poblacion")

censo_3 <- censo_2 |> 
  separate(col = año, sep = "_", into = c("tipo", "año")) |> 
  mutate(año = as.numeric(año))


censo_3 |> 
  filter(nombre_comuna == "La Florida",
         año == 2025)

censo_region <- censo_3 |> 
  group_by(nombre_region, año) |> 
  summarise(poblacion = sum(poblacion))

censo_3 |> 
  group_by(nombre_region, nombre_comuna, año) |> 
  summarise(poblacion = sum(poblacion)) |> 
  print(n=100)

options(scipen = 9999)

censo_region_2 <- censo_region |> 
  pivot_wider(names_from = año,
              values_from = poblacion)

censo_region_2 |> 
  writexl::write_xlsx("datos/estimaciones-regiones.xlsx")


genero <- read_xlsx("datos/P5_Genero.xlsx", sheet = 2)
genero <- read_xlsx("datos/P5_Genero.xlsx", sheet = "1")

genero |> 
  row_to_names(3) |> 
  clean_names() |> 
  mutate(across(3:10, as.numeric)) |> 
  filter(!is.na(region))

genero_2 <- genero |> 
  row_to_names(3) |> 
  # clean_names() |> 
  mutate(across(3:10, as.numeric)) |> 
  filter(!is.na(Región)) |> 
  pivot_longer(where(is.numeric),
               names_to = "género", values_to = "población")

genero_2


eleccion <- readr::read_csv2("datos/presidenciales_2021_comuna.csv")

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

eleccion_region_2 <- eleccion_region |> 
  pivot_wider(names_from = candidatura, values_from = votos)

eleccion_region_2


eleccion_3 <- eleccion_2 |> 
  filter(candidatura %in% c("Gabriel Boric", "Jose Antonio Kast")) |> 
  select(2:7) |> 
  pivot_wider(names_from = candidatura, values_from = votos)


eleccion_3
pobreza_4

left_join(eleccion_3,
          pobreza_4, 
          by = "comuna")

left_join(eleccion_3,
          pobreza_4, 
          by = c("comuna" = "nombre_comuna"))

left_join(eleccion_3,
          pobreza_4, 
          by = c("cut_comuna" = "codigo"))

eleccion_4 <- eleccion_3 |> 
  rename(codigo_comuna = cut_comuna)

pobreza_5 <- pobreza_4 |> 
  rename(codigo_comuna = codigo) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  select(codigo_comuna, poblacion, pobreza_porcentaje)

eleccion_pobreza <- left_join(eleccion_4,
                              pobreza_5, 
                              by = "codigo_comuna")


eleccion_pobreza |> 
  arrange(desc(pobreza_porcentaje))

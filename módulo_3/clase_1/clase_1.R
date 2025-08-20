install.packages("ggplot2")

library(ggplot2)

ggplot()

iris
mtcars

# ggplot ----
ggplot(iris)

iris |> 
  ggplot()

names(iris)

## dispersión ----
iris |> 
  ggplot() +
  # mapeo
  aes(x = Sepal.Length, y = Sepal.Width) +
  # geometría
  geom_point()


iris |> 
  ggplot() +
  # mapeo
  aes(x = Sepal.Length, y = Sepal.Width, colour = Species) +
  # geometría
  geom_point()

iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, colour = Species) +
  # geometría
  geom_point()


iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, 
      y = Sepal.Width, 
      colour = Petal.Length) +
  # geometría
  geom_point()



iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, colour = Species) +
  # geometría
  geom_point(colour = "orchid", size = 5, alpha = 0.3)

iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, alpha = Sepal.Length) +
  # geometría
  geom_point(colour = "orchid", size = 5)

iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, size = Petal.Width) +
  # geometría
  geom_point(colour = "orchid", alpha = 0.3)

iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, color = Petal.Width) +
  # geometría
  geom_point(alpha = 0.3)

## histograma ----
iris |> 
  ggplot() +
  aes(x = Petal.Length) +
  geom_histogram()

iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(bins = 10)

iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(binwidth = 0.2)

iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(binwidth = 0.2, colour = "palegreen4", fill = "palegreen3")


iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_density()

iris |> 
  ggplot() +
  aes(x = Sepal.Width) +
  geom_density(fill = "tomato", colour = "tomato3", alpha = 0.6)


iris |> 
  ggplot() +
  aes(x = Sepal.Width, fill = Species) +
  geom_density(alpha = 0.4, linewidth = 0)

iris |> 
  ggplot() +
  aes(x = Sepal.Width, 
      fill = Species, color = Species) +
  geom_density(alpha = 0.4)


iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, color = Petal.Width) +
  # geometría
  geom_point(alpha = 0.3) +
  scale_x_continuous(name = "Ancho", breaks = c(0, 1, 2, 3))
  # scale_x_continuous(name = "Ancho", n.breaks = 10)



# datos ----
library(dplyr)

temp <- readr::read_csv2("datos/temperaturas_chile.csv")

temp |> 
  glimpse()

temp |> distinct(nombre) |> print(n=Inf)

temp |> 
  select(-t_med) |> 
  na.omit()

temp |> 
  filter(nombre == "Quinta Normal, Santiago") |> 
  ggplot() +
  aes(t_max) +
  geom_histogram()

temp |> 
  # filter(nombre == "El Loa, Calama Ad.") |>
  filter(nombre == "Quinta Normal, Santiago") |>
  ggplot() +
  aes(t_max) +
  geom_histogram()

temp |> 
  # filter(nombre == "El Loa, Calama Ad.") |>
  filter(nombre == "Quinta Normal, Santiago") |>
  ggplot() +
  aes(fecha) +
  geom_histogram()

temp |> 
  filter(nombre == "Quinta Normal, Santiago") |>
  ggplot() +
  aes(t_max) +
  geom_histogram() +
  geom_vline(xintercept = 20)

temp |> 
  filter(nombre == "Quinta Normal, Santiago") |>
  ggplot() +
  geom_histogram(aes(t_max)) +
  geom_vline(aes(xintercept = mean(t_max, na.rm = T)))


temp_q <- temp |> 
  filter(nombre == "Quinta Normal, Santiago") |>
  # filter(nombre == "El Loa, Calama Ad.") |>
  filter(!is.na(t_max))

prom <- mean(temp_q$t_max)

temp_q |> 
  ggplot() +
  aes(t_max) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "indianred3", linewidth = 1.2)


temp_q |> 
  mutate(tipo = ifelse(t_max > prom, 
                       "alta", "baja")) |> 
  ggplot() +
  aes(t_max, fill = tipo) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "indianred3", linewidth = 1.2)

temp_q |> 
  ggplot() +
  aes(t_max, fill = t_max > prom) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "indianred3", linewidth = 1.2)

temp_q |> 
  mutate(tipo = ifelse(t_max > prom, 
                       "alta", "baja")) |> 
  ggplot() +
  aes(t_max, fill = tipo) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "cyan3", linewidth = 1.2) +
  scale_fill_manual(values = c("alta" = "brown2", 
                               "baja" = "royalblue2")) +
  annotate(geom = "text", x = prom*0.96, y = 250, 
           label = round(prom, 1), hjust = 1) +
  theme_classic()
  


temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(t_min, t_max, colour = mes) +
  geom_point(alpha = 0.3)

# {gganimate}

temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(x = fecha, y = t_max) +
  geom_line(linewidth = 0.1)


temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  # scale_color_gradient(low = "royalblue2", high = "brown2")
  scale_color_gradient2(low = "royalblue2", mid = "black", 
                        high = "brown2", midpoint = 22)


temp_q <- temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018)

prom <- mean(temp_q$t_max, na.rm = TRUE)
prom <- temp_q |> 
  summarise(mean(t_max, na.rm = TRUE)) |> 
  pull()

temp_q |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  scale_color_gradient(low = "royalblue2", high = "brown2") +
  geom_hline(yintercept = prom)

temp_q |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  scale_color_gradient(low = "royalblue2", high = "brown2") +
  geom_hline(yintercept = prom)



temp |> distinct(nombre)
temp |> 
  filter(nombre == "Eulogio Sánchez, Tobalaba Ad.",
         año > 2018) |> 
  ggplot() +
  aes(fecha, t_max) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

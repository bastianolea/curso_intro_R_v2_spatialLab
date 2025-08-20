# install.packages("ggplot2")
library(ggplot2)

# ggplot ----

# inicializar gráfico
ggplot()

# definir el conjunto de datos que usaremos
mtcars
iris
# estos vienen con R por defecto

# datos ----
# pasamos los datos al gráfico 
# así
ggplot(iris)

# o así
iris |> 
  ggplot()


## dispersión ----
# ver variables del conjunto de datos
names(iris)

iris |> # pasar datos al gráfico
  ggplot() + # iniciar gráfico
  # mapeo de variables
  aes(x = Sepal.Length, # eje horizontal
      y = Sepal.Width) + # eje vertical
  # geometría (figuras que representarán los datos)
  geom_point()

# aplicar color
iris |> 
  ggplot() +
  # mapeo
  aes(x = Sepal.Length, 
      y = Sepal.Width, 
      colour = Species) + # mapear una variable al color de las geometrías
  # geometría
  geom_point()

# probar con otras variables
iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, 
      y = Petal.Length, 
      colour = Species) +
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


# definir la apariencia de una geometría
iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length) +
  # geometría
  geom_point(colour = "orchid", # color
             size = 5, # tamaño
             alpha = 0.3) # transparencia

# mapear una variable a la transparencia de los puntos
iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, alpha = Sepal.Length) +
  # geometría
  geom_point(colour = "orchid", size = 5)

# mapear una variable al tamaño de los puntos
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

# configurar cantidad de barras
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(bins = 10)

# configurar cantidad de barras
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(binwidth = 0.2)

# cambiar colores
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(binwidth = 0.2, 
                 colour = "palegreen4", # borde
                 fill = "palegreen3") # relleno

# densidad ----
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_density()

iris |> 
  ggplot() +
  aes(x = Sepal.Width) +
  geom_density(fill = "tomato", # relleno
               colour = "tomato3", # borde
               alpha = 0.6) # transparencia

iris |> 
  ggplot() +
  aes(x = Sepal.Width, fill = Species) +
  geom_density(alpha = 0.4, linewidth = 0)

iris |> 
  ggplot() +
  aes(x = Sepal.Width, 
      fill = Species, color = Species) +
  geom_density(alpha = 0.4)

# modificar escala horizontal
iris |> 
  ggplot() +
  # mapeo
  aes(x = Petal.Width, y = Petal.Length, color = Petal.Width) +
  # geometría
  geom_point(alpha = 0.3) +
  # escala horizontal
  scale_x_continuous(name = "Ancho", breaks = c(0, 1, 2, 3))
  # scale_x_continuous(name = "Ancho", n.breaks = 10)



# datos ----
library(dplyr)

# cargar datos
temp <- readr::read_csv2("datos/temperaturas_chile.csv")

# explorar datos
temp |> 
  glimpse()

temp |> distinct(nombre) |> print(n=Inf)

temp |> 
  select(-t_med) |> 
  na.omit()

# crear histograma con los datos
temp |> 
  # filtrar datos
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

# agregar una línea vertical
temp |> 
  filter(nombre == "Quinta Normal, Santiago") |>
  ggplot() +
  aes(t_max) +
  geom_histogram() +
  geom_vline(xintercept = 20)

# hacer que la línea vertical muestre el promedio
# primero, filtrar datos
temp_q <- temp |> 
  filter(nombre == "Quinta Normal, Santiago") |>
  # filter(nombre == "El Loa, Calama Ad.") |>
  filter(!is.na(t_max))

# calcular promedio
prom <- mean(temp_q$t_max)

# aplicar promedio en la línea vertical
temp_q |> 
  ggplot() +
  aes(t_max) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "indianred3", linewidth = 1.2)

# o bien, calcular el promedio directamente en la aes() de la geometría correspondiente
temp |> 
  filter(nombre == "Quinta Normal, Santiago") |>
  ggplot() +
  geom_histogram(aes(t_max)) +
  geom_vline(aes(xintercept = mean(t_max, na.rm = T)))


# crear una variable para aplicar color a una geometría
temp_q |> 
  mutate(tipo = ifelse(t_max > prom, 
                       "alta", "baja")) |> 
  ggplot() +
  aes(t_max, fill = tipo) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "indianred3", linewidth = 1.2)

# o bien, calcular directamente en la geometría (opcional, no recomendado)
temp_q |> 
  ggplot() +
  aes(t_max, fill = t_max > prom) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "indianred3", linewidth = 1.2)

# aplicar variable nueva y cambiarle los colores
temp_q |> 
  mutate(tipo = ifelse(t_max > prom, 
                       "alta", "baja")) |> 
  ggplot() +
  aes(t_max, fill = tipo) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "cyan3", linewidth = 1.2) +
  # escala de colores manual
  scale_fill_manual(values = c("alta" = "brown2", 
                               "baja" = "royalblue2")) 

# agregar un texto para la línea del promedio
temp_q |> 
  mutate(tipo = ifelse(t_max > prom, 
                       "alta", "baja")) |> 
  ggplot() +
  aes(t_max, fill = tipo) +
  geom_histogram() +
  geom_vline(xintercept = prom, 
             colour = "cyan3", linewidth = 1.2) +
  # escala de colores manual
  scale_fill_manual(values = c("alta" = "brown2", 
                               "baja" = "royalblue2")) +
  annotate(geom = "text", 
           x = prom*0.96, y = 250, 
           label = round(prom, 1),
           hjust = 1 # justificación
           ) +
  theme_classic() # tema
  

# dispersión ----
temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(t_min, t_max, colour = mes) +
  geom_point(alpha = 0.3)

# es posible animal el gráfico con el paquete {gganimate}

# líneas ----
temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(x = fecha, y = t_max) +
  geom_line(linewidth = 0.1)

# aplicar color a una variable contínua
temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  scale_color_gradient(low = "royalblue2", high = "brown2")

# escala de color con color intermedio
temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018) |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  scale_color_gradient2(low = "royalblue2", mid = "black", 
                        high = "brown2", 
                        midpoint = 22) # punto medio de la escala

# otra forma de calcular el promedio
# primero, filtrar
temp_q <- temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 2018)

# opción a
prom <- mean(temp_q$t_max, na.rm = TRUE)

# opción b
prom <- temp_q |> 
  summarise(mean(t_max, na.rm = TRUE)) |> 
  pull()

temp_q |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  scale_color_gradient(low = "royalblue2", high = "brown2") +
  geom_hline(yintercept = prom)

temp |> distinct(nombre)

# agregar una geometría que agrega una línea de regresión
temp |> 
  filter(nombre == "Eulogio Sánchez, Tobalaba Ad.",
         año > 2018) |> 
  ggplot() +
  aes(fecha, t_max) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

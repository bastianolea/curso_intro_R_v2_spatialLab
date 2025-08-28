library(ggplot2)
library(dplyr)

# seguimos explorando el dataset de temperaturas
temp <- readr::read_csv2("datos/temperaturas_chile.csv")

temp |> 
  glimpse()

temp |> distinct(nombre) |> print(n=Inf)

estacion <- "Quinta Normal, Santiago"

# crear gráfico y asignarlo a un objeto
grafico <- temp |> 
  filter(nombre == estacion,
         año > 2018) |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.1) +
  scale_color_gradient(low = "royalblue2", high = "brown2")

# ver el gráfico
grafico

## temas ----
# agregarle más capas
grafico +
  theme_minimal()

grafico +
  theme_classic()

grafico +
  theme_linedraw()

# personalizar apariencia especificando el tema
grafico +
  scale_y_continuous(minor_breaks = c(15, 25, 35)) +
  theme_classic() +
  theme(axis.text.y = element_text(face = "bold", 
                                   colour = "black"),
        axis.text.x = element_text(face = "italic", 
                                   colour = "grey")) +
  theme(panel.grid.major = element_line(colour = "gray90", 
                                        linewidth = 0.3),
        panel.grid.minor = element_line(colour = "gray90", 
                                        linewidth = 0.1),)

grafico +
  theme_minimal()

# agregar textos
grafico +
  theme_minimal() +
  labs(title = "Fluctuación de temperaturas",
       subtitle = paste("Estación", estacion)) +
  labs(x = "Años",
       y = "temperatura máxima",
       color = "temperatura\nmáxima") +
  theme(plot.title = element_text(family = "Verdana", face = "bold"))


## gráficos de barras ----
# filtrar los datos
estacion <- "Quinta Normal, Santiago"

temp_filt <- temp |> 
  filter(nombre == estacion,
         año > 2018)

# gráfico de barras que genera un conteo de las observaciones de la variable elegida
temp |> 
  filter(!is.na(zona_geografica)) |> 
  ggplot() +
  aes(zona_geografica) +
  geom_bar()

# contar primero y luego crear barras en base a la variable y su conteo, por separado
temp |> 
  filter(!is.na(zona_geografica)) |> 
  distinct(zona_geografica, nombre) |> 
  # conteo
  count(zona_geografica) |> 
  ggplot() +
  aes(x = zona_geografica, y = n) + # el eje y tiene el alto de cada barra
  geom_col(width = .6)

# si damos vuelta x e y, las barras aparecen laterales
temp |> 
  filter(!is.na(zona_geografica)) |> 
  distinct(zona_geografica, nombre) |> 
  count(zona_geografica) |> 
  ggplot() +
  aes(n, zona_geografica) +
  geom_col(width = .6)

# agregar conteos encima de las barras
temp_conteo <- temp |> 
  filter(!is.na(zona_geografica)) |> 
  distinct(zona_geografica, nombre) |> 
  count(zona_geografica) |> 
  arrange(n)

temp_conteo

temp_conteo |> 
  ggplot() +
  aes(x = zona_geografica, y = n) +
  geom_col(width = .6) +
  geom_text(aes(label = n, y = n+0.6)) +
  scale_y_continuous(expand = expansion(c(0, 0.1)))

## ordenar las barras ----
class(temp_conteo$zona_geografica)

# las variables de texto tienen un orden alfabético
variable <- c("alto", "medio", "bajo")

sort(variable)

# necesitamos establecer un orden específico para la variable
# para esto existe el tipo de datos "factor"
variable_factor <- factor(variable, c("alto", "medio", "bajo"))

variable_factor |> sort()
# ahora los datos aparecen en el orden esperado

# paquete para trabajar con factores
library(forcats)
# install.packages("forcats")

# ver nombres de los niveles de la variable
temp_conteo$zona_geografica |> cat(sep = "\n")

# ordenar los niveles del factor manualmente
temp_conteo |> 
  mutate(zona_geografica = fct_relevel(zona_geografica,
                                       "Litoral",
                                       "Secano Costero",
                                       "Valle",
                                       "PreCordillera")) |> 
  ggplot() +
  aes(x = zona_geografica, y = n) +
  geom_col(width = .6) +
  geom_text(aes(label = n, y = n+0.6)) +
  scale_y_continuous(expand = expansion(c(0, 0.1)))

# ordenar los niveles del factor por el orden de una segunda variable (con los conteos)
temp_conteo |> 
  mutate(zona_geografica = fct_reorder(zona_geografica,
                                       n, .desc = T)) |> 
  ggplot() +
  aes(x = zona_geografica, y = n) +
  geom_col(width = .6) +
  geom_text(aes(label = n, y = n+0.6)) +
  scale_y_continuous(expand = expansion(c(0, 0.1)))


glimpse(temp)

temp |> 
  group_by(año) |> 
  filter(año > 1970) |> 
  summarise(t_max = max(t_max, na.rm = T)) |> 
  ggplot() +
  aes(año, t_max) +
  geom_col()

# sacar 3 estaciones al azar
estaciones <- unique(temp$nombre)

estacion <- sample(estaciones, 3)

estacion

temp_azar <- temp |> 
  filter(nombre %in% estacion)

temp_azar_año <- temp_azar |> 
  filter(año >= 2010) |> 
  group_by(año, nombre) |> 
  summarise(t_max = max(t_max, na.rm = T))

temp_azar_año |> 
  ggplot() +
  aes(año, t_max, fill = t_max) +
  geom_col(width = 0.5) +
  geom_text(aes(label = t_max, y = t_max+1),
            angle = -90, hjust = 1, size = 2.7,
            color = "gray40") +
  scale_fill_gradient(low = "royalblue2", high = "brown2") +
  facet_wrap(~nombre) +
  theme_linedraw() +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  theme(axis.text.x = element_text(angle = -50, 
                                   hjust = 0)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  theme(legend.key.width = unit(2, "mm")) +
  labs(subtitle = "Temperaturas máximas")

# guardar gráfico
ggsave("temperaturas.jpg")

# guardar con un tamaño distinto al por defecto
ggsave("temperaturas_b.jpg", 
       width = 9, height = 6,
       scale = 1.2)

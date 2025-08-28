library(dplyr)
library(ggplot2)

# cargar datos de temperatura
temp <- readr::read_csv2("datos/temperaturas_chile.csv")

temp

## histograma ----
temp |> 
  ggplot() +
  aes(t_max) +
  geom_histogram()

## densidad ----
temp |> 
  ggplot() +
  aes(t_max) +
  geom_density()

## boxplot ----
temp |> 
  ggplot() +
  aes(x = t_max) +
  geom_boxplot()
# muestra la distribución de los datos con una caja que representa los percentiles 25, 50 (mediana) y 75

temp |> 
  ggplot() +
  aes(y = t_max, color = zona_geografica) +
  geom_boxplot()

## violín ----
# muestra la distribución de los datos
temp |> 
  ggplot() +
  aes(1, y = t_max) +
  geom_violin()
# en este caso ponemos la distribución sobre el número 1, para que sea una sola figura

# una distribución por cada valor de la variable
temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max) +
  geom_violin()

# combinar capas de geometrías
temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max) +
  geom_violin() +
  geom_boxplot()

# modificar apariencia
temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(alpha = 0.6, outlier.size = 1, outliers = F) +
  theme_classic()

temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(alpha = 0.6, outlier.size = 1, outliers = F) +
  theme_classic()

## puntos repartidos ----
# permite que los puntos se muevan al azar en una dirección para poder ver distribuciones cuando hay demasiados puntos en una misma posición
temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(alpha = 0.4, outlier.size = 1, outliers = F) +
  geom_jitter(alpha = 0.1, size = 0.2) +
  theme_classic()

# filtrar los datos
# filtrar datos
temp_f <- temp |> 
  filter(!is.na(zona_geografica),
         año == 2023,
         mes == 4)

temp_f |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_boxplot(alpha = 0.4, outlier.size = 1, outliers = F) +
  geom_jitter(alpha = 0.4, size = 0.4,
              height = 0, width = 0.2) +
  theme_classic()

# combinar jitter con violín
temp_f |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.4) +
  geom_jitter(alpha = 0.4, size = 0.4,
              height = 0, width = 0.2) +
  theme_classic()


# calcular resumen de datos
temp_r <- temp_f |> 
  group_by(zona_geografica) |> 
  summarise(promedio = mean(t_max, na.rm = T),
            median = median(t_max, na.rm = T))

# prueba de cómo queda el resumen
temp_r |> 
  ggplot() +
  aes(zona_geografica, promedio) +
  geom_point()

# poner el resumen encima del gráfico
# para esto, creamos una capa que usa sus propios datos
temp_f |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.4) +
  geom_jitter(alpha = 0.4, size = 0.4,
              height = 0, width = 0.2) +
  # capa que usa datos de otro objeto distinto al principal del gráfico
  geom_point(data = temp_r,
             aes(zona_geografica, promedio),
             size = 3, color = "black") +
  theme_classic()


## mosaico ----
# calcular resumen de datos por año y mes
temp_m <- temp |> 
  filter(año > 2015) |> 
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            n = n())

# visualizar con puntos para obtener una suerte de matriz
temp_m |> 
  ggplot() +
  aes(x = año, y = mes, color = n) +
  geom_point()

# visualizar con geometría de baldozas o mapa de calor
temp_m |> 
  filter(año > 2015) |> 
  ggplot() +
  aes(x = año, y = mes, fill = t_max) +
  geom_tile()

# aplicar escala de colores viridis
temp_m |> 
  filter(año > 1980) |>
  ggplot() +
  aes(x = año, y = mes, fill = t_max) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c(option = "inferno")


temp_m |> 
  filter(año > 1990) |>
  ggplot() +
  aes(x = año, y = mes, fill = t_max) +
  geom_tile() +
  coord_equal(expand = F) +
  scale_fill_viridis_c(option = "inferno") +
  scale_y_continuous(breaks = 1:12) +
  scale_x_continuous(breaks = 1990:2024) +
  theme(axis.text.x = element_text(angle = -90,
        hjust = 0, vjust = 0.5))

# dispersión con texto ----
grafico_disp <- temp |> 
  filter(año == 2023,
         mes == 3,
         dia == 15) |> 
  ggplot() +
  aes(t_min, t_max) +
  geom_point(size = 3, alpha = 0.5) +
  theme_linedraw()

grafico_disp

# agregar texto
grafico_disp + geom_text(aes(label = t_max), size = 3)
# no queda bien
  
# probar mover los puntos manualmente
grafico_disp + geom_text(aes(label = t_max, y = t_max+1), size = 3)
# queda mejor, pero no es ideal

# mover puntos automáticamente con {ggrepel}
# install.packages("ggrepel")
grafico_disp + ggrepel::geom_text_repel(aes(label = t_max), size = 3)

# filtrar los datos que pasan a una capa
grafico_disp +
  ggrepel::geom_text_repel(data = ~filter(.x, 
                                          t_max < 20,
                                          t_min < 5),
                             aes(label = t_max), size = 3)

# —----

library(arrow)

# cargar datos de delincuencia
cead <- arrow::read_parquet("datos/cead_delincuencia_chile.parquet")

# sumar cantidad total de delitos
cead_suma <- cead |> 
  # filtrar el año
  filter(fecha >= "2013-01-01",
         fecha <= "2013-12-31") |> 
  group_by(delito) |> 
  summarize(n = sum(delito_n))

# paquete para trabajar con fechas
library(lubridate)

# repetir pero con {lubridate}
cead_suma <- cead |> 
  # filtrar el año
  filter(year(fecha) == 2024) |> 
  group_by(delito) |> 
  summarize(n = sum(delito_n))

cead_suma |> 
  arrange(desc(n))

## barras ----
cead_suma |> 
  ggplot() +
  aes(delito, n) +
  geom_col()

# barras ordenadas
cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  ggplot() +
  aes(n, delito) +
  geom_col()

# barras ordenadas con filtro previo
cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(n, delito) +
  geom_col()

# calcular mayores delitos
top_delitos <- cead_suma |> 
  arrange(desc(n)) |> 
  slice_max(n, n = 10) |> 
  pull(delito)

top_delitos

# filtrar para tener solo estos delitos
cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(n, delito, fill = delito) +
  geom_col() +
  # theme(legend.position = "none")
  guides(fill = guide_none())

## tortas ----
# para hacer un gráfico de torta, primero hay que hacer una barra apilada
grafico_cead <- cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito)

grafico_cead + 
  geom_col()

# luego "enrollar" la barra cambiando la capa de coordenadas
grafico_cead +
  geom_col(color = "white", linewidth = 1.2) +
  coord_polar(theta = "y") +
  theme_void()

# agregar texto
grafico_cead +
  geom_col(color = "white", linewidth = 0.6) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = n))
# sale pésimo, veamos por qué

# los textos están apareciendo abajo, porque no están ajustándose a la barra apilada
grafico_cead +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(label = scales::comma(n, big.mark = ".")),
            size = 3)

# ajustar a la barra apilada
grafico_cead +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3)

# ahora volver a la torta
grafico_cead +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3) +
  coord_polar(theta = "y")

# modificar la posición del texto para que aparezca más hacia un lado
grafico_cead +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(x = 1.3, label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3)

# aplicar a la torta
grafico_cead +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(x = 1.3, label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3) +
  coord_polar(theta = "y")


## sumar valores en "otros" ----
cead_suma |> 
  arrange(desc(n)) |> 
  print(n=20)

# crear una variable que agrupa las categorías minoritarias en una sola
cead_suma_otros <- cead_suma |> 
  arrange(desc(n)) |> 
  mutate(delito2 = forcats::fct_lump_n(delito, 
                                       n = 6, 
                                       w = n, # variable con el conteo
                                       other_level = "Otros delitos")) |>
  # volver a sumar los grupos para colapsar el "otros"
  group_by(delito2) |> 
  summarize(n = sum(n))

# aplicar nueva fuente de datos al gráfico anterior
cead_suma_otros |> 
  rename(delito = delito2) |> 
  # filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito) +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(x = 1.3, label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(reverse = T, position = "bottom", 
                             ncol = 2, title = NULL))


## donas ----
# agregar expansión en el eje x para crear un espacio dentro de la torta
cead_suma_otros |> 
  rename(delito = delito2) |> 
  # filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito) +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(x = 1, label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3) +
  coord_polar(theta = "y") +
  scale_x_continuous(expand = expansion(c(1.5, 0))) +
  guides(fill = guide_legend(reverse = T, position = "bottom", 
                             ncol = 2, title = NULL))

## lollypop ---- 
cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  ggplot() +
  aes(n, delito) +
  geom_col()

# en lugar de barras, ponemos puntos conectados con un segmento al origen del eje
cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 50000) |> 
  ggplot() +
  aes(x = n, y = delito) +
  # geom_col() +
  geom_point(size = 5) +
  geom_point(size = 10, alpha = 0.1) +
  geom_segment(aes(xend = 0, yend = delito))


## temas ----
# crear un gráfico base
grafico <- cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 30000) |> 
  ggplot() +
  aes(n, delito) +
  geom_col(width = 0.5)

grafico

# cargar el paquete para los temas
library(thematic)
# install.packages("thematic")

# definir dos colores
thematic_on(bg = "gray20",
            fg = "indianred2")

# aplicarlos al tema
grafico

# cambiar paleta
thematic_on(bg = "#1F284D",
            fg = "#37C0C7")

# ver como queda
grafico


## tipografías ----
library(showtext)
# install.packages("showtext")

# agregar una tipografía desde Google Fonts
font_add_google("Fira Code")

# activar uso de tipografías
showtext_auto()

# ajustar tamaño de las letras
showtext_opts(dpi = 250)

# probar
grafico +
  theme(text = element_text(family = "Fira Code")) +
  geom_text(aes(label = scales::comma(n, big.mark = "."),
                x = n-2000), size = 3, hjust = 1,
            color = "#1F284D") +
  labs(title = "Delitos totales",
       subtitle = "Año 2024") +
  theme(plot.title.position = "plot")

# desactivar temas
thematic_off()

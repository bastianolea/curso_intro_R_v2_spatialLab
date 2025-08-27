library(dplyr)
library(ggplot2)

temp <- readr::read_csv2("datos/temperaturas_chile.csv")

temp

temp |> 
  ggplot() +
  aes(t_max) +
  geom_histogram()

temp |> 
  ggplot() +
  aes(t_max) +
  geom_density()

temp |> 
  ggplot() +
  aes(x = t_max) +
  geom_boxplot()

temp |> 
  ggplot() +
  aes(y = t_max) +
  geom_boxplot()

temp |> 
  ggplot() +
  aes(y = t_max, color = zona_geografica) +
  geom_boxplot()


temp |> 
  ggplot() +
  aes(1, y = t_max) +
  geom_violin()

temp |> 
  ggplot() +
  aes(1, y = t_max) +
  geom_violin()

temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max) +
  geom_violin()


temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max) +
  geom_violin() +
  geom_boxplot()

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


temp |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(alpha = 0.4, outlier.size = 1, outliers = F) +
  geom_jitter(alpha = 0.1, size = 0.2) +
  theme_classic()


temp |> 
  filter(!is.na(zona_geografica),
         año == 2023,
         mes == 4) |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_boxplot(alpha = 0.4, outlier.size = 1, outliers = F) +
  geom_jitter(alpha = 0.4, size = 0.4,
              height = 0, width = 0.2) +
  theme_classic()


temp |> 
  filter(!is.na(zona_geografica),
         año == 2023,
         mes == 4) |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.4) +
  geom_jitter(alpha = 0.4, size = 0.4,
              height = 0, width = 0.2) +
  theme_classic()

temp_f <- temp |> 
  filter(!is.na(zona_geografica),
         año == 2023,
         mes == 4)

temp_r <- temp_f |> 
  group_by(zona_geografica) |> 
  summarise(promedio = mean(t_max, na.rm = T),
            median = median(t_max, na.rm = T))

temp_r |> 
  ggplot() +
  aes(zona_geografica, promedio) +
  geom_point()

temp_f |> 
  ggplot() +
  aes(x = zona_geografica, y = t_max, 
      colour = zona_geografica, fill = zona_geografica) +
  geom_violin(alpha = 0.4) +
  geom_jitter(alpha = 0.4, size = 0.4,
              height = 0, width = 0.2) +
  geom_point(data = temp_r,
             aes(zona_geografica, promedio),
             size = 3, color = "black") +
  theme_classic()


temp |> 
  filter(año > 2015) |> 
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            n = n()) |> 
  ggplot() +
  aes(x = año, y = mes, color = n) +
  geom_point()


temp |> 
  filter(año > 2015) |> 
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            n = n()) |> 
  ggplot() +
  aes(x = año, y = mes, fill = t_max) +
  geom_tile()


temp |> 
  filter(año > 1980) |>
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            n = n()) |> 
  ggplot() +
  aes(x = año, y = mes, fill = t_max) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c(option = "inferno")


temp |> 
  filter(año > 1990) |>
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            n = n()) |> 
  ggplot() +
  aes(x = año, y = mes, fill = t_max) +
  geom_tile() +
  coord_equal(expand = F) +
  scale_fill_viridis_c(option = "inferno") +
  scale_y_continuous(breaks = 1:12) +
  scale_x_continuous(breaks = 1990:2024) +
  theme(axis.text.x = element_text(angle = -90,
        hjust = 0, vjust = 0.5))




temp |> 
  filter(año == 2023,
         mes == 3,
         dia == 15) |> 
  ggplot() +
  aes(t_min, t_max) +
  geom_point(size = 3, alpha = 0.5) +
  theme_linedraw() +
  # geom_text(aes(label = t_max), size = 3)
  # geom_text(aes(label = t_max, y = t_max+1), size = 3)
  ggrepel::geom_text_repel(aes(label = t_max), size = 3)


temp |> 
  filter(año == 2023,
         mes == 3,
         dia == 15) |> 
  ggplot() +
  aes(t_min, t_max) +
  geom_point(size = 3, alpha = 0.5) +
  theme_linedraw() +
  ggrepel::geom_text_repel(data = ~filter(.x, 
                                          t_max < 20,
                                          t_min < 5),
                             aes(label = t_max), size = 3)



# —----

library(arrow)

cead <- arrow::read_parquet("datos/cead_delincuencia_chile.parquet")

cead_suma <- cead |> 
  filter(fecha >= "2013-01-01",
         fecha <= "2013-12-31") |> 
  group_by(delito) |> 
  summarize(n = sum(delito_n))

library(lubridate)

cead_suma <- cead |> 
  filter(year(fecha) == 2024) |> 
  group_by(delito) |> 
  summarize(n = sum(delito_n))

cead_suma |> 
  arrange(desc(n))


cead_suma |> 
  ggplot() +
  aes(delito, n) +
  geom_col()

cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  ggplot() +
  aes(n, delito) +
  geom_col()

cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(n, delito) +
  geom_col()


top_delitos <- cead_suma |> 
  arrange(desc(n)) |> 
  slice_max(n, n = 10) |> 
  pull(delito)

top_delitos

cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(n, delito, fill = delito) +
  geom_col() +
  # theme(legend.position = "none")
  guides(fill = guide_none())


cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito) +
  geom_col()


cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito) +
  geom_col(color = "white", linewidth = 1.2) +
  coord_polar(theta = "y") +
  theme_void()


cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito) +
  geom_col(color = "white", linewidth = 0.6) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = n))


cead_suma |> 
  filter(delito %in% top_delitos) |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 10000) |> 
  ggplot() +
  aes(1, n, fill = delito) +
  geom_col(color = "white", linewidth = 0.6) +
  theme_void() +
  geom_text(aes(x = 1.3, label = scales::comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3
            ) +
  coord_polar(theta = "y")


cead_suma |> 
  arrange(desc(n)) |> 
  print(n=20)

cead_suma_otros <- cead_suma |> 
  arrange(desc(n)) |> 
  mutate(delito2 = forcats::fct_lump_n(delito, n = 6, 
                                       w = n, other_level = "Otros delitos")) |> 
  group_by(delito2) |> 
  summarize(n = sum(n))

  
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


cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  ggplot() +
  aes(n, delito) +
  geom_col()

cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 50000) |> 
  ggplot() +
  aes(x = n, y = delito) +
  # geom_col() +
  geom_point(size = 5) +
  geom_point(size = 10, alpha = 0.1) +
  geom_segment(aes(xend = 0, yend = delito))


grafico <- cead_suma |> 
  mutate(delito = forcats::fct_reorder(delito, n)) |> 
  filter(n > 30000) |> 
  ggplot() +
  aes(n, delito) +
  geom_col(width = 0.5)

grafico

library(thematic)
# install.packages("thematic")

thematic_on(bg = "gray20",
            fg = "indianred2")

grafico

thematic_on(bg = "#1F284D",
            fg = "#37C0C7")


library(showtext)
# install.packages("showtext")

font_add_google("Fira Code")
showtext_auto()
showtext_opts(dpi = 250)

grafico +
  theme(text = element_text(family = "Fira Code")) +
  geom_text(aes(label = scales::comma(n, big.mark = "."),
                x = n-2000), size = 3, hjust = 1,
            color = "#1F284D") +
  labs(title = "Delitos totales",
       subtitle = "Año 2024") +
  theme(plot.title.position = "plot")

thematic_off()

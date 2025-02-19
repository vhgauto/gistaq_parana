
# datos ------------------------------------------------------------------

# vector transecta
v <- vect("vector/transecta.gpkg")

# ráster de recortes
archivos_r <- list.files("recorte/", pattern = "tif$", full.names = TRUE)
archivos_r <- archivos_r[!str_detect(archivos_r, "rsi")]
fechas_r <- str_remove(basename(archivos_r), ".tif")

lista_r <- map(
  .x = archivos_r,
  rast
)

# agrupo en ventana de 3x3
lista_r3 <- map(lista_r, ~focal(.x, w = 3, fun = "mean"))
lista_r3 <- set_names(lista_r3, fechas_r)

# factor de las bandas
orden_bandas <- c(
  "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12"
)

# reflectancias a lo largo de la transecta, por banda y fecha
e_tbl <- map(lista_r3, ~terra::extract(.x, v, xy = TRUE)) |>
  list_rbind(names_to = "fecha") |>
  as_tibble() |>
  mutate(fecha = ymd(fecha)) |>
  pivot_longer(
    cols = starts_with("B"),
    values_to = "reflect",
    names_to = "banda"
  ) |>
  reframe(
    reflect = mean(reflect)/10000,
    y = y,
    .by = c(x, banda, fecha)
  ) |>
  mutate(banda = factor(banda, levels = orden_bandas))

g <- e_tbl |>
  mutate(banda = fct_rev(banda)) |>
  ggplot(aes(reflect, banda)) +
  geom_boxplot(
    color = c1, linewidth = .3, outlier.alpha = .4, outlier.color = c2,
    fill = c11
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(.1),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  coord_cartesian(xlim = c(0, .45), expand = FALSE, clip = "off") +
  labs(x = "R<sub>rs</sub>") +
  theme_void() +
  theme(
    plot.margin = margin(t = 3, b = 3),
    axis.title.x = element_markdown(family = "ubuntu", margin = margin(t = 3)),
    axis.text.x = element_text(
      family = "jet", margin = margin(t = 10), size = 9
    ),
    axis.text.y = element_text(
      family = "Ubuntu", face = "bold", margin = margin(r = 3), size = 9
    ),
    panel.grid.major.x = element_line(
      color = c9, linewidth = .1, linetype = "FF"
    )
  )

ggsave(
  plot = g,
  filename = "figuras/boxplot_reflect.png",
  width = 10,
  height = 14,
  units = "cm"
)

#

# datos ------------------------------------------------------------------

# 3 puntos para extraer la reflectancia
puntos <- vect("vector/3puntos_transecta.gpkg")

# ráster de recortes
archivos_r <- list.files("recorte/", pattern = "tif$", full.names = TRUE)
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
e_tbl <- map(lista_r3, ~terra::extract(.x, puntos, xy = TRUE)) |>
  list_rbind(names_to = "fecha") |>
  as_tibble() |>
  mutate(fecha = ymd(fecha)) |>
  mutate(punto = paste0("P", ID)) |>
  pivot_longer(
    cols = starts_with("B"),
    values_to = "reflect",
    names_to = "banda"
  ) |>
  mutate(banda = factor(banda, levels = orden_bandas)) |>
  mutate(reflect = reflect/10000) |>
  select(-ID)

g <- e_tbl |>
  filter(fecha != ymd(20231212)) |>
  ggplot(aes(punto, reflect, group = x, fill = punto)) +
  geom_boxplot(
    linewidth = .2, outlier.size = .3, key_glyph = draw_key_point,
    outlier.color = c9, width = .6
  ) +
  facet_wrap(vars(banda), scales = "free", nrow = 2) +
  scale_y_continuous(
    breaks = seq(.1, .3, .05),
    limits = c(.1, .3),
    # expand = c(0, 0),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_manual(
    values = c(c1, c5, c2),
    name = NULL
  ) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "R<sub>rs</sub>", color = "Sitio") +
  guides(
    fill = guide_legend(
      override.aes = list(color = c7, shape = 22, size = 4, stroke = .3)
    )
  ) +
  theme_minimal(base_size = 7) +
  theme(
    aspect.ratio = 1.5,
    plot.margin = margin(r = 5, l = 5, t = 5, b = 5),
    panel.background = element_rect(fill = c10, color = NA),
    panel.spacing.x = unit(1.6, "line"),
    panel.spacing.y = unit(1, "line"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = c4, linewidth = .06, linetype = "FF"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      margin = margin(r = 0), hjust = 1, family = "jet", color = c7
    ),
    axis.title.y = element_markdown(
      family = "Ubuntu", angle = 0, margin = margin(r = 10), vjust = .5
    ),
    strip.text = element_text(family = "Ubuntu", face = "bold", color = c7),
    legend.position = "inside",
    legend.position.inside = c(.95, .1),
    legend.background = element_blank(),
    legend.justification.inside = c(1, 0),
    legend.text = element_text(margin = margin(l = 0, t = 0)),
    legend.key.height = unit(3, "pt")
  )

ggsave(
  plot = g,
  filename = "figuras/puntos_boxplot.png",
  width = 20,
  height = 7.5,
  units = "cm"
)

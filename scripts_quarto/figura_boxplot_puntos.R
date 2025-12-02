#

# función -----------------------------------------------------------------

# función que verifica la diferencia entre reflectancias por banda espectral
# y sitio
f_puntos <- function(df, p_i, p_f) {
  p <- df |>
    filter(punto %in% c(p_i, p_f)) |>
    pivot_wider(
      id_cols = fecha,
      names_from = punto,
      values_from = reflect
    ) |>
    unnest(everything()) |>
    rename(
      "p_inicial" = 2,
      "p_final" = 3
    )

  wilcox.test(p$p_inicial, p$p_final, paired = FALSE, exact = TRUE) |>
    broom::tidy() |>
    select(p.value) |>
    mutate(
      es_significativo = p.value < .05
    )
}

# datos ------------------------------------------------------------------

# 3 puntos para extraer la reflectancia
puntos <- vect("vector/3puntos_transecta.gpkg")

# ráster de recortes
archivos_r <- list.files(
  "recorte_acolite/",
  pattern = "tif$",
  full.names = TRUE
)
archivos_r <- archivos_r[!str_detect(archivos_r, "rsi")]
fechas_r <- str_remove(basename(archivos_r), ".tif")

lista_r <- map(
  .x = archivos_r,
  rast
)

# agrupo en ventana de 3x3
lista_r3 <- map(lista_r, ~ focal(.x, w = 3, fun = "mean"))
lista_r3 <- set_names(lista_r3, fechas_r)

# factor de las bandas
orden_bandas <- c(
  "B01",
  "B02",
  "B03",
  "B04",
  "B05",
  "B06",
  "B07",
  "B08",
  "B8A",
  "B11",
  "B12"
)

# reflectancias a lo largo de la transecta, por banda y fecha
e_tbl <- map(lista_r3, ~ terra::extract(.x, puntos, xy = TRUE)) |>
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
  # mutate(reflect = reflect/10000) |>
  select(-ID)

# obtengo la significancia en los diferentes puntos, por banda
p_tbl <- e_tbl |>
  nest(.by = banda) |>
  mutate(
    P1P3 = map(data, ~ f_puntos(.x, "P1", "P3"))
  ) |>
  select(-data) |>
  pivot_longer(
    cols = -banda
  ) |>
  unnest(value) |>
  mutate(
    x = 1,
    xend = 3,
    y = .3,
    yend = .3
  ) |>
  mutate(
    label = if_else(es_significativo, simbolo_sig, NA)
  )

g <- e_tbl |>
  ggplot(aes(punto, reflect, group = x, fill = punto)) +
  geom_boxplot(
    linewidth = .2,
    outlier.size = .3,
    key_glyph = draw_key_point,
    outlier.color = c9,
    width = .8
  ) +
  geom_richtext(
    data = p_tbl,
    aes(Inf, Inf, label = label),
    inherit.aes = FALSE,
    fill = c11,
    size = 4,
    label.color = NA,
    hjust = 1,
    vjust = 1
  ) +
  facet_wrap(vars(banda), scales = "free", nrow = 4) +
  scale_y_continuous(
    breaks = seq(0, .3, .05),
    # limits = c(.1, .3),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_manual(
    values = c(c1, c5, c2),
    breaks = c("P1", "P2", "P3"),
    labels = c("Costa\nchaqueña", "Punto\nintermedio", "Costa\ncorrentina"),
    name = NULL
  ) +
  coord_cartesian(
    clip = "off",
    xlim = c(.5, 3.5),
    ylim = c(0, .27),
    expand = FALSE
  ) +
  labs(x = NULL, y = "R<sub>rs</sub>", color = "Sitio") +
  guides(
    fill = guide_legend(
      override.aes = list(color = c7, shape = 22, size = 5, stroke = .3)
    )
  ) +
  theme_minimal(base_size = 9) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(r = 1, l = 1, t = 0, b = 2),
    plot.background = element_blank(),
    panel.background = element_rect(fill = c11, color = NA),
    panel.spacing.x = unit(1.6, "line"),
    panel.spacing.y = unit(1, "line"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = c4,
      linewidth = .06,
      linetype = "FF"
    ),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      margin = margin(r = 0),
      hjust = 1,
      family = "jet",
      color = c7
    ),
    axis.title.y = element_markdown(
      family = "Ubuntu",
      angle = 0,
      margin = margin(r = 0),
      vjust = .5
    ),
    strip.text = element_text(family = "Ubuntu", face = "bold", color = c7),
    legend.position = "top",
    legend.key.spacing.x = unit(24, "pt"),
    legend.background = element_blank(),
    legend.justification.inside = c(1, 0),
    legend.text = element_text(
      margin = margin(l = 0, t = 0),
      hjust = 0,
      vjust = .5
    ),
    legend.text.position = "right",
    legend.box.spacing = unit(0, "pt")
  )

ggsave(
  plot = g,
  filename = "figuras/puntos_boxplot.png",
  width = 20,
  height = 25,
  units = "cm"
)

# typst ------------------------------------------------------------------

g_typst <- e_tbl |>
  ggplot(aes(punto, reflect, group = x, fill = punto)) +
  geom_boxplot(
    linewidth = .2,
    outlier.size = .3,
    key_glyph = draw_key_point,
    outlier.color = c9,
    width = .8
  ) +
  geom_richtext(
    data = p_tbl,
    aes(Inf, Inf, label = label),
    inherit.aes = FALSE,
    fill = c11,
    size = 4,
    label.color = NA,
    hjust = 1,
    vjust = 1
  ) +
  facet_wrap(vars(banda), scales = "free", nrow = 3) +
  scale_y_continuous(
    breaks = seq(0, .3, .05),
    # limits = c(.1, .3),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_manual(
    values = c(c1, c5, c2),
    breaks = c("P1", "P2", "P3"),
    labels = c("Costa\nchaqueña", "Punto\nintermedio", "Costa\ncorrentina"),
    name = NULL
  ) +
  coord_cartesian(
    clip = "off",
    xlim = c(.5, 3.5),
    ylim = c(0, .27),
    expand = FALSE
  ) +
  labs(x = NULL, y = "R<sub>rs</sub>", color = "Sitio") +
  guides(
    fill = guide_legend(
      override.aes = list(color = c7, shape = 22, size = 5, stroke = .3)
    )
  ) +
  theme_minimal(base_size = 15) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(r = 1, l = 1, t = 0, b = 2),
    panel.background = element_rect(fill = c11, color = NA),
    panel.spacing.x = unit(1.6, "line"),
    panel.spacing.y = unit(1, "line"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = c4,
      linewidth = .06,
      linetype = "FF"
    ),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      margin = margin(r = 0),
      hjust = 1,
      family = "jet",
      color = c7
    ),
    axis.title.y = element_markdown(
      family = "Ubuntu",
      angle = 0,
      margin = margin(r = 0),
      vjust = .5
    ),
    strip.text = element_text(family = "Ubuntu", face = "bold", color = c7),
    legend.position = "top",
    legend.key.spacing.x = unit(24, "pt"),
    legend.background = element_blank(),
    legend.justification.inside = c(1, 0),
    legend.text = element_text(
      margin = margin(l = 0, t = 0),
      hjust = 0,
      vjust = .5
    ),
    legend.text.position = "right",
    legend.box.spacing = unit(0, "pt")
  )

ggsave(
  plot = g_typst,
  filename = paste0(getwd(), "/figuras/puntos_boxplot.png"),
  width = 30,
  height = 22,
  units = "cm"
)

# datos -------------------------------------------------------------------

banda_orden <- c(
  "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12"
)

d <- read_csv("datos/base_de_datos_gis_acolite.csv", show_col_types = FALSE) |>
  drop_na() |>
  # filter(pixel == "3x3") |>
  group_by(fecha, banda) |>
  arrange(longitud) |>
  mutate(p = row_number()) |>
  ungroup() |>
  mutate(reflect = round(reflect, 3)) |>
  mutate(banda = fct(banda, banda_orden)) |>
  mutate(label = glue("P{p}: {reflect}"))

pal <- colorRampPalette(colors = c(c1, c4, c2))(max(d$p))
names(pal) <- 1:max(d$p)

col_tbl <- d |>
  distinct(fecha, p) |>
  nest(.by = fecha) |>
  mutate(
    col = map(.x = data, ~ colorRampPalette(colors = c(c1, c9, c2))(nrow(.x)))
  ) |>
  unnest(cols = c(data, col))

e <- inner_join(
  d,
  col_tbl,
  by = join_by(fecha, p)
)

# figura ------------------------------------------------------------------

f_firma_espectral <- function(x) {
  d <- e |>
    filter(fecha == x)

  fig_evo_gis_label <- c("Chaco", rep("", length(unique(d$p)) - 2), "Corrientes")

  colores_gis <- colorRampPalette(colors = c(c1, c4, c2))(length(unique(d$p)))

  midpoint_gis <- median(unique(d$p))

  g <- ggplot(d, aes(banda, reflect, group = p, color = p, fill = p)) +
    geom_line_interactive(
      aes(data_id = interaction(fecha, p)),
      hover_nearest = TRUE,
      linewidth = 1, alpha = .8, show.legend = FALSE
    ) +
    geom_point_interactive(
      aes(data_id = interaction(fecha, p)),
      size = .7, shape = 21
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(),
      labels = scales::label_number(
        decimal.mark = ",", big.mark = ".", accuracy = .01
      )
    ) +
    scale_fill_gradient2(
      low = c1, mid = c4, high = c2, midpoint = midpoint_gis,
      breaks = unique(d$p),
      labels = fig_evo_gis_label
    ) +
    scale_color_gradient2(
      low = c1, mid = c4, high = c2, midpoint = midpoint_gis,
      breaks = unique(d$p),
      labels = fig_evo_gis_label
    ) +
    coord_cartesian(clip = "off", xlim = c(.8, 11.2)) +
    labs(y = "R<sub>rs</sub>", x = NULL, color = NULL, fill = NULL) +
    guides(
      fill = guide_colorbar(
        nrow = 1,
        override.aes = list(size = 16, shape = 15, color = colores_gis),
        reverse = FALSE
      ),
      color = guide_none()
    ) +
    theme_void(base_size = 11) +
    theme(
      aspect.ratio = 1,
      plot.margin = margin(6, 6, 6, 6),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = c4, linewidth = .06, linetype = 3
      ),
      panel.spacing = unit(1.1, "line"),
      axis.title.x = element_text(family = "ubuntu", margin = margin(t = 3)),
      axis.title.y = element_markdown(
        family = "ubuntu", margin = margin(r = 10), size = 12
      ),
      axis.text = element_text(family = "jet", color = c7),
      axis.text.y = element_text(hjust = 1, margin = margin(r = 2)),
      axis.text.x = element_text(margin = margin(t = 2)),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = c11, color = NA),
      strip.background = element_blank(),
      strip.text = element_markdown(
        family = "jet", size = 7, margin = margin(b = 3)
      ),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(1, "mm"),
      legend.position = "top",
      legend.box = "horizontal",
      legend.text = element_text(
        vjust = .5, hjust = .5, family = "ubuntu", margin = margin(t = 3),
        size = 11
      ),
      legend.text.position = "bottom",
      legend.key.width = unit(10, "mm"),
      legend.key.height = unit(2, "mm"),
      legend.ticks = element_blank()
    )

  figura_evolucion_gis <- girafe(
    ggobj = g,
    bg = "transparent",
    options = list(
      opts_hover(
        css = girafe_css(css = "")
      ),
      opts_tooltip(
        opacity = 1,
        css = glue(
          "color:{c1};padding:5px;font-family:JetBrains Mono;",
          "border-style:solid;border-color:{c4};background:{c3}"
        ),
        use_cursor_pos = TRUE,
        offx = 5,
        offy = 5
      ),
      opts_sizing(width = 1, rescale = TRUE),
      opts_hover_inv(css = "opacity:.2"),
      opts_toolbar(saveaspng = FALSE)
    )
  )

  return(figura_evolucion_gis)
}

fechas_gis_v <- rev(sort(unique(e$fecha)))

lista_firma_espectral <- map(fechas_gis_v, f_firma_espectral)

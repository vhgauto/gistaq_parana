
# datos -------------------------------------------------------------------

# param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_v <- c("turb", "secchi", "sol_sus", "cond", "ph")
param_unid_v <- c(
  "<i>turb</i> (NTU)", "<i>secchi</i> (cm)", "<i>susp</i> (ppm)",
  "<i>cond</i> (μS/cm)", "pH"
)
names(param_unid_v) <- param_v

d <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE) |>
  mutate(
    param = param_unid_v[param]
  ) |>
  drop_na() |>
  group_by(fecha, param) |>
  arrange(longitud) |>
  mutate(p = row_number()) |>
  ungroup() |>
  mutate(
    unidad = str_extract(param, "\\(([^)]+)\\)"),
    unidad = str_remove(unidad, "\\("),
    unidad = str_remove(unidad, "\\)")
  ) |>
  mutate(
    unidad = if_else(is.na(unidad), "", unidad)) |>
  mutate(v = format(valor, nsmall = 1, digits = 1, decimal.mark = ",")) |>
  mutate(label = glue("{fecha}<br>{v} {unidad}")) |>
  mutate(p = glue("P{p}")) |>
  mutate(p = fct_reorder(p, longitud)) |>
  mutate(
    estado = if_else(
      fecha == max(fecha),
      "Actual",
      "Previos"
    )
  )

estado_color <- c(
  Actual = c2,
  Previos = c1
)

# figura ------------------------------------------------------------------

# función que genera las figuras interactivas por parámetros
f_figura_evolucion_lab <- function(parametro) {
  g <- d |>
    filter(param == parametro) |>
    ggplot(aes(longitud, valor, group = fecha, color = estado, fill = estado)) +
    geom_line_interactive(
      aes(data_id = interaction(fecha, param)), hover_nearest = TRUE,
      linewidth = 2, alpha = .8) +
    geom_point_interactive(
      aes(
        data_id = interaction(fecha, param), tooltip = label,
        hover_nearest = TRUE), size = 2.5, shape = 21, color = c3,
      stroke = .2, show.legend = FALSE) +
    facet_wrap(vars(param), ncol = 3, scales = "free") +
    scale_y_continuous(
      breaks = scales::breaks_pretty(),
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    ) +
    scale_x_continuous(
      breaks = range(d$longitud),
      labels = c("Orilla\nchaqueña", "Orilla\ncorrentina")
    ) +
    scale_color_manual(values = estado_color) +
    scale_fill_manual(values = estado_color) +
    coord_cartesian(clip = "off") +
    labs(y = parametro, x = NULL, fill = NULL, color = NULL) +
    theme_void(base_size = 10) +
    theme(
      aspect.ratio = 1,
      plot.margin = margin(6, 6, 6, 6),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        color = c4, linewidth = .06, linetype = 3),
      panel.spacing = unit(1.1, "line"),
      axis.title.y = element_markdown(
        family = "Ubuntu", angle = 90, margin = margin(r = 10), size = 12
      ),
      axis.text = element_text(family = "jet", color = c7),
      axis.text.y = element_markdown(hjust = 1, margin = margin(r = 2)),
      axis.text.x = element_text(margin = margin(t = 2), family = "Ubuntu"),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = c11, color = NA),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.text = element_text(
        family = "ubuntu", size = 12, margin = margin(r = 10, l = 3)
      ),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.spacing.x = unit(.3, "cm")
    )

  gg_int <- girafe(
    ggobj = g,
    bg = "transparent",
    options = list(
      opts_hover(
        css = girafe_css(
          css = "",
        )
      ),
      opts_tooltip(
        opacity = 1,
        css = glue(
          "color:{c7};padding:5px;font-family:JetBrains Mono;",
          "border-style:solid;border-color:{c4};border-width:2px;",
          "background:{c3}"),
        use_cursor_pos = TRUE,
        offx = 5,
        offy = 5),
      opts_sizing(width = 1, rescale = TRUE),
      opts_hover_inv(css = "opacity:.2"),
      opts_toolbar(saveaspng = FALSE)
    )
  )

  return(gg_int)
}

# lista con las figuras
lista_figura_evolucion_lab <- map(param_unid_v, f_figura_evolucion_lab)

# datos -------------------------------------------------------------------

cantidad_fechas <- length(unique(d$fecha))
cantidad_muestras <- d |>
  filter(str_detect(param, "turb")) |>
  nrow()

# typst ------------------------------------------------------------------

d_typst <- filter(d, str_detect(param, "turb"))

g_typst <- d_typst |>
  ggplot(aes(longitud, valor, group = fecha, color = estado, fill = estado)) +
  geom_line(linewidth = 2, alpha = .8) +
  geom_point(
    size = 2.5, shape = 21, color = c3, stroke = .2, show.legend = FALSE
  ) +
  facet_wrap(vars(param), ncol = 3, scales = "free") +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(add = c(0, 10))
  ) +
  scale_x_continuous(
    breaks = range(d$longitud),
    labels = c("Orilla\nchaqueña", "Orilla\ncorrentina")
  ) +
  scale_color_manual(values = estado_color) +
  scale_fill_manual(values = estado_color) +
  coord_cartesian(clip = "off", ylim = c(0, NA)) +
  labs(y = param_unid_v[1], x = NULL, fill = NULL, color = NULL) +
  theme_void(base_size = 14) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(6, 15, 6, 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = c4, linewidth = .06, linetype = 3),
    panel.spacing = unit(1.1, "line"),
    axis.title.y = element_markdown(
      family = "Ubuntu", angle = 90, margin = margin(r = 10), size = 15
    ),
    axis.text = element_text(family = "jet", color = c7),
    axis.text.y = element_markdown(hjust = 1, margin = margin(r = 2)),
    axis.text.x = element_text(margin = margin(t = 2), family = "Ubuntu"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = c11, color = NA),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "top",
    legend.text = element_text(
      family = "ubuntu", size = 12, margin = margin(r = 10, l = 3)
    ),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.spacing.x = unit(.3, "cm")
  )

ggsave(
  plot = g_typst,
  filename = paste0(getwd(), "/figuras_typst/datos_fisicoquimicos.png"),
  width = 15,
  height = 15,
  units = "cm"
)

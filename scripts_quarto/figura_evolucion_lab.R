
# datos -------------------------------------------------------------------

param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "Cond (μS/cm)", "Sól. susp. (ppm)", "Turb (NTU)", "SDD (cm)")
names(param_unid_v) <- param_v

d <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE) |> 
  mutate(
    param = param_unid_v[param]
  ) |> 
  drop_na() |> 
  group_by(fecha, param) |> 
  mutate(p = row_number()) |> 
  ungroup() |> 
  mutate(
    unidad = str_extract(param, "\\(([^)]+)\\)"),
    unidad = str_remove(unidad, "\\("),
    unidad = str_remove(unidad, "\\)")
  ) |> 
  mutate(
    unidad = if_else(is.na(unidad), "", unidad)) |> 
  mutate(valor = round(valor, 2)) |> 
  mutate(label = glue("{fecha}<br>{valor} {unidad}")) |> 
  mutate(p = glue("P{p}"))

# figura ------------------------------------------------------------------

g <- ggplot(d, aes(p, valor, group = fecha)) +
  geom_line_interactive(
    aes(data_id = interaction(fecha, param)), hover_nearest = TRUE,
    linewidth = .6, color = c1, alpha = .8) +
  geom_point_interactive(
    aes(
      data_id = interaction(fecha, param), tooltip = label, 
      hover_nearest = TRUE), size = 1, shape = 21, color = c3, 
    stroke = .4, fill = c1) +
  facet_wrap(vars(param), ncol = 3, scales = "free") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  coord_cartesian(clip = "off") +
  labs(y = NULL, x = NULL) +
  theme_void(base_size = 6) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(6, 6, 6, 6),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = c4, linewidth = .06, linetype = 3),
    panel.spacing = unit(1.1, "line"),
    axis.title.x = element_text(family = "ubuntu", margin = margin(t = 3)),
    axis.text = element_text(family = "jet", color = c4),
    axis.text.y = element_text(hjust = 1, margin = margin(r = 2)),
    axis.text.x = element_text(margin = margin(t = 2)),
    panel.background = element_rect(fill = c3),
    strip.background = element_blank(),
    strip.text = element_markdown(
      family = "ubuntu", size = 7, margin = margin(b = 3))
  )

figura_evolucion_lab <- girafe(
  ggobj = g,
  bg = c6,
  options = list(
    opts_hover(
      css = girafe_css(
        css = "",
        point = glue("fill:{c2};stroke:{c2}"),
        line = glue("stroke:{c2}")
      )
    ),
    opts_tooltip(
      opacity = 1,
      css = glue(
        "color:{c1};padding:5px;font-family:JetBrains Mono;",
        "border-style:solid;border-color:{c4};background:{c3}"),
      use_cursor_pos = TRUE,
      offx = 5,
      offy = 5),
    opts_sizing(width = 1, rescale = TRUE),
    opts_hover_inv(css = "opacity:.4"),
    opts_toolbar(saveaspng = FALSE)
  )
)

# datos -------------------------------------------------------------------

cantidad_fechas <- length(unique(d$fecha))
cantidad_muetras <- d |> 
  filter(str_detect(param, "Turb")) |> 
  nrow()

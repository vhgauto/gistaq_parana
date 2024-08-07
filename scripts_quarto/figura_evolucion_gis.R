
# datos -------------------------------------------------------------------

banda_orden <- c(
  "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")

d <- read_csv("datos/base_de_datos_gis.csv", show_col_types = FALSE) |> 
  drop_na() |> 
  filter(pixel == "3x3") |> 
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

g <- ggplot(e, aes(banda, reflect, group = p, color = col)) +
  geom_line_interactive(
    aes(data_id = interaction(fecha, p)), hover_nearest = TRUE,
    linewidth = .6, alpha = .8, show.legend = FALSE) +
  geom_point(size = 1, shape = 20, color = c3) +
  geom_point_interactive(
    aes(data_id = interaction(fecha, p), tooltip = label), size = .7, 
    shape = 20, hover_nearest = TRUE) +
  facet_wrap(vars(fecha), ncol = 3, scales = "free_x") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Orilla\ncorrentina", rep("", 13), "Orilla\nchaqueña")
  ) +
  coord_cartesian(clip = "off") +
  labs(y = NULL, x = NULL, color = NULL) +
  guides(
    color = guide_legend(
      nrow = 1, override.aes = list(size = 6, shape = 15), reverse = TRUE)
  ) +
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
      family = "jet", size = 7, margin = margin(b = 3)),
    legend.background = element_rect(fill = c6, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(1, "mm"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(
      vjust = .5, hjust = .5, family = "ubuntu", margin = margin(l = 0),
      size = 5),
    legend.text.position = "bottom",
    legend.key.width = unit(1, "mm"),
    legend.key.spacing.x = unit(-4.8, "mm")
  )

figura_evolucion_gis <- girafe(
  ggobj = g,
  bg = c6,
  options = list(
    opts_hover(
      css = girafe_css(css = "")
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


# datos -------------------------------------------------------------------

# leo los datos de laboratorio
d <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE)

# nombres de los parámetros y sus etiquetas
param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "<i>cond</i>", "<i>susp</i>",
  "<i>turb</i>", "<i>secchi</i>")
names(param_unid_v) <- param_v

# correlación -------------------------------------------------------------

# correlación R
e_r <- d |>
  drop_na() |>
  pivot_wider(
    names_from = param,
    values_from = valor
  ) |>
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |>
  correlate(
    method = "pearson", use = "pairwise.complete.obs", quiet = TRUE) |>
  shave() |>
  pivot_longer(
    cols = -term,
    names_to = "param",
    values_to = "r"
  ) |>
  drop_na()

# pvalor
e_pvalor <- d |>
  drop_na() |>
  pivot_wider(
    names_from = param,
    values_from = valor
  ) |>
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |>
  colpair_map(f_pvalor) |>
  shave() |>
  pivot_longer(
    cols = -term,
    names_to = "param",
    values_to = "pvalor"
  ) |>
  drop_na()

# combino datos y aplico formatos si la correlación es mayor a |R| > .5
e <- inner_join(
  e_r,
  e_pvalor,
  by = join_by(term, param)
) |>
  mutate(term = factor(term, levels = param_v)) |>
  mutate(term = fct_rev(term)) |>
  mutate(param = factor(param, levels = param_v)) |>
  mutate(es_significativo = pvalor < .05) |>
  mutate(label = if_else(es_significativo, "&#9733;", NA)) |>
  mutate(id = row_number()) |>
  mutate(r_label = paste0("r: ", f_formato(r)))

g <- e |>
  ggplot(
    aes(term, param, fill = r)
  ) +
  geom_tile_interactive(color = c10, linewidth = 1, aes(tooltip = r_label, data_id = id)) +
  geom_richtext(aes(label = label), label.color = NA, fill = NA, color = c7) +
  scale_x_discrete(
    labels = param_unid_v
  ) +
  scale_y_discrete(
    labels = param_unid_v
  ) +
  scale_fill_gradient2(
    low = c1,
    mid = c11,
    high = c2,
    limits = c(-1, 1),
    labels = f_formato(seq(-1, 1, .5), digits = 1, nsmall = 1)
  ) +
  labs(x = NULL, y = NULL, fill = "r") +
  coord_equal() +
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(b = 15),
    axis.text = element_markdown(lineheight = 1.2),
    legend.text = element_text(family = "jet", hjust = 1),
    legend.title = element_text(
      family = "jet", margin = margin(b = 10), hjust = .5
    ),
    legend.position = "right",
    legend.key.height = unit(25, "pt"),
    legend.justification.right = c(0, 1)
  )

heatmap_lab <- girafe(
  ggobj = g,
  bg = "transparent",
  options = list(
    opts_hover(
      css = girafe_css(
        css = ""
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
    opts_hover_inv(css = "opacity:.9"),
    opts_toolbar(saveaspng = FALSE)
  )
)

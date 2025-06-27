
# parámetros y unidades
param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "<i>cond</i> (μS/cm)", "<i>susp</i> (ppm)", "<i>turb</i> (NTU)",
  "<i>secchi</i> (cm)")
names(param_unid_v) <- param_v

# leo base de datos
d <- read_csv("datos/base_de_datos_lab.csv") |>
  filter(param != "hazemeter")

# distingo dos grupos: lado Chaco y lado Corrientes
# según la mediana de la longitud geográfica
m <- median(d$longitud)

# función que calcula si los grupos son diferentes significativamente
# usando el test de Wilcox
f_lado_signif <- function(x) {
  d_chaco <- d |>
    mutate(
      lado = if_else(
        longitud >= m,
        "Corrientes",
        "Chaco"
      )
    ) |>
    filter(param == x & lado == "Chaco") |>
    pull(valor)

  d_corrientes <- d |>
    mutate(
      lado = if_else(
        longitud >= m,
        "Corrientes",
        "Chaco"
      )
    ) |>
    filter(param == x & lado == "Corrientes") |>
    pull(valor)


  signif <- wilcox.test(d_corrientes, d_chaco, paired = FALSE, exact = TRUE) |>
    broom::tidy() |>
    select(p.value) |>
    mutate(
      es_significativo = p.value < .05
    ) |>
    mutate(
      param = x
    )

  return(signif)
}

# función que genera los boxplot y almacena
f_figura_lado <- function(x) {

  e <- d |>
  mutate(
    lado = if_else(
      longitud >= m,
      "Lado\nCorrientes",
      "Lado\nChaco"
    )
  ) |>
  filter(param == x)

  if (f_lado_signif(x)$es_significativo) {
    signif_label <- simbolo_sig
  } else {
    signif_label <- ""
  }

  g <- ggplot(e, aes(lado, valor)) +
    geom_boxplot(
      color = c1, outlier.color = c2, fill = c3, outlier.alpha = .8
    ) +
    annotate(
      geom = "richtext", x = Inf, y = Inf, hjust = 1, vjust = 1, fill = c11,
      label = signif_label, label.color = NA, size = 5
    ) +
    facet_wrap(vars(param), nrow = 1, scales = "free") +
    labs(x = NULL, y = param_unid_v[x]) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    theme_void() +
    theme(
      aspect.ratio = 1,
      plot.margin = margin(r = 5, l = 5, t = 0, b = 0),
      panel.background = element_rect(fill = c11, color = NA),
      panel.grid.major.y = element_line(
        color = c4, linewidth = .1, linetype = "FF"),
      axis.text.x = element_text(
        margin = margin(t = 6), size = 10, family = "ubuntu"
      ),
      axis.text.y = element_text(
        margin = margin(r = 5), hjust = 1, size = 8, family = "jet"
      ),
      axis.title.y = element_markdown(
          family = "Ubuntu", angle = 90, margin = margin(r = 10), size = 12
      ),
      strip.text = element_blank()
    )

  ggsave(
      plot = g,
      filename = glue("figuras/lado_{x}.png"),
      width = 13,
      height = 13,
      units = "cm"
    )
}

# genero boxplot por cada parámetro
map(param_v, f_figura_lado)

# typst ------------------------------------------------------------------

e_typst <- d |>
  mutate(
    lado = if_else(
      longitud >= m,
      "Lado\nCorrientes",
      "Lado\nChaco"
    )
  ) |>
  filter(param == "turb")

if (f_lado_signif("turb")$es_significativo) {
  signif_label_typst <- simbolo_sig
} else {
  signif_label_typst <- ""
}

g_typst <- ggplot(e_typst, aes(lado, valor)) +
  geom_boxplot(
    color = c1, outlier.color = c2, fill = c3, outlier.alpha = .8
  ) +
  annotate(
    geom = "richtext", x = Inf, y = Inf, hjust = 1, vjust = 1, fill = c11,
    label = signif_label_typst, label.color = NA, size = 5
  ) +
  facet_wrap(vars(param), nrow = 1, scales = "free") +
  labs(x = NULL, y = param_unid_v[4]) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme_void(base_size = 15) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(r = 5, l = 5, t = 0, b = 0),
    panel.background = element_rect(fill = c11, color = NA),
    panel.grid.major.y = element_line(
      color = c4, linewidth = .1, linetype = "FF"),
    axis.text.x = element_text(
      margin = margin(t = 6), family = "ubuntu"
    ),
    axis.text.y = element_text(
      margin = margin(r = 5), hjust = 1, family = "jet"
    ),
    axis.title.y = element_markdown(
        family = "Ubuntu", angle = 90, margin = margin(r = 10)
    ),
    strip.text = element_blank()
  )

ggsave(
  plot = g_typst,
  filename = paste0(getwd(), "/figuras_typst/lado_turb.png"),
  width = 15,
  height = 15,
  units = "cm"
)

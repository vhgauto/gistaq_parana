#
# paquetes ---------------------------------------------------------------

param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "<i>cond</i> (μS/cm)", "<i>susp</i> (ppm)", "<i>turb</i> (NTU)",
  "<i>secchi</i> (cm)")
names(param_unid_v) <- param_v

d <- read_csv("datos/base_de_datos_lab.csv") |>
  filter(param != "hazemeter") #|>
  # mutate(
  #   param_label = param_unid_v[param]
  # )

m <- median(d$longitud)


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

  g <- ggplot(e, aes(lado, valor)) +
    geom_boxplot(color = c1, outlier.color = c2, fill = c3, outlier.alpha = .8) +
    facet_wrap(vars(param), nrow = 1, scales = "free") +
    labs(x = NULL, y = param_unid_v[x]) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    theme_void() +
    theme(
      aspect.ratio = 1,
      plot.margin = margin(r = 5, l = 5, t = 0, b = 0),
      panel.background = element_rect(fill = c6, color = NA),
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

map(param_v, f_figura_lado)

# browseURL(paste0(getwd(), "/figuras/x.png"))

# f_guardar <- function(prop1, prop2) {
#   g <- f_gg(prop1, prop2)
#   ggsave(
#     plot = g,
#     filename = glue("figuras/{prop1}_vs_{prop2}.png"),
#     width = 13,
#     height = 13,
#     units = "cm"
#   )
# }

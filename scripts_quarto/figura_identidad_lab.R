
# datos -------------------------------------------------------------------

d <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE)

param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "<i>cond</i> (μS/cm)", "<i>susp</i> (ppm)", "<i>turb</i> (NTU)",
  "<i>secchi</i> (cm)")
names(param_unid_v) <- param_v

# figuras -----------------------------------------------------------------
linea_tipo <- 2
linea_ancho <- 1
texto_tamaño <- 4
punto_transparencia <- .6
punto_tamaño <- 3

# configuración de las figuras asociadas a una RECTA
g_recta <- function(g, etq) {
  g +
    geom_smooth(
      method = lm, formula = y ~ x, se = FALSE, color = c1,
      linetype = linea_tipo, linewidth = linea_ancho) +
    annotate(
      geom = "richtext", x = I(1), y = I(1), hjust = 1, vjust = 1,
      size = texto_tamaño, label = etq, label.color = NA, fill = c11,
      family = "jet") +
    scale_x_log10(breaks = scales::breaks_pretty()) +
    scale_y_log10(expand = c(0, 0))
}

# configuración de las figuras asociadas a una EXPONENCIAL
g_log <- function(g, etq) {
  g +
    geom_smooth(
      method = lm, formula = y ~ x, se = FALSE, color = c1,
      linetype = linea_tipo, linewidth = linea_ancho) +
    annotate(
      geom = "richtext", x = I(0), y = I(1), hjust = 0, vjust = 1,
      size = texto_tamaño, label = etq, label.color = NA, fill = c11,
      family = "jet") +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    scale_x_continuous(breaks = scales::breaks_pretty())
}

# figuras
f_gg <- function(eje_x, eje_y) {
  e <- d |>
    pivot_wider(
      names_from = param,
      values_from = valor,
      values_fn = list
    ) |>
    unnest(everything()) |> 
    select(any_of(c(eje_x, eje_y))) |>
    rename(x = 1, y = 2) |>
    drop_na()

  # figura básica
  g <- ggplot(e, aes(x, y)) +
    geom_point(size = punto_tamaño, alpha = punto_transparencia, color = c2) +
    coord_cartesian(clip = "off") +
    labs(x = param_unid_v[eje_x], y = param_unid_v[eje_y]) +
    theme_void() +
    theme(
      aspect.ratio = 1,
      plot.margin = margin(t = 5, r = 7, b = 0, l = 5),
      panel.background = element_rect(fill = c11, color = NA),
      panel.grid.major = element_line(
        color = c4, linewidth = .1, linetype = "FF"),
      axis.title = element_markdown(family = "Ubuntu", size = 12),
      axis.title.y = element_markdown(angle = 90, margin = margin(r = 3, l = 5)),
      axis.title.x = element_markdown(margin = margin(t = 6, b = 10)),
      axis.text = element_text(family = "jet", size = 8),
      axis.text.x = element_text(margin = margin(t = 6)),
      axis.text.y = element_text(margin = margin(r = 5), hjust = 1)
    )

  # figuras que contienen profundidad de disco de Secchi
  if (eje_x == "secchi" | eje_y == "secchi") {

    mod <- lm(log(y) ~ log(x), data = e)

    r2 <- broom::glance(mod)$r.squared |>
      f_formato()

    n <- nrow(e)

    etq_recta <- glue("R<sup>2</sup> = {r2}<br>n = {n}")

    g <- g_recta(g, etq_recta)


  } else { # figuras NO asociadas a profundidad de disco de Secchi

    mod <- lm(y ~ x, data = e)

    r2 <- broom::glance(mod)$r.squared |>
      f_formato()

    n <- nrow(e)

    etq_log <- glue("R<sup>2</sup> = {r2}<br>n = {n}")

    g <- g_log(g, etq_log)

  }

  return(g)
}

# función que genera y guarda las figuras como .png
f_guardar <- function(prop1, prop2) {
  g <- f_gg(prop1, prop2)
  ggsave(
    plot = g,
    filename = glue("figuras/identidad_{prop1}_vs_{prop2}.png"),
    width = 1500,
    height = 1478,
    units = "px"
  )
}

# genero y guardo cada combinación de parámetros
f_guardar("cond", "turb")
f_guardar("cond", "sol_sus")
f_guardar("cond", "secchi")
f_guardar("turb", "sol_sus")
f_guardar("turb", "secchi")
f_guardar("sol_sus", "secchi")

# typst ------------------------------------------------------------------

d_typst <- d |> 
  filter(param %in% c("sol_sus", "cond")) |> 
  pivot_wider(
    names_from = param,
    values_from = valor,
    values_fn = list
  ) |> 
  unnest(c(cond, sol_sus))

mod_typst <- lm(sol_sus ~ cond, data = d_typst)

r2_typst <- broom::glance(mod_typst)$r.squared |>
  f_formato()

n_typst <- nrow(d_typst)

etq_typst <- glue("R<sup>2</sup> = {r2_typst}<br>n = {n_typst}")

g_typst <- d_typst |> 
  ggplot(aes(cond, sol_sus)) +
  geom_point(size = punto_tamaño, alpha = punto_transparencia, color = c2) +
  coord_cartesian(clip = "off") +
  geom_smooth(
    method = lm, formula = y ~ x, se = FALSE, color = c1,
    linetype = linea_tipo, linewidth = linea_ancho) +
  annotate(
    geom = "richtext", x = I(0), y = I(1), hjust = 0, vjust = 1,
    size = 5, label = etq_typst, label.color = NA, fill = c11,
    family = "jet"
  ) +
  labs(x = param_unid_v[2], y = param_unid_v[3]) +
  theme_void(base_size = 15) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(t = 5, r = 7, b = 0, l = 5),
    panel.background = element_rect(fill = c11, color = NA),
    panel.grid.major = element_line(
      color = c4, linewidth = .1, linetype = "FF"
    ),
    axis.title = element_markdown(family = "Ubuntu"),
    axis.title.y = element_markdown(angle = 90, margin = margin(r = 3, l = 5)),
    axis.title.x = element_markdown(margin = margin(t = 6, b = 10)),
    axis.text = element_text(family = "jet"),
    axis.text.x = element_text(margin = margin(t = 6)),
    axis.text.y = element_text(margin = margin(r = 5), hjust = 1)
  )

ggsave(
  plot = g_typst,
  filename = paste0(getwd(), "/figuras_typst/identidad_cons_vs_sol_sus.png"),
  width = 15,
  height = 15,
  units = "cm"
)

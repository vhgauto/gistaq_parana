
# fuentes -----------------------------------------------------------------

f_formato <- function(x, digits = 3, nsmall = 3) {
  format(
    x, digits = digits, nsmall = 3, trim = TRUE, decimal.mark = ",",
    big.mark = ".")
}

c1 <- "#377EB8"
c2 <- "#E41A1C"
c3 <- "#F2F2F2" # "grey95"
c4 <- "#CCCCCC" # "grey80"
c5 <- "white"
c6 <- "#FAFAFA" # "grey98"

font_add(
  family = "jet",
  regular = "extras/JetBrainsMonoNLNerdFontMono-Regular.ttf"
)

font_add(
  family = "ubuntu",
  regular = "extras/Ubuntu-Regular.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# datos -------------------------------------------------------------------

d <- read_csv("datos/base_de_datos_lab.csv")

param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "Cond (μS/cm)", "Sól. susp. (ppm)", "Turb (NTU)", "SDD (cm)")
names(param_unid_v) <- param_v

# figuras -----------------------------------------------------------------
linea_tipo <- 2
linea_ancho <- 1
texto_tamaño <- 4
punto_transparencia <- .4
punto_tamaño <- 3

# configuración de las figuras asociadas a una REACTA
g_recta <- function(g, etq) {
  g +
    geom_smooth(
      method = lm, formula = y ~ x, se = FALSE, color = c1,  
      linetype = linea_tipo, linewidth = linea_ancho) +
    annotate(
      geom = "richtext", x = I(1), y = I(1), hjust = 1, vjust = 1, 
      size = texto_tamaño, label = etq, label.color = NA, fill = c3, 
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
      size = texto_tamaño, label = etq, label.color = NA, fill = c3, 
      family = "jet") +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    scale_x_continuous(breaks = scales::breaks_pretty())
}

# figuras
f_gg <- function(eje_x, eje_y) {
  e <- d |> 
    pivot_wider(
      names_from = param,
      values_from = valor
    ) |> 
    select({{ eje_x }}, {{ eje_y }}) |> 
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
      plot.margin = margin(t = 3, r = 7, b = 14, l = 10),
      panel.background = element_rect(fill = c3, color = NA),
      panel.grid.major = element_line(
        color = c4, linewidth = .3, linetype = 3),
      axis.title = element_markdown(family = "ubuntu", size = 15),
      axis.title.y = element_markdown(angle = 90, margin = margin(r = 3)),
      axis.title.x = element_markdown(margin = margin(t = 6)),
      axis.text = element_text(family = "jet", size = 10),
      axis.text.x = element_text(margin = margin(t = 7)),
      axis.text.y = element_text(margin = margin(r = 7), hjust = 1)
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

# genero todas las figuras
g1 <- f_gg("cond", "turb")
g2 <- f_gg("cond", "sol_sus")
g3 <- f_gg("turb", "sol_sus")
g4 <- f_gg("sol_sus", "secchi")
g5 <- f_gg("turb", "secchi")

# combino todas las figuras
g_identidad_lab <- g1 + g2 + g3 + g4 + g5 +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = c6, color = NA)
    )
  )

ggsave(
  plot = g_identidad_lab,
  filename = "figuras/g_identidad_lab.png",
  width = 30,
  height = 20,
  units = "cm"
)


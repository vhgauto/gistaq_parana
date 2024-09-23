
# datos ------------------------------------------------------------------

# elementos de la tabla, línea por línea
eq1 <- "$$-229.34\\left( \\frac{B03}{B08}\\right)^{3}+1001.65\\left( \\frac{B03}{B08}\\right)^{2}-1422.7\\left( \\frac{B03}{B08}\\right)+665.17$$"
eq2 <- "$$-244.83+40.21\\cdot B01-3.67\\cdot NDWI$$"

banda1 <- "B03, B08"
banda2 <- "B01, NDWI (B03, B08)"

met1 <- "R<sup>2</sup>"
met2 <- "R<sup>2</sup>, RMSE, d"

agua1 <- "Embalse"
agua2 <- "Río"

plat1 <- "Landsat-8"
plat2 <- "GeoEye"

aut1 <- "@Ramirez2017"
aut2 <- "@Gomez2014"

nota1 <- "^[Aguas lénticas.]"
nota2 <- "^[d = prueba estadística de <b>Durbin-Watson</b>.]"

agua_tbl <- c(
  glue("{agua1} {nota1}"),
  agua2
)

# tabla ------------------------------------------------------------------

tabla_tsm <- tibble(
  Ecuación = c(eq1, eq2),
  `Bandas (nm)` = c(banda1, banda2),
  Métricas = c(met1, glue("{met2} {nota2}")),
  Aguas = agua_tbl,
  Plataforma = c(plat1, plat2),
  Referencia = c(aut1, aut2)  
) |> 
  gt() |> 
  fmt_markdown() |> 
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = Referencia)
  ) |> 
    tab_style(
      style = cell_text(align = "left", weight = "bold"),
      locations = cells_column_labels()
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = Ecuación)
    ) |> 
  cols_width(
    Referencia ~ px(20)
  ) |> 
  as_raw_html()

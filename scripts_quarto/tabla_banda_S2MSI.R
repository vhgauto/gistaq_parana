
# datos -------------------------------------------------------------------

# datos de nombre e banda, longitud de onda, centro de banda y
# resolución espacial
d <- read_csv("datos/s2msi.csv", show_col_types = FALSE) |> 
  mutate(`Band Number` = toupper(`Band Number`)) |> 
  mutate(`Band Number` = if_else(
    nchar(`Band Number`) == 1,
    glue("0{`Band Number`}"),
    `Band Number`
  )) |> 
  mutate(
    `Band Number` = if_else(
      `Band Number` == "8A",
      "B8A",
      glue("B{`Band Number`}")
    )
  )

# tabla -------------------------------------------------------------------

tab_s2msi <- gt(d) |> 
  # agrego spanners para S2A y S2B
  tab_spanner(
    label = "Sentinel-2A",
    columns = c(3, 4)
  ) |> 
  tab_spanner(
    label = "Sentinel-2B",
    columns = c(5, 6)
  ) |> 
  # renombro las columnas
  cols_label(
    "Band Number" = "Banda<br><br>",
    "Spatial resolution (m)" = "Resolución<br>espacial (m)",
    "Sentinel-2A Central wavelength (nm)" = "Longitud de<br>onda (nm)",
    "Sentinel-2A Bandwidth (nm)" = "Ancho de<br>banda (nm)",
    "sentinel-2B Central wavelength (nm)" = "Longitud de<br>onda (nm)",
    "Sentinel-2B Bandwidth (nm)" = "Ancho de<br>banda (nm)"
  ) |> 
  # aplico formato markdown
  cols_label_with(
    columns = everything(), 
    fn = gt::md
  ) |> 
  # aplico formato a la columna de bandas, spanners y títulos de columna
  tab_style(
    locations = list(
      cells_column_spanners(spanners = everything()),
      cells_column_labels(columns = everything()),
      cells_body(columns = "Band Number")
    ),
    style = cell_text(font = "Ubuntu", weight = "bold")
  ) |> 
  # aplico estilo a los números
  tab_style(
    locations = cells_body(columns = -"Band Number"),
    style = cell_text(font = "JetBrains Mono")
  ) |> 
  # aplico formato de coma a los números
  fmt_number(
    dec_mark = ",", sep_mark = "", decimals = 1, drop_trailing_zeros = TRUE
  )

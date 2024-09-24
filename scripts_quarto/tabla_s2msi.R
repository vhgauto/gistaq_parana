
# datos -------------------------------------------------------------------

# agrego la descripción de las bandas
nombre_banda <- c(
  B01 = "aerosol",
  B02 = "blue",
  B03 = "green",
  B04 = "red",
  B05 = "red edge",
  B08 = "NIR",
  B11 = "SWIR 1",
  B12 = "SWIR 2"
)

banda_label <- glue("{names(nombre_banda)} ({nombre_banda})")
names(banda_label) <- names(nombre_banda)

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
  ) |> 
  mutate(
    `Band Number` = if_else(
      `Band Number` %in% names(nombre_banda),
      banda_label[`Band Number`],
      `Band Number`
    )
  ) |> 
  mutate(
    across(
      .cols = contains("Central"),
      .fns = ~format(.x, nsmall = 1, digits = 1, decimal.mark = ",")
    )
  )

# tabla -------------------------------------------------------------------

tabla_s2msi <- gt(d) |> 
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
      # cells_column_spanners(spanners = everything()),
      cells_column_labels(columns = everything()),
      cells_body(columns = "Band Number")
    ),
    style = cell_text(
      font = "Ubuntu", weight = "bold", align = "right", v_align = "bottom"
    )
  ) |>
  tab_style(
    locations = cells_column_labels(columns = "Band Number"),
    style = cell_text(align = "left")
  ) |> 
  tab_style(
    locations = cells_column_spanners(spanners = everything()),
    style = cell_text(align = "center", weight = "bold")
  ) |> 
  # aplico estilo a los números
  tab_style(
    locations = cells_body(columns = -"Band Number"),
    style = cell_text(font = "JetBrains Mono")
  ) |> 
  # aplico formato de coma a los números
  fmt_number(
    dec_mark = ",", sep_mark = "", decimals = 1, 
    drop_trailing_zeros = TRUE
  ) |> 
  # alineamiento a izq de nombre de bandas
  tab_style(
    locations = cells_body(columns = "Band Number"),
    style = cell_text(align = "left")
  )


# datos -------------------------------------------------------------------

# próximos 10 muestreos
adq <- tibble(
  Fecha = seq.Date(
    from = ymd(20240101),
    to = ymd(20241231),
    by = "5 day"
  )
) |> 
  mutate(Día = weekdays(Fecha)) |> 
  filter(!Día %in% c("sábado", "domingo")) |> 
  filter(Fecha >= today()) |> 
  slice_head(n = 10)

# agrego al inicio la fecha actual
d <- tibble(
  Fecha = today(),
  Día = weekdays(today())
) |> 
  bind_rows(
    adq
  ) |> 
  mutate(Día = str_to_sentence(Día))

# tabla -------------------------------------------------------------------

tabla_adquisicion <- gt(d) |> 
  # formato de la fecha actual
  tab_style(
    locations = cells_body(columns = everything(), rows = 1),
    style = cell_text(weight = "bold", color = c2)
  ) |> 
  # aplico formato a la columna de días
  tab_style(
    locations = cells_body(columns = Día),
    style = cell_text(font = "Ubuntu")
  ) |> 
  # aplico formato a la columna de fechas
  tab_style(
    locations = cells_body(columns = Fecha),
    style = cell_text(font = "JetBrains Mono", align = "center")
  ) |> 
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(font = "Ubuntu", weight = "bold", align = "center")
  ) |> 
  cols_width(
    Fecha ~ px(250) 
  )

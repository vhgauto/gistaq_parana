
# datos -------------------------------------------------------------------

# próximos 10 muestreos
# fecha de referencia: 20240101
adq <- tibble(
  Fecha = seq.Date(
    from = ymd(20240101),
    to = today() + months(6),
    by = "5 day"
  )
) |>
  mutate(Día = weekdays(Fecha)) |>
  filter(!Día %in% c("sábado", "domingo")) |>
  filter(Fecha >= today()) |>
  slice_head(n = 10)

# agrego al inicio la fecha actual
tabla_adquisicion_tbl <- tibble(
  Fecha = today(),
  Día = weekdays(today())
) |>
  bind_rows(
    adq
  ) |>
  mutate(Día = str_to_sentence(Día))

tabla_adquisicion_tbl2 <- tabla_adquisicion_tbl |>
  mutate(Fecha = as.character(Fecha)) |>
  mutate(Fecha = if_else(
    row_number() == 1,
    glue("**{Fecha}**"),
    Fecha
  )) |>
  mutate(Día = if_else(
    row_number() == 1,
    glue("**{Día}**"),
    Día
  ))

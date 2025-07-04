
# https://corrr.tidymodels.org/reference/colpair_map.html

# datos -------------------------------------------------------------------

# leo los datos de laboratorio
d <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE)

# nombres de los parámetros y sus etiquetas
param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "<i>cond</i><br>(μS/cm)", "<i>susp</i><br>(ppm)",
  "<i>turb</i><br>(NTU)", "<i>secchi</i><br>(cm)")
names(param_unid_v) <- param_v

# correlación -------------------------------------------------------------

# correlación R
e_r <- d |> 
  drop_na() |>  
  pivot_wider(
    names_from = param,
    values_from = valor,
    values_fn = list
  ) |> 
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |> 
  unnest(everything()) |> 
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
    values_from = valor,
    values_fn = list
  ) |> 
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |> 
  unnest(everything()) |> 
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
  mutate(
    es_significativo = pvalor < .05
  ) |> 
  mutate(
    label = f_formato(r, digits = 2)
  ) |> 
  mutate(
    label = if_else(
      abs(r) > .5,
      glue("<b style='color:{c2}'>{label}</b>"),
      label
    )
  ) |>
  mutate(
    label = if_else(
      es_significativo,
      glue("{label}{simbolo_sig}"),
      label
    )
  ) |> 
  select(term, param, label) |> 
  pivot_wider(
    names_from = param,
    values_from = label,
    id_cols = term
  ) |> 
  mutate(
    term = param_unid_v[term]
  )

# tabla -------------------------------------------------------------------

tabla_corr_lab <- gt(e) |> 
  sub_missing(missing_text = "---") |> 
  # nombre de columnas
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(align = "center", v_align = "top", weight = "bold")
  ) |> 
  cols_label_with(
    columns = everything(),
    fn = gt::md
  ) |> 
  # nombre de filas
  fmt_markdown(
    columns = "term"
  ) |> 
  tab_style(
    locations = cells_body(columns = "term"),
    style = cell_text(weight = "bold")
  ) |> 
  tab_style(
    locations = cells_body(columns = -"term"),
    style = cell_text(font = "JetBrains Mono", align = "right")
  ) |> 
  cols_label(
    term = ""
  ) |> 
  # números de las celdas
  fmt_markdown() |>
  cols_label(
    ph = param_unid_v[1],
    cond = param_unid_v[2],
    sol_sus = param_unid_v[3],
    turb = param_unid_v[4]
  ) |> 
  cols_label_with(
    fn = gt::md
  ) |> 
  tab_options(table.background.color = c6
  )

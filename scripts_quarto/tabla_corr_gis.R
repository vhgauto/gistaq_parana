
# https://corrr.tidymodels.org/reference/colpair_map.html

# datos -------------------------------------------------------------------

# leo los datos espectrales S2-MSI
d <- read_csv("datos/base_de_datos_gis.csv", show_col_types = FALSE) |> 
  filter(pixel == "3x3") |> 
  select(-pixel)

# correlación -------------------------------------------------------------

# pvalor
e_pvalor <- d |> 
  drop_na() |>  
  pivot_wider(
    names_from = banda,
    values_from = reflect
  ) |> 
  select(starts_with("B")) |> 
  colpair_map(f_pvalor) |> 
  shave() |> 
  pivot_longer(
    cols = -term,
    names_to = "param",
    values_to = "pvalor"
  ) |> 
  drop_na()

# correlación R
e_r <- d |> 
  drop_na() |>  
  pivot_wider(
    names_from = banda,
    values_from = reflect
  ) |> 
  select(starts_with("B")) |> 
  correlate(method = "pearson", use = "pairwise.complete.obs", quiet = TRUE) |> 
  shave() |> 
  pivot_longer(
    cols = -term,
    names_to = "param",
    values_to = "r"
  ) |> 
  drop_na()

# combino datos y aplico formatos si la correlación es significativa
e <- inner_join(
  e_r,
  e_pvalor,
  by = join_by(term, param)
) |> 
  mutate(
    es_significativo = pvalor < .05
  ) |> 
  mutate(
    label = f_formato(r, digits = 2, nsmall = 2)
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
  )

# tabla -------------------------------------------------------------------

tabla_corr_gis <- gt(e) |> 
  sub_missing(missing_text = "---") |> 
  # nombre de columnas
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(align = "center", v_align = "top", weight = "bold")
  ) |>  
  tab_style(
    locations = cells_body(columns = "term"),
    style = cell_text(weight = "bold")
  ) |> 
  tab_style(
    locations = cells_body(columns = starts_with("B")),
    style = cell_text(font = "JetBrains Mono")
  ) |> 
  cols_label(
    term = ""
  ) |> 
  # números de las celdas
  fmt_markdown() |> 
  tab_options(table.background.color = c6
  )

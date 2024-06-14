
# https://corrr.tidymodels.org/reference/colpair_map.html

# funciones ---------------------------------------------------------------

# función que aplica formato a los números de R
f_formato <- function(x, digits = 3, nsmall = 3) {
  format(
    x, digits = digits, nsmall = nsmall, trim = TRUE, decimal.mark = ",",
    big.mark = ".")
}

# función que obtiene el pvalor de las correlaciones
f_pvalor <- function(vec_a, vec_b) {
  cor.test(vec_a, vec_b)$p.value
}

# datos -------------------------------------------------------------------

# leo los datos de laboratorio
d <- read_csv("datos/base_de_datos_lab.csv")

# nombres de los parámetros y sus etiquetas
param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "Cond<br>(μS/cm)", "Sól. susp.<br>(ppm)", "Turb<br>(NTU)",
  "SDD<br>(cm)")
names(param_unid_v) <- param_v

# correlación -------------------------------------------------------------

# símbolo utilizado para las correlaciones que sean significativas
simbolo_sig <- "✦"

# pvalor
e_pvalor <- d |> 
  drop_na() |>  
  pivot_wider(
    names_from = param,
    values_from = valor
  ) |> 
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |> 
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
    names_from = param,
    values_from = valor
  ) |> 
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |> 
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
    label = f_formato(r)
  ) |> 
  mutate(
    label = if_else(
      es_significativo,
      glue("{label}{simbolo_sig}"),
      label
    )
  ) |> 
  mutate(
    label = if_else(
      abs(r) > .5,
      glue("<b style='color:{c2}'>{label}</b>"),
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

tab_corr_lab <- gt(e) |> 
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

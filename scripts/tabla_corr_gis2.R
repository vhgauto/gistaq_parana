
# https://corrr.tidymodels.org/reference/colpair_map.html

# funciones ---------------------------------------------------------------

# función que aplica formato a los números de R
f_formato <- function(x, digits = 3, nsmall = 3) {
  format(
    x, digits = digits, nsmall = 3, trim = TRUE, decimal.mark = ",",
    big.mark = ".")
}

# función que obtiene el pvalor de las correlaciones
f_pvalor <- function(vec_a, vec_b) {
  cor.test(vec_a, vec_b)$p.value
}

# datos -------------------------------------------------------------------

# leo los datos espectrales S2-MSI
d <- read_csv("datos/base_de_datos_gis.csv") |> 
  filter(pixel == "3x3") |> 
  select(-pixel)

# correlación -------------------------------------------------------------

# símbolo utilizado para las correlaciones que sean significativas
simbolo_sig <- "✦"

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
  )

# tabla -------------------------------------------------------------------

tab_corr_gis <- gt(e) |> 
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
  # ancho de columna
  # cols_width(everything() ~ px(120)) |> 
  tab_footnote(
    footnote = md(
      glue(
        "{simbolo_sig} : |<b>R</b>| > 0,5<br>",
        "<b style='color:{c2};'>R</b> : p-valor < 0,05")
    ),
    placement = "right"
  ) |> 
  tab_style(
    locations = cells_footnotes(),
    style = cell_text(align = "right")
  ) |> 
  tab_options(table.background.color = c6
  )

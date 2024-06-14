
# fuente ------------------------------------------------------------------

f_formato <- function(x, digits = 3, nsmall = 3) {
  format(
    x, digits = digits, nsmall = 3, trim = TRUE, decimal.mark = ",",
    big.mark = ".")
}

# datos -------------------------------------------------------------------

d <- read_csv("datos/base_de_datos_gis.csv") |> 
  filter(pixel == "3x3") |> 
  select(-pixel)

m <- d |> 
  pivot_wider(
    names_from = banda,
    values_from = reflect
  ) |> 
  select(starts_with("B")) |> 
  cor(use = "pairwise.complete.obs")

m[upper.tri(m, diag = TRUE)] <- NA

d_fmt <- as_tibble(m) |>
  mutate(param = rownames(m), .before = 1) |>
  slice(-1) |> 
  select(-B12) |> 
  pivot_longer(
    cols = -param,
    names_to = "x",
    values_to = "valor"
  ) |> 
  mutate(
    es_corr = abs(valor) > .5
  ) |> 
  mutate(
    valor = f_formato(valor)
  ) |> 
  mutate(
    valor = if_else(
      es_corr, 
      glue("<b style='color:{c2}'>{valor}</b>"),
      glue("{valor}")
    )
  ) |> 
  select(-es_corr) |> 
  pivot_wider(
    names_from = x,
    values_from = valor
  )

# tabla -------------------------------------------------------------------

tab_corr_gis <- gt(d_fmt) |> 
  sub_missing(missing_text = "---") |> 
  # nombre de columnas
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(align = "center", v_align = "top", weight = "bold")
  ) |>  
  tab_style(
    locations = cells_body(columns = "param"),
    style = cell_text(weight = "bold")
  ) |> 
  tab_style(
    locations = cells_body(columns = starts_with("B")),
    style = cell_text(font = "JetBrains Mono")
  ) |> 
  cols_label(
    param = ""
  ) |> 
  # números de las celdas
  fmt_markdown() |> 
  # ancho de columna
  cols_width(everything() ~ px(120))


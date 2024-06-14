
# fuente ------------------------------------------------------------------

color_p <- "#E41A1C"

f_formato <- function(x, digits = 3, nsmall = 3) {
  format(
    x, digits = digits, nsmall = 3, trim = TRUE, decimal.mark = ",",
    big.mark = ".")
}

# datos -------------------------------------------------------------------

d <- read_csv("datos/base_de_datos_lab.csv")

param_v <- c("ph", "cond", "sol_sus", "turb", "secchi")
param_unid_v <- c(
  "pH", "Cond<br>(μS/cm)", "Sól. susp.<br>(ppm)", "Turb<br>(NTU)",
  "SDD<br>(cm)")
names(param_unid_v) <- param_v

# correlación -------------------------------------------------------------

m <- d |> 
  drop_na() |>  
  pivot_wider(
    names_from = param,
    values_from = valor
  ) |> 
  select(-c(contains("hazemeter"), fecha, longitud, latitud)) |> 
  cor(use = "pairwise.complete.obs")
  
m[upper.tri(m, diag = TRUE)] <- NA

d_fmt <- as_tibble(m) |>
  mutate(param = rownames(m), .before = 1) |>
  slice(-1) |> 
  select(-secchi) |> 
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
      glue("<b style='color:{color_p}'>{valor}</b>"),
      glue("{valor}")
    )
  ) |>
  select(-es_corr) |> 
  pivot_wider(
    names_from = x,
    values_from = valor
  ) |> 
  mutate(
    param = param_unid_v[param]
  )

# tabla -------------------------------------------------------------------

tab_corr_lab <- gt(d_fmt) |> 
  sub_missing(missing_text = ".") |> 
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
    columns = "param"
  ) |> 
  tab_style(
    locations = cells_body(columns = "param"),
    style = cell_text(weight = "bold")
  ) |> 
  tab_style(
    locations = cells_body(columns = -"param"),
    style = cell_text(font = "JetBrains Mono")
  ) |> 
  cols_label(
    param = ""
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
  )

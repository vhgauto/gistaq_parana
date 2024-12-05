#
source("scripts_quarto/soporte.R")

# datos ------------------------------------------------------------------

# vector transecta
v <- vect("vector/transecta.gpkg")

# ráster de recortes
archivos_r <- list.files("recorte/", pattern = "tif$", full.names = TRUE)
fechas_r <- str_remove(basename(archivos_r), ".tif")

lista_r <- map(
  .x = archivos_r,
  rast
)

# agrupo en ventana de 3x3
lista_r3 <- map(lista_r, ~focal(.x, w = 3, fun = "mean"))
lista_r3 <- set_names(lista_r3, fechas_r)

# factor de las bandas
orden_bandas <- c(
  "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12"
)

# reflectancias a lo largo de la transecta, por banda y fecha
e_tbl <- map(lista_r3, ~terra::extract(.x, v, xy = TRUE)) |>
  list_rbind(names_to = "fecha") |>
  as_tibble() |>
  mutate(fecha = ymd(fecha)) |>
  pivot_longer(
    cols = starts_with("B"),
    values_to = "reflect",
    names_to = "banda"
  ) |>
  reframe(
    reflect = mean(reflect)/10000,
    y = y,
    .by = c(x, banda, fecha)
  ) |>
  mutate(banda = factor(banda, levels = orden_bandas))

tabla_boxplot_firma_espectral <- e_tbl |>
  filter(fecha != ymd(20231212)) |>
  group_by(banda) |>
  summarise(
    r = list(reflect),
    .groups = "drop"
  ) |>
  gt() |>
  gtExtras::gt_plt_dist(
    column = r,
    type = "boxplot",
    line_color = c1,
    fill_color = c3
  ) |>
  cols_label(
    banda = "Banda",
    r = md("Distribución R<sub>rs</sub>")
  ) |>
  tab_style(
    locations = cells_body(columns = "banda"),
    style = cell_text(weight = "bold")
  ) |>
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(v_align = "top", weight = "bold")
  )

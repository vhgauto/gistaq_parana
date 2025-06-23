# paquetes ----------------------------------------------------------------

library(terra)
library(tidyverse)

bandas_s2 <- c(
  "B01",
  "B02",
  "B03",
  "B04",
  "B05",
  "B06",
  "B07",
  "B08",
  "B8A",
  "B11",
  "B12"
)

v <- vect("vector/recorte_puente.gpkg")

fechas <- list.files("acolite/") |>
  str_sub(9, 18) |>
  unique() |>
  sort()

fechas_label <- str_remove_all(fechas, "_")

rasters <- list.files("acolite/", pattern = "tif$", full.names = TRUE)

# recorte -----------------------------------------------------------------

f_crop <- function(fecha_i) {
  r <- tibble(
    a = rasters[str_detect(rasters, fecha_i)]
  ) |>
    mutate(orden = row_number()) |>
    mutate(
      wl = str_sub(a, 53, 100)
    ) |>
    mutate(
      wl = str_remove(wl, ".tif") |> as.integer()
    ) |>
    arrange(wl) |>
    mutate(banda = bandas_s2) |>
    mutate(
      r = map(a, rast)
    ) |>
    mutate(
      croped = map(r, ~ crop(.x, v, mask = TRUE))
    ) |>
    pull(croped) |>
    rast()

  names(r) <- bandas_s2

  return(r)
}

l_crop <- map(fechas, f_crop)

f_save <- function(x, y) {
  writeRaster(
    x,
    filename = paste0("recorte_acolite/", ymd(y), ".tif"),
    overwrite = TRUE
  )
}

walk2(l_crop, fechas_label, f_save)

# extracción --------------------------------------------------------------

l_acolite <- list.files("recorte_acolite/", full.names = TRUE, pattern = "tif$")

r_acolite <- map(l_acolite, rast)
r_acolite <- set_names(r_acolite, fechas_label)

sitios_v <- readxl::read_xlsx("datos/datos_gistaq.xlsx") |>
  select(fecha = 1, Longitud, Latitud) |>
  fill(fecha) |>
  drop_na() |>
  rename_with(tolower) |>
  # mutate(punto = row_number(), .before = 1) |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326") |>
  project("EPSG:32721")

f_extraccion <- function(raster_i, fecha_i) {
  nro_v <- length(sitios_v[sitios_v$fecha == fecha_i])

  r_3x3 <- terra::focal(
    raster_i,
    w = 3,
    fun = mean,
    na.rm = TRUE
  )

  terra::extract(
    r_3x3,
    sitios_v[sitios_v$fecha == fecha_i]
  ) |>
    as_tibble() |>
    mutate(punto = row_number(), .before = 1) |>
    pivot_longer(
      cols = -c(ID, punto),
      names_to = "banda",
      values_to = "reflect"
    ) |>
    mutate(banda = factor(banda, levels = bandas_s2)) |>
    mutate(fecha = ymd(fecha_i)) |>
    select(-ID)
}

reflect_tbl <- map2(r_acolite, fechas_label, f_extraccion) |>
  list_rbind()

lab_tbl <- readxl::read_xlsx("datos/datos_gistaq.xlsx") |>
  select(fecha = 1, Longitud, Latitud) |>
  fill(fecha) |>
  drop_na() |>
  rename_with(tolower) |>
  mutate(punto = row_number(), .before = 1, .by = fecha) |>
  mutate(fecha = ymd(fecha))

inner_join(reflect_tbl, lab_tbl, by = join_by(fecha, punto)) |> 
  write_csv("datos/base_de_datos_gis_acolite.csv")

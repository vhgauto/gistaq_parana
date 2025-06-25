# library(glue)
# library(terra)
# library(tidyverse)

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

v <- terra::vect("vector/recorte_puente.gpkg")

# genera mensajes en la consola
mensaje <- function(x) {
  print(glue("\n\n--- {x} ---\n\n"))
}

# archivo Excel, con la fecha y sitios de muestreo
archivo_excel <- function() {
  r <- "datos/datos_gistaq.xlsx"
  return(r)
}

archivo_quarto <- function() {
  r <- "manuscrito.qmd"
  return(r)
}

# fecha para la descarga de producto
fecha <- function(archivo_i) {
  fechas_excel <- readxl::read_xlsx(
    path = archivo_i,
    sheet = 1,
    .name_repair = "unique_quiet"
  ) |>
    select(fecha = 1, latitud = 4, longitud = 5) |>
    mutate(fecha = ymd(fecha)) |>
    drop_na() |>
    select(fecha) |>
    distinct(fecha)

  # fechas_base_de_datos <- read_csv(
  #   file = "datos/base_de_datos_lab.csv",
  #   show_col_types = FALSE
  # ) |>
  #   distinct(fecha)
  fechas_gis <- read_csv(
    file = "datos/base_de_datos_gis_acolite.csv",
    show_col_types = FALSE
  ) |>
    distinct(fecha)

  # idealmente, un único elemento
  fecha_faltante <- anti_join(
    fechas_excel,
    fechas_gis,
    by = join_by(fecha)
  ) |>
    pull()

  return(fecha_faltante)
}

recorte <- function(fecha_i) {
  rasters <- list.files(
    path = "acolite/",
    pattern = format(fecha_i, "%Y_%m_%d"),
    full.names = TRUE
  )

  r <- tibble(
    a = rasters
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

  writeRaster(
    r,
    filename = paste0("recorte_acolite/", fecha_i, ".tif"),
    overwrite = TRUE
  )

  rds <- "recorte_acolite/recorte.rds"

  terra::saveRDS(object = r, file = rds)

  return(rds)
}

# extraigo los valores de píxel
reflectancia <- function(fecha_i, archivo_i) {
  # leemos el Excel que contiene las coordenadas geográficas de los puntos
  # de muestreo
  coord_sitios <- readxl::read_xlsx(
    path = archivo_i,
    sheet = 1,
    .name_repair = "unique_quiet"
  ) |>
    select(fechas = 1, longitud = 4, latitud = 5) |>
    fill(fechas) |>
    mutate(fechas = ymd(fechas)) |>
    dplyr::filter(fechas == ymd(fecha_i)) |>
    drop_na() |>
    select(-fechas) |>
    mutate(punto = row_number(), .before = 1)

  coord_sitios_v <- vect(
    coord_sitios,
    geom = c("longitud", "latitud"),
    crs = "EPSG:4326",
    keepgeom = FALSE
  ) |>
    terra::project("EPSG:32721")

  mensaje("Vector de sitios de muestreo")

  # nombre del producto
  r <- list.files(
    path = "recorte_acolite/",
    pattern = as.character(fecha_i),
    full.names = TRUE
  ) |>
    rast()

  # 3X3
  r_3x3 <- terra::focal(
    r,
    w = 3,
    fun = mean,
    na.rm = TRUE
  )

  reflect <- terra::extract(r_3x3, coord_sitios_v) |>
    as_tibble() |>
    rename(punto = ID) |>
    pivot_longer(cols = -punto, names_to = "banda", values_to = "reflect") |>
    mutate(fecha = ymd(fecha_i), .before = 1) |>
    inner_join(coord_sitios, by = join_by(punto))

  datos <- "datos/base_de_datos_gis_acolite.csv"

  # guardo la tabla como .csv
  if (file.exists(datos)) {
    base_de_datos <- read_csv(datos, show_col_types = FALSE)

    bind_rows(base_de_datos, reflect) |>
      arrange(fecha, punto) |>
      write_csv(datos)

    mensaje("Base de datos actualizada")
  } else {
    write_csv(reflect, datos)

    mensaje("Datos almacenados")
  }

  # elimino los archivos descargados
  # unlink("producto/*", recursive = TRUE)
  # mensaje("Archivos eliminados")

  return(datos)
}

# extraigo parámetros de laboratorio
lab <- function(archivo_i, fecha_i) {
  d <- readxl::read_xlsx(
    path = archivo_i,
    sheet = 1,
    .name_repair = "unique_quiet"
  ) |>
    select(
      fecha = 1,
      longitud = 4,
      latitud = 5,
      ph = 6,
      cond = 8,
      secchi = 10,
      sol_sus = 12,
      turb = 13,
      hazemeter = 14
    ) |>
    fill(fecha) |>
    mutate(fecha = ymd(fecha)) |>
    pivot_longer(
      cols = -c(fecha, latitud, longitud),
      names_to = "param",
      values_to = "valor"
    ) |>
    drop_na(valor, latitud, longitud)

  u <- "datos/base_de_datos_lab.csv"

  write_csv(d, file = u)

  return(u)
}

# elimino todos los archivos descargados
# elimino <- function() {
#   unlink("producto/*", recursive = TRUE)
#   mensaje("Archivos eliminados")
# }

# publico sitio web en Github
# publico_quarto <- function(x) {
#   # corro el script Python que descarga la imagen
#   system(glue("quarto publish {x}"))
# }

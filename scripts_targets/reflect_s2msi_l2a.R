library(glue)
library(terra)
library(tidyverse)

mensaje <- function(x) {
  print(glue("\n\n--- {x} ---\n\n"))
}

recorte <- function(fecha) {
  mensaje("Leo producto S2-MSI")

  # extraigo .zip
  unzip(
    zipfile = paste0("rasters/", ymd(fecha), ".zip"),
    exdir = paste0("rasters/", ymd(fecha))
  )

  mensaje("Producto extraído")

  # nombre del producto
  lis <- list.files(
    path = paste0("rasters/", ymd(fecha)),
    pattern = "SAFE",
    full.names = TRUE
  )

  # carpeta con las carpetas de distintas resoluciones
  carpeta1 <- glue("{lis}/GRANULE")
  carpeta2 <- list.files(carpeta1)
  carpeta3 <- glue("{carpeta1}/{carpeta2}/IMG_DATA")

  r10m <- list.files(glue("{carpeta3}/R10m"), full.names = TRUE)
  r20m <- list.files(glue("{carpeta3}/R20m"), full.names = TRUE)

  #nombres de las bandas en el orden correcto
  bandas_nombres <- c(
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

  #caminos para cada archivo de la banda requerida
  b01 <- r20m[2]
  b02 <- r10m[2]
  b03 <- r10m[3]
  b04 <- r10m[4]
  b05 <- r20m[6]
  b06 <- r20m[7]
  b07 <- r20m[8]
  b08 <- r10m[5]
  b8a <- r20m[11]
  b11 <- r20m[9]
  b12 <- r20m[10]

  #vector de los caminos de los archivos en el orden correcto
  vector_bandas <- c(b01, b02, b03, b04, b05, b06, b07, b08, b8a, b11, b12)

  #leo los archivos
  lista_bandas <- map(vector_bandas, rast)
  names(lista_bandas) <- bandas_nombres

  mensaje("Recorto y reproyecto el producto")

  #vector para recortar los raster alrededor del puente
  recorte_puente <- vect("vector/recorte_puente.gpkg")

  #recorte de cada elemento de la lista con el vector puente
  lista_recortes <- map(
    .x = lista_bandas,
    ~ terra::crop(x = .x, y = recorte_puente)
  )

  #los raster de 20m los reproyecto a 10m
  lista_recortes$B01 <- project(lista_recortes$B01, lista_recortes$B02)
  lista_recortes$B05 <- project(lista_recortes$B05, lista_recortes$B02)
  lista_recortes$B06 <- project(lista_recortes$B06, lista_recortes$B02)
  lista_recortes$B07 <- project(lista_recortes$B07, lista_recortes$B02)
  lista_recortes$B8A <- project(lista_recortes$B8A, lista_recortes$B02)
  lista_recortes$B11 <- project(lista_recortes$B11, lista_recortes$B02)
  lista_recortes$B12 <- project(lista_recortes$B12, lista_recortes$B02)

  #creamos un stack con todas las bandas recortadas y la misma resolución
  # espacial (10m)
  stack_bandas <- rast(lista_recortes)

  # guardo stack de bandas recortado
  nombre_raster <- as.character(fecha) |> str_remove_all("-")

  writeRaster(
    stack_bandas,
    glue("recorte_sen2cor/{nombre_raster}.tif"),
    overwrite = TRUE
  )

  mensaje("Recorte almacenado")
}

reflectancia <- function(fecha) {
  # leemos el Excel que contiene las coordenadas geográficas de los puntos
  # de muestreo
  coord_sitios <- readxl::read_xlsx(
    path = "datos/datos_gistaq.xlsx",
    sheet = 1,
    .name_repair = "unique_quiet"
  ) |>
    select(fechas = 1, longitud = 4, latitud = 5) |>
    fill(fechas) |>
    mutate(fechas = ymd(fechas)) |>
    dplyr::filter(fechas == ymd(fecha)) |>
    drop_na() |>
    select(-fechas) |>
    mutate(punto = row_number(), .before = 1)

  # convertimos la tabla de coordenadas a sf
  coord_vect <- vect(
    coord_sitios,
    geom = c("longitud", "latitud"),
    crs = "EPSG:4326",
    keepgeom = FALSE
  ) |>
    terra::project("EPSG:32721")

  mensaje("Vector de sitios de muestreo")

  r <- rast(paste0("recorte_sen2cor/", fecha, ".tif"))

  r_3x3 <- terra::focal(
    r,
    w = 3,
    fun = mean,
    na.rm = TRUE
  )

  reflect <- terra::extract(r_3x3, coord_vect) |>
    as_tibble() |>
    rename(punto = ID) |>
    pivot_longer(cols = -punto, names_to = "banda", values_to = "reflect") |>
    mutate(fecha = ymd(fecha), .before = 1) |>
    inner_join(coord_sitios, by = join_by(punto)) |>
    mutate(reflect = reflect / 10000)

  reflect_prev <- read_csv(
    "datos/base_de_datos_gis_sen2cor.csv",
    show_col_types = FALSE
  )

  rbind(reflect_prev, reflect) |>
    write_csv("datos/base_de_datos_gis_sen2cor.csv")

  mensaje("Datos sen2cor almacenados")
}

# recorte(20251126)
# reflectancia(20251126)

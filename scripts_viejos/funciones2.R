
# paquetes necesarios
# paquetes <- function() {
#
#   library(glue)
#   library(terra)
#   library(tidyverse)
#
# }

# genera mensajes en la consola
mensaje <- function(x) {
  print(glue("\n\n--- {x} ---\n\n"))
}

# mensaje("Paquetes cargados correctamente")

archivo_excel <- function() {
  r <- "datos/2024 Todos_los_parametros_Victor1.xlsx"

  return(r)
}

fecha <- function(x) {
  fechas_excel <- readxl::read_xlsx(
    path = x,
    sheet = 1,
    .name_repair = "unique_quiet") |>
    select(fecha = 1, latitud = 4,longitud = 5) |>
    mutate(fecha = ymd(fecha)) |>
    drop_na() |>
    select(fecha)

  fechas_base_de_datos <- read_csv(
    file = "datos/base_de_datos_gis.csv",
    show_col_types = FALSE) |>
    distinct(fecha)

  # idealmente, un único elemento
  fecha_faltante <- anti_join(
    fechas_excel,
    fechas_base_de_datos,
    by = join_by(fecha)
  ) |>
    pull()

  return(fecha_faltante)

}

# función que genera script Python para la descarga de la imagen de la
# fecha dada
script_descarga_py <- function(x) {

  # rango de fechas para la búsqueda de la imagen
  fecha_inicio <- ymd(x)
  fecha_final <- ymd(x) + 1

  # leo las líneas del template
  r_txt <- readLines("scripts/plantilla.py")

  # remplazo las variables con las fechas
  r_txt <- gsub(
    pattern = "fecha_i",
    replacement = fecha_inicio,
    x = r_txt
  )

  r_txt <- gsub(
    pattern = "fecha_f",
    replacement = fecha_final,
    x = r_txt
  )

  # archivo Python
  script_py <- "scripts/d.py"

  # guardo el script
  writeLines(r_txt, con = script_py)

  mensaje("Script Python creado")

  return(script_py)

}

# ejecuto script Python para la descarga de la imagen
descarga <- function(x) {

  # corro el script Python que descarga la imagen
  system(glue("python {x}"))

  p <- "producto/producto.zip"

  return(p)
}

recorte <- function(x, y) {

  # condición de ERROR
  # si NO existe el SAFE, NO extrae la reflectancia
  # if (file.exists(x) == FALSE)
  #   stop(mensaje("SAFE NO descargado"))

  mensaje("Leo producto S2-MSI")

  # cambio de nombre la variable
  # fecha_date <- fecha

  # extraigo .zip
  unzip(zipfile = x, exdir = "producto/")

  mensaje("Producto extraído")

  # nombre del producto
  lis <- list.files(path = "producto/", pattern = "SAFE", full.names = TRUE)

  # carpeta con las carpetas de distintas resoluciones
  carpeta1 <- glue("{lis}/GRANULE")
  carpeta2 <- list.files(carpeta1)
  carpeta3 <- glue("{carpeta1}/{carpeta2}/IMG_DATA")

  r10m <- list.files(glue("{carpeta3}/R10m"), full.names = TRUE)
  r20m <- list.files(glue("{carpeta3}/R20m"), full.names = TRUE)

  #nombres de las bandas en el orden correcto
  bandas_nombres <- c(
    "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")

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
  lista_bandas <-  map(vector_bandas, rast)
  names(lista_bandas) <- bandas_nombres

  mensaje("Recorto y reproyecto el producto")

  #vector para recortar los raster alrededor del puente
  recorte_puente <- vect("vector/recorte_puente.gpkg")

  #recorte de cada elemento de la lista con el vector puente
  lista_recortes <- map(
    .x = lista_bandas,
    ~terra::crop(x = .x, y = recorte_puente))

  #los raster de 20m los reproyecto a 10m
  lista_recortes$B01 <- project(lista_recortes$B01, lista_recortes$B02)
  lista_recortes$B05 <- project(lista_recortes$B05, lista_recortes$B02)
  lista_recortes$B06 <- project(lista_recortes$B06, lista_recortes$B02)
  lista_recortes$B07 <- project(lista_recortes$B07, lista_recortes$B02)
  lista_recortes$B8A <- project(lista_recortes$B8A, lista_recortes$B02)
  lista_recortes$B11 <- project(lista_recortes$B11, lista_recortes$B02)
  lista_recortes$B12 <- project(lista_recortes$B12, lista_recortes$B02)

  #creamos un stack con todas las bandas recortadas y la misma resolucion espacial (10m)
  stack_bandas <- rast(lista_recortes)

  # guardo stack de bandas recortado
  nombre_raster <- as.character(y[1]) |> str_remove_all("-")

  writeRaster(
    stack_bandas,
    glue("recorte/{nombre_raster}.tif"),
    overwrite = TRUE)

  # para el siguiente target tengo que guardarlo como RDS
  # SI NO, NO FUNCIONA
  r <- "recorte/wrap_recorte.rds"

  terra::saveRDS(object = stack_bandas, file = r)

  mensaje("Recorte almacenado")

  return(r)

}

reflectancia <- function(x, y, z) {

  # leemos el Excel que contiene las coordenadas geográficas de los puntos
  # de muestreo
  coord_sitios <- readxl::read_xlsx(
    path = x,
    sheet = 1,
    .name_repair = "unique_quiet") |>
    select(fechas = 1,latitud = 4,longitud = 5) |>
    fill(fechas) |>
    mutate(fechas = ymd(fechas)) |>
    dplyr::filter(fechas == ymd(y)) |>
    drop_na() |>
    select(-fechas) |>
    mutate(punto = row_number(), .before = 1)

  # convertimos la tabla de coordenadas a sf
  coord_vect <- vect(
    coord_sitios,
    geom = c("longitud", "latitud"),
    crs="EPSG:4326",
    keepgeom = FALSE) |>
    project("EPSG:32721")

  mensaje("Vector de sitios de muestreo")

  # verificar probabilidad de NUBES

  mensaje("Verifico presencia de nubes")

  # nombre del producto
  lis <- list.files(path = "producto/", pattern = "SAFE", full.names = TRUE)

  # carpeta con las carpetas de distintas resoluciones
  carpeta1 <- glue("{lis}/GRANULE")
  carpeta2 <- list.files(carpeta1)
  carpeta3 <- glue("{carpeta1}/{carpeta2}/IMG_DATA")
  carpeta4 <- glue("{carpeta1}/{carpeta2}/QI_DATA")

  # leo el ráster
  raster_prob <- terra::rast(glue("{carpeta4}/MSK_CLDPRB_20m.jp2"))

  # extraemos los valores de pixel para cada punto
  nubes <- terra::extract(raster_prob, coord_vect) |>
    as_tibble() |>
    rename(punto = ID, probabilidad = MSK_CLDPRB_20m) |>
    dplyr::filter(probabilidad == 0)

  # se conservan los sitios de muestreo con probabilidad de nubes CERO
  coord_sf_sin_nubes <- inner_join(
    st_as_sf(coord_vect),
    nubes,
    by = join_by(punto))

  if (nrow(coord_sitios) != nrow(coord_sf_sin_nubes)) {

    # sitios con nubes
    puntos_nubes <- anti_join(
      st_as_sf(coord_vect),
      nubes,
      by = join_by(punto)) |>
      pull(punto) |>
      stringr::str_flatten_comma(string = _, last = " y ")

    mensaje(glue("Los sitios {} presentan nubes y se descartan"))
  }

  # leo recorte
  # stack_rds <- read_rds(z)
  # stack_bandas <- unwrap(stack_rds)
  stack_bandas <- rast(z)

  # extraemos los valores de pixel para cada punto
  # acomodo de los datos de reflectancia y se agregan las coord geof

  # 1X1
  reflect_1x1 <- terra::extract(stack_bandas, coord_sf_sin_nubes) |>
    as_tibble() |>
    rename(punto = ID) |>
    pivot_longer(cols = -punto, names_to = "banda", values_to = "reflect") |>
    mutate(reflect = reflect/10000) |>
    mutate(fecha = ymd(y), .before = 1) |>
    inner_join(coord_sitios, by = join_by(punto)) |>
    mutate(pixel = "1x1", .before = banda)

  # 3X3
  stack_bandas_3x3 <- terra::focal(
    stack_bandas, w = 3, fun = mean, na.rm = TRUE)

  reflect_3x3 <- terra::extract(stack_bandas_3x3, coord_sf_sin_nubes) |>
    as_tibble() |>
    rename(punto = ID) |>
    pivot_longer(cols = -punto, names_to = "banda", values_to = "reflect") |>
    mutate(reflect = reflect/10000) |>
    mutate(fecha = ymd(y), .before = 1) |>
    inner_join(coord_sitios, by = join_by(punto)) |>
    mutate(pixel = "3x3", .before = banda)

  # combino las reflectancias 1x1 y 3x3
  reflect <- bind_rows(reflect_1x1, reflect_3x3)

  datos <- "datos/base_de_datos_gis.csv"

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
  unlink("producto/*", recursive = TRUE)
  mensaje("Archivos eliminados")

  return(datos)
}

# x <- "datos/2023 Todos_los_parametros_Victor.xlsx"

lab <- function(x) {
  d <- readxl::read_xlsx(
    path = x,
    sheet = 1,
    skip = 2,
    .name_repair = "unique_quiet") |>
    select(
      fecha = 1,latitud = 4,longitud = 5, ph = 6, cond = 7, secchi = 8,
      sol_tot = 12, sol_sus = 15, turb = 18) |>
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
elimino <- function() {

  unlink("producto/*", recursive = TRUE)
  mensaje("Archivos eliminados")

}

# elimino todos los archivos descargados, confirmación mediante
# elimino <- function() {
#
#   # abro una ventana de confirmación
#   if (askYesNo("¿Eliminar archivos descargados?")) {
#
#     unlink("producto/*", recursive = TRUE)
#     mensaje("Archivos eliminados")
#
#   } else {
#
#     mensaje("Archivos NO eliminados")
#
#   }
# }

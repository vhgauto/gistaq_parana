
# options(viewer = NULL)

# datos -------------------------------------------------------------------

# vector de sitios de muestreo
v <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE) |>
  distinct(fecha, latitud, longitud) |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326")

v_tbl <- as.data.frame(v, geom = "XY") |>
  tibble() |>
  arrange(fecha, desc(x), desc(y))

fechas_v <- unique(v$fecha) |> str_remove_all("-")

# ráster
r_files <- list.files(
  path = "recorte/",
  pattern = stringr::str_flatten(fechas_v, "|"),
  full.names = TRUE)

# creo el stack de bandas y agrego las fechas como nombres
r_list <- map(r_files, raster::stack)

names(r_list) <- fechas_v

# vector de bbox, le quito el nombre a los elementos
e <- ext(rast(r_list[[1]])) |>
  vect(crs = "EPSG:32721") |>
  project("EPSG:4326") |>
  ext() |>
  as.vector()

names(e) <- NULL

# funciones ---------------------------------------------------------------

# etiquetas con las propiedades de los puntos
f_label <- function(fecha_date) {
  d |>
    filter(fecha == ymd(fecha_date)) |>
    mutate(label = glue("{param}: {round(valor, 1)}")) |>
    reframe(
      l = stringr::str_flatten(label, collapse = "\r"),
      .by = c(longitud, latitud)
    ) |>
    pull(l)
}

# escala de colores de los marcadores
f_relleno <- function(fecha_date) {
  n <- d |>
    filter(fecha == ymd(fecha_date)) |>
    distinct(longitud, latitud) |>
    nrow()

  colorRampPalette(c(c1, c2))(n)

}

# función que agrega puntos con su estilo
f_circulo <- function(
    map, fecha, color = c5, opacity = 1, radius = 8) {
  addCircleMarkers(
    map,
    lng = pull(unique(v_tbl[v_tbl$fecha == ymd(fecha), "y"])),
    lat = pull(unique(v_tbl[v_tbl$fecha == ymd(fecha), "x"])),
    color = color,
    stroke = TRUE,
    weight = 1,
    fill = TRUE,
    label = f_label(fecha),
    fillColor = f_relleno(fecha),
    fillOpacity = opacity,
    opacity = opacity,
    group = ymd(fecha),
    radius = radius)
}

# función que agrega ráster en composición RGB
f_rgb <- function(map, fecha, qmin = .03, qmax = .97) {
  addRasterRGB(
    map,
    r_list[[fecha]], r = 4, g = 3, b = 2, quantiles = c(qmin, qmax),
    na.color = NA,
    group = ymd(fecha)
  )
}

# mapa --------------------------------------------------------------------

# coordenadas para centrar el mapa
zoom_lon <- (e[1] + e[2])/2
zoom_lat <- (e[3] + e[4])/2

# logo GISTAQ
# conviene usar una url en lugar de un archivo local, agrego link a Instagram
logo_asp <- 1535/538
logo_ancho <- 100
logo_alto <- round(logo_ancho/logo_asp) # tienen que ser números enteros
logo_link <- "https://raw.githubusercontent.com/vhgauto/sameep/main/extras/gistaq_logo.png"
logo_url <- "https://www.instagram.com/gistaq.utn/"

# mapa base
base <- leaflet() |>
  # posición y zoom del mapa
  setView(lng = zoom_lon, lat = zoom_lat, zoom = 15) |>
  # mapa base
  addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap")

# loop que agregar puntos de muestreo y ráster
for (i in 1:length(fechas_v)) {
  base <- base |>
    f_circulo(fecha = fechas_v[i]) |>
    f_rgb(fecha = fechas_v[i])
}

# mapa
mapa_interactivo <- base |>
  # control de capas
  addLayersControl(
    baseGroups = ymd(fechas_v),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  # logo
  addLogo(
    img = logo_link, position = "bottomleft", src = "remote",
    width = logo_ancho, height = logo_alto, url = logo_url
  ) |>
  # botones
  addResetMapButton() |>
  addFullscreenControl(position = "bottomright") |>
  # muestro la última fecha al iniciar
  showGroup(max(ymd(fechas_v)))


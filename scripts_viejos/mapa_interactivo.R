
# options(viewer = NULL)

# datos -------------------------------------------------------------------

# vector de sitios de muestreo
v <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE) |> 
  distinct(fecha, latitud, longitud) |> 
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326")

fechas_v <- unique(v$fecha) |> str_remove_all("-")

# ráster
r_files <- list.files(
  path = "recorte/", 
  pattern = str_flatten(fechas_v, "|"), 
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

# función que agrega puntos con su estilo
f_circulo <- function(
    map, fecha, color = c7, relleno = c8, opacity = 1, radius = 15) {
  addCircles(
    map,
    lng = terra::geom(v[v$fecha == ymd(fecha)])[, 4],
    lat = terra::geom(v[v$fecha == ymd(fecha)])[, 3],
    color = color, 
    stroke = TRUE,
    weight = 3,
    fill = TRUE,
    fillColor = relleno,
    fillOpacity = 1,
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


# paquetes ----------------------------------------------------------------

library(leaflet)
library(leafem)
library(leaflet.extras)
library(terra)
library(glue)
library(tidyverse)

options(viewer = NULL)

# datos -------------------------------------------------------------------

# vector de sitios de muestreo
v <- read_csv("datos/base_de_datos_lab.csv") |> 
  distinct(fecha, latitud, longitud) |> 
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326")

fechas_v <- unique(v$fecha) |> str_remove_all("-")

# ráster
r_files <- list.files(
  path = "recorte/", 
  pattern = str_flatten(fechas_v, "|"), 
  full.names = TRUE)

r_list <- map(r_files, raster::stack)

names(r_list) <- fechas_v

e <- ext(rast(r_list[[1]])) |> 
  vect(crs = "EPSG:32721") |> 
  project("EPSG:4326") |> 
  ext() |> 
  as.vector()

names(e) <- NULL

# funciones ---------------------------------------------------------------

# función que agrega puntos con su estilo
f_circulo <- function(
    map, fecha, color = "black", relleno = "gold", opacity = 1, radius = 15) {
  addCircles(
    map,
    lng = terra::geom(v[v$fecha == ymd(fecha)])[, 3],
    lat = terra::geom(v[v$fecha == ymd(fecha)])[, 4],
    color = color, 
    stroke = TRUE,
    weight = 3,
    fill = TRUE,
    # layerId = fecha,
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
    # layerId = fecha,
    group = ymd(fecha)
  )
}

# mapa --------------------------------------------------------------------

# coordenadas para centrar el mapa
zoom_lon <- (e[1] + e[2])/2
zoom_lat <- (e[3] + e[4])/2

# logo GISTAQ
# logo_img <- "extras/logo_gistaq.png"
logo_asp <- 1535/538
logo_ancho <- 100
logo_alto <- round(logo_ancho/logo_asp) # tienen que ser números enteros
logo_link <- "https://raw.githubusercontent.com/vhgauto/sameep/main/extras/gistaq_logo.png"
logo_url <- "https://www.instagram.com/gistaq.utn/"


# mapa
mapa_int <- leaflet() |> 
  # posición y zoom del mapa
  setView(lng = zoom_lon, lat = zoom_lat, zoom = 15) |>
  # mapa base
  addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap") |>
  addProviderTiles(provider = "Esri.WorldImagery", group = "Esri.WorldImagery") |>
  # sitios de muestreo
  f_circulo(fecha = fechas_v[1]) |>
  f_circulo(fecha = fechas_v[2]) |>
  f_circulo(fecha = fechas_v[3]) |>
  f_circulo(fecha = fechas_v[4]) |>
  # RGB
  f_rgb(fecha = fechas_v[1]) |>
  f_rgb(fecha = fechas_v[2]) |>
  f_rgb(fecha = fechas_v[3]) |>
  f_rgb(fecha = fechas_v[4]) |>
  # control de capas
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
    overlayGroups = ymd(fechas_v),
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
  # ocultar capas
  hideGroup(ymd(fechas_v))

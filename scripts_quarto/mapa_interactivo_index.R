
# paquetes ----------------------------------------------------------------

library(terra)
library(glue)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(tidyverse)

# datos -------------------------------------------------------------------

v <- read_csv("datos/base_de_datos_gis.csv") |> 
  filter(fecha == max(fecha)) |> 
  distinct(fecha, punto, latitud, longitud) |> 
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326")

fecha <- unique(v$fecha) |> 
  format(x = _, "%Y%m%d")

r <- list.files(path = "recorte/", pattern = fecha, full.names = TRUE) |> 
  stars::read_stars()


e <- ext(rast(r)) |> 
  vect(crs = "EPSG:32721") |> 
  project("EPSG:4326") |> 
  ext() |> 
  as.vector()

names(e) <- NULL

zoom_lon <- (e[1] + e[2])/2
zoom_lat <- (e[3] + e[4])/2

tt <- as.character(ymd(fecha))

logo_img <- "img/logo_gistaq.png"
logo_asp <- 1535/538
logo_ancho <- 100
logo_alto <- logo_ancho/logo_asp

m <- leaflet(v) |> 
  setView(lng = zoom_lon, lat = zoom_lat, zoom = 15) |> 
  
  # addTiles(group = "OpenStreetMap") |>
  addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap") |> 
  addProviderTiles(provider = "Esri.WorldImagery", group = "Esri.WorldImagery") |> 
  addCircles(
    color = "white", opacity = 1, group = "puntos", stroke = FALSE,
    fillOpacity = 1, radius = 10) |> 
  addRasterRGB(
    r, r = 4, g = 3, b = 2, quantiles = c(.05, .98),
    na.color = NA, group = tt, bringToFront = TRUE, options = tileOptions(  zIndex = 100)
  ) |> 
  # https://rstudio.github.io/leaflet/articles/showhide.html
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
    overlayGroups = c(tt, "puntos"),
    options = layersControlOptions(collapsed = FALSE)
  ) |> 
  addLogo(
    img = logo_img, position = "bottomleft", src = "local",
    width = logo_ancho, height = logo_alto) |> 
  addResetMapButton() |> 
  addFullscreenControl(position = "bottomright")


# options(viewer = NULL)

# datos -------------------------------------------------------------------

# nombre de los parámetros para los popups del mapa
label_param <- c(
  ph = "<b>pH</b>",
  cond = "<b>Cond</b>",
  sol_sus = "<b>Sol. susp</b>",
  turb = "<b>Turb</b>",
  secchi = "<b>SDD</b>",
  hazemeter = "<b>Hazem</b>")

# unidades de los parámetros para los popups del mapa
label_unidad <- c(
  ph = "",
  cond = "μS/cm",
  sol_sus = "ppm",
  turb = "NTU",
  secchi = "cm",
  hazemeter = "BC")

# datos de laboratorio
d <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE)

# vector de sitios de muestreo
v_tbl <- read_csv("datos/base_de_datos_lab.csv", show_col_types = FALSE) |> 
  distinct(fecha, longitud, latitud) |> 
  arrange(fecha, longitud, latitud)

fechas_v <- unique(v_tbl$fecha) |> str_remove_all("-")

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

# genera íconos con las formas predeterminadas de R
pchIcons <- function(pch = 0:14, width = 30, height = 30, ...) {
  n <- length(pch)
  files <- character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    png(f, width = width, height = height, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] <- f
  }
  files
}

# etiquetas con las propiedades de los puntos
f_label <- function(fecha_date) {
  labels <- d |> 
    mutate(nombre = label_param[param]) |> 
    mutate(unidad = label_unidad[param]) |> 
    filter(fecha == ymd(fecha_date)) |> 
    mutate(label = glue("{nombre}: {round(valor, 1)} {unidad}")) |> 
    reframe(
      l = str_flatten(label, collapse = "<br>"),
      .by = c(longitud, latitud)
    ) |> 
    pull(l)
  
  filas <- glue(
    "<span style='font-size:13px; font-family: Ubuntu'>{labels}</span>")
  
  p_label <- glue(
    "<span style='font-family: JetBrains Mono; font-size: 16px'>",
    "P{1:length(labels)}</span>")
  
  glue("{p_label}<br>{filas}")
  
}

# escala de colores de los marcadores
f_relleno <- function(fecha_date) {
  n <- d |>
    filter(fecha == ymd(fecha_date)) |>
    distinct(longitud, latitud) |>
    nrow()
  
  colorRampPalette(c(c1, c2))(n)
  
}

# genera los archivos de las formas predeterminadas como marcadores
f_icono <- function(fecha, pch = 21, color = c5, lwd = 1) {
  map(f_relleno(fecha), ~pchIcons(pch, bg = .x, lwd = lwd, col = color)) |> 
    list_c()
}

# función que agrega puntos con su estilo
f_circulo <- function(
    map, fecha_date, color = c5, opacity = 1, radius = 8) {
  addMarkers(
    map,
    lng = pull(unique(v_tbl[v_tbl$fecha == ymd(fecha_date), "longitud"])),
    lat = pull(unique(v_tbl[v_tbl$fecha == ymd(fecha_date), "latitud"])),
    icon = icons(iconUrl = f_icono(fecha_date)),
    popup = f_label(fecha_date),
    
    popupOptions = popupOptions(
      closeButton = FALSE,
      closeOnClick = TRUE,
      offset = c(10, 10),
      fontSize = 100
    ),
    
    group = ymd(fecha_date)
    )
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


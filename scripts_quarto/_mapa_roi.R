
# https://leaflet-extras.github.io/leaflet-providers/preview/

# paquete ----------------------------------------------------------------

library(terra)
library(tidyterra)
library(ggspatial)
library(ggplot2)

# funciones --------------------------------------------------------------

# función que genera vector a partir de un punto central en el puente
# y al ancho y alto de la extensión, en metros
f_roi <- function(ancho, alto = NULL) {

	if (is.null(alto)) {
		alto <- ancho
	}

	centro_sf <- vect(centro, geom = c("lon", "lat"), crs = "EPSG:4326") |>
		project("EPSG:5346") |>
		geom()

	x <- centro_sf[1, 3]
	y <- centro_sf[1, 4]

	e <- ext(x-ancho, x+ancho, y-alto, y+alto) |>
		vect(crs = "EPSG:5346")

	return(e)
}

# devuelve la relación de aspecto del vector roi
f_asp <- function(x){
	e <- ext(x)
	asp <- (e$ymax - e$ymin)/(e$xmax - e$xmin)
	return(asp)
}

# ancho <- 30

# coordenadas del punto centrar del mapa
centro <-data.frame(lon = -58.862000, lat = -27.468983)
centro_sf <- vect(centro, geom = c("lon", "lat"), crs = "EPSG:4326") |>
  project("EPSG:5346")

# figura -----------------------------------------------------------------

# roi y relación de aspecto
roi <- f_roi(1920*4, 1080*4)
asp <- f_asp(roi)

# imagen RGB ESRI
esri <- maptiles::get_tiles(
  x = roi,
  provider = "Esri.WorldImagery",
  zoom = 15,
  crop = TRUE
)

# créditos
cred <- stringr::str_wrap(maptiles::get_credit("Esri.WorldImagery"), 70)

# mapa
g <- ggplot() +
	geom_spatraster_rgb(
		data = esri,
		interpolate = FALSE,
		maxcell = size(esri)
	) +
	annotation_north_arrow(
		location = "tr",
		height = unit(.6, "cm"),
		width = unit(.4, "cm"),
		pad_x = unit(.2, "cm"),
		pad_y = unit(.2, "cm"),
		style = north_arrow_orienteering(
			line_col = "white", text_col = NA, line_width = .4
		)
	) +
	annotate(
		geom = "label", x = I(.99), y = I(.01), hjust = 1, vjust = 0, size = 1.3,
		label = cred, family = "ubuntu", color = "black", fill = alpha("white", .7),
		label.size = unit(0, "mm"), label.r = unit(0, "mm"), fontface = "plain"
	) +
	annotation_scale(
		location = "bl",
		bar_cols = c("black", "white"),
		line_width = .5,
		line_col = "white",
		height = unit(0.2, "cm"),
		pad_x = unit(0.2, "cm"),
		pad_y = unit(0.2, "cm"),
		text_pad = unit(0.15, "cm"),
		text_cex = .7,
		text_col = "white",
		text_family = "ubuntu",
		width_hint = .17
	) +
	coord_sf(expand = FALSE) +
	theme_void()

# guardo
ggsave(
	plot = g,
	filename = "figuras/roi.png",
	dpi = 300,
	width = 1920,
	height = 1080,
	units = "px"
)

# abro
browseURL(paste0(getwd(), "/figuras/roi.png"))

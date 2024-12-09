
# https://leaflet-extras.github.io/leaflet-providers/preview/

# paquete ----------------------------------------------------------------

library(terra)
library(tidyterra)
library(sf)
library(ggtext)
library(patchwork)
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

# datos ------------------------------------------------------------------

# coordenadas del punto centrar del mapa
centro <-data.frame(lon = -58.862000, lat = -27.468983)
centro_sf <- vect(centro, geom = c("lon", "lat"), crs = "EPSG:4326") |>
  project("EPSG:5346")

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
cred <- stringr::str_wrap(maptiles::get_credit("Esri.WorldImagery"), 70) |>
  str_replace_all("\\n", "<br>")

# línea interprovincial
# provincias
arg_sf <- st_read("vector/pcias_continental.gpkg")

# extensión
bb_sf <- st_bbox(esri) |>
	st_as_sfc()

# recorte
lin_interprov <- st_intersection(arg_sf, bb_sf)

l <- st_difference(
	st_cast(lin_interprov, "MULTILINESTRING"),
	st_cast(bb_sf, "MULTILINESTRING")
) |>
	st_geometry()

# puntos para boxplot
p <- sf::st_read("vector/3puntos_transecta.gpkg")

# figura -----------------------------------------------------------------

# mapa Argentina
g_arg <- ggplot() +
	geom_sf(
		data = arg_sf, color = c7, fill = c3, linewidth = .05
	) +
	geom_sf(
		data = centro_sf, fill = c5, shape = 21, color = c7, size = 1, stroke = .5
	) +
	geom_sf(
		data = centro_sf, fill = c5, shape = 21, color = c7, size = .3, stroke = .3
	) +
	coord_sf(expand = TRUE) +
	theme_void() +
	theme(
		plot.background = element_rect(
			color = NA, fill = alpha(c9, .6)
		)
	)

# relación de aspecto de Argentina
asp_arg <- (7588953 - 3891909)/(5437928 - 3736422)

# guardo
ggsave(
	plot = g_arg,
	filename = "figuras/arg.png",
	dpi = 300,
	width = 200,
	height = round(200*asp_arg),
	units = "px"
)

# leo la figura
png_arg <- png::readPNG("figuras/arg.png", native = TRUE)

# mapa ROI
g_roi <- ggplot() +
	# mapa RGB
	geom_spatraster_rgb(
		data = esri,
		interpolate = FALSE,
		maxcell = size(esri)
	) +
	# límite interprovincial
	geom_sf(
		data = l, linewidth = .2, linetype = 2, color = alpha(c5, .5)
	) +
  # 3 puntos
  geom_sf(
    data = p, aes(fill = punto), shape = 21, size = 1.3, stroke = .3,
    show.legend = FALSE
  ) +
  geom_sf_text(
    data = p, aes(label = punto), size = 1.3, nudge_y = -160, color = c5,
    family = "jet", show.legend = FALSE
  ) +
	# crédito del mapa base
	annotate(
		geom = "richtext", x = I(1), y = I(0), hjust = 1, vjust = 0, size = 1.6,
		label = cred, family = "ubuntu", color = c5, fill = c4,
		label.color = NA, label.r = unit(0, "mm"),
    label.padding = unit(.1, "lines")
	) +
	# Chaco
	annotate(
		geom = "richtext", x = I(.32), y = I(.01), hjust = .5, vjust = 0, size = 2,
		label = "Chaco", family = "ubuntu", color = c5,
		fill = c4, label.color = NA, label.r = unit(0, "mm")
	) +
	# Corrientes
	annotate(
		geom = "richtext", x = I(.42), y = I(.01), hjust = .5, vjust = 0, size = 2,
		label = "Corrientes", family = "ubuntu", color = c5,
		fill = c4, label.color = NA, label.r = unit(0, "mm")
	) +
	annotation_north_arrow(
		location = "tr",
		height = unit(.5, "cm"),
		width = unit(.3, "cm"),
		pad_x = unit(.2, "cm"),
		pad_y = unit(.2, "cm"),
		style = north_arrow_orienteering(
			line_col = c5, text_col = NA, line_width = .4
		)
	) +
	annotation_scale(
		location = "bl",
		bar_cols = c(c7, c5),
		line_width = .5,
		line_col = c5,
		height = unit(0.1, "cm"),
		pad_x = unit(0.2, "cm"),
		pad_y = unit(0.2, "cm"),
		text_pad = unit(0.15, "cm"),
		text_cex = .5,
		text_col = c5,
		text_family = "ubuntu",
		width_hint = .17
	) +
  scale_fill_manual(
    values = c(c1, c5, c2)
  ) +
	coord_sf(expand = FALSE) +
	theme_void()

# composición del mapa
g_mapa <- g_roi + inset_element(
	p = png_arg,
	left = .01,
	bottom = .6,
	right = .11,
	top = .999
) &
theme_void()

# guardo
ggsave(
	plot = g_mapa,
	filename = "figuras/roi.png",
	dpi = 300,
	width = 1920,
	height = 1080,
	units = "px"
)

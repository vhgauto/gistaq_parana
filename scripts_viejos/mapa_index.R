
# paquetes ----------------------------------------------------------------

library(terra)
library(tidyterra)
library(ggspatial)
library(glue)
library(tidyverse)

# datos -------------------------------------------------------------------

v <- read_csv("datos/base_de_datos_gis.csv") |> 
  filter(fecha == max(fecha)) |> 
  distinct(fecha, punto, latitud, longitud) |> 
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326")

fecha <- unique(v$fecha) |> 
  format(x = _, "%Y%m%d")

r <- list.files(path = "recorte/", pattern = fecha, full.names = TRUE) |> 
  rast()

g <- ggplot() +
  geom_spatraster_rgb(
    data = r, r = 4, g = 3, b = 2, interpolate = FALSE, max_col_value = 5e3
  ) +
  geom_spatvector(
    data = v, color = "white", size = 2, shape = 4
  ) +
  geom_sf_text(
    data = v, aes(label = glue("P{punto}")), nudge_y = -40, color = "white", 
    family = "serif", face = "bold"
  ) +
  annotation_north_arrow(
    location = "tr", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(
    location = "bl", width_hint = .2, pad_x = unit(1, "cm"), 
    pad_y = unit(1, "cm"), text_col = "white"
  ) +
  labs(title = ymd(fecha)) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = .5, size = 20
    )
  )

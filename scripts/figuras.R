

# paquetes ----------------------------------------------------------------

library(tidyverse)

# datos -------------------------------------------------------------------

bandas <- c(
  "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12"
)

gis <- read_csv("datos/base_de_datos_gis.csv") |> 
  mutate(banda = fct(banda, bandas)) |> 
  mutate(punto = factor(punto))


gis |> 
  dplyr::filter(fecha == ymd(20230511)) |> 
  dplyr::filter(banda == "B01" & punto == 1)




gis |> 
  dplyr::filter(fecha == ymd(distinct(gis, fecha)$fecha[3])) |> 
  ggplot(
  aes(
    banda, reflect, group = paste(pixel, punto), linetype = pixel,
    color = punto)) +
  geom_line(show.legend = TRUE) +
  guides(
    color = guide_none()
  ) +
  facet_wrap(vars(punto), nrow = 2, scales = "free") +
  theme(
    aspect.ratio = 1
  )


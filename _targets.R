library(targets)

# paquetes
tar_option_set(packages = c("glue", "terra", "tidyverse"))

# funciones
source("scripts_targets/funciones.R")

# targets
list(
  # archivo Excel, con la fecha y sitios de muestreo
  tar_target(
    name = excel,
    command = archivo_excel(),
    format = "file"
  ),

  # manuscrito
  tar_target(
    name = manuscrito,
    command = archivo_quarto(),
    format = "file"
  ),

  # fecha del último muestreo
  tar_target(
    name = fecha_actual,
    command = fecha(archivo_i = excel)
  ),

  # recorto el producto al área de interés
  tar_target(
    name = recorte_tif,
    command = recorte(fecha_i = fecha_actual),
    format = "file"
  ),

  # extraigo los valores de píxel
  tar_target(
    name = datos_gis,
    command = reflectancia(fecha_i = fecha_actual, archivo_i = excel),
    format = "file"
  ),

  # extraigo parámetros de laboratorio
  tar_target(
    name = datos_lab,
    command = lab(excel),
    format = "file"
  ),

  # creo manuscrito
  tarchetypes::tar_quarto(
    name = render_manuscrito,
    path = "."
  )
)

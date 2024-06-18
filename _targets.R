library(targets)

# paquetes
tar_option_set(packages = c("glue", "terra", "sf", "tidyverse"))

# funciones
source("scripts_targets/funciones.R")

# cue = tar_cue(mode = "always")

# browseURL("https://raps-with-r.dev/targets.html")

# targets
list(
  # archivo Excel, con la fecha y sitios de muestreo
  tar_target(
    name = excel, 
    command = archivo_excel(), 
    format = "file"),
  
  # fecha para la descarga de producto
  tar_target(
    name = fecha_descarga, 
    command = fecha(excel)),
  
  # script para la descarga de producto
  tar_target(
    name = script_py, 
    command = script_descarga_py(fecha_descarga), 
    format = "file"),
  
  # ejecuto la descarga del producto
  tar_target(
    name = producto_zip, 
    command = descarga(script_py)),
  
  # recorto el producto al área de interés
  tar_target(
    name = recorte_tif, 
    command = recorte(producto_zip, fecha_descarga), 
    format = "file"),
  
  # extraigo los valores de píxel
  tar_target(
    name = datos_gis, 
    command = reflectancia(excel, fecha_descarga, recorte_tif), 
    format = "file"),
  
  # extraigo parámetros de laboratorio
  tar_target(
    name = datos_lab, 
    command = lab(excel), 
    format = "file")
)


# paquetes ----------------------------------------------------------------

library(corrr)
library(ggtext)
library(glue)
library(gt)
library(patchwork)
library(showtext)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(terra)
library(ggiraph)
library(tidyverse)

# colores -----------------------------------------------------------------

c1 <- "#377EB8"
c2 <- "#E41A1C"
c3 <- "#F2F2F2" # "grey95"
c4 <- "#4D4D4D" # "grey30"
c5 <- "white"
c6 <- "#FAFAFA" # "grey98"
c7 <- "black"
c8 <- "gold"

# fuentes -----------------------------------------------------------------

# para elementos interactivos de las figuras con {ggiraph}
# JetBrains Mono
systemfonts::register_font(
  name = "jet",
  plain = "extras/JetBrainsMonoNLNerdFontMono-Regular.ttf"
)

# Ubuntu
systemfonts::register_font(
  name = "ubuntu",
  plain = "extras/Ubuntu-Regular.ttf"
)

# para el texto estático de las figuras con {ggplot2}
font_add(
  family = "ubuntu",
  regular = "extras/Ubuntu-Regular.ttf"
)

font_add(
  family = "jet",
  regular = "extras/JetBrainsMonoNLNerdFontMono-Regular.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# contacto ----------------------------------------------------------------

# íconos de Nerd Fonts, https://www.nerdfonts.com/cheat-sheet
icono_instagram <- "<span class='nf nf-fa-instagram'></span>"

icono_facebook <- "<span class='nf nf-fa-facebook'></span>"

icono_mail <- "<span class='nf nf-cod-mail'></span>"

icono_github <- "<span class='nf nf-md-github'></span>"

icono_flecha <- "<span class='nf nf-fa-circle_arrow_up'></span>"

icono_circulo <- "<span class='nf nf-fa-circle'></span>"

icono_triangulo <- "<span class='nf nf-md-triangle'></span>"

# enlaces a redes sociales y mails
link_instagram <- "https://www.instagram.com/gistaq.utn/"

link_facebook <- "https://www.facebook.com/GISTAQ"

link_mail_vhg <- "mailto:victor.gauto@outlook.com"

link_mail_gistaq <- "mailto:gistaq@ca.frre.utn.edu.ar"

link_github_vhg <- "https://github.com/vhgauto"

link_github_gistaq <- "https://github.com/vhgauto/gistaq_parana"

# fecha de actualización
actualizado <- format(now(), "%d/%m/%Y %H:%M")

actualizado_label <- glue(
  "<p style='font-family:JetBrains Mono; color:{c4};text-align:right'>",
  "{actualizado}</p>")

caption_tabla <- glue(
  "<span style='font-family:JetBrains Mono'>",
  "<br>$\\star$ = p-valor < 0,05<br>",
  "<b style='color:{c2};'>R</b> = |<b>R</b>| > 0,5</span>"
)

# scripts -----------------------------------------------------------------

r <- list.files(path = "scripts_quarto/", full.names = TRUE)
r <- r[!str_detect(r, "soporte")]
purrr::map(r, source)

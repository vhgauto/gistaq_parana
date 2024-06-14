
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
c4 <- "grey30"
c5 <- "white"
c6 <- "#FAFAFA" # "grey98"

# contacto ----------------------------------------------------------------

# íconos de Nerd Fonts, https://www.nerdfonts.com/cheat-sheet
icono_instagram <- glue('<span class="nf nf-fa-instagram"></span>')

icono_facebook <- glue('<span class="nf nf-fa-facebook"></span>')

icono_mail <- glue('<span class="nf nf-cod-mail"></span>')

icono_github <- glue('<span class="nf nf-md-github"></span>')

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

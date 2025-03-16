
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

c1 <- "#B86092" # violeta
c2 <- "#007E2E" # verde
c3 <- "#F2F2F2" # "grey95"
c4 <- "#4D4D4D" # "grey30"
c5 <- "#FFFFFF" # "white"
c6 <- "#FAFAFA" # "grey98"
c7 <- "#000000" # "black"
c8 <- "#FFD700" # "gold"
c9 <- "#7F7F7F" # "grey50"
c10 <- "#F5F5F5" # "grey96"
c11 <- "#EDEDED" # "grey93"

# fuentes -----------------------------------------------------------------

# para elementos interactivos de las figuras con {ggiraph}
# JetBrains Mono
systemfonts::register_font(
  name = "jet",
  plain = "fuentes/JetBrainsMonoNLNerdFontMono-Regular.ttf"
)

# Ubuntu
systemfonts::register_font(
  name = "ubuntu",
  plain = "fuentes/Ubuntu-Regular.ttf"
)

# para el texto estático de las figuras con {ggplot2}
font_add(
  family = "ubuntu",
  regular = "fuentes/Ubuntu-Regular.ttf"
)

font_add(
  family = "jet",
  regular = "fuentes/JetBrainsMonoNLNerdFontMono-Regular.ttf"
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

link_facebook <- "https://www.facebook.com/GISTAQ/"

link_mail_vhg <- "mailto:victor.gauto@outlook.com"

link_mail_gistaq <- "mailto:gistaq@ca.frre.utn.edu.ar"

link_github_vhg <- "https://github.com/vhgauto"

link_github_gistaq <- "https://github.com/vhgauto/gistaq_parana"

# documento --------------------------------------------------------------

# símbolo utilizado para las correlaciones que sean significativas
simbolo_sig <- glue("<span style='color: {c1}'>&#9733;</span>") # estrella

# separación entre logos de insticiones
espacios <- \(x = 15) stringr::str_flatten(rep("&nbsp;", x))

# fecha de actualización
actualizado <- format(now(), "%d/%m/%Y %H:%M")

actualizado_label <- glue(
  "<p style='font-family:JetBrains Mono; color:{c9};text-align:right'>",
  "{actualizado}</p>")

# descripción de las tablas, con p-valor < .05 & |R| > .5
caption_tabla <- glue(
  "<br>{simbolo_sig} = p-valor < 0,05<br>",
  "<b style='color:{c2};'>R</b> = |<b>R</b>| > 0,5"
)

# caption adquisición
caption_adq <- glue(
  "<b style='color:{c2};'>Fecha actual</b> y de los próximos 10 ",
  "muestreos."
)

# caption boxplot
caption_boxplot <- glue(
  "<br><br><br>{simbolo_sig} = p-valor < 0,05<br><br>",
  "Existe diferencia significativa entre los grupos."
)

# funciones ---------------------------------------------------------------

# función que aplica formato a los números de R
f_formato <- function(x, digits = 3, nsmall = 3) {
  format(
    x, digits = digits, nsmall = nsmall, trim = TRUE, decimal.mark = ",",
    big.mark = ".")
}

# función que obtiene el pvalor de las correlaciones
f_pvalor <- function(vec_a, vec_b) {
  cor.test(vec_a, vec_b)$p.value
}

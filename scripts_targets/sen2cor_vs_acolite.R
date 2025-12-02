library(terra)
library(tidyverse)

bandas_nombres <- c(
  "B01",
  "B02",
  "B03",
  "B04",
  "B05",
  "B06",
  "B07",
  "B08",
  "B8A",
  "B11",
  "B12"
)

d_sen2cor <- read_csv(
  "datos/base_de_datos_gis_sen2cor.csv",
  show_col_types = FALSE
)
d_acolite <- read_csv(
  "datos/base_de_datos_gis_acolite.csv",
  show_col_types = FALSE
)

fechas_muestreos <- unique(d_acolite$fecha)

f_vs <- function(i) {
  bind_rows(sen2cor = d_sen2cor, acolite = d_acolite, .id = "tipo") |>
    mutate(banda = factor(banda, levels = bandas_nombres)) |>
    filter(fecha == ymd(fechas_muestreos[i])) |>
    ggplot(aes(
      banda,
      reflect,
      linetype = tipo,
      group = tipo
    )) +
    geom_line() +
    facet_wrap(vars(punto), scale = "fixed", axes = "all") +
    labs(
      title = ymd(fechas_muestreos[i]),
      x = NULL,
      y = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 12, base_family = "JetBrains Mono") +
    theme(
      aspect.ratio = 1,
      legend.position = "top"
    )
}

f_vs(5)

acolite_l <- list.files("recorte_acolite/", pattern = "tif", full.names = TRUE)
sen2cor_l <- list.files("recorte_sen2cor/", pattern = "tif", full.names = TRUE)

acolite_r <- map(acolite_l, rast)
sen2cor_r <- map(sen2cor_l, rast)

names(acolite_r) <- gsub("-", "", fechas_muestreos)
names(sen2cor_r) <- gsub("-", "", fechas_muestreos)

f_mndwi <- function(A) {
  a <- (A$B03 - A$B11) / (A$B03 + A$B11)
  b <- terra::thresh(a, method = "mean")
  b[b == 0] <- NA
  return(b)
}

acolite_mndwi <- map(acolite_r, f_mndwi)
sen2cor_mndwi <- map(sen2cor_r, f_mndwi)

mascara <- map2(acolite_mndwi, sen2cor_mndwi, ~ .x * .y)

plot(mascara[[1]], legend = FALSE, axes = FALSE)

acolite_agua <- map2(acolite_r, mascara, ~ .x * .y)
sen2cor_agua <- map2(sen2cor_r, mascara, ~ .x * .y / 10000)

plot(acolite_agua[[1]], legend = TRUE, axes = FALSE)
plot(sen2cor_agua[[1]], legend = TRUE, axes = FALSE)

f_tbl <- function(X) {
  terra::as.data.frame(X, xy = TRUE, wide = FALSE) |>
    as_tibble()
}

acolite_tbl <- map(acolite_agua, f_tbl) |>
  list_rbind(names_to = "fecha") |>
  mutate(fecha = ymd(fecha)) |>
  rename(banda = layer, reflect_acolite = values)

sen2cor_tbl <- map(sen2cor_agua, f_tbl) |>
  list_rbind(names_to = "fecha") |>
  mutate(fecha = ymd(fecha)) |>
  rename(banda = layer, reflect_sen2cor = values)

d <- inner_join(acolite_tbl, sen2cor_tbl, by = join_by(fecha, x, y, banda)) |>
  mutate(banda = factor(banda, bandas_nombres)) |>
  select(-x, -y)

d_mod <- d |>
  nest(.by = banda, .key = "datos") |>
  mutate(
    mod = map(.x = datos, ~ lm(reflect_acolite ~ reflect_sen2cor, data = .x))
  ) |>
  mutate(
    r2 = map_dbl(mod, ~ broom::glance(.x)$r.squared)
  ) |>
  mutate(
    ordenada = map_dbl(mod, ~ broom::tidy(.x)$estimate[1]),
    pendiente = map_dbl(mod, ~ broom::tidy(.x)$estimate[2])
  ) |>
  select(banda, r2, ordenada, pendiente)

f_gg_banda <- function(B) {
  d |>
    filter(as.numeric(banda) == B) |>
    ggplot(aes(reflect_sen2cor, reflect_acolite)) +
    geom_point(alpha = .1) +
    geom_abline(linetype = 2) +
    geom_abline(
      data = filter(d_mod, as.numeric(banda) == B),
      aes(slope = pendiente, intercept = ordenada)
    ) +
    facet_wrap(vars(banda)) +
    theme_classic(base_family = "JetBrains Mono", base_size = 15) +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank()
    )
}

f_gg_banda(9)

# ggplot(d, aes(reflect_sen2cor, reflect_acolite)) +
#   geom_point() +
#   geom_abline(linetype = 2) +
#   facet_wrap(vars(banda)) +
#   theme(
#     aspect.ratio = 1
#   )

d |>
  mutate(dif = reflect_sen2cor - reflect_acolite) |>
  mutate(banda = fct_reorder(banda, dif)) |>
  ggplot(aes(dif, banda)) +
  geom_boxplot(outlier.alpha = .1) +
  labs(x = "sen2cor - acolite", y = NULL) +
  theme_minimal(base_size = 15, base_family = "JetBrains Mono") +
  theme(
    aspect.ratio = 1
  )

d |>
  mutate(dif = reflect_sen2cor - reflect_acolite) |>
  ggplot(aes(dif)) +
  geom_histogram(binwidth = .02) +
  facet_wrap(vars(banda), axes = "all_x") +
  scale_x_continuous(
    breaks = scales::breaks_width(.1),
    labels = scales::label_number(drop0trailing = TRUE, flag = "0")
  ) +
  theme_bw(base_size = 15, base_family = "JetBrains Mono") +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank()
  )

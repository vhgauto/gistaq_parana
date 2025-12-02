library(tidyverse)

e <- list.files(
  "datos/barrido_20251126/",
  full.names = TRUE,
  pattern = "Sample.Raw.csv"
)

read_delim(e, delim = " ", id = "punto", show_col_types = FALSE) |>
  mutate(punto = basename(punto)) |>
  mutate(punto = sub(".Sample.Raw.csv", "", punto)) |>
  rename(nm = nm., abs = A) |>
  mutate(nm = str_remove(nm, ".$")) |>
  mutate(nm = as.numeric(nm)) |>
  filter(punto != "blanco") |>
  mutate(punto = sub("P", "", punto)) |>
  mutate(punto = as.numeric(punto)) |>
  ggplot(aes(nm, abs, group = punto, color = punto)) +
  # geom_line() +
  # geom_point(size = .1) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = .2) +
  scale_color_gradient(low = "darkred", high = "green") +
  guides(color = guide_bins(show.limits = TRUE)) +

  # coord_cartesian(xlim = c(650, 700), ylim = c(-.1, .1)) +

  theme_classic(base_size = 15) +
  theme_sub_legend(text = element_text(size = 20))

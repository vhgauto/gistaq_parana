# implementación del algoritmo de Dogliotti
# 
# A single algorithm to retrieve turbidity from remotely-sensed data in all 
# coastal and estuarine waters
# 
# 10.1016/j.rse.2014.09.020

# paquetes ----------------------------------------------------------------

library(glue)
library(tidyverse)
library(conflicted)

theme_set(
  theme_linedraw() +
    theme(
      aspect.ratio = 1
    )
)

# datos -------------------------------------------------------------------

f_datos <- function(csv) {
  
  tbl <- read_delim(
    file = csv, 
    delim = ";",
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    skip = 3) |> 
    select(dia = 1, hora = 2, turb = 6) |> 
    fill(dia) |> 
    mutate(dia = dmy(dia)) |> 
    mutate(hora = str_trim(hora)) |> 
    mutate(fecha = ymd_hm(glue("{dia} {hora}")))
  
  if (is.character(tbl$turb)) {
    tbl <- tbl |> 
      mutate(turb = parse_number(turb, locale = locale(grouping_mark = ".", decimal_mark = ",")))
  }
  
  return(tbl)
}

lista_csv <- glue(
  "datos/Antecedentes calidad agua cruda-GISTAQ5_{1:5}.csv")

sameep <- map(lista_csv, f_datos) |> 
  list_rbind() |> 
  drop_na()

ggplot(sameep, aes(fecha, turb)) +
  geom_line()

banda_orden <- c(
  "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")

gis_tidy <- read_tsv("datos/base_de_datos_gis_sameep.tsv") |> 
  rename(dia = fecha, reflect = reflec) |> 
  # mutate(id = row_number()) |> 
  pivot_wider(
    names_from = banda,
    values_from = reflect
  ) |> 
  unnest(everything()) |> 
  dplyr::filter(B01 < .15) |> 
  pivot_longer(
    cols = starts_with("B"),
    names_to = "banda",
    values_to = "reflect"
  ) |> 
  mutate(banda = fct(banda, levels = banda_orden))

d <- inner_join(
  sameep, 
  gis_tidy, 
  by = join_by(dia),
  relationship = "many-to-many") |> 
  rename(fecha_sameep = fecha) |> 
  mutate(
    fecha_s2 = ymd_hm(glue("{dia} 11:00")), 
    .after = fecha_sameep) |> 
  mutate(d = abs(fecha_sameep - fecha_s2)) |> 
  dplyr::filter(
    d == min(d),
    .by = c(dia)
  ) |> 
  select(dia, turb, banda, reflect) |> 
  mutate(rango = cut_interval(turb, n = 7))

ggplot(d, aes(banda, reflect, color = rango, group = interaction(turb, dia))) +
  geom_line() +
  scale_color_viridis_d(option = "turbo")

# algoritmo ---------------------------------------------------------------

m <- (1-0)/(.07-.05)

f_turb <- function(B04, B8A) {
  
  if (B04 < .05) {
    
    cte_a <- 228.1
    cte_c <- .1641
    
    turb <- (cte_a*B04)/(1-B04/cte_c)
  } else {
    
    if (B04 >= .05 & B04 <= .07) {
      
      turb_645 <- (228.1*B04)/(1-B04/.1641)
      turb_859 <- (3078.9*B8A)/(1-B8A/.2112)
      w <- m*(B04-.05)
      
      turb <- (1-w)*turb_645 + w*turb_859
      
    } else {
      
      cte_a <- 3078.9
      cte_c <- .2112
      
      turb <- (cte_a*B8A)/(1-B8A/cte_c)
      
    } 
  }
  
  return(turb)
  
}

f_turb_B04 <- function(B04) {
  cte_a <- 228.1
  cte_c <- .1641
  
  turb <- (cte_a*B04)/(1-B04/cte_c)
  
  return(turb)
}

f_turb_B8A <- function(B8A) {
  cte_a <- 3078.9
  cte_c <- .2112
  
  turb <- (cte_a*B8A)/(1-B8A/cte_c)
  
  return(turb)
}

d |> 
  select(-rango) |> 
  dplyr::filter(banda == "B04" | banda == "B8A")
  




t_dogliotti <- gis |> 
  mutate(turb = map2(.x = B04, .y = B8A, ~ f_turb(B04 = .x, B8A = .y))) |> 
  unnest(turb) |> 
  mutate(turb_b04 = f_turb_B04(B04)) |> 
  mutate(turb_b8a = f_turb_B8A(B8A)) |> 
  pivot_longer(
    cols = starts_with("turb"),
    names_to = "tipo",
    values_to = "valor"
  ) |> 
  pivot_longer(
    cols = B04:B8A,
    names_to = "banda",
    values_to = "reflect"
  ) |> 
  dplyr::filter(valor > 0)

ggplot(t_dogliotti, aes(reflect, valor, color = tipo)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  theme(
    aspect.ratio = 1
  )


#
#
#
#
#
#
#
#
#
#


m <- (1-0)/(.07-.05)

f_turb <- function(B04, B8A) {
  
  if (B04 < .05) {
    
    cte_a <- 228.1
    cte_c <- .1641
    
    turb <- (cte_a*B04)/(1-B04/cte_c)
  } else {
    
    if (B04 >= .05 & B04 <= .07) {
      
      turb_645 <- (228.1*B04)/(1-B04/.1641)
      turb_859 <- (3078.9*B8A)/(1-B8A/.2112)
      w <- m*(B04-.05)
      
      turb <- (1-w)*turb_645 + w*turb_859
      
    } else {
      
      cte_a <- 3078.9
      cte_c <- .2112
      
      turb <- (cte_a*B8A)/(1-B8A/cte_c)
      
    } 
  }
  
  return(turb)
  
}


t_tbl <- tibble(
  B04 = seq(1e-4, 1e0, length.out = 1000),
  B8A = seq(1e-4, 1e0, length.out = 1000)
) |> 
  mutate(turb = map2(.x = B04, .y = B8A, ~ f_turb(B04 = .x, B8A = .y))) |> 
  unnest(turb) |> 
  mutate(turb_b04 = f_turb_B04(B04)) |> 
  mutate(turb_b8a = f_turb_B8A(B8A)) |> 
  pivot_longer(
    cols = starts_with("turb"),
    names_to = "tipo",
    values_to = "valor"
  ) |> 
  pivot_longer(
    cols = B04:B8A,
    names_to = "banda",
    values_to = "reflect"
  ) |> 
  dplyr::filter(valor > 0)


ggplot(
  t_tbl, 
  aes(reflect, valor, color = tipo, linewidth = tipo, alpha = tipo)) +
  geom_line() +
  geom_vline(
    xintercept = c(.05, .07), linetype = 2, 
    color = c("darkred", "darkblue"), alpha = .6) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("black", "red", "blue")) +
  scale_linewidth_manual(values = c(1, 2, 2)) +
  scale_alpha_manual(values = c(1, .25, .25)) +
  coord_cartesian(
    xlim = c(1e-4, 1e0), ylim = c(1e-1, 1e4), expand = FALSE) +
  theme_linedraw() +
  theme(
    aspect.ratio = 1
  )



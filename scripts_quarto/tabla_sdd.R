
# datos ------------------------------------------------------------------

# elementos de la tabla, línea por línea
eq1 <- "$$A\\left[ \\frac{R_{rs}(\\lambda_{1})}{R_{rs}(\\lambda_{2})} \\right]^{B}$$"
eq2 <- "$$e^{a\\left[ \\frac{R_{rs}(\\lambda_{1})}{R_{rs}(\\lambda_{2})} \\right]+b}$$"
eq3 <- "$$A\\left[ \\frac{R_{rs}(\\lambda_{1})}{R_{rs}(\\lambda_{2})} \\right]^{B}$$"
eq4 <- "$$1,79-134,15\\cdot B_{RE1}+157,72\\cdot B_{NIR}+0,52\\frac{B_{RE3}}{NIR_{n}}$$"
eq5 <- "$$\\log(SD)=-3,0257-1,4379\\cdot \\log(R_{B08A})-0,127\\left[\\log(R_{B08A})\\right]^{2}$$"

banda1 <- "409 (B02)<br>709 (B05)"
banda2 <- "409 (B02)<br>709 (B05)"
banda3 <- "409 (B02)<br>560 (B03)"
banda4 <- ""
banda5 <- ""

met1 <- "R<sup>2</sup>, RMSE"
met2 <- "R<sup>2</sup>, RMSE, Bias"
met3 <- "R<sup>2</sup>, RMSE"
met4 <- "R<sup>2</sup>, RMSE"
met5 <- "R<sup>2</sup>"

agua1 <- "Ópticamente complejas"
agua2 <- "Interiores variadas"
agua3 <- "Embalses"
agua4 <- "Reserva"
agua5 <- "Oceános"

plat1 <- "MERIS"
plat2 <- "Sentinel-2"
plat3 <- "Sentinel-2"
plat4 <- "Sentinel-2"
plat5 <- "Sentinel-2"

aut1 <- "@Alikas2017"
aut2 <- "@Pereira-Sandoval2019"
aut3 <- "@Delegido2019"
aut4 <- "@Bonansea2019"
aut5 <- "@Delegido2016"

nota1 <- "^[Señalan una mala correlación para el cociente 409/560 (B02/B03).]"
nota2 <- "^[Modificación de Alikas-Kratzer. Señala que el previamente descartado cociente 409/560 se usa para aguas donde el contribuyente principal a la turbidez es el fitoplancton.]"
nota3 <- "^[Señala Polymer y C2X como los mejores algoritmos de corrección atmosférica. Indica que su ecuación es equivalente a la de Pereira-Sandoval.]"
nota4 <- "^[Corrección atmosférica con el módulo Sen2Cor. Al ser lineal, produce una sobrestimación para valores bajos y una subestimación para valores altos.]"
nota5 <- "^[Corrección con Sen2Cor.]"

agua_tbl <- c(
  glue("{agua1} {nota1}"),
  glue("{agua2} {nota2}"),
  glue("{agua3} {nota3}"),
  glue("{agua4} {nota4}"),
  glue("{agua5} {nota5}")
)

# tabla ------------------------------------------------------------------

tabla_sdd <- tibble(
  Ecuación = c(eq1, eq2, eq3, eq4, eq5),
  `Bandas (nm)` = c(banda1, banda2, banda3, banda4, banda5),
  Métricas = c(met1, met2, met3, met4, met5),
  Aguas = agua_tbl,
  Plataforma = c(plat1, plat2, plat3, plat4, plat5),
  Referencia = c(aut1, aut2, aut3, aut4, aut5)  
) |> 
  gt() |> 
  fmt_markdown() |> 
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = Referencia)
  ) |> 
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  cols_width(
    Referencia ~ px(20)
  ) |> 
  as_raw_html()

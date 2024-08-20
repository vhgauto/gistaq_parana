
# datos ------------------------------------------------------------------

# elementos de la tabla, línea por línea
eq1 <- "$$A\\left[ \\frac{R_{rs}(\\lambda_{1})}{R_{rs}(\\lambda_{2})} \\right]^{B}$$"
eq2 <- "$$e^{a\\left[ \\frac{R_{rs}(\\lambda_{1})}{R_{rs}(\\lambda_{2})} \\right]+b}$$"
eq4 <- "$$1,79-134,15B_{RE1}+157,72B_{NIR}+0,52\\frac{B_{RE3}}{NIR_{n}}$$"
eq5 <- "$$\\log(SD)=-3,0257-1,4379\\log(R_{B08A})-0,127\\left[\\log(R_{B08A})\\right]^{2}$$"

banda1 <- "409 (B02)<br>709 (B05)"
banda3 <- "409 (B02)<br>560 (B03)"

met1 <- "$$R^{2} \\\\ RMSE$$"
met2 <- "$$R^{2} \\\\ RMSE \\\\ Bias$$"
met5 <- "$$R^{2}$$"

agua1 <- "Ópticamente complejas"
agua2 <- "Interiores variadas"
agua3 <- "Embalses"
agua4 <- "Reserva"
agua5 <- "Oceános"

plat1 <- "MERIS"
plat2 <- "Sentinel-2"

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

# tabla ------------------------------------------------------------------

tabla_sdd <- tibble(
  Ecuación = c(eq1, eq2, eq1, eq4, eq5),
  `Bandas (nm)` = c(banda1, banda1, banda3, "", ""),
  Métricas = c(met1, met2, met1, met1, met5),
  Aguas = c(agua1, agua2, agua3, agua4, agua5),
  Plataforma = c(plat1, plat2, plat2, plat2, plat2),
  Autor = c(aut1, aut2, aut3, aut4, aut5),
  " " = c(nota1, nota2, nota3, nota4, nota5)
) |> 
  gt() |> 
  fmt_markdown() |> 
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = Autor)
  ) |> 
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) |> 
  cols_width(
    Autor ~ px(20),
    " " ~ px(20)
  ) |> 
  as_raw_html()

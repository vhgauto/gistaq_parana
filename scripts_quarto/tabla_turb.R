
# datos ------------------------------------------------------------------

# elementos de la tabla, línea por línea
eq1 <- "$$1,559e^{35,533B03} \\\\ 1,879e^{37,745(B03\\cdot B5)/(B04+B12)}$$"
eq2 <- "$$2677,2B04^{1,856}$$"
eq3 <- "$$969-1,5468\\cdot R_{1200nm}+2,07\\frac{B8A}{B02}$$"
eq4 <- "$$y=-1,1+5,8\\frac{B02}{B04} \\\\ y=3,896-4,186\\frac{B02}{B03}$$"
eq5 <- "$$y=37661B8A^{2}+1845\\cdot B8A \\\\ y=531,5-\\frac{B04}{0,88}$$"

banda1 <- "B03, B04, B05, B12"
banda2 <- "B04"
banda3 <- "B02, B8A, 1200nm"
banda4 <- "B02, B03, B04"
banda5 <- "B04, B8A"

met1 <- "R<sup>2</sup>, RMSE, MAE"
met2 <- "R<sup>2</sup>, RMSE, Bias"
met3 <- "IOA, SI, RMSE, MAE"
met4 <- "R<sup>2</sup>, RMSE"
met5 <- "R<sup>2</sup>, RMSE, MAPE"

agua1 <- "Lago"
agua2 <- "Interiores variadas"
agua3 <- "Río"
agua4 <- "Río"
agua5 <- "Estuario"

plat1 <- "Sentinel-2"
plat2 <- "Landsat-8"
plat3 <- "Landsat-8"
plat4 <- "Landsat-8"
plat5 <- "Pléiades"

aut1 <- "@Ma2021"
aut2 <- "@Hossain2021"
aut3 <- "@Najafzadeh2023"
aut4 <- "@Allam2020"
aut5 <- "@Luo2020"

nota1 <- "^[0,83 - 112,26 NTU.]"
nota2 <- "^[2,3 - 107,02 NTU.]"
nota3 <- "^[IOA = index of agreement<br>SI = scatter index.]"
nota4 <- "^[20,6 - 112 NTU<br>2,3 - 15,4 NTU.]"
nota5 <- "^[MAPE = Mean Absolute Percentage Error<br>0 - 1300 NTU<br>0 - 80 NTU.]"

agua_tbl <- c(
  glue("{agua1} {nota1}"),
  glue("{agua2} {nota2}"),
  glue("{agua3} {nota3}"),
  glue("{agua4} {nota4}"),
  glue("{agua5} {nota5}")
)


# tabla ------------------------------------------------------------------

tabla_turb <- tibble(
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

# aula 02: Câmaras --------------------------------------------------------

camaras1 <- readxl::read_excel("~/Downloads/camaras 1.xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(
    c("id", "start_time", "completion_time", "email",
      "name", "coleta", "prop", "oque")
  )

camaras1 |>
  dplyr::count(coleta, sort = TRUE) |>
  bidTrib:::barras()

camaras1 |>
  dplyr::mutate(proporcao = scales::percent(prop)) |>
  dplyr::count(proporcao, sort = TRUE) |>
  bidTrib:::barras()

camaras2 <- readxl::read_excel("~/Downloads/camaras 2.xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(c("id", "start_time", "completion_time", "email",
                     "name", "probabilidade", "reamostragem", "distribuicao"))

camaras2 |>
  dplyr::count(probabilidade, sort = TRUE) |>
  bidTrib:::barras()


library(decryptr)
modelo <- load_model("rfb")
img <- "download.png"
plot(read_captcha(img)[[1]])

decrypt(img, modelo)


# aula 03: carf -----------------------------------------------------------


carf <- readxl::read_excel("~/Downloads/carf.xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(
    c("id", "start_time", "completion_time", "email",
      "name", "oficio", "mediana", "pizza")
  )

carf |>
  dplyr::count(oficio, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

carf |>
  dplyr::count(mediana, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

carf |>
  dplyr::mutate(palavra1 = toupper(stringr::str_extract(pizza, "\\w+"))) |>
  dplyr::mutate(decisao = dplyr::case_when(
    palavra1 == "SIM" ~ "Sim",
    palavra1 == "NÃO" ~ "Não",
    TRUE ~ "Depende / Outros"
  )) |>
  dplyr::count(decisao) |>
  dplyr::mutate(
    prop = n/sum(n),
    lab = scales::percent(prop)
  ) |>
  bidTrib:::pizza()

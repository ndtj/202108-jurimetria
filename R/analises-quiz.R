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


x <- rnorm(10000, mean = 10, sd=2)
hist(x)
median(x)
x <- rexp(10000)
hist(x)
mean(x)
median(x)


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

# aula 04: db -----------------------------------------------------------

db <- readxl::read_excel("~/Documents/puc/202108-jurimetria/data-raw/Doing Business.xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(
    c("id", "start_time", "completion_time", "email",
      "name", "indicador", "cifra", "sobrevivencia", "prospectivo")
  )

db |>
  dplyr::count(indicador, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

db |>
  dplyr::count(cifra, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

db |>
  dplyr::count(sobrevivencia, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))


# aula 5 - Preditivos -----------------------------------------------------

mp <- readxl::read_excel("~/Documents/puc/202108-jurimetria/data-raw/Modelos Preditivos.xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(
    c("id", "start_time", "completion_time", "email",
      "name", "aprendizado", "treino_teste", "metricas", "visualizacao")
  )

mp |>
  dplyr::count(aprendizado, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

mp |>
  dplyr::count(treino_teste, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

mp |>
  dplyr::count(metricas, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

sample(mp$visualizacao, 1)

# aula 8 - Observatórios -----------------------------------------------------

obs <- readxl::read_excel("~/Documents/puc/202108-jurimetria/data-raw/Observatórios(1-24).xlsx") |>
  dplyr::select(where(~!is.logical(.x))) |>
  janitor::clean_names() |>
  purrr::set_names(
    c("id", "start_time", "completion_time", "email", "name",
      "por_que", "quais_situacoes", "explique")
  )

obs |>
  dplyr::count(por_que, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))

obs |>
  dplyr::mutate(quais_situacoes = stringr::str_squish(quais_situacoes)) |>
  dplyr::mutate(quais_situacoes = stringr::str_split(quais_situacoes, ";")) |>
  tidyr::unnest(quais_situacoes) |>
  dplyr::count(quais_situacoes, sort = TRUE)

obs |>
  dplyr::mutate(quais_situacoes = stringr::str_squish(quais_situacoes)) |>
  dplyr::count(quais_situacoes, sort = TRUE) |>
  bidTrib:::barras() |>
  highcharter::hc_xAxis(labels = list(style = list(fontSize = "15px"))) |>
  highcharter::hc_yAxis(labels = list(style = list(fontSize = "15px")))


sample(obs$explique, 1)

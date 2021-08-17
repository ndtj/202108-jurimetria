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

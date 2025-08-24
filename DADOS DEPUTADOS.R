# Buscar informações sobre deputados

deputados <- function(pagina) {
  params <- list(
    pagina = pagina,
    dataInicio = "1990-07-13",
    ordem = "ASC",
    ordenarPor = "nome"
  )
  res <- httr::GET("https://dadosabertos.camara.leg.br/api/v2/deputados", query = params)
  res_cont <- httr::content(res, as = "text") |>
    jsonlite::fromJSON() |>
    purrr::pluck("dados")
  return(res_cont)
}

a <- purrr::map_dfr(1:10, ~ {deputados(.x)
})

## O objeto "a" armazena todos os dados dos deputados desde 1990

# Buscar dados completos sobre os deputados

deputados_completo <- function(id) {
  res <- httr::GET(glue::glue("https://dadosabertos.camara.leg.br/api/v2/deputados/{id}"))
  res_cont <- httr::content(res, as = "text") |>
    jsonlite::fromJSON() |>
    purrr::pluck("dados")
  res_cont$ultimoStatus <- NULL
  res_cont$redeSocial <- NULL
  df <- data.frame(
    id = res_cont$id,
    nome_civil = res_cont$nomeCivil,
    data_nascimento = as.Date(res_cont$dataNascimento),
    municipio_nascimento = res_cont$municipioNascimento,
    uf_nascimento = res_cont$ufNascimento,
    escolaridade = res_cont$escolaridade,
    sexo = res_cont$sexo
  )
  return(df)
}

# para teste filtrei os primeiros 20 deputados (fazer com o teu filtro gabs)

a <- head(a, 20)

b <- purrr::map_dfr(a$id, purrr::slowly(~ {
  deputados_completo(.x)
}, rate = purrr::rate_delay(0.3)))

# só para juntar as duas tabelas pelo id

c <- dplyr::inner_join(
  a,
  b,
  by = "id"
) |> dplyr::distinct()

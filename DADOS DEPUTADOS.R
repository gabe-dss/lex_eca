# Buscar informações sobre deputados

library(httr2)

deputados <- function(pagina){
  params = list(
    pagina = pagina,
    dataInicio = "1990-07-13",
    ordem = "ASC",
    ordenarPor = "nome"
  )
  res <- httr::GET("https://dadosabertos.camara.leg.br/api/v2/deputados", query = params)
  res_cont <- httr::content(res, as = "text") |> jsonlite::fromJSON() |> purrr::pluck('dados')
  return(res_cont)
}

a <- purrr::map_dfr(1:10, ~{
  deputados(.x)
})

## O objeto "a" armazena todos os dados dos deputados desde 1990
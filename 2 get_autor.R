frame <- readRDS("alter_2.xlsx") |>
  dplyr::mutate(
    id_2 = stringr::str_extract(link_2, "\\d+$")
    ) |>
  dplyr::select(-link, -link_2)

get_autor <- function(id_proposicao) {
  
  if (is.na(id_proposicao)) {
    return(NULL)
  }
  
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", 
                id_proposicao, 
                "/autores")
  
  Sys.sleep(0.5)
  
  tryCatch({
    resposta_api <- httr::GET(url, httr::timeout(10))
    if (httr::status_code(resposta_api) != 200) {
      return(NULL)
    }
    
    dados <- jsonlite::fromJSON(rawToChar(resposta_api$content))
    
    if (length(dados$dados) == 0 || nrow(dados$dados) == 0) {
      return(NULL)
    }
    
    return(dados$dados)
    
  }, error = function(e) {
    return(NULL)
  })
}

frame2 <- frame |>
  dplyr::rowwise() |>
  dplyr::mutate(autores_tabela = list(get_autor(id_2))) |>
  dplyr::ungroup() |>
  tidyr::unnest(autores_tabela) |>
  dplyr::select(-uri, -codTipo, -proponente, -ordemAssinatura)

saveRDS(frame2, "frame2.RDS")
frame2 <- readRDS("frame2.RDS")

## Separa as tabelas
# Tabela SF
frame2_sf <- frame2 |>
  dplyr::filter(tipo != "Deputado(a)") |>
  dplyr::filter(nome != "Poder Executivo") |>
  dplyr::filter(!stringr::str_detect(nome, "Comissão|CPI|CPMI"))

saveRDS(frame2_sf, "frame2_sf.RDS")

# Tabela OUT
frame2_out <- frame2 |>
  dplyr::filter(nome == "Poder Executivo" | stringr::str_detect(nome, "Comissão|CPI|CPMI"))

saveRDS(frame2_out, "frame2_out.RDS")

# Tabela CD
frame2_cd <- frame2 |>
  dplyr::filter(tipo == "Deputado(a)")

saveRDS(frame2_cd, "frame2_cd.RDS")

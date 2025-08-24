frame2_cd <- readRDS("frame2_cd.RDS")

frame3_cd <- frame2_cd |>
  dplyr::mutate(
    tipo = "DEP",
    partido = NA_character_,
    estado = NA_character_,
    genero = NA_character_,
    nome = stringr::str_to_upper(nome)
  )

# Conseguir genero
## Genérica
frame3_cd <- frame3_cd |>
  dplyr::mutate(
    genero = dplyr::case_when(
      genderBR::get_gender(nome) == "Female" ~ "F",
      genderBR::get_gender(nome) == "Male" ~ "M",
      TRUE ~ "Erro"
    )
  )

## Caso Específicos
frame3_cd$genero[frame3_cd$nome == "MANDETTA"] <- "M"
frame3_cd$genero[frame3_cd$nome == "DR. LEONARDO"] <- "M"
frame3_cd$genero[frame3_cd$nome == "DRA. SORAYA MANATO"] <- "F"
frame3_cd$genero[frame3_cd$nome == "ELEUSES PAIVA"] <- "M"
frame3_cd$genero[frame3_cd$nome == "MAINHA"] <- "M"
frame3_cd$genero[frame3_cd$nome == "ALÊ SILVA"] <- "F"

# Conseguir partido e estado
get_info_deputado <- function(nome_deputado) {
  nome_codificado <- utils::URLencode(nome_deputado)
  

  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/deputados?nome=",
                nome_codificado,
                "&ordem=ASC&ordenarPor=nome")
  
  Sys.sleep(0.5)
  
  tryCatch({
    resposta <- httr::GET(url)
    
    
    if (httr::status_code(resposta) != 200) {
      return(tibble::tibble(partido = NA_character_, estado = NA_character_))
    }
    
    dados <- jsonlite::fromJSON(rawToChar(resposta$content))
    
    if (length(dados$dados) == 0) {
      return(tibble::tibble(partido = NA_character_, estado = NA_character_))
    }
    
    info <- dados$dados[1, ]
    
    return(tibble::tibble(partido = info$siglaPartido, estado = info$siglaUf))
    
  }, error = function(e) {
    return(tibble::tibble(partido = NA_character_, estado = NA_character_))
  })
}

frame3_cd <- frame3_cd |>
  dplyr::rowwise() |>
  dplyr::mutate(info_api = list(get_info_deputado(nome))) |>
  dplyr::mutate(partido = info_api$partido,
                estado = info_api$estado) |>
  dplyr::select(-info_api) |>
  dplyr::ungroup()

# Conseguir Estado e Partido Remanescente

frame3_cd <- frame3_cd |>
  dplyr::mutate(
    codDep = NA
  )








get_info_deputado <- function(nome_deputado) {
  
  nome_codificado <- utils::URLencode(nome_deputado)
  
  url_busca <- paste0("https://dadosabertos.camara.leg.br/api/v2/deputados?nome=",
                      nome_codificado,
                      "&ordem=ASC&ordenarPor=nome")
  
  Sys.sleep(0.5)
  
  id_deputado <- tryCatch({
    resposta_busca <- httr::GET(url_busca)
    if (httr::status_code(resposta_busca) != 200) return(NA)
    
    dados_busca <- jsonlite::fromJSON(rawToChar(resposta_busca$content))
    if (length(dados_busca$dados) == 0) return(NA)
    
    dados_busca$dados$id[1]
    
  }, error = function(e) { NA })
  
  if (is.na(id_deputado)) {
    return(data.frame(id = NA, partido = NA_character_, estado = NA_character_))
  }
  
  url_detalhes <- paste0("https://dadosabertos.camara.leg.br/api/v2/deputados/", id_deputado)
  
  Sys.sleep(0.5)
  
  tryCatch({
    resposta_detalhes <- httr::GET(url_detalhes)
    if (httr::status_code(resposta_detalhes) != 200) {
      return(data.frame(id = id_deputado, partido = NA_character_, estado = NA_character_))
    }
    
    dados_detalhes <- jsonlite::fromJSON(rawToChar(resposta_detalhes$content))
    
    info <- dados_detalhes$dados$ultimoStatus
    
    return(data.frame(id = id_deputado, partido = info$siglaPartido, estado = info$siglaUf))
    
  }, error = function(e) {
    return(data.frame(id = id_deputado, partido = NA_character_, estado = NA_character_))
  })
}


saveRDS(frame3_cd, "frame3_cd.rds")











xlsx::write.xlsx(frame3_cd, "C:\\Users\\gabri\\OneDrive\\Área de Trabalho\\cd_teste.xlsx")

frame3_cd <- xlsx::read.xlsx("C:\\Users\\gabri\\OneDrive\\Área de Trabalho\\cd_teste.xlsx", 1)

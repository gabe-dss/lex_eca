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
  # Para cada linha, criamos uma coluna temporária com o resultado da API
  dplyr::mutate(info_api = list(get_info_deputado(nome))) |>
  # Usamos a coluna temporária para atualizar as colunas finais
  dplyr::mutate(partido = info_api$partido,
                estado = info_api$estado) |>
  # Removemos a coluna temporária
  dplyr::select(-info_api) |>
  # Desagrupamos para o dataframe voltar ao normal
  dplyr::ungroup()

saveRDS(frame3_cd, "frame3_cd.rds")

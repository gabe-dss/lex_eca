alter <- xlsx::read.xlsx("alter.xlsx", 1)

get_initial_proposition_link <- function(url) {
  Sys.sleep(1) 
  tryCatch({
    page <- rvest::read_html(url)
    link_node <- page |>
      rvest::html_element("#content .dadosNorma > div > .sessao .listaMarcada a")
    
    # Extrai o atributo 'href', que contém o link
    if (!is.na(link_node)) {
      href <- link_node |>
        rvest::html_attr("href")
      return(href)
    } else {
      # Retorna NA (Not Available) se o elemento não for encontrado
      return(NA_character_)
    }
    
  }, error = function(e) {
    message(paste("Erro ao processar a URL:", url, "-", e$message))
    return(NA_character_)
  })
}

alter_2 <- alter |>
  dplyr::rowwise() |> # Agrupa por linha para aplicar a função em cada 'link'
  dplyr::mutate(
    # Cria a nova coluna 'link_proposicao' com o resultado da função
    link_2 = get_initial_proposition_link(link)
  ) |>
  dplyr::ungroup() # Desagrupa para voltar ao formato de dataframe normal

get_author_name <- function(url) {
  # Verifica se a URL recebida é válida. Se for NA, retorna NA.
  if (is.na(url)) {
    return(NA_character_)
  }
  
  Sys.sleep(1)
  tryCatch({
    page <- rvest::read_html(url)
    
    # Seletor para encontrar o nome do autor
    author_node <- page |>
      rvest::html_element("#identificacaoProposicao #colunaPrimeiroAutor a")
    
    # Extrai o TEXTO do link, que é o nome do autor
    if (!is.na(author_node)) {
      # Usamos html_text2() para limpar o texto de espaços extras
      author_name <- author_node |> rvest::html_text2()
      return(author_name)
    } else {
      return(NA_character_)
    }
    
  }, error = function(e) {
    message(base::paste("Erro (Passo 2) na URL:", url, "-", e$message))
    return(NA_character_)
  })
}


# --- 5. Aplicação das duas funções em sequência ---
alter_3 <- alter_2 |>
  dplyr::mutate(
    autor = get_author_name(link_2)
  ) |>
  dplyr::ungroup()

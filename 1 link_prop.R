alter <- xlsx::read.xlsx("alter.xlsx", 1)

# Cria get_linkprop
get_linkprop <- function(url) {
  Sys.sleep(1) 
  tryCatch({
    page <- rvest::read_html(url)
    link_node <- page |>
      rvest::html_element("#content .dadosNorma > div > .sessao .listaMarcada a")
    
    if (!is.na(link_node)) {
      redirect_url <- link_node |>
        rvest::html_attr("href")
      
      if (!is.na(redirect_url)) {
        resposta <- httr::HEAD(redirect_url, httr::timeout(10))
        final_url <- resposta$url
        return(final_url)
      } else {
        return(NA_character_)
      }
      
    } else {
      return(NA_character_)
    }
    
  }, error = function(e) {
    message(paste("Erro ao processar a URL:", url, "-", e$message))
    return(NA_character_)
  })
}

alter_2 <- alter |>
  dplyr::rowwise() |>
  dplyr::mutate(
    link_2 = get_linkprop(link)
  ) |>
  dplyr::ungroup()

saveRDS(alter_2, "alter_2.xlsx")

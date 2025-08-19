frame2_sf <- readRDS("frame2_sf.RDS")

# Cria tabela excel para edição manual das variáveis, visto impossibilidade de raspagem direta da interface do SF
xlsx::write.xlsx(frame2_sf, "frame2_sf_cru.xlsx")

# Após coleta manual, recarrega os dados
frame3_sf <- xlsx::read.xlsx("frame2_sf_cru.xlsx", 1)

frame3_sf <- frame3_sf |>
  dplyr::mutate(
    nome = stringr::str_remove_all(nome, "Senado Federal - "),
    nome = stringr::str_remove_all(nome, "Senado Federal-"),
    nome = stringr::str_to_upper(nome)
  )

saveRDS(frame3_sf, "frame3_sf.rds")

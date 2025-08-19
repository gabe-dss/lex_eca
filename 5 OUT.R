frame2_out <- readRDS("frame2_out.rds")

frame3_out <- frame2_out |>
  dplyr::mutate(
    tipo = dplyr::case_when(
      nome == "Poder Executivo" ~ "EXEC",
      stringr::str_detect(nome, "Comissão|CPI|CPMI|COMISSÃO") ~ "CP",
      TRUE ~ NA_character_),
    partido = dplyr::case_when(
      tipo == "CP" ~ NA_character_,
      tipo == "EXEC" ~ "MANUAL"),
    estado = NA_character_,
    genero = NA_character_,
    nome = stringr::str_to_upper(nome)
  )

# Cria planilha para preenchimento manual da Presidência da República
xlsx::write.xlsx(frame3_out, "frame3_out.xlsx")

# Carrega planilha atualizada
frame3_out <- xlsx::read.xlsx("frame3_out.xlsx", 1) |>
  dplyr::select(-NA.) |>
  dplyr::mutate(
    partido = dplyr::case_when(
      partido == "N/A" ~ NA_character_,
      TRUE ~ partido
    ),
    estado = dplyr::case_when(
      estado == "N/A" ~ NA_character_,
      TRUE ~ estado
    ),
    genero = dplyr::case_when(
      genero == "N/A" ~ NA_character_,
      TRUE ~ genero
    )
  )

saveRDS(frame3_out, "frame3_out.RDS")

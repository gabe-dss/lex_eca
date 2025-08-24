frame3_sf$nome <- frame3_sf$nome |>
  stringr::str_to_upper() |>
  stringr::str_squish()

frame4 <- dplyr::full_join(frame3_cd, frame3_sf) |>
  dplyr::full_join(frame3_out) |>
  dplyr::arrange(id) |>
  dplyr::select(-id_2) |>
  dplyr::mutate(
    ideo = dplyr::case_when(
      partido == "PT" ~ "2",
      partido == "PSDB" ~ "6",
      partido == "CIDADANIA" ~ "4",
      partido == "PL" ~ "6",
      partido == "MDB" ~ "6",
      partido == "DEM" ~ "7",
      partido == "PCdoB" ~ "2",
      partido == "PDT" ~ "3",
      partido == "PODE" ~ "6",
      partido == "PP" ~ "6",
      partido == "PRN" ~ "6",
      partido == "PSB" ~ "3",
      partido == "PSD" ~ "6",
      partido == "PSL" ~ "6",
      partido == "PSOL" ~ "1",
      partido == "PTB" ~ "5",
      partido == "PV" ~ "4",
      partido == "REDE" ~ "4",
      partido == "REPUBLICANOS" ~ "6",
      partido == "SOLIDARIEDADE" ~ "5",
      partido == "PSBD" ~ "6",
      TRUE ~ "NA"),
      ideo = as.numeric(ideo)
    )

frame4 <- frame4 |>
  dplyr::mutate(
    ideo_class = dplyr::case_when(
      partido %in% 1:3 ~ "ESQ",
      partido == 4 ~ "CEN",
      partido %in% 5:7 ~ "DIR",
      TRUE ~ "OUTRO")
    )
  

xlsx::write.xlsx(frame4, "frame4.xlsx")

frame5 <- frame4 |>
  dplyr::distinct(num, .keep_all = TRUE)

print(prop.table(table(frame4$genero)) * 100)

View(table(frame4$partido))

View(table(frame4$ideo))

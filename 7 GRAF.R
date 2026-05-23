library(tidyverse)
library(ggplot2)

# 1. Gráfico de Barras: Tipo de Legislador
# ------------------------------------------
# Ordena os tipos de legislador pela frequência para um visual mais limpo
dados_tipo <- frame4 %>%
  filter(!is.na(tipo)) %>% # Garante que não há valores ausentes
  count(tipo) %>%
  mutate(
    percentual = n / sum(n),
    # --- ALTERAÇÃO AQUI ---
    # Cria um rótulo que combina o número absoluto (n) e o percentual formatado
    rotulo_completo = paste0(n, " (", scales::percent(percentual, accuracy = 0.1), ")"),
    tipo = dplyr::case_when(
      tipo == "CP" ~ "Comissão",
      tipo == "SEN" ~ "Senador/a",
      tipo == "DEP" ~ "Deputado/a",
      tipo == "EXEC" ~ "Presidência",
    )
  )

grafico_tipo <- ggplot(dados_tipo, aes(x = reorder(tipo, -n), y = n)) +
  # Usamos stat="identity" porque já fornecemos os valores de y (a contagem 'n')
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  # Usa a nova coluna de rótulos para o texto
  geom_text(aes(label = rotulo_completo), vjust = -0.5, size = 5) +
  scale_y_continuous(limits = c(0, max(dados_tipo$n) * 1.1)) +
  labs(
    title = " ",
    x = "Tipo de Legislador/a",
    y = "Número de Propositores/as"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Salvar o gráfico
ggsave(
  "GRAF/grafico_tipo_legislador_FHD.png",
  plot = grafico_tipo,
  width = 1920,
  height = 1080,
  units = "px"
)


# 2. Gráfico de Pizza: Gênero (Masculino/Feminino/Não informado)
# -----------------------------------------------------------------
# Prepara os dados, tratando NA (valores ausentes) como "Não Informado"
dados_genero <- frame4 %>%
  mutate(genero = ifelse(is.na(genero), "Não Aplicável", genero)) %>%
  count(genero) %>%
  mutate(
    percentual = n / sum(n),
    rotulo = scales::percent(percentual, accuracy = 0.01)
  )

# Cria o gráfico de pizza
grafico_genero <- ggplot(dados_genero, aes(x = "", y = n, fill = genero)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = rotulo), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = " ",
    fill = "Gênero",
    x = NULL,
    y = NULL
  ) +
  theme_void() + # Remove eixos e fundo desnecessários
  theme(legend.position = "bottom")

# Salvar o gráfico
ggsave(
  "GRAF/grafico_genero_pizza_FHD.png",
  plot = grafico_genero,
  width = 1920,
  height = 1080,
  units = "px"
)


# 3. Gráfico de Barras: Unidades da Federação com mais propositores (Top 10)
# ---------------------------------------------------------------------------
# Filtra os dados para remover UFs ausentes e pega as 10 mais frequentes
top_10_uf <- frame4 %>%
  filter(!is.na(estado)) %>%
  count(estado, sort = TRUE)

# Cria o gráfico
grafico_uf <- ggplot(top_10_uf, aes(x = reorder(estado, -n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon", color = "black") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_y_continuous(limits = c(0, max(top_10_uf$n) * 1.1)) +
  labs(
    title = " ",
    x = "Unidade da Federação",
    y = "Número de Propositores/as"
  ) +
  theme_minimal()

# Salvar o gráfico
ggsave(
  "GRAF/grafico_top10_uf_FHD.png",
  plot = grafico_uf,
  width = 1920,
  height = 1080,
  units = "px"
)

# Exibe os gráficos (opcional, útil se estiver rodando interativamente)
print(grafico_tipo)
print(grafico_genero)
print(grafico_uf)

# 4. Gráfico de Barras: Partidos Políticos (Top 10)
# ---------------------------------------------------------------------------
# Filtra os dados para remover partidos ausentes e pega os 10 mais frequentes
frame4$partido[frame4$partido == "PSBD"] <- "PSDB"

partidos <- frame4 |>
  dplyr::filter(!is.na(frame4$partido)) |>
  dplyr::count(partido, sort = TRUE)

# Cria o gráfico
grafico_partidos <- ggplot(partidos, aes(x = reorder(partido, -n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +
  scale_y_continuous(limits = c(0, max(partidos$n) * 1.15)) +
  labs(
    title = " ",
    x = "Partido Político",
    y = "Número de Propositores/as"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

# Salvar o gráfico
ggsave(
  "GRAF/grafico_top10_partidos_FHD.png",
  plot = grafico_partidos,
  width = 1920,
  height = 1080,
  units = "px"
)

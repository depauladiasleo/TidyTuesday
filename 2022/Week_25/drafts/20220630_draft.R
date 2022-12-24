#' Autor: Leonardo Dias de Paula
#' Assunto: Tidy Tuesday 25: Juneteenth


# Preparação -------------------------------------------------------------------


## Bibliotecas necessárias


library(tidyverse)
library(janitor)


## Diretórios


full_df_repo <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv"


# Importação de dados ----------------------------------------------------------


slave_routes <- readr::read_csv(full_df_repo)


slave_routes_br <- read_csv("brasil_tráfico_escravizados.csv")


# Tidy -------------------------------------------------------------------------


## Variáveis para o gráfico 1

df <- slave_routes |> 
  select(port_origin, port_arrival, year_arrival, n_slaves_arrived) |> 
  group_by(year_arrival) |>
  summarise(n_slaves_arrived = sum(n_slaves_arrived, na.rm = TRUE)) |> 
  clean_names() |> 
  remove_empty()


## Variáveis para o gráfico 2

### Base de dados

slave_routes_br <- slave_routes_br |> 
  clean_names() |> 
  remove_empty()

df_br <- slave_routes_br |> 
  group_by(year_of_arrival_at_port_of_disembarkation) |> 
  summarise(n_slaves_arrived = sum(slaves_arrived_at_1st_port, na.rm = TRUE))


### Outros dados


total_escravizados <- scales::comma(sum(df_br$n_slaves_arrived), big.mark = ".", decimal.mark = ",")

legenda <- "\nTidyTuesday #25 | Gráfico por: @depauladiasleo | Dados: Slave Voyages, RICE University | Inspiração: @danoehm"


# Visualização -----------------------------------------------------------------

## Gráfico 1: total de escravizados transportados em rotas transatlânticas -----

df |> 
  ggplot(aes(x = year_arrival, y = n_slaves_arrived)) +
  geom_area(fill = "gray5") +
  geom_area(aes(y = n_slaves_arrived*1.5), fill = "gray5", alpha = 0.1) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray90"),
    plot.margin = margin(3, 3, 3, 3, unit = "cm"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray5", size = 12)
    ) +
  scale_x_continuous(breaks = c(1514, 1866, 2022),
                     limits = c(1500, 2022))


# Gráfico 2: total de escravizados transportados em rotas transatlânticas para o Brasil ----


gráfico2 <- 
  df_br |> 
  ggplot(aes(x = year_of_arrival_at_port_of_disembarkation, y = n_slaves_arrived)) +
  geom_area(fill = "gray5") +
  geom_area(aes(y = n_slaves_arrived*1.5), fill = "gray5", alpha = 0.1) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray90"),
    plot.margin = margin(t = 2, r = 1.5, b = 2, l = 1.5, unit = "cm"),
    plot.caption = element_text(color = "gray25", size = 14, hjust = 0.5),
    plot.caption.position = "panel",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray5", size = 14, face = "bold")
  ) +
  scale_x_continuous(breaks = c(1530, 1856, 2022),
                     limits = c(1500, 2022)) +
  annotate("text", label = total_escravizados, 
           x = 1530, y = 60000, color = "gray5", size = 36, hjust = 0) +
  annotate("text", label = "Mais de", 
           x = 1530, y = 68000, color = "gray25", size = 9, hjust = 0) +
  annotate("text", label = "pessoas foram escravizadas\ne transportadas a portos brasileiros", 
           x = 1530, y = 53000, color = "gray25", size = 9, hjust = 0) +
  annotate("text", label = "Dados do IBGE, por exemplo, estimam que\n4 milhões de pessoas escravizadas\ndesembarcaram no Brasil.", 
           x = 2022, y = 20000, color = "gray5", size = 5, hjust = 1) +
  labs(caption = legenda)


# Export -----------------------------------------------------------------------


ggsave("TidyTuesday25.png", plot = gráfico2,
       width = 4800, height = 4800, dpi = 320, units = "px")


ggsave("TidyTuesday25_story.png", plot = gráfico2,
       width = 4800, height = 8500, dpi = 320, units = "px")


# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(janitor)
library(ragg)
library(scales)


# Importação de dados ------------------------------------------


tuesdata <- tidytuesdayR::tt_load('2022-09-27')


full_df <- tuesdata$artists


# Faxina de dados ----------------------------------------------

df <- 
  full_df |> 
  add_column(y = 0)


# Visualização -------------------------------------------------


## Títulos ---------------------------------------------------------------

títulos <- list(título = "Artist share in the workforce of U.S. states according to racial self-identification", 
                subtítulo = "Each dot represents a state; red dots represent Indiana",
                caption = "Tidy Tuesday 2022 #39 | Gráfico por: @depauladiasleo | Dados: arts.gov")


## Gráfico ---------------------------------------------------------------


df |> 
  filter(race != "Other") |> 
  ggplot(aes(artists_share, type)) +
  geom_jitter(size = 0.25,
            alpha = 0.35,
            color = "gray85",
            width = 0) +
  geom_point(data = df |>
                    filter(race != "Other",
                           state == "Indiana"),
             color = "#540b0e",
             size = 1.5) +
  scale_x_continuous(labels = label_percent()) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(family = "Segoe UI",
                               size = 10,
                               color = "gray95",
                               face = "bold"),
    axis.text.x = element_text(family = "Segoe UI",
                               size = 10,
                               color = "gray95"),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.15,
                                      color = "gray95",
                                      linetype = 2),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.background = element_rect(fill = "#335c67",
                                   color = "#335c67"),
    plot.title = element_text(family = "Segoe UI",
                              size = 24,
                              color = "gray95",
                              face = "bold"),
    plot.subtitle = element_text(family = "Segoe UI",
                                 size = 18,
                                 color = "gray85",
                                 margin = margin(6, 0, 36, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe UI",
                                size = 12,
                                color = "gray95",
                                hjust = 0.5,
                                margin = margin(36, 0, 0, 0, "pt")),
    plot.caption.position = "plot",
    strip.text = element_text(family = "Segoe UI",
                              size = 12,
                              color = "gray95",
                              face = "bold",
                              hjust = 0)
  ) +
  facet_wrap("race", ncol = 4) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$caption)
  

# Exportação ---------------------------------------------------


ggsave("TidyTuesday39.png", 
       width = 5120, height = 2880, dpi = 320, units = "px")

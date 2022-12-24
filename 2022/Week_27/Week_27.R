# Título: TidyTuesday 2022 #27
# Script por: Leonardo Dias de Paula
# Última atualização: Julho de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas -------------------------------------------------


library(tidyverse)
library(ggridges)
library(ragg)


## Textos para o gráfico ---------------------------------------------------


plot_title_text <- "Rent distribution in Bay Area counties"

plot_subtitle_text <- "According to Craigslist posts"

caption_text <- "TidyTuesday 2022 #27 | Gráfico por: @depauladiasleo | Dados: Kate Pennington"


# Importação de dados ------------------------------------------


rent <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')


# Faxina de dados ----------------------------------------------


rent_summary <- 
  rent |> 
  select(post_id, nhood, county, city, price, date) |> 
  drop_na() |> 
  mutate(county = fct_reorder(county, price))
  

# Visualização -------------------------------------------------


rent_summary |> 
  ggplot() +
  geom_density_ridges(aes(y = county, x = price, scale = 1.75), 
                      color = "#B71234",
                      size = 0.85,
                      fill = "#B71234",
                      alpha = 0.7) +
  ggtitle(plot_title_text,
          subtitle = plot_subtitle_text) +
 # scale_y_discrete(labels = c("non registered", "sonoma", "solano", "santa cruz", 
 #                             "santa clara", "san mateo", "san francisco", "napa",
 #                             "marin", "contra costa", "alameda")) +
  scale_x_continuous(limits = c(0, 5000),
                     expand = expansion(),
                     labels = scales::label_dollar()) +
  theme_minimal() +
  theme(
        plot.background = element_rect(color = "gray95"),
        plot.margin = margin(2, 2, 2, 2, unit = "cm"),
        plot.title = element_text(family = "Segoe UI",
                                  size = 24,
                                  color = "#B71234",
                                  face = "bold"),
    plot.subtitle = element_text(family = "Segoe UI",
                                 size = 18,
                                 color = "gray60",
                                 margin = margin(t = 6, b = 24, unit = "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe UI",
                                size = 14,
                                color = "gray60", 
                                margin = margin(t = 32, unit = "pt"),
                                hjust = 0.5),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = "Segoe UI",
                             size = 12, color = "gray60")) +
  labs(
    caption = caption_text)


# Exportação ---------------------------------------------------

ggsave("TidyTuesday27.png",
       width = 4800, height = 3600, dpi = 320, units = "px")


ggsave("TidyTuesday27_story.png",
       width = 3240, height = 5760, dpi = 320, units = "px")

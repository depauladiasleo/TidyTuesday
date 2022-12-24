# Título: TidyTuesday Week 31
# Script por: Leonardo Dias
# Última atualização: Agosto de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(NatParksPalettes)
library(showtext)


# Importação de dados ------------------------------------------


ribbit_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv"


ribbit_df <- read_csv(ribbit_url)


# Faxina de dados ----------------------------------------------


beaver <- ribbit_df |> 
  filter(Beaver != "No beaver")


# Visualização -------------------------------------------------



## Títulos ---------------------------------------------------------------


títulos <- list(título = "Are there beavers where there are frogs?",
                subtítulo = "Not really, only in the Southeast Pond and the North Res there was\nnoticeable presence of beavers near to frogs",
                caption = "Tidy Tuesday 2022 #31 | Gráfico por: @depauladiasleo | Dados: USGS")


# # Fontes ----------------------------------------------------------------


font_add_google("Montserrat")

showtext_auto(enable = TRUE)
showtext_opts(dpi = 320)


## Gráfico 1 -------------------------------------------------------------


ribbit_df |> 
  ggplot() +
  geom_bar(aes(x = Water, fill = Beaver),
           position = "stack") +
  scale_fill_natparks_d("Olympic", override.order = TRUE) +
  theme_minimal() +
  theme(
    plot.margin = margin(2, 2, 2, 2, unit = "cm"),
    plot.background = element_rect(fill = "#FAF3CE", color = NA),
    plot.title = element_text(family = "Montserrat",
                              size = 24,
                              color = "gray10",
                              face = "bold"),
    plot.subtitle = element_text(family = "Montserrat",
                                 size = 18,
                                 color = "gray25",
                                 face = "plain",
                                 margin = margin(t = 6, b = 24, unit = "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Montserrat",
                                size = 12,
                                color = "gray25",
                                face = "plain",
                                hjust = 0.5,
                                margin = margin(t = 24, unit = "pt")),
    plot.caption.position = "plot",
    axis.title = element_blank(),
    axis.text = element_text(family = "Montserrat",
                             size = 10,
                             color = "gray35",
                             face = "plain"),
    axis.ticks = element_line(color = "gray75",
                              size = 0.25),
    axis.ticks.length = unit(4, "pt"),
    axis.line = element_blank(),
    legend.text = element_text(family = "Montserrat",
                               size = 10,
                               color = "gray35",
                               face = "plain"),
    legend.title = element_text(family = "Montserrat",
                                size = 10,
                                color = "gray35",
                                face = "bold"),
    panel.grid.major.y = element_line(color = "gray85",
                                      size = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85",
                                      size = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(0.25, unit = "cm"),
    strip.text = element_text(family = "Montserrat",
                              size = 12,
                              color = "gray25",
                              face = "bold",
                              hjust = 0)
      ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$caption) +
  facet_wrap("Subsite", ncol = 2)


# Exportação ---------------------------------------------------


ggsave("TidyTuesday31.png",
       width = 4800, height = 3600, dpi = 320, units = "px")


ggsave("TidyTuesday31_story.png",
       width = 3240, height = 5760, dpi = 320, units = "px")

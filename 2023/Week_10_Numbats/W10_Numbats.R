# Título: Tidy Tuesday | 2023 | Week 10 | Numbats in Australia
# Script por: @depauladiasleo
# Última atualização: julho de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, week = 10)


df_full <- tuesdata$numbats


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_full |> 
  filter(year != is.na(year)) |> 
  group_by(year) |> 
  summarize(sightings = n())


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


bg_color <- "#F2F3AE"


plot_color <- "#69140E"


title_color <- "#3C1518"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |>
  ggplot(aes(x = year,
             y = sightings)) +
  geom_area(fill = plot_color) +
  scale_y_continuous(limits = c(0, 125),
                     breaks = c(0, 25, 50, 75, 100)) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(size = 24,
                                  color = title_color,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 18,
                                     color = title_color,
                                     margin = margin(6, 0, 24, 0, "pt"),
                                     hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    color = title_color,
                                    margin = margin(22, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        axis.text = element_text(size = 12,
                                 color = plot_color),
        axis.title.y = element_text(size = 14,
                                    color = plot_color,
                                    angle = 0,
                                    vjust = 1,
                                    hjust = 1,
                                    margin = margin(0, 0, 10, 0, "pt"))) +
  labs(title = "Numbats observations in Australia across the years",
       subtitle = "Includes observations registered in zoos and sanctuaries",
       caption = "Tidy Tuesday | 2023 | Week 10 | Numbats | plot by: @depauladiasleo | data from: Atlas of Living Australia")


# Exportação -------------------------------------------------------------------


ggsave("W10_Numbats.png", 
       height = 3840,
       width = 3840*1.778,
       unit = "px",
       dpi = 320)

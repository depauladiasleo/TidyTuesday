# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(sf)
library(spData)
library(ggtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, week = 5)


df_groundhog <- tuesdata$groundhogs |> 
  select(id, name, latitude, longitude, predictions_count, country)


df_predictions <- tuesdata$predictions |> 
  select(id, year, shadow) |> 
  na.omit()

us <- us_states


# Faxina de dados --------------------------------------------------------------


df_map <- 
  left_join(df_predictions, df_groundhog) |> 
  filter(year == 2023,
         country == "USA")


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Groundhogs predict a <span style='color:#3F4E4A'>longer Winter</span> or a <span style='color:#D9A734'>sooner Spring</span>",
                subtítulo = "Data for 2023",
                legenda = "Tidy Tuesday | 2024 | Week 04 | Groundhog day |\nchart: @depauladiasleo | data: groundhog-day.com")



## Paleta de cores -------------------------------------------------------------


col1 <- "#D9A734" #f6bd60

col2 <- "#f28482"

col3 <-  "#3F4E4A" #84a59d

col4 <-  "#f5cac3"

col5 <-  "#FCEDC2"

title_col <- "gray10"

text_col <- "gray35"


pal <- c(col1, col3)


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_map |> 
  ggplot() +
  geom_sf(data = us, 
          color = text_col, 
          fill = bg_col, 
          alpha = 0.85) +
  geom_point(aes(x = longitude,
                 y = latitude,
                 color = shadow,
                 size = predictions_count
                 ),
             alpha = 0.85) +
  scale_color_manual(values = pal) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray85",
                                       color = "gray85"),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_markdown(size = 24,
                                  color = title_col,
                                  face = "bold"),
        plot.subtitle = element_markdown(size = 18,
                                     color = title_col,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16,
                                    color = title_col,
                                    hjust = 0.5,
                                    margin = margin(20, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        legend.position = "none"
        ) +
  coord_sf() +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda) +
  annotate("curve", x = -78.96225, xend = -70, 
           y = 40.93120, yend = 32,
           curvature = -0.75,
           arrow = arrow(length = unit(0.15, 'cm'))) +
  annotate("text", x = -79.5, y = 30, hjust = 0,
           label = "Punxsutawney Phil is\nthe groundhog\nwith the most predictions\nin U.S.: 128.")




# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1800,
               height = 1012.5,
               units = "px",
               dpi = 110)


ggsave("Week_05_Groundhog.png",
       width = 1800,
       height = 1012,
       units = "px",
       dpi = 110)

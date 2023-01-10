# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(sf)
library(spData)


# Importação de dados ----------------------------------------------------------


birdfeeder <- tt_load(2023, week = 2)


df_raw <- birdfeeder$PFW_2021_public


indiana <- us_states |> 
  janitor::clean_names() |> 
  filter(name == "Indiana")



# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_raw |> 
  filter(subnational1_code == "US-IN")


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------

títulos <- list(título = "Are there birds in your neighboorhood?",
                subtítulo = "Size denotes the number of individuals seen during the observation period near supplementary feeding stations",
                legenda = "TidyTuesday | 2023 | Week 02 | Bird feeders | Plot by: @depauladiasleo | Data from: FeederWatch")



## Paleta de cores -------------------------------------------------------------


bg_col <- "#F5C2B5"
  
map_col <- "#5A2A27"

feeder_col <- "#A5978B"

title_col <- "gray15"

txt_col <- "gray35"


## Fontes ----------------------------------------------------------------------


limelight <- "Limelight"
font_add_google(limelight)


montserrat <- "montserrat"
font_add_google(montserrat)


showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


ggplot() +
  geom_sf(data = indiana,
          fill = map_col) +
  geom_point(data = df_tidy,
             aes(longitude, latitude, size = how_many),
             alpha = 0.1,
             color = feeder_col) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col,
                                       color = NA),
        plot.title = element_text(family = limelight,
                                  size = 24,
                                  color = title_col,
                                  face = "bold",
                                  hjust = 0.5,
                                  lineheight = 1.25),
        plot.subtitle = element_text(family = montserrat,
                                     size = 20,
                                     color = txt_col,
                                     margin = margin(6, 0, 18, 0, "pt"),
                                     hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(family = montserrat,
                                    size = 16,
                                    face = "bold",
                                    color = title_col,
                                    margin = margin(20, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        plot.margin = margin(2, 2, 2, 2, "cm"),
        legend.position = "none") +
  coord_sf() +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)



# Exportação -------------------------------------------------------------------


ggsave("W02.png", 
       height = 3840,
       width = 6796,
       unit = "px",
       dpi = 320)

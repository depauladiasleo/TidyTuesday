# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)



# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 47)

museums <- tuesdata$museums


# Faxina de dados --------------------------------------------------------------


museums_tidy <- 
  museums |> 
  janitor::clean_names() |> 
  select(latitude, longitude, name_of_museum) |> 
  filter(latitude < 90)


map <- map_data("world", "UK")
  

# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Museums across the United Kingdom",
                caption = "Tidy Tueday | 2022 | Week 47 | Plot by: @depauladiasleo | Data from: Mapping Museums project")



## Paleta de cores -------------------------------------------------------------


bg_col <- "#030421"

fill_col <- "#013E54"


## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)


showtext_auto()


theme_set(theme_void(base_family = montserrat))


## Gráfico ---------------------------------------------------------------------


  ggplot() +
  #geom_sf(data = coord) +
  geom_polygon(data = map, aes(long, lat, group = group),
               fill = fill_col) +
  geom_point(data = museums_tidy,
             aes(longitude, latitude),
             color = "gray90",
             size = 0.15,
             alpha = 0.25) +
  coord_map() +
  theme_void() +
  theme(
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(color = "gray95",
                              size = 42,
                              face = "bold",
                              margin = margin(0, 0, 30, 0, "pt"),
                              hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "gray95",
                                size = 14,
                                #face = "bold",
                                margin = margin(20, 0, 0, 0, "pt"),
                                hjust = 0.5),
    plot.background = element_rect(fill = bg_col,
                                   color = bg_col)
  ) +
  labs(title = títulos$título,
       caption = títulos$caption)




# Exportação -------------------------------------------------------------------


ggsave("W47.png",
       width = 1368,
       height = 1368,
       units = "px",
       dpi = 100)


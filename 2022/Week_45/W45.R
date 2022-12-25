# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(geofacet)
library(tidyverse)
library(tidytuesdayR)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 45)



state_st <- tuesdata$state_stations


st_info <- tuesdata$station_info



# Faxina de dados --------------------------------------------------------------



# Genres ------------------------------------------------------------------


genres <- "Classic rock|Alternative rock|Jazz|Classic hip-hop|Country"



## Dataframe for the plot ------------------------------------------------


df <- 
  state_st |> 
  right_join(st_info, by = c("call_sign")) |> 
  mutate(st_genre = str_detect(format, genres)) |> 
  filter(st_genre == TRUE) |> 
  mutate(format = str_extract(format, genres))



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Radio stations around the U.S. that play my favorite music",
                subtitle = "Have a suggestion for me? Write down the station's callsign",
                caption = "Tidy Tuesday 2022 | Week 45 | Plot by: @depauladiasleo | Data from: @frankiethull")



## Paleta de cores -------------------------------------------------------------


bg_col <- "#394649" 
  
#"#011019"
  
title_col <- "gray95"


## Fontes ----------------------------------------------------------------------


montserrat <- "Montserrat"
font_add_google(montserrat)

showtext_auto()

theme_set(theme_void(base_family = montserrat))


## Gráfico ---------------------------------------------------------------------


df |> 
  ggplot(aes(x = format)) +
  geom_bar(aes(fill = format)) +
  theme_void() +
  NatParksPalettes::scale_fill_natparks_d("Glacier") +
#  scale_fill_manual(name = "Music genre", palette = natparks.pals("Glacier")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(color = title_col,
                                   size = 12,
                                   hjust = 0),
        legend.margin = margin(0, 0, 24, 0, "pt"),
        legend.position = "top",
        plot.title = element_text(color = title_col,
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(color = title_col,
                                     size = 18,
                                     hjust = 0.5,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(color = title_col,
                                    size = 14,
                                    hjust = 0.5,
                                    margin = margin(34, 0, 0, 0, "pt")),
        plot.background = element_rect(fill = bg_col,
                                       color = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        strip.text = element_text(face = "bold",
                                  color = title_col,
                                  size = 8, 
                                  hjust = 0),
        panel.spacing = unit(6, "pt"),
        panel.background = element_rect(fill = NA,
                                        color = NA)) +
  facet_geo(~ state, grid = "us_state_grid2", label = TRUE) +
  labs(title = títulos$título,
       subtitle = títulos$subtitle,
       caption = títulos$caption)


# Exportação -------------------------------------------------------------------
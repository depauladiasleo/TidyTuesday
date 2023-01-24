# Título: Tidy Tuesday | 2023 | W04 | Alone
# Script por: @depauladiasleo
# Última atualização: janeiro de 2023.


# Descrição:



# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(glue)
library(ggtext)
library(ggrepel)
library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, week = 4)


df <- tuesdata$episodes


# Faxina de dados --------------------------------------------------------------

df <- 
  df |> 
  mutate(label = str_wrap(quote, 45, 0, 0, TRUE))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Imdb ratings for Alone episodes",
                caption = "Tidy Tuesday | 2023 | Week 04 | Alone | plot by: @depauladiasleo | data from: {alone}")


labels = tibble(x = c(36, 66),
                y = c(6.9, 8.8))



## Paleta de cores -------------------------------------------------------------


title_col <- "#42D7C5"

txt_col <- "gray85"
  
gray <- "gray70"
  
color <- "#091A1C"
  
bg <- "#2F4F4D"


## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)


theme_set(theme_minimal(base_family = montserrat))


showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df |> 
  ggplot() +
  geom_area(aes(x = episode_number_overall, y = imdb_rating),
            fill = color) +
  geom_text(data = df |> 
                      filter(imdb_rating %in% c(6.9, 8.8)),
            aes(x = c(36, 66) + 1, y = 9.5,
                label = glue("Season {season}, episode {episode}\nImdb rating: {imdb_rating}\n'{label}'\nby {author}")),
            size = 3.75,
            hjust = 0,
            vjust = 0,
            color = txt_col) +
  scale_y_continuous(limits = c(0, 12)) +
  scale_x_continuous(breaks = c(36, 66)) +
  theme_void() +
  theme(panel.grid.major.x = element_line(color = gray,
                                          linewidth = 0.3,
                                          linetype = 3),
        plot.title = element_text(size = 24, 
                                  color = title_col,
                                  face = "bold",
                                  margin = margin(0, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 12,
                                    color = txt_col,
                                    margin = margin(24, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        plot.background = element_rect(color = bg,
                                       fill = bg),
        plot.margin = margin(2, 2, 2, 2, "cm")) + 
  labs(title = títulos$título,
       caption = títulos$caption)



# Exportação -------------------------------------------------------------------


ggsave("W04_Alone.png",
       height = 2736,
       widt = 4840,
       units = "px",
       dpi = 320)

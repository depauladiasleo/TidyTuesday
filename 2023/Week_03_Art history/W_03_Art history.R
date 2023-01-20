# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------



library(tidytuesdayR)
library(tidyverse)
library(showtext)



# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, week = 03)


df <- tuesdata$artists


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  filter(book == "Gardner",
         artist_race != "White",
         artist_gender %in% c("Male", "Female")) |>
  select(artist_name, artist_race, artist_gender, edition_number, space_ratio_per_page_total) |> 
    mutate(count = sum(str_detect(artist_name, artist_name)),
         artist_name = fct_reorder(artist_name, count, max),
         x = ifelse(artist_gender == "Female", edition_number + 18, edition_number))



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Non-white artists displayed in Gardner's art history textbook",
                subtítulo = "Size of the points refer to the space dedicated to these artists work in each edition;\nLeft side depicts male artists and the right side refers to female artists",
                legenda = "Tidy Tuesday | 2023 | Week 03 | Plot by: @depauladiasleo | data from: {arthistory} | inspiration: @danoehm")


## Paleta de cores -------------------------------------------------------------


title_col <- "gray15"


gray <-  "gray90"
  

pink <- "#531C3E"
  

bg <- "#DBA73B"



## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)


theme_set(theme_void(base_family = montserrat))


showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df_tidy |>
  ggplot(aes(
    x = x,
    y = artist_name,
    size = space_ratio_per_page_total,
    fill = artist_gender
  )) +
  geom_point(color = gray,
             shape = 21,
             alpha = .75) +
  geom_text(
    aes(x = 17,
        y = artist_name,
        label = artist_name),
    color = title_col,
    size = 3.5,
    family = montserrat,
    hjust = 0,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(pink, gray)) +
  theme_void() +
  theme(
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.background = element_rect(fill = bg,
                                   color = bg),
    plot.title = element_text(size = 24,
                              color = title_col,
                              face = "bold"),
    plot.subtitle = element_text(
      size = 18,
      color = "gray25",
      lineheight = 1.15,
      margin = margin(6, 0, 24, 0, "pt")
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = 12,
      color = title_col,
      #  face = "bold",
      hjust = 0.5,
      margin = margin(24, 0, 0, 0, "pt")
    ),
    legend.position = "none"
  ) +
  labs(
    title = títulos$título,
    subtitle = títulos$subtítulo,
    caption = títulos$legenda
  )


# Exportação -------------------------------------------------------------------


ggsave("W03_art_history.png",
       height = 3091,
       width = 5472,
       unit = "px",
       dpi = 320)


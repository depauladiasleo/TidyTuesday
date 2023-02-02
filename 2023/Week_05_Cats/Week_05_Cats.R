# Título: Tidy Tuesday | 2023 | Week 05 | Cats
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 5)


pets_df <- tuesdata$cats_uk_reference


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  pets_df |> 
  select(animal_sex, hrs_indoors, age_years)


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Cats in the United Kingdom, according to sex",
                subtítulo = "Where are you cat people?",
                legenda = "Tidy Tuesday | 2023 | Week 05 | Cats | plot by: @depauladiasleo | data from: movebank",
                y = "Daily hours\nindoors",
                x = "Age, in years")


## Paleta de cores -------------------------------------------------------------


bg_col <- "#040f16"


title_col <- "gray99"
  

txt_col <- "gray85"


sex_pal <- c("#EB174C","#0094C6")


## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)


showtext_opts(dpi = 320)
showtext_auto()


## Theme -----------------------------------------------------------------------


theme_set(theme_void(base_family = montserrat))


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_jitter(aes(x = age_years, 
                  y = hrs_indoors, 
                  color = animal_sex),
              size = 3,
              alpha = 0.7,
              width = 0.25,
              height = 0.45) +
  scale_color_manual(values = sex_pal) +
  theme_void() +
  theme(
     axis.ticks = element_line(color = title_col,
                              linewidth = 0.2),
    axis.title.x = element_text(color = title_col,
                              size = 10,
                              face = "bold",
                              hjust = 1),
    axis.title.y = element_text(color = title_col,
                                size = 10,
                                face = "bold",
                                hjust = 0,
                                vjust = 1),
    axis.text = element_text(color = txt_col,
                             size = 9,
                             hjust = 0.5),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.background = element_rect(fill = bg_col,
                                   color = bg_col),
    plot.title = element_text(color = title_col,
                              size = 24,
                              face = "bold"),
    plot.subtitle = element_text(color = txt_col,
                                 size = 18,
                                 margin = margin(6, 0, 18, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(color = txt_col,
                                size = 12,
                                face = "bold",
                                hjust = 0.5,
                                margin = margin(24, 0, 0, 0, "pt")),
    plot.caption.position = "plot",
    legend.position = "none"
  ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda,
       x = títulos$x,
       y = títulos$y)


# Exportação -------------------------------------------------------------------


ggsave("W05_Cats.png",
       height = 2304,
       width = 4104,
       dpi = 320,
       unit = "px")

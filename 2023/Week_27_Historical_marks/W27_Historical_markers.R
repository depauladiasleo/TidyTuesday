# Título: TidyTuesday | Week 27 | Historical marks
# Script por: @depauladiasleo
# Última atualização: julho de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(sf)
library(spData)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, 27)


us_map <- spData::us_states


# Faxina de dados --------------------------------------------------------------


df_full <- tuesdata$historical_markers |> 
  filter(longitude_minus_w > -130,
         latitude_minus_s > 20)


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


bg_color <- "#2E294E"


line_color <- "#BE97C6"


point_color <- "#EFBCD5"


## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"

font_add_google(montserrat)


showtext_opts(dpi = 320)
showtext_auto()


theme_set(theme_void(base_family = montserrat))


## Gráfico ---------------------------------------------------------------------


ggplot() +
  geom_sf(
    data = us_map,
    color = line_color,
    fill = "gray99",
    alpha = 0.1
  ) +
  geom_point(
    data = df_full,
    aes(x = longitude_minus_w,
        y = latitude_minus_s),
    size = 0.05,
    color = point_color,
    alpha = 0.5
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_color,
                                   color = bg_color),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.title = element_markdown(
      color = "gray99",
      face = "bold",
      size = 24,
      hjust = 0
    ),
    plot.subtitle = element_markdown(
      color = "gray99",
      size = 18,
      margin = margin(6, 0, 24, 0, "pt"),
      hjust = 0
    ),
    plot.caption = element_markdown(
      color = "gray99",
      size = 14,
      hjust = 0.5,
      margin = margin(22, 0, 0, 0, "pt")
    ),
    plot.title.position = "plot"
  ) +
  labs(title = "Historical Markers in the 48 contiguous states",
       subtitle = "According to the <i>Historical Markers Database</i>, there are over 42k historical markers in US",
       caption = "Tidy Tuesday | 2023 | Week 27 | Historical Markers | Plot by: @depauladiasleo | Data from: Historical Markers Database")


# Exportação -------------------------------------------------------------------


ggsave("W27_Historical_markers.png", 
       height = 3840,
       width = 3840*1.77778,
       unit = "px",
       dpi = 320)


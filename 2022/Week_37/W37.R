# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(ggfx)
library(showtext)
library(sf)
library(spData)



# Importação de dados ------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 37)


bigfoot_df <- tuesdata$bigfoot


us <- us_states |> 
  select(NAME, geometry)


# Faxina de dados ----------------------------------------------


df <- 
  bigfoot_df |>
  select(state, latitude, longitude,
         date, season, 
         classification) |> 
  drop_na(latitude) |> 
  left_join(us, by = c("state" = "NAME"))


# States with most and least observations


df |> 
  select(state, classification, date) |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  arrange(-n) |> 
  print(n = 49)


# IU location

IU <- tibble(latitude = 39.17815562811598,
                 longitude = -86.51095625649216)


# Visualização -------------------------------------------------


## Title --------------------------------------------------------------


titulos <- list(titulo = "Indiana, the home to... the Big Foot?",
                subtitulo = "Is the Big Foot a challenger to the demogorgon?",
                legenda = "Tidy Tuesday 2022 #37 | Gráfico por: @depauladiasleo | Dados: Data.World")


## Palette --------------------------------------------------------------


bg_col <- "#1E0707"


title_col <- "gray95"


txt_col <- "gray90"


class_pal <- c("white", "gray80")


## Font ------------------------------------------------------------------


font_title <- "Kanit"


font <- "Open Sans"


font_add_google(font_title, font_title)
font_add_google(font, font)


showtext_auto()
showtext_opts(dpi = 320)


## Plot ------------------------------------------------------------------


df |> 
  st_as_sf() |> 
  filter(state == "Indiana") |> 
  ggplot() +
  geom_sf(
          #color = "gray90",
          fill = "#5C1C11",
          size = 0) +
  with_outer_glow(geom_point(aes(longitude, latitude,
                 color = classification,
                 alpha = ifelse(classification == "Class A", 0.75, 0.55)),
             size = 4),
             colour = "white",
             sigma = 5) +
  geom_point(data = IU,
             aes(longitude, latitude),
             shape = 18,
             color = "#AE5507",
             alpha = 1,
             size = 5.5) +
  scale_color_manual(values = class_pal) +
  theme_void() +
  theme(
    legend.position = "none",
     plot.title = element_text(family = font_title,
                               face = "bold",
                               color = title_col,
                               size = 24,
                               lineheight = 1.1,
                               hjust = 0.5),
    plot.subtitle = element_text(family = font,
                                  color = txt_col,
                                  size = 18,
                                margin = margin(t = 6, b = 24, "pt"),
                                hjust = 0.5),
     plot.title.position = "plot",
     plot.background = element_rect(fill = bg_col,
                                    colour = bg_col),
  plot.margin = margin(2, 2, 2, 2, "cm"),
  plot.caption = element_text(family = font,
                              size = 14,
                              color = title_col,
                              margin = margin(t = 36),
                              hjust = 0.5
                              ),
  plot.caption.position = "plot"
  ) +
  labs(title = titulos$titulo,
       subtitle = titulos$subtitulo,
       caption = titulos$legenda) +
  annotate("text", x = -89.5, y = 39.17,
           color = txt_col, size = 6, family = font,
           label = "Trivia: do you know where\nthe yellow marker is?",
           hjust = 0, vjust = 1) +
  annotate("text", x = -89.5, y = 41,
           color = txt_col, size = 6, family = font,
           label = "Each white dot represents\none sighting of\nthe Big Foot",
           hjust = 0, vjust = 1)



# Exportação ---------------------------------------------------


ggsave("TidyTuesday37.png", 
       width = 3792, height = 3792, dpi = 320, units = "px")


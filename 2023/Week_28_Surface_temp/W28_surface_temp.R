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


tuesdata <- tt_load(2023, week = 28)


df_global <- tuesdata$global_temps


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_global |>
  janitor::clean_names() |> 
  select(-c(j_d:son)) |> 
  pivot_longer(jan:dec, names_to = "month", values_to = "variation") |> 
  filter(!is.na(variation)) |> 
  mutate(date = ymd(paste(year, "-", month, "-01")),
         month = month(date, label = TRUE, abbr = TRUE),
         t = 1:n())
  



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Variação global de temperatura",
                subtítulo = "Considerando a média de temperaturas\nregistradas entre 1951 e 1980",
                legenda = "Tidy Tuesday | 2023 | Semana 28 | Temperaturas globais\nelaborado por: @depauladiasleo | dados de: GISTEMP")


## Paleta de cores -------------------------------------------------------------


title_color <- "gray5"

subtitle_color <- "gray15"

text_color <-  "gray25"

bg_color <- "gray95"


## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)


showtext_opts(dpi = 300)
showtext_auto()


theme_set(theme_void(base_family = montserrat))



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_point(aes(x = month,
                 y = year,
                 color = variation),
             size = 2) +
  scale_color_viridis_c(option = "plasma", direction = 1,
                        limits = c(-1.4, 1.4),
                         breaks = c(-1.4, 0, 1.4)) +
  # MetBrewer::scale_color_met_c("Hiroshige", direction = -1,
  #                             limits = c(-1.4, 1.4)) +
  scale_y_reverse(breaks = c(1880, 1951, 1980, 2023)) +
  coord_cartesian() +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.backgroun = element_rect(fill = bg_color,
                                      color = bg_color),
        plot.title = element_text(size = 24,
                                  color = title_color,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = subtitle_color,
                                     margin = margin(6, 0, 12, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    color = subtitle_color,
                                    margin = margin(22, 0, 0, 0, "pt"), 
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.text = element_text(size = 12,
                                   color = text_color),
        legend.title = element_text(size = 12,
                                    face = "bold",
                                    color = text_color),
        legend.text = element_text(size = 12,
                                   color = text_color),
        legend.position = "top") +
  guides(color = guide_colorbar(title = "Variação de temperatura (ºC)",
                               title.position = "top", 
                               title.hjust = 0, 
                               barwidth = unit(12, "cm"),
                               ticks = FALSE)) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)


# Exportação -------------------------------------------------------------------


nflplotR::ggpreview(width = 1024*2,
                    height = 1024*3,
                    unit = "px",
                    dpi = 300)


ggsave("W28_surface_temp.png",
       width = 1024*2,
       height = 1024*3,
       unit = "px",
       dpi = 300)

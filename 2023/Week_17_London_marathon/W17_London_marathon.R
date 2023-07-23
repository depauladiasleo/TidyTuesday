# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(ggtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 17)


df_winners <- tuesdata$winners |> 
  janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_winners |> 
  mutate(wheelchair = str_extract(category, "Wheelchair"),  
         wheelchair = case_when(wheelchair == "Wheelchair" ~ "Wheelchair athletes",
                     TRUE ~ ""),
         gender = str_replace(category, "Wheelchair ", ""))


df_difference <- 
  df_tidy |> 
  select(-category) |> 
  pivot_wider(names_from = gender, values_from = time) |> 
  janitor::clean_names()


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


texto <- list(título = "The London Marathon records for winners",
              subtítulo = "<span style='color:#A41623'><b>Men</span></b> and <span style='color:#F85E00'><b>women</span></b> compete yearly to find the fastest person on the streets of London.<br><br>
              There's also a competition for <b><span style='color:#3D4027'>wheelchair athletes</b></span>, which is held in the same 42 kilometers path.",
              legenda = "Tidy Tuesday | 2023 | Week 17 | London Marathon | plot by: @depauladiasleo | data from: @nrennie35")



## Paleta de cores -------------------------------------------------------------


color1 <- "#A41623"

color2 <- "#F85E00"

color3 <- "#3D4027"

#918450

bg_color <- "#FFD29D"



title_color <- "gray5"

text_color <- "gray15"


palette_gender <- c(color1, color2)


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = time, 
                color = gender)) +
  scale_color_manual(values = palette_gender) +
  theme_void() +
  theme(plot.margin =  margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        plot.title = element_text(size = 24,
                                  face = "bold",
                                  color = title_color),
        plot.subtitle = element_markdown(size = 16,
                                         color = text_color,
                                         margin = margin(8, 0, 24, 0, "pt"),
                                         lineheight = 1.25),
        plot.caption = element_text(size = 16,
                                    color = text_color,
                                    margin = margin(20, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.title.position = "plot",
        legend.position = "none",
        axis.text = element_text(size = 12,
                                 color = text_color),
        strip.text = element_text(size = 14,
                                  color = color3,
                                  face = "bold",
                                  hjust = 0)
        ) +
  facet_wrap(~wheelchair, nrow = 2) +
  labs(title = texto$título,
       subtitle = texto$subtítulo,
       caption = texto$legenda)


# Exportação -------------------------------------------------------------------


ggsave("W17_London_marathon.png",
       width = 1368,
       height = 768,
       unit = "px",
       dpi = 100)

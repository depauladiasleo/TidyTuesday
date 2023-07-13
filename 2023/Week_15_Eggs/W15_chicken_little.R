# Título: Tidy Tuesday | 2023 | Week 15 | Eggs
# Script por: @depauladiasleo
# Última atualização: julho de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(geomtextpath)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 15)


df_egg <- tuesdata$`egg-production` |> 
  janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_egg |> 
  select(!source) |> 
  group_by(observed_month, prod_process) |> 
  summarize(sum_hens = sum(n_hens),
            sum_eggs = sum(n_eggs)) 

# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


titles <- list(title = "The sky is falling down!",
               subtitle = "Cage free eggs are still a small share of <span style='color:#bf6329'><b>total eggs</span></b> production",
               caption = "Tidy Tuesday | 2023 | Week 15 | Chicken Little | plot by: depauladiasleo | data from: The Humane League, by Samara Mendez")



## Paleta de cores -------------------------------------------------------------


color1 <- "#54692c"

color2 <- "#93261f"

color3 <- "#41251b"

color4 <- "#bf6329"

color5 <- "#eabb82"


palette <- c(color1, color2)


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_area(data = df_tidy |> 
              filter(prod_process == "all"),
            aes(x = observed_month,
                y = sum_eggs/1000000),
            fill = color4,
            alpha = 0.3) +
  geomtextpath::geom_textline(data = df_tidy |> 
                                 filter(prod_process != "all"),
                               aes(x = observed_month,
                                   y = sum_eggs/1000000,
                                   color = prod_process,
                                   label = prod_process)) +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(0, 12000),
                     breaks = c(0, 2000, 4000, 6000, 8000, 10000),
                     labels = scales::label_number()) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color5,
                                   color = color5),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(size = 24,
                              face = "bold",
                              color = color3),
    plot.subtitle = element_markdown(size = 18,
                                     color = color3,
                                     margin = margin(6, 0, 24, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(size = 14,
                                color = color3,
                                hjust = 0.5,
                                margin = margin(22, 0, 0, 0, "pt")),
    plot.caption.position = "plot",
    axis.text = element_text(size = 12,
                             color = color3),
    axis.title.y = element_text(size = 12,
                              color = color3,
                              face = "bold",
                              vjust = 1,
                              hjust = 1),
    legend.position = "none"
  ) +
  labs(title = titles$title,
       subtitle = titles$subtitle,
       caption = titles$caption,
       y = "million\neggs")

  
# Exportação -------------------------------------------------------------------


ggsave("W15_chicken_little.png",
       height = 768,
       width = 1368,
       dpi = 100,
       unit = "px")

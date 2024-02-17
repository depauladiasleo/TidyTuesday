# Título: TidyTuesday | Week 07 | Valentines
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2024.


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(snakecase)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 7)


df <- tuesdata$historical_spending


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  select(Year, Candy:GiftCards) |> 
  pivot_longer(cols = Candy:GiftCards, names_to = "gift", values_to = "amount") |> 
  mutate(gift = to_mixed_case(gift, sep_out = " ")) |> 
  janitor::clean_names()



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos = list(título = "What gift did you prepare to your significant other?",
               subtítulo = "Personal spending on Valentines' Day according to National Retail Federation",
               legenda = "Tidy Tuesday | 2024 | Week 07 | Valentines' Day | chart: @depauladiasleo | data: National Retail Federation")


## Paleta de cores -------------------------------------------------------------


valentine_red <- "#BF0700"


bg_color <- "#F2CDCC"


valentine_palette <- monochromeR::generate_palette(valentine_red, 
                                                   modification = "go_both_ways", 
                                                   n_colours = 7)


title_col <- "gray10"

text_col <- "gray20"



## Fontes ----------------------------------------------------------------------


kanit <- "kanit"
font_add_google(kanit)


open_sans <- "open sans"
font_add_google(open_sans)

theme_set(theme_void(base_family = open_sans))


showtext_opts(dpi = 110)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot(aes(x = year, y = amount)) +
  geom_area(fill = valentine_palette[[5]]) +
  scale_fill_manual(values = valentine_palette) +
  scale_x_continuous(breaks = c(2010, 2014, 2018, 2022),
                     labels = c("'10", "'14",
                                "'18", "'22")) +
  scale_y_continuous(name = "$ spent/\nperson", breaks = c(0, 20, 40)) +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(fill = "#F2CDCC",
                                       color = "#F2CDCC"),
        plot.title = element_text(family = kanit,
                                  size = 24,
                                  color = title_col,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = title_col,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16,
                                    color = title_col,
                                    margin = margin(32, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.title.y = element_text(size = 14,
                                    color = title_col,
                                    face = "bold",
                                    hjust = 1,
                                    vjust = 1,
                                    margin = margin(0, 4, 0, 0, "pt")),
        axis.text = element_text(size = 12,
                                 color = text_col),
        strip.text = element_text(size = 12,
                                  color = text_col,
                                  face = "bold",
                                  hjust = 0),
        strip.switch.pad.wrap = unit(20, "mm")) +
  facet_wrap("gift", nrow = 2) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)




# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600,
               height = 900,
               units = "px",
               dpi = 110)


ggsave("Week_07_Valentines.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

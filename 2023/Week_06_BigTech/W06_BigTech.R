# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(showtext)
library(lubridate)
library(tidyverse)
library(tidytuesdayR)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, 6)


stock <- tuesdata$big_tech_stock_prices


bigtech_companies <- tuesdata$big_tech_companies


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  stock |> 
  left_join(y = bigtech_companies)


tsla <- 
  df_tidy |> 
  filter(stock_symbol == "TSLA")


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "All that is solid melts into the air",
                subtítulo = "T$LA stock prices since june, 2010",
                legenda = "Tidy Tuesday | 2023 | Week 06 | Big Tech | plot by: @depauladiasleo | data from: kaggle")


## Paleta de cores -------------------------------------------------------------



## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)


theme_set(theme_void(base_family = montserrat))


showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


tsla |> 
  ggplot() +
  geom_area(aes(date, close),
            fill = "#4d0b15") +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(fill = "#95A3B3",
                                       color = "#95A3B3"),
        plot.title = element_text(face = "bold",
                                  size = 24,
                                  color = "#001514"),
        plot.subtitle = element_text(size = 18,
                                color = "#001514",
                                margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 12,
                                    color = "#001514",
                                    margin = margin(24, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot") +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)



# Exportação -------------------------------------------------------------------


ggsave("W06_BigTech.png",
       unit = "px",
       height = 2318,
       width = 4100,
       dpi = 320)

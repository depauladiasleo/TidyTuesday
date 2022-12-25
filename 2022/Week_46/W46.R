# Título: TidyTuesday 2022 | Week 46
# Script por: @depauladiasleo
# Última atualização: dezembro 2022


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(showtext)
library(tidyverse)
library(lubridate)


# Importação de dados ----------------------------------------------------------


bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')


# Faxina de dados --------------------------------------------------------------


df <- 
  bytes_total |> 
  pivot_longer(p10:p90, names_to = "p", values_to = "value") |> 
  mutate(date = as_date(date))



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Webpage's size has been increasing",
                subtítulo = "How far can we go?",
                caption = "Tidy Tuesday 2022 | Week 46 | Plot by: @depauladiasleo | Data from: httparchive.org @ Data is plural")


## Paleta de cores -------------------------------------------------------------


title_col <- "gray15"
subtitle_col <- "gray35"
txt_col <- "gray50"


bg_col <- "#FEE6E7"


## Fontes ----------------------------------------------------------------------


montserrat <- "montserrat"
font_add_google(montserrat)

showtext_opts(dpi = 320)
showtext_auto()

theme_set(theme_minimal(base_family = montserrat))


## Gráfico ---------------------------------------------------------------------


df |> 
  ggplot() +
  geom_line(aes(x = date, y = value,
                color = p),
            linewidth = 0.9) +
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")) +
  scale_y_continuous(name = "page\nsize (kB)",
                     labels = scales::number_format()) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10,
                               color = txt_col,
                               hjust = 0),
   plot.title = element_text(size = 24,
                              color = title_col,
                              face = "bold",
                              hjust = 0),
    plot.subtitle = element_text(size = 18,
                                 color = subtitle_col,
                                 margin = margin(6, 0, 0, 24, "pt"),
                                 hjust = 0),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12,
                                color = subtitle_col,
                                margin = margin(24, 0, 0, 0, "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col,
                                   color = bg_col),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10,
                                color = txt_col,
                                angle = 0,
                                vjust = 1,
                                hjust = 0),
    axis.text = element_text(size = 9,
                             color = txt_col),
    strip.text = element_text(size = 12,
                              face = "bold",
                              color = txt_col,
                              hjust = 0)
  ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$caption) +
  facet_wrap("client", ncol = 1)


# Exportação -------------------------------------------------------------------


ggsave("W46.png",
       width = 6840,
       height = 3840,
       unit = "px",
       dpi = 320)

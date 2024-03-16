# Título: TidyTuesday | 2024 | Week 11 | Fiscal sponsors
# Script por: @depauladiasleo
# Última atualização: março de 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ghibli)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 11)


df <- tuesdata$fiscal_sponsor_directory


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  select(year_fiscal_sponsor, n_sponsored) |> 
  group_by(year_fiscal_sponsor) |> 
  summarize(sum_sponsored = sum(n_sponsored))



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Yearly sponsored projects",
                subtítulo = "In time, how has the policy of fiscal sponsoring changed?",
                legenda = "Tidy Tuesday | Week 11 | Fiscal Sponsors | graph by: @depauladiasleo | data from: Fiscal Sponsors Directory")


## Paleta de cores -------------------------------------------------------------


marnie_palette <- ghibli::ghibli_palette("MarnieMedium1")

print(marnie_palette)


title_col <- "gray15"

text_col <- "gray20"


## Fontes ----------------------------------------------------------------------

open_sans <- "Open sans"
kanit     <- "Kanit"   

font_add_google(open_sans)
font_add_google(kanit)

showtext_auto()


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_line(aes(x = year_fiscal_sponsor, y = sum_sponsored),
            color = marnie_palette[[2]],
            linewidth = 1.1) +
  theme_void() +
  theme(plot.background = element_rect(color = marnie_palette[[5]],
                                       fill = marnie_palette[[5]]),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(size = 24, 
                                  family = kanit,
                                  color = marnie_palette[[2]],
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     family = open_sans,
                                     color = title_col,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    family = open_sans,
                                    color = text_col,
                                    hjust = 0.5,
                                    margin = margin(34, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        axis.text.x = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col),
        axis.text.y = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col),?
        panel.grid.major.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1),
        panel.grid.major.y = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1),
        panel.grid.minor.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1)) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)


# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600,
               height = 900,
               units = "px",
               dpi = 110)


ggsave("Week_11_Fiscal_sponsors.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

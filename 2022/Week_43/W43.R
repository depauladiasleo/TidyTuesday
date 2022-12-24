# Título: Tidy Tuesday | Week #43
# Script por: @depauladiasleo
# Última atualização: dezembro de 2022


# Descrição:


# Preparação -------------------------------------------------------------------


## Bibliotecas -----------------------------------------------------------------


library(bakeoff)
library(glue)
library(ggtext)
library(tidyverse)
library(tidytuesdayR)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 43)


bakers <- tuesdata$bakers


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  bakers |> 
  select(!first_date_appeared:baker_first)
#|> 
#  pivot_longer(cols = star_baker:technical_lowest,
#               names_to = "prize",
#               values_to = "number")


# Visualização -----------------------------------------------------------------


texto_plot <- list(subtítulo = "Bars correspond to the number of episodes in which contenders appeared; <span style='color:#7d2732;'><b>red markers</b></span> reveal the median technical rating for contenders,<br>the <b><span style='color:#f4c630;'>yellow marker</span></b> refers to the median technical rating for the series winner.",
                   legenda = "Tidy Tuesday 2022 | Week 43 | Plot by: @depauladiasleo | Data from: {bakeoff}")


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


bg_col <- "#f4dedb"

txt_col <- "#7d2732"

median_col <- c("#7d2732", "#f4c630")


## Fontes ----------------------------------------------------------------------


txt_font <- "montserrat"
font_add_google(txt_font)

showtext_opts(dpi = 320)
showtext_auto()

## Gráfico ---------------------------------------------------------------------


  cake_plot <- function(df, season) {
    df |> 
    filter(series == season) |>
    ggplot(aes(y = baker)) +
    ggtitle(label = glue("Contenders for Series {season} of the British Bakeoff"),
            subtitle = texto_plot$subtítulo) +
    geom_col(aes(x = total_episodes_appeared,
               fill = baker),
           alpha = 0.95) +
  geom_point(aes(x = technical_median,
           color = as_factor(series_winner),
           shape = as_factor(series_winner)),
           size = 2.5) +
  MetBrewer::scale_fill_met_d("Signac") +
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(values = median_col) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(2, 4, 6, 8, 10)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(family = txt_font,
                             size = 12,
                             face = "bold",
                             color = txt_col),
    axis.line = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(color = txt_col,
                                    linewidth = 0.2,
                                    linetype = 3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = bg_col,
                                   color = bg_col),
    plot.caption = element_text(family = txt_font,
                                face = "bold",
                                size = 14,
                                color = "gray35",
                                hjust = 0.5,
                                margin = margin(24, 0, 0, 0, "pt")),
    plot.caption.position = "plot",
    plot.title = element_text(family = txt_font,
                              size = 24,
                              face = "bold",
                              color = txt_col),
    plot.subtitle = element_markdown(family = txt_font,
                                 color = "gray35",
                                 lineheight = 1.25,
                                 size = 17,
                                 margin = margin(7, 0, 24, 0, "pt")),
    plot.title.position = "plot",
    plot.margin = margin(2, 2, 2, 2, "cm"),
  ) +
      labs(caption = texto_plot$legenda)
  }


df_tidy |> 
cake_plot(10)



# Exportação -------------------------------------------------------------------

## Caution to save the latest plot and do not override it.

ggsave("BBO Contenders — S10.png",
       width = 6372, height = 3600, unit = "px",
       dpi = 320)

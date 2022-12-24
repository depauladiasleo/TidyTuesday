# Título: Tidy Tuesday 2022 | Week 44 | Horror movies
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(glue)
library(ggtext)
library(ggrepel)
library(showtext)
library(lubridate)
library(tidyverse)
library(tidytuesdayR)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 44)

horror_movies <- tuesdata$horror_movies


# Faxina de dados --------------------------------------------------------------


df <-
  horror_movies |> 
  arrange(desc(revenue)) |> 
  head(13) |> 
  mutate(year_release = year(release_date),
         log_revenue = log(revenue, 100))



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------

títulos <- list(título = "Top 13 revenue ranking for horror movies",
                subtítulo = "How much is the value of your fear?",
                caption = "Tidy Tuesday 2022 | Week 44 |\nPlot by: @depauladiasleo | Data from: Tanya Shapiro's Horror Movies")

## Paleta de cores -------------------------------------------------------------

line_col <- "#b8272a"

txt_col <- "gray90"

bg_col <- "#1f1f1f"


## Fontes ----------------------------------------------------------------------


creep <- "Creepster"
font_add_google(creep)

showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df |>
  ggplot(aes(x = 1, y = log(revenue, 100))) +
  geom_line(color = line_col) +
  geom_point(color = line_col,
             size = 2) +
  geom_text_repel(
    label = glue("{df$title} ({df$year_release})"),
    hjust = 1.25,
    color = txt_col,
    segment.color = line_col,
    nudge_x = 0,
    size = 5
  ) +
  geom_text_repel(
    label = glue("US$ {round(df$revenue/1000000, 1)} million"),
    hjust = -0.25,
    color = txt_col,
    segment.color = line_col,
    nudge_x = 0,
    size = 5
  ) +
  scale_y_continuous(label = scales::number_format(prefix = "US$", big.mark = " ")) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col,
                                       color = bg_col),
        plot.title = element_text(family = creep,
                                  size = 24, 
                                  color = line_col,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = "gray70",
                                     margin = margin(12, 0, 18, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    color = txt_col,
                                    margin = margin(22, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        plot.margin = margin(2, 1, 2, 1, "cm")) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$caption)
  



# Exportação -------------------------------------------------------------------


ggsave("W44.png",
       height = 7200,
       width = 3000,
       unit = "px",
       dpi = 320)

# Título: TidyTuesday | 2023 | Week 36 | Unionize
# Script por: @depauladiasleo
# Última atualização: setembro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(showtext)


# Importação de dados ----------------------------------------------------------


demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv') |> 
  as_tibble()


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  demographics |> 
  filter(facet == "all wage and salary workers")
  

# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


color_union <- "#EA1821"

color_covered <- "#28356A"

color_employed <- "#055A5B"

color_bg <- "#FFEFE5"


color_title = "gray5"

color_text <- "gray30"



## Fontes ----------------------------------------------------------------------

montserrat <- "montserrat"
font_add_google(montserrat)



showtext_auto()


## Tema ------------------------------------------------------------------------

theme_set(theme_void(base_family = montserrat))


tema <- 
theme(plot.background = element_rect(color = color_bg,
                                     fill = color_bg),
      axis.text = element_text(color = color_text,
                                size = 10))


## Gráfico ---------------------------------------------------------------------


main_plot <- 
df_tidy |>
  ggplot() +
  geom_area(aes(x = year, y = employment), 
            alpha = 0.1, 
            color = color_employed,
            fill = color_employed) +
  geom_area(aes(x = year, y = covered),
            alpha = 0.25,
            linetype = 1,
            color = color_covered,
            fill = color_covered) +
  geom_area(aes(x = year, y = members),
            alpha = 0.5,
            fill = color_union,
            color = color_union) +
  scale_x_continuous(limits = c(1973, 2022)) +
  scale_y_continuous(limits = c(0, 150000),
                     labels = scales::label_number()) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(color = color_bg,
                                       fill = color_bg),
        axis.text = element_text(color = color_text,
                                 size = 12))
  
  
  
employed_plot <- 
  df_tidy |>
  ggplot() +
  geom_area(aes(x = year, y = employment), 
            alpha = 0.5, 
            color = color_employed,
            fill = color_employed)  + 
  scale_x_continuous(limits = c(1973, 2022)) +
  scale_y_continuous(limits = c(0, 150000),
                     breaks = c(0, 150000),
                      labels = scales::label_number()
                     ) +
  theme_void() +
  theme(plot.title = element_text(color = color_employed,
                                  face = "bold",
                                  size = 16,
                                  margin = margin(0, 0, 8, 0, "pt")),
        plot.title.position = "plot") +
  tema +
  labs(title = "Employed workforce")


covered_plot <- 
  df_tidy |>
  ggplot() +
  geom_area(aes(x = year, y = covered),
            alpha = 0.5,
            linetype = 1,
            color = color_covered,
            fill = color_covered) +
  scale_x_continuous(limits = c(1973, 2022)) +
  scale_y_continuous(limits = c(0, 30000),
                     breaks = c(0, 30000),
                     labels = scales::label_number()) +
  theme_void() +
  theme(plot.title = element_text(color = color_covered,
                                  size = 16,
                                  face = "bold",
                                  margin = margin(0, 0, 8, 0, "pt")),
        plot.title.position = "plot") +
  tema +
  labs(title = "Covered by collective agreement workforce")


union_plot <- 
  df_tidy |>
  ggplot() +
  geom_area(aes(x = year, y = members),
            alpha = 0.5,
            fill = color_union,
            color = color_union) +
  scale_x_continuous(limits = c(1973, 2022)) +
  scale_y_continuous(limits = c(0, 30000),
                     breaks = c(0, 30000),
                     labels = scales::label_number()) +
  theme_void() +
  theme(plot.title = element_text(color = color_union,
                                  face = "bold",
                                  size = 16,
                                  margin = margin(0, 0, 8, 0, "pt")),
        plot.title.position = "plot") +
  tema +
  labs(title = "Unionized workforce")


## Patchwork -------------------------------------------------------------------


main_plot + (employed_plot/covered_plot/union_plot) +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(subtitle = "U.S. workforce and unionization since 1973, in thousands",
                  caption = "Tidy Tuesday | 2023 | Week 36 | Unions | plot by: @depauladiasleo\ndata from: Hirsch, B. T.; MacPherson, D. A.; Even, W. E. (2023). Union Membership, Coverage, and Earnings from the CPS") &
  theme(plot.subtitle = element_text(color = color_title,
                                  size = 24,
                                  face = "bold",
                                  margin = margin(0, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(color = color_text,
                                    size = 12,
                                    margin = margin(24, 0, 0, 0, "pt"),
                                    hjust = 0.5,
                                    lineheight = 1.2),
        plot.caption.position = "plot",
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(color = color_bg,
                                       fill = color_bg))


# Exportação -------------------------------------------------------------------



ggsave("W36_unionize.png",
       dpi = 100,
       width = 1369,
       height = 769,
       unit = "px")

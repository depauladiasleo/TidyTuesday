# Título: TidyTuesday 2022 Week 34
# Script por: @depauladiasleo
# Última atualização: agosto de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas -----------------------------------------------------------


library(tidyverse)
library(ggtext)
library(lubridate)



# Importação de dados ------------------------------------------


chips <- 
  read_csv("chip_dataset.csv") |> 
  janitor::clean_names() |> 
  filter(type == "GPU") |> 
  drop_na(release_date, transistors_million)


# Faxina de dados ----------------------------------------------


chips_summary <- 
  chips |> 
  select(product, type, release_date, transistors_million, freq_m_hz,
         foundry) |> 
  mutate(foundry = as_factor(foundry),
         release_date = as_date(release_date),
         release_year = year(release_date))


# Visualização -------------------------------------------------



## Preparativos ----------------------------------------------------------



### Títulos ---------------------------------------------------------------


títulos <- list(título = "Tale of a narrow market",
                subtítulo1 = "Distribution of processors foundries and their products",
                subtítulo2 = "Is it possible to observe the Moore's law?",
                legenda = "Tidy Tuesday 2022 #34 | Graph by: @depauladiasleo | Data: (Yifan Sun; Agostini; Shi Dong; Kaeli, 2019)")



### Anotações ------------------------------------------------------------


notas <- c("**What is Moore's law?**", 
           "[...] the observation that the number of transistors<br>in a dense integrated circuit (IC) doubles about every two years.<br>Moore's law is an observation and projection of a historical trend.<br>Rather than a law of physics, it is an empirical relationship linked<br>to gains from experience in production.")



df_notas<- data.frame(x = as_date(c("2000-01-01", "2000-01-01")),
         y = c(48000, 5000),
         labels = notas)


### Fontes ---------------------------------------------------------------


txt_font <- "Segoe UI"


#### Paleta de cores -----------------------------------------------------


foundry_palette <- c("#00BCEF", "#174397", "#EAEAEA", "#e53440", "gray45",
                     "#2D3D92", "#F25B11", "gray5", "#131398")


title_col <- "gray15"

txt_col <- "gray45"


## Gráfico 1 --------------------------------------------------------------


gráfico1 <-
  chips_summary |>
  ggplot() +
  geom_point(aes(x = release_date,
                 y = transistors_million,
                 color = foundry),
             alpha = 0.25) +
  # geom_smooth(
  #   aes(x = release_date,
  #       y = transistors_million),
  #   color = "gray95",
  #   linetype = 3,
  #   size = 0.75,
  #   se = FALSE
  # ) +
  scale_color_manual(
    values = foundry_palette,
    name = "",
    guide = guide_legend(override.aes = list(size = 3,
                                             alpha = 1))
  ) +
  scale_y_log10(labels = scales::label_number()) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(
      color = title_col,
      size = 10,
      angle = 0,
      hjust = 0
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      color = txt_col,
      size = 10,
      angle = 0
    ),
    axis.text.y.left = element_text(
      color = txt_col,
      size = 10,
      angle = 0
    ),
    axis.line.y.left = element_blank(),
    axis.line.y.right = element_line(size = 0.15,
                                     color = txt_col),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.text = element_text(
      size = 12,
      color = title_col,
      hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray99",
      size = 0.1,
      linetype = 3
    ),
    plot.background = element_rect(fill = "gray80",
                                   color = "gray80"),
    plot.margin = margin(2, 2, 2, 2, unit = "cm"),
    plot.title = element_text(
      size = 24,
      color = title_col,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 18,
      color = title_col,
      margin = margin(t = 6, b = 24, unit = "pt")
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = 12,
      color = title_col,
      margin = margin(t = 36, b = 12, unit = "pt"),
      hjust = 0.5
    ),
    plot.caption.position = "plot",
    
  ) +
  labs(
    title = títulos$título,
    subtitle = títulos$subtítulo2,
    caption = títulos$legenda,
    y = "Transistors,\nmillion"
  )



## Gráfico anotado ---------------------------------------------------------

gráfico_anotado <-
  gráfico1 +
  geom_richtext(
    data = df_notas,
    aes(x = x,
        y = y,
        label = labels),
    text.color = "gray5",
    label.size = NA,
    lineheight = 1.15,
    fill = "NA",
    hjust = 0,
    vjust = 0,
    size = 4.5
  )


## Gráfico de Waffle circular  -------------------------------------------------------------


# chips_waffle <-
#   chips_summary |>
#   drop_na(release_date) |>
#   filter(release_date > "2000-03-04") |>
#   add_column(expand.grid(-24:24,-24:24))
# 
# 
# 
# gráfico1 <-
#   chips_waffle |>
#   ggplot(aes(Var1, Var2,
#              color = foundry),
#          size  = 0.75,
#          alpha = 0.8) +
#   scale_color_manual(values = foundry_palette,
#                      aesthetics = "color",
#                      name = "") +
#   geom_point() +
#   theme_void() +
#   theme(
#     legend.position = "top",
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.line = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank(),
#     plot.background = element_rect(fill = "gray80",
#                                    color = "gray80"),
#     plot.margin = margin(2, 2, 2, 2, unit = "cm"),
#     plot.title = element_text(
#       family = txt_font,
#       size = 24,
#       color = title_col,
#       face = "bold"
#     ),
#     plot.subtitle = element_text(
#       family = "Montserrat",
#       size = 18,
#       color = title_col,
#       margin = margin(t = 6, b = 24, unit = "pt")
#     ),
#     plot.title.position = "plot",
#     plot.caption = element_text(
#       family = "Montserrat",
#       size = 12,
#       color = title_col,
#       margin = margin(t = 24, b = 12, unit = "pt"),
#       hjust = 0.5
#     ),
#     plot.caption.position = "plot"
#   ) +
#   labs(title = títulos$título,
#        subtitle = títulos$subtítulo1) +
#   coord_fixed()


# Exportação ---------------------------------------------------


ggsave("TidyTuesday34.png", plot = gráfico_anotado,
       width = 5688, height = 3200, dpi = 320, units = "px")

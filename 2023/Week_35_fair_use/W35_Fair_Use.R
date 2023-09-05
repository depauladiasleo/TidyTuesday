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


tuesdata <- tt_load(2023, week = 35)


df_cases <- tuesdata$fair_use_cases


# Faxina de dados --------------------------------------------------------------


df_cases_tidy <- 
  df_cases |> 
  filter(year > 1850) |> 
  group_by(year, fair_use_found) |> 
  summarize(n = n())



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Total Fair Use cases decisions",
                subtítulo = "Cases in which Fair Use <span style='color:#0E103D'><b>has</b></span> and <span style='color:#69306D'><b>has not</b></span> been found,<br>according to the U.S. Copyright Office Fair Use Index",
                legenda = "Tidy Tuesday | 2023 | Fair Use\nplot by: @depauladiasleo | data from: U.S. Copyright Office Fair Use Index")



## Paleta de cores -------------------------------------------------------------


bg_color <- "#F2D7EE"


palette <- c("#69306D", "#0E103D")


title_color <- "gray10"

text_color = "gray15"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_cases_tidy |> 
  ggplot(aes(x = year,
             y = 0, 
             color = fair_use_found)) +
  geom_segment(aes(xend = year,
                   yend = n),
               linewidth = 1.25) +
  scale_color_manual(values = palette) +
  scale_x_continuous(limits = c(1949, 2023),
                     breaks = c(1950, 2022)) +
  theme_void() +
  theme(plot.background = element_rect(color = bg_color,
                                       fill = bg_color),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(color = title_color,
                                  size = 24,
                                  face = "bold"),
        plot.subtitle = element_markdown(color = title_color,
                                     size = 18,
                                     lineheight = 1.2,
                                     margin = margin(8, 0, 22, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(color = text_color,
                                    size = 16,
                                    lineheight = 1.2,
                                    margin = margin(32, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.text = element_text(size = 12,
                                 color = text_color),
        legend.position = "none",
        legend.title = element_blank()
        ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       captions = títulos$legenda)

  
# Exportação -------------------------------------------------------------------


ggsave("W35.png",
       width = 1024*1.2,
       height = 1024,
       unit = "px",
       dpi = 100)

# Título: Week 32: Ferris wheels
# Script por: Leonardo Dias de Paula
# Última atualização: agosto de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(janitor)
#library(ragg)
library(ggtext)
library(ggrepel)
library(MetBrewer)
library(ggfx)
library(glue)


# Importação de dados ------------------------------------------


url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv"


wheels_full <- read_csv(url)


# Faxina de dados ----------------------------------------------


## Dataset's summary -----------------------------------------------------


skimr::skim(wheels_full)


## Limpeza de observações e variáveis --------------------------------------



wheels_tidy <- 
  wheels_full |> 
  select(height, diameter, seating_capacity, opened, status, name, ...1) |> 
  filter(height != is.na(height)) |> 
  add_column(y = runif(nrow(wheels_tidy), min = 2, max = 3),
             x = runif(nrow(wheels_tidy)), min = 1, max = 20)

wheel_max_min <- 
  wheels_tidy |> 
  filter(height == max(height) | height == min(height))



# Visualização -------------------------------------------------



## Títulos -----------------------------------------------------------------


títulos <- list(título = "Acrofobia?",
                subtítulo = "Sorte que dá para ler este gráfico com os pés no chão.<br><br><span style = 'font-size:12pt; font-weight:bold'>Como ler este gráfico?</span><br><span style = 'font-size:10pt; font-family:Montserrat'>Cada círculo representa uma roda-gigante na base de dados.<br>O tamanho da observação indica a altura máxima do passeio.</span>",
                caption = "Tidy Tuesday 2022 #32 | Gráfico por: @depauladiasleo | Dados: @Emil_Hvitfeldt's {ferriswheels}")


## Gráfico 1 -------------------------------------------------------------


wheel <-
  wheels_tidy |> 
  ggplot() +
  with_inner_glow(geom_jitter(aes(x = ...1,
                                  y = y,
                                  size = height,
                                  fill = opened,
                                  color = opened),
                                  width = 0.15,
                                  alpha = 0.8,
                                  shape = 21),
                  colour = "white",
                  sigma = 5) +
  scale_size_continuous(range = c(.5, 12)) +
  scale_fill_met_c("Greek") +
  scale_color_met_c("Greek") +
  scale_y_continuous(limits = c(0, 3)) +
  coord_polar("x") +
  theme_minimal() +
  theme(
    plot.margin = margin(2, 2, 2, 2, unit = "cm"),
    plot.background = element_rect(fill = "#051C3B",
                                   color = "#051C3B"),
    plot.title = element_text(family = "Montserrat Extrabold",
                              color = "gray90",
                              size = 24,
                              hjust = 0,
                              face = "bold"),
    plot.subtitle = element_markdown(
                                 family = "Montserrat",
                                 color = "gray90",
                                 size = 18, 
                                 margin = margin(t = 6, b = 12)),
    plot.caption = element_markdown(family = "Montserrat",
                                color = "gray90",
                                size = 12, 
                                margin = margin(t = 24),
                                hjust = 0.5),
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none") +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$caption)
  

# Exportação ---------------------------------------------------


ggsave("TidyTuesday32.png", plot = wheel,
       width = 3200, height = 3200, dpi = 320, units = "px")


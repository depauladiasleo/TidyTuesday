# Título: Tidy Tuesday | 2023 | Week 08 | Bob Ross
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, 8)


bob_ross_df <- tuesdata$bob_ross


# Faxina de dados --------------------------------------------------------------


hex_df <- bob_ross_df  |>   
  mutate(color_hex = gsub("\\[|\\]|\\'", "", color_hex)) |> 
  separate_rows(color_hex, sep = ",") |> 
  mutate(color_hex = gsub("[[:space:]]", "", color_hex)) |> 
  group_by(color_hex) |> 
  summarize(n_hex = n())


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "As cores de Bob Ross",
                subtítulo = "Frequência de cada cor nas pinturas de Bob Ross em sua série",
                legenda = "Tidy Tuesday | 2023 | Semana 08 | Bob Ross\nelaborado por: @depauladiasleo | dados de: {bob ross colors}")



## Paleta de cores -------------------------------------------------------------


bg_col <- "#FAC7A3"


title_col <- "gray15"


txt_col <- "gray30"


## Fontes ----------------------------------------------------------------------


font <- "montserrat"
font_add_google(font)


showtext_opts(dpi = 120)
showtext_auto()


theme_set(theme_void(base_family = font))


## Gráfico ---------------------------------------------------------------------


hex_df |> 
  ggplot() +
  geom_col(aes(y = fct_reorder(color_hex, n_hex), 
               x = n_hex,
               fill = color_hex), 
           width = 0.8) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col),
        plot.title = element_text(color = title_col,
                                  size = 24,
                                  face = "bold"),
        plot.subtitle = element_text(color = txt_col,
                                     size = 18,
                                     margin = margin(6, 0, 36, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(color = title_col,
                                    size = 12,
                                    hjust = 0.5,
                                    margin = margin(36, 0, 0, 0, "pt"),
                                    lineheight = 1.25),
        plot.caption.position = "plot",
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)


# Exportação -------------------------------------------------------------------


ggsave("W08_BobRoss.png",
      unit = "px",
      width = 1600,
      height = 900,
      dpi = 120)

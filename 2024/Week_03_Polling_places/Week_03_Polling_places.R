# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(geofacet)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, week = 3)


df <- tuesdata$polling_places


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  group_by(election_date, state) |> 
  summarize(n = n())


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------

títulos <- list(título = "A disponibilidade de locais de votação influencia a participação popular?",
                subtítulo = "Evolução temporal dos locais de votação em 35 estados americanos entre 2012 e 2020",
                legenda = "Tidy Tuesday | 2024 | Semana 03 | Polling places |\ngráfico por: @depauladiasleo | dados: The Center for Public Integrity")


## Paleta de cores -------------------------------------------------------------


col1 <- "#011536"

bg_col <- "gray80"

text_col <- "gray15"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot(aes(election_date, n)) +
  geom_area(fill = col1) +
  scale_x_date(breaks = c(min(df_tidy$election_date), max(df_tidy$election_date)),
                          labels = scales::label_date()) +
  scale_y_continuous(breaks = c(0, 6000, 12000), 
                     labels = scales::label_number(big.mark = " ",
                                                   decimal.mark = ",")) +
  theme_void() +
  theme(plot.background = element_rect(color = bg_col,
                                       fill = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(size = 24,
                                  color = col1,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = text_col,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16,
                                    color = text_col,
                                    lineheight = 1.15,
                                    hjust = 0.5,
                                    margin = margin(20, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        axis.text.y = element_text(color = text_col,
                                 size = 10),
        strip.text = element_text(color = text_col,
                                  size = 12,
                                  face = "bold",
                                  hjust = 0.5)) +
  facet_geo(~ state, grid = "us_state_grid2", label = NULL) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)



# Exportação -------------------------------------------------------------------

ggview::ggview(width = 1600,
               height = 900,
               units = "px",
               dpi = 110)


ggsave("Week_03_Polling_places.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

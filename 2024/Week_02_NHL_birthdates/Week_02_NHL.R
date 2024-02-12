# Título: TidyTuesday | 2024 | Week 02 | NHL players' birthdate
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(ggridges)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 2)


df_birthdate <- tuesdata$nhl_player_births


df_roster <- tuesdata$nhl_rosters


# Faxina de dados --------------------------------------------------------------


hist(df_birthdate$birth_date, breaks = 10)


cities <- 
  df_birthdate |> 
  group_by(birth_city) |> 
  summarize(n = n()) |> 
  arrange(desc(n)) |> 
  head(20) |> 
  select(birth_city)


df_tidy <- 
  inner_join(df_roster, cities) |> 
  select(player_id, birth_date, birth_city, birth_country, 
         height_in_centimeters, weight_in_kilograms) |> 
  mutate(month = month(birth_date),
         year = year(birth_date))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


oilers3 <- "#0c62da"

bg_col <- "#031c40"

oilers1 <- "#ff4c00"

oilers2 <- "#b23500"

title_col <- "gray99"

text_col <- "gray95"

line_col <- "gray85"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  filter(birth_date >= as_date("1985-01-01")) |> 
  ggplot(aes(y = birth_city, x = year)) +
  ggridges::geom_density_ridges(color = oilers1,
                                fill = oilers2,
                                alpha = 0.5) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005)) +
  theme_void() +
  theme(plot.background = element_rect(color = bg_col,
                                       fill = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(color = title_col,
                                  size = 24,
                                  face = "bold",
                                  lineheight = 1.2),
        plot.subtitle = element_text(color = text_col,
                                     size = 18,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(color = text_col,
                                    size = 16,
                                    margin = margin(20, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.text.y = element_text(size = 10,
                                 color = text_col,
                                 hjust = 1),
        axis.text.x = element_text(size = 12,
                                   color = text_col,
                                   hjust = 0.5)) +
  labs(title = "Distribution of players born in the 20 cities with most athletes in NHL\naccording to year of birth",
       subtitle = "How's your town in it?",
       caption = "Tidy Tuesday | 2024 | Week 02 | NHL |\nviz by: @depauladiasleo | data from: NHL API")
  


# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600,
               height = 900,
               units = "px",
               dpi = 110)


ggsave("Week_02_NHL.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

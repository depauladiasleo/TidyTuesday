# Título: TidyTuesday | 2023 | Week 13 | Timezones
# Script por: @depauladiasleo
# Última atualização: julho de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(ggfx)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 13)


df_transitions <- tuesdata$transitions



# Faxina de dados --------------------------------------------------------------


cities <- c("Belem", "Boa Vista", "Cuiaba", 
            "Fortaleza", "Maceio", "Manaus", 
            "Porto Velho", "Recife", "Rio Branco", "Sao Paulo")


cities_relevel <- c("Boa Vista", "Belém", "Fortaleza", 
                    "Manaus", "Recife", "Porto Velho", 
                    "Maceió", "Rio Branco", "Cuiabá", 
                    "São Paulo")


df_tidy <- 
  df_transitions |> 
  group_by(zone) |> 
  slice_max(end) |> 
  mutate(continent = str_extract(zone, "([A-Z].*)(?=/)"),
         city = str_extract(zone, "(?<=/)(.*)") |> 
           str_replace("_", " ") |> 
           as_factor()) |> 
  filter(continent == "America",
         city %in% cities) |> 
  mutate(city = case_when(
                          city == "Cuiaba" ~ "Cuiabá",
                          city == "Sao Paulo" ~ "São Paulo",
                          city == "Maceio" ~ "Maceió",
                          city == "Belem" ~ "Belém",
                          TRUE ~ city) |> 
           as_factor(),
          offset = offset/3600,
          offset_lab = ifelse(offset < 0, as.character(offset), paste0("+", offset)))




# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


bg_color <- "#0b1c24"

alternate_color <- "#068a6b"

title_color <- "#fddd42"

txt_color <- "gray95"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  with_inner_glow(
    geom_point(aes(y = fct_relevel(city, rev(cities_relevel)),
                   x = offset),
               size = 5, 
               color = txt_color),
    colour = title_color,
    sigma = 4,
    expand = 0) +
  scale_x_continuous(limits = c(-5, 0),
                     breaks = c(-5, -4, -3, -2, -1, 0)) +
  theme_void() + 
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        plot.title = element_text(size = 32,
                                  color = title_color,
                                  face = "bold"),
        plot.subtitle = element_text(size = 27,
                                     color = txt_color,
                                     margin = margin(5, 0, 32, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 19,
                                    color = txt_color,
                                    hjust = 0.5,
                                    margin = margin(29, 0, 0, 0, "pt")),
        axis.text = element_text(size = 16,
                                 color = txt_color),
        axis.text.x = element_text(hjust = 0.4),
        axis.text.y = element_text(hjust = 1),
        panel.grid.major.x = element_line(linewidth = 0.4,
                                        linetype = 3,
                                        color = alternate_color)) +
  labs(title = "Fusos horários em dez capitais brasileiras em relação ao Tempo Universal Coordenado",
       subtitle = "Onde você mora, aonde você foi morar?",
       caption = "Tidy Tuesday | 2023 | Week 13 | Time Zones | plot by: @depauladiasleo | data from: {clock} and {tzdb} packages")


# Exportação -------------------------------------------------------------------



ggsave("W13_timezones.png", 
       width = 1024*4*1.77778,
       height = 1024*4,
       dpi = 320,
       units = "px"
       )

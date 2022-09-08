# Título: Tidy Tuesday 2022 #28
# Script por: Leonardo Dias de Paula
# Última atualização: Julho de 2022


# Descrição:


# Preparação ---------------------------------------------------

## Bibliotecas -------------------------------------------------


library(tidyverse)
library(lubridate)
library(ragg)


## Texto -------------------------------------------------


caption_text <- "TidyTuesday 2022 #28 | Gráfico por: @depauladiasleo | Dados: Eurocontrol"

subtitle_text <- "Covid-19 pandemics has severely impacted the number of flights in Europe"


# Importação de dados ------------------------------------------


flt_data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')


# Faxina de dados ----------------------------------------------


flt_summary <- 
  flt_data |>
  janitor::clean_names() |> 
  select(flt_date, apt_icao, apt_name, flt_tot_1, pivot_label) |>
  filter(apt_icao %in% c("EGLL", "LFPG", 
                         "EHAM", "EDDF", 
                         "LTFJ", "LEMD")) |> 
  mutate(week = floor_date(flt_date, unit = "week"),
         apt_name = case_when(
           apt_icao == "EGLL" ~ "London Heathrow",
           apt_icao == "LFPG" ~ "Paris Charles de Gaulle",
           apt_icao == "EHAM" ~ "Amsterdam Schiphol",
           apt_icao == "EDDF" ~ "Frankfurt am Main",
           apt_icao == "LTFJ" ~ "Istanbul Sabiha Gökçen",
           apt_icao == "LEMD" ~ "Madrid Barajas")) |> 
  group_by(week, apt_name) |>
  summarise(flt_tot_1 = sum(flt_tot_1))
  
  
# Visualização -------------------------------------------------


flt_summary |> 
    ggplot(aes(x = week, y = flt_tot_1)) +
    geom_area(fill = "gray75") +
    ggtitle("Weekly arrivals and departures in major European Union airports",
            subtitle = subtitle_text) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#002843"),
      plot.margin = margin(1.5, 1.5, 1.5, 1.5, unit = "cm"),
      plot.title = element_text(color = "Gray85", size = 20,
                                face = "bold",
                                family = "Segoe UI Black",
                                margin = margin(t = 17, b = 3, unit = "pt")),
      plot.subtitle = element_text(color = "Gray60", size = 17,
                                   family = "Segoe UI",
                                   margin = margin(b = 23, unit = "pt")),
      plot.title.position = "plot",
      plot.caption = element_text(color = "gray65", size = 12,
                                  hjust = 0.5, family = "Segoe UI",
                                  face = "bold", vjust = 0,
                                  margin = margin(t = 24, unit = "pt")),
      plot.caption.position = "panel",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "gray65", size = 10, family = "Segoe UI"),
      strip.text = element_text(color = "#044E59", size = 14, hjust = 0,
                                family = "Segoe UI", face = "bold",
                                margin = margin(t = 7, b = 7, unit = "pt")),
      strip.background = element_blank(),
      strip.placement = "outside") +
      facet_wrap(vars(apt_name), nrow = 6) +
  labs(caption = caption_text)


# Exportação ---------------------------------------------------


ggsave("TidyTuesday28.png",
       width = 4800, height = 3600, dpi = 320, units = "px")


ggsave("TidyTuesday28_story.png",
       width = 3240, height = 5760, dpi = 320, units = "px")

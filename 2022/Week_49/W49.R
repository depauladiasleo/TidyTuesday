# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(ggtext)
library(tidyverse)
library(tidytuesdayR)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 49)

elevators <- tuesdata$elevators |> 
             janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_plot <- 
  elevators |> 
  select(borough, dv_device_status_description, device_type, dv_speed_fpm,
         dv_capacity_lbs) |> 
  mutate(elephants = dv_capacity_lbs/15000,
         dv_speed_fpm = as.double(dv_speed_fpm),
         color = case_when(device_type == "Passenger Elevator (P)" ~ "#520B09",
                           device_type == "Freight (F)" ~ "#05524B",
                           device_type == "Escalator (E)" ~ "#520549",
                           TRUE ~ "gray75")) |> 
  filter(borough == "Manhattan",
         dv_capacity_lbs != is.na(dv_capacity_lbs),
         dv_speed_fpm != is.na(dv_speed_fpm))
  

# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Elevators in Manhattan",
                subtítulo = "Highlighted, <span style = 'color:#520B09'><b>Passenger</span></b>, <span style ='color:#05524B'><b>Freight</span></b>, and <span style ='color:#520549'><b>Escalator</span></b> devices",
                y = "Capacity\n(lbs)",
                x = "Speed\n(Feet per minute)",
                caption = "Tidy Tuesday | 2022 | Week 49 | Plot by: @depauladiasleo | Data from:")


## Paleta de cores -------------------------------------------------------------


title_col <- "gray15"
  
sbtitle_col <- "gray35"


## Fontes ----------------------------------------------------------------------


kanit <- "kanit"
font_add_google(kanit)


open_sans <- "open sans"
font_add_google(open_sans)


showtext_auto()

theme_set(theme_minimal(base_family = open_sans))


## Gráfico ---------------------------------------------------------------------


df_plot |> 
  ggplot() +
  geom_point(aes(x = dv_speed_fpm, y = dv_capacity_lbs,
                 fill = color,
                 size = elephants),
             color = "gray99",
             shape = 21,
             alpha = 0.35) +
  scale_y_continuous(labels = scales::number_format()) +
  scale_x_continuous(labels = scales::number_format()) +
  scale_fill_identity() +
  #scale_color_continuous() +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = sbtitle_col,
                                size = 10,
                                angle = 0,
                                hjust = 1),
    axis.title.y = element_text(color = sbtitle_col,
                                size = 10,
                                angle = 0,
                                hjust = 0,
                                vjust = 1),
    axis.text = element_text(size = 8,
                             color = sbtitle_col),
    legend.position = "none",
    plot.background = element_rect(color = "gray95",
                                   fill = "gray95"),
    plot.title = element_text(family = kanit,
                              face = "bold",
                              size = 24,
                              color = title_col),
    plot.subtitle = element_markdown(size = 18,
                                 color = sbtitle_col,
                                 margin = margin(6, 0, 24, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12,
                                color = title_col,
                                margin = margin(36, 0, 0, 0, "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    plot.margin = margin(2, 2, 2, 2, "cm")
  ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       x = títulos$x,
       y = títulos$y,
       caption = títulos$caption
       )


# Exportação -------------------------------------------------------------------
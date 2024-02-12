# Título: Tidy Tuesday | 2024 | Week 01 | BYOD: a new lifestyle
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2024


# Descrição:

# Neste projeto, meu objetivo é visualizar as atividades físicas que fiz em janeiro de 2024,
# quando decidi criar novos hábitos e ser mais saudável. Pretendo criar uma visualização que
# permita observar a quantidade e a duração das atividades ao longo dos dias da semana.


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)



# Importação de dados ----------------------------------------------------------


df <- read_csv("data/activities.csv") |> 
      janitor::clean_names()



# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  select(activity_date, activity_name, activity_type, 
         elapsed_time_6, distance_7, distance_18) |>
  mutate(elapsed_time_min = round(elapsed_time_6/60, 0),
         distance_18 = round(distance_18, 2),
         date = str_sub(activity_date, 1, 12) |>
           str_remove_all(",") |> 
           mdy()
         ) |> 
  select(date, activity_type, elapsed_time_min, distance_18) |> 
  group_by(date) |> 
  summarize(sum_time = sum(elapsed_time_min)/60,
            sum_distance = sum(distance_18)/1000,
            n_activities = n())


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


strava_orange <- monochromeR::generate_palette("#fc5200", "go_darker", 3)


line_col <- "gray50"

text_col <- "gray35"

title_col <- "gray10"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot(aes(date, sum_time)) + 
  geom_point(aes(size = sum_distance,
                 color = as_factor(n_activities))) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_x_date(breaks = as_date(c("2023-12-01", 
                                  "2024-01-01",
                                  "2024-02-01")),
               labels = scales::label_date_short()) +
  scale_color_manual(values = strava_orange) +
  guides(size = guide_legend("Distância percorrida"),
         color = guide_legend("Atividades diárias")) +
  theme_minimal() + 
  theme(plot.background = element_rect(color = "#faf0e6",
                                       fill = "#faf0e6"),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        axis.title = element_text(color = text_col,
                                  size = 12,
                                  face = "bold"),
        axis.text = element_text(color = text_col,
                                   size = 10),
        plot.title = element_text(color = title_col,
                                  size = 24,
                                  face = "bold"),
        plot.subtitle = element_text(color = title_col,
                                     size = 18,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.caption = element_text(color = text_col,
                                   size = 16,
                                   hjust = 0.5,
                                   lineheight = 1.15),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray50",
                                        linetype = 3),
        legend.position = "top"
        ) +
  labs(x = "",
       y = "empenho diário (horas)",
       title = "Novos hábitos para uma vida mais saudável",
       subtitle = "No finalzinho de 2023, voltei a me exercitar. Em março, vou correr minha primeira prova de 5 km", 
       caption = "Tidy Tuesday | 2024 | Semana 01 | BYOD |\ngráfico por: @depauladiasleo | dados: Perfil do autor no Strava") +
  annotate(geom = "text", x = as_date("2024-01-10"), y = 5.7, hjust = 0,
           label = "Romaria a Aparecida do Norte\nCaminho da Fé",
           color = title_col) +
  annotate(geom = "curve",
           x = as_date("2024-01-06"), xend = as_date("2024-01-12"),
           y = 5.12, yend = 5.35, color = text_col,
           arrow = arrow(length = unit(0.15, 'cm'))) +
  annotate(geom = "text", x = as_date("2024-02-10"), y = 2.7, hjust = 1,
           label = "Primeiros 3 km sem caminhar",
           color = title_col) +
  annotate(geom = "curve",
           x = as_date("2024-02-12"), xend = as_date("2024-02-11"),
           y = 0.85, yend = 2.5, color = text_col,
           arrow = arrow(length = unit(0.15, 'cm')))
  
 

# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600,
               height = 900,
               units = "px",
               dpi = 110)


ggsave("Week_01_BYOD.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

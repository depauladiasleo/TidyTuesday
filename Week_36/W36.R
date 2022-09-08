# Título: TidyTuesday 2022 | Week 36
# Script por: Leonardo Dias
# Última atualização: setembro de 2022.


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas

library(tidyverse)
library(NatParksPalettes)
library(ragg)
library(ggstream)


# Importação de dados ------------------------------------------


sets <- read_csv("sets.csv")


themes <- read_csv("themes.csv") 


# Faxina de dados ----------------------------------------------


df <-
  sets |> 
  left_join(themes, by = c("theme_id" = "id")) |> 
  select(year, name.y, name.x, num_parts) |> 
  rename(set_theme = name.y,
         set_name = name.x) |> 
  group_by(set_theme, year) |> 
  summarize(mean_parts = mean(num_parts),
            count_sets = n())


top10_count_sets <- 
  df |> 
  group_by(set_theme) |> 
  summarize(total_sets = sum(count_sets)) |> 
  arrange(-total_sets)


plot_df <- 
  df |> 
  filter(set_theme  %in% c("Star Wars", "Technic", 
                           "Bionicle", "City",
                           "Harry Potter", "Creator",
                           "Architecture"),
         year >= 2002)

# Visualização -------------------------------------------------



## Preparação para o gráfico ---------------------------------------------


### Títulos --------------------------------------------------------------

títulos <- list(título = "No Natal de 2002, ganhei meu único conjunto de Lego",
                subtítulo = "Mas, desde lá, sempre me encantei por alguns temas",
                legenda = "Tidy Tuesday 2022 #36 | Gráfico por: @depauladiasleo | Dados: Rebrickable.com")



### Palette --------------------------------------------------------------

title_col <- "Gray10"

txt_col <- "Gray40"


## Gráfico ---------------------------------------------------------------


plot_df |> 
  ggplot() +
  geom_stream(aes(x = year,
                y = count_sets,
              fill = set_theme),
              extra_span = .2,
              true_range = "none",
              bw = 0.8,
              color = "white",
              size = 0.25,
              sorting = "none") +
  scale_fill_manual(values = natparks.pals("DeathValley"), 
                    name = "Temas", aesthetics = "fill") +
  scale_x_continuous(breaks = c(2002, 2010, 2020)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Quire Sans",
                               size = 10,
                               color = txt_col),
    legend.position = "top",
    legend.title = element_text(family = "Quire Sans",
                                face = "bold",
                                size = 12,
                                color = txt_col),
    legend.text = element_text(family = "Quire Sans",
                               size = 10,
                               color = txt_col),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray95",
                                      size = 0.35,
                                      linetype = 3),
    panel.grid.minor.x = element_blank(),
    panel.ontop = TRUE,
    plot.background = element_rect(fill = "#F5B38A",
                                   color = NA),
    plot.title = element_text(family = "Segoe UI",
                              face = "bold",
                              size = 24,
                              color = title_col),
    plot.subtitle = element_text(family = "Quire Sans",
                                 size = 18,
                                 color = title_col,
                                 margin = margin(t = 6, b = 24, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Quire Sans",
                                face = "bold",
                                size = 12,
                                color = txt_col,
                                margin = margin(t = 48, "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    plot.margin = margin(2, 2, 2, 2, "cm")
  ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda) +
  annotate(geom = "text", x = 1995, y = Inf, 
           label = "Este gráfico representa\no número de conjuntos\nlançados pela Lego\nnas minhas coleções favoritas",
           color = txt_col, 
           size = 4,
           hjust = 0,
           vjust = 1)



# Exportação ---------------------------------------------------
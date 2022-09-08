# Título: TidyTuesday 33 | Open Psychometrics
# Script por: @depauladiasleo
# Última atualização: Agosto de 2022


# Descrição:


# Bibliotecas -----------------------------------------------------------


library(tidyverse)
library(ggrepel)
library(ragg)


# Importação de dados ------------------------------------------


tt <- tidytuesdayR::tt_load(2022, week = 33)


# Faxina de dados ----------------------------------------------


## Questions we want -------------------------------------------------------


questions <- c("emotional/unemotional", "fantastical/realistic",
               "idealist/realist", "optimistic/pessimistic", 
               "ironic/profound")




## Top questions ---------------------------------------------------------


top_questions <- 
  tt$psych_stats |> 
  filter(uni_name == "Calvin and Hobbes") |> 
  select(question, number_ratings) |> 
  group_by(question) |> 
  summarise(number_ratings = sum(number_ratings)) |> 
  arrange(desc(number_ratings)) |> 
  filter(question %in% questions)


## Calvin df -------------------------------------------------------------


calvin <- 
  tt$psych_stats |> 
  filter(uni_name == "Calvin and Hobbes",
         question %in% questions) |>
  select(!c(rank, number_ratings)) |> 
  mutate(y = as.numeric(factor(question)),
         x = case_when(
             str_locate(question, personality)[,1] > 1 ~ avg_rating,
             str_locate(question, personality)[,1] == 1 ~ 100 - avg_rating))


# Visualização -------------------------------------------------


## Preparação para o gráfico ---------------------------------------------


### Fontes ---------------------------------------------------------------


font_title <- "Calvin and Hobbes"

font_text <- "Montserrat"


color_title <- "gray15"

color_text <- "gray35"



### Palette -------------------------------------------------------------

calvin_palette <- c("#CB0C11", "#3d757c", "#96095d", "#ce6904", "#BE619A")

### Títulos --------------------------------------------------------------

títulos <- list(título = "Calvin and his tiger",
                subtítulo = "Explore your personality in Bill Waterson's magical world!",
                legenda = "Tidy Tuesday 2022 #33 | Graph by: @depauladiasleo | Data: Open Psychometrics")

## Gráfico 1 ------------------------------------------------------------



calvin |> 
  ggplot() +
  geom_pointrange(aes(y = question,
                      x = x,
                      xmin = x - rating_sd,
                      xmax = x + rating_sd,
                      color = char_name),
                  alpha = 0.75) +
  geom_point(aes(y = question,
                      x = x,
                      color = char_name)) +
  geom_text(aes(y = question,
                x = x,
                label = personality,
                color = char_name),
            size = 3,
            vjust = -0.85) +
  scale_x_continuous(limits = c(-25, 125),
                     breaks = c(0, 50, 100),
                     expand = expansion()) +
  scale_colour_manual(values = calvin_palette, aesthetics = "color") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#d6f2f9",
                                  color = "#d6f2f9"),
    plot.title = element_text(family = font_title,
                              size = 24,
                              color = color_title),
    plot.subtitle = element_text(family = "Montserrat",
                                 size = 18,
                                 color = color_title,
                                 margin = margin(t = 6, b = 24, unit = "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Montserrat",
                                size = 12,
                                color = color_text,
                                margin = margin(t = 24, b = 12, unit = "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    plot.margin = margin(2, 2, 2, 2, unit = "cm"),
    panel.grid.major.y = element_line(color = "#88bbc9",
                                    size = 0.1),
    panel.grid.major.x = element_line(color = "#88bbc9",
                                      size = 0.2,
                                      linetype = 2),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "cm"),
#   panel.background = element_rect(fill = "#88bbc9",
#                                    color = NA),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Montserrat",
                             size = 10,
                             color = color_text),
    legend.position = "none",
    strip.text = element_text(family = font_title,
                              size = 12,
                              color = color_text,
                              hjust = 0)) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda) +
  facet_wrap("char_name", nrow = 1)



# Exportação ---------------------------------------------------


ggsave("TidyTuesday33.png",
       width = 5688, height = 3200, dpi = 320, units = "px")

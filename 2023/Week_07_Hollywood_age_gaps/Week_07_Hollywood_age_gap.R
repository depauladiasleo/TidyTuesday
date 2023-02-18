# Título: Tidy Tuesday | 2023 | Week 07 | Age gaps
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(ggtext)
library(ggstar)
library(showtext)
library(tidyverse)
library(tidytuesdayR)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, week = 7)


age_gap_df <- tuesdata$age_gaps


# Faxina de dados --------------------------------------------------------------


df_tidy <-  
  age_gap_df |> 
  select(age_difference:actor_2_age) |> 
  mutate(color = case_when(age_difference > 0 ~ character_1_gender,
                           TRUE ~ "gray80") |> 
           str_replace("^man$", "#010A33") |>
           str_replace("woman", "#BD1735"),
         sexuality = case_when(character_1_gender == character_2_gender ~ "same sex",
                               TRUE ~ "different sex"))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Age gap in Hollywood movies",
                subtitle = "Axes represent each actors' age when filming",
                legenda = "Tidy Tuesday | 2023 | Week 07 | Age gap in Hollywood | plot by: @depauladiasleo | data from: Hollywood Age Gap")


couples <- tibble(x = 15,
                  y = 60,
                  label = "Couples in which <b><span style='color:#010A33'>men</b></span> or <span style='color:#BD1735'><b>women</b></span> are the oldest partner.<br><b><span style='color:#B08915'>Same sex couples starring</b></span> in Hollywood.")


## Paleta de cores -------------------------------------------------------------


title_col <- "gray10"

txt_col <- "gray35"


star_col <- "#B08915"

men <- "#010A33"

women <- "#BD1735"



## Fontes ----------------------------------------------------------------------


kanit <- "kanit"
font_add_google(kanit)

open_sans <- "open sans"
font_add_google(open_sans)


theme_set(theme_void(base_family = open_sans))

showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_point(aes(x = actor_1_age,
                 y = actor_2_age,
                 color = color,
                 size = age_difference),
             alpha = 0.75) +
  geom_star(data = df_tidy |> 
               filter(sexuality == "same sex"),
             aes(x = actor_1_age,
                 y = actor_2_age),
            color = star_col,
            fill = star_col, 
            size = 2.5) +
  ggtext::geom_richtext(data = couples,
                        aes(x = x,
                            y = y,
                            label = label),
                        family = open_sans,
                        fill = "#F1DBFF",
                        lineheight = 1.25,
                        size = 5,
                        label.size = 0,
                        color = title_col,
                        hjust = 0) +
  scale_color_identity() +
  scale_size_area(max_size = 2) +
  scale_x_continuous(limits = c(15, 85)) +
  scale_y_continuous(limits = c(15, 85)) +
  theme_void() +
  theme(axis.text = element_text(size = 10,
                                 color = txt_col),
        legend.position = "none",
        plot.background = element_rect(fill = "#F1DBFF",
                                       color = "#F1DBFF"),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(family = kanit,
                                  size = 24,
                                  color = title_col,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = txt_col,
                                     margin = margin(10, 0, 26, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 12,
                                    color = txt_col,
                                    # face = "bold",
                                    hjust = 0.5,
                                    margin = margin(24, 0, 0, 0, "pt")),
        plot.caption.position = "plot") +
  labs(title = títulos$título,
       subtitle = títulos$subtitle,
       caption = títulos$legenda)



# Exportação -------------------------------------------------------------------


ggsave("Week_07_Hollywood_age_gap.png",
       units = "px",
       height = 2304,
       width = 5368,
       dpi = 320)

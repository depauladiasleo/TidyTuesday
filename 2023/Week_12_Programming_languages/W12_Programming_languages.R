# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, week = 12)


df_full <- tuesdata$languages


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_full |> 
  filter(is_open_source == TRUE) |> 
  mutate(github_language_type = replace_na(github_language_type, "no description") |> 
           as_factor()) |> 
  group_by(github_language_type) |> 
  summarize(n = n())



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


bg_color = "#F1DAC4"

bar_color = "#161B33"

txt_color = "#474973"

title_color = "#0D0C1D"


## Fontes ----------------------------------------------------------------------


roboto <- "roboto"

font_add_google(roboto)


showtext_opts(dpi = 320)
showtext_auto()


theme_set(theme_void(base_family = roboto))


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot(aes(x = n,
             y = fct_reorder(github_language_type, n))) +
  geom_col(fill = bar_color) +
  scale_x_continuous(limits = c(0, 300),
                     breaks = c(0, 50, 100, 150, 200, 250)) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(color = title_color,
                                  size = 36,
                                  face = "bold"),
        plot.subtitle = element_text(color = title_color,
                                     size = 24,
                                     lineheight = 1.5,
                                     margin = margin(12, 0, 36, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(color = title_color,
                                    size = 18,
                                    margin = margin(30, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.text = element_text(size = 16,
                                 color = txt_color),
        axis.text.y = element_text(hjust = 1),
        panel.grid.major.x = element_line(color = txt_color,
                                          linewidth = .5,
                                          linetype = 3)) +
  labs(title = "Open Source Programming languages",
       subtitle = "As per their type classification in GitHub",
       caption = "Tidy Tuesday 2023 | Week 12 | Programming languages\nplot by: @depauladiasleo | data from: Programming Language DataBase")


# Exportação -------------------------------------------------------------------


ggsave("W12_Programming_languages.png",
       unit = "px",
       height = 1024*4,
       width = 1024*1.7778*4,
       dpi = 320)

# Título: TidyTuesday | 2024 | Week 09 | Leap Day
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2024, 9)


df_event <- tuesdata$events



# Faxina de dados --------------------------------------------------------------


marcos <- df_event |> 
  filter(year %in% c(1712, 1916, 1940, 1988, 1996, 2004, 2020)) 


df_tidy <- marcos[c(1, 3, 4, 7, 8, 10, 11, 14), ]


texto <- df_tidy |> 
         mutate(x = c(1700, 1860, 1905, 1915, 1960, 1980, 2012, 2020),
                y = c(1.5, 5, 1.5, 5, 1.5, 5, 1.5, 5))


curvas <- tibble(x = df_tidy$year,
                 xend = c(1710, 1885, 1925, 1955, 1970, 1996, 2012, 2035),
                 y = 2.5,
                 yend = c(1.75, 3.45, 1.75, 4.1, 1.75, 4.35, 1.75, 3.9))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Historical events on February 29th across the time",
                subtítulo = "What else has happened on Leap Day?",
                legenda = "Tidy Tuesday | Week 09 | Leap day | graph by: @depauladiasleo | data from: Wikipedia")


## Paleta de cores -------------------------------------------------------------


col1 <- "#ec492d"

col2 <- "#66b3c3"

bg_col <- "#fffcbc"


text_col <- "#242020"

title_col <- "gray5"


## Fontes ----------------------------------------------------------------------


fraunces <- "Fraunces"
font_add_google(fraunces)

open_sans <- "Open sans"
font_add_google(open_sans)

showtext_opts(dpi = 110)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df_event |> 
  filter(year > 1700) |> 
  ggplot(aes(x = year, y = 2.5)) +
  geom_jitter(color = col2,
              alpha = 0.5,
              size = 1.5) +
  geom_point(data = df_tidy,
             aes(x = year, y = 2.5),
             size = 3,
             color = col1) +
  geom_curve(data = curvas,
             aes(x = x,
                 xend = xend,
                 y = y,
                 yend = yend),
             color = col1,
             arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = texto, 
            aes(x = x, y = y,
            label = str_wrap(event, 20)),
            color = text_col, family = open_sans,
            hjust = 0, vjust = 1) +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(limits = c(1700, 2050)) +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(color = bg_col,
                                       fill = bg_col),
        plot.title = element_text(size = 24, 
                                  family = fraunces,
                                  color = title_col,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     family = open_sans,
                                     color = title_col,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    family = open_sans,
                                    color = text_col,
                                    hjust = 0.5,
                                    margin = margin(34, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        axis.text.x = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col),
        panel.grid.major.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1),
        panel.grid.minor.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1)) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)


# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600*1.08,
               height = 900*1.08,
               units = "px",
               dpi = 110)


ggsave("Week_09_Leap_day.png",
       width = 1600*1.08,
       height = 900*1.08,
       units = "px",
       dpi = 110)

# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 16)


df_full <- tuesdata$founder_crops


# Faxina de dados --------------------------------------------------------------

df_tidy <- 
  df_full |> 
  group_by(category) |> 
  summarize(sum_prop = sum(prop)) |> 
  drop_na() |>
  arrange(desc(sum_prop)) |> 
  mutate(category = as_factor(category),
         pct = sum_prop/sum(sum_prop),
         pct_label = paste(round(pct*100, 0)),
         y_label = cumsum(pct) - pct/4)


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


color1 <- "#9B2226"

color2 <-  "#001219"

color3 <- "#005F73"

color4 <- "#0A9396"


palette <- c(color1, color2, color3, color4)



bg_color <- "#E9D8A6"

title_color <- "gray15"

text_color <- "gray25"



## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_col(aes(0.5, pct, fill = fct_reorder(category, pct)),
           color = "white",
           width = 0.8,
           position = "stack") +
  geom_text(aes(1, y_label, label = category,
                color = fct_reorder(category, pct)),
            hjust = 0) +
  geom_text(aes(1, y_label - 0.03,
            label = paste(pct_label, "%"),
            color = fct_reorder(category, pct)),
            hjust = 0) +
  geom_segment(aes(x = 0.9, xend = 1,
                y = y_label - 0.015, yend = y_label - 0.015,
                color = category),
               linetype = 2) +
  geom_point(aes(0.9, y = y_label - 0.015,
                 color = category),
             size = 1.2) +
  scale_x_continuous(limits = c(0, 1.25)) +
  scale_fill_manual(values = rev(palette)) +
  scale_color_manual(values = rev(palette)) +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        plot.title = element_text(size = 24,
                                  color = title_color,
                                  face = "bold",
                                  margin = margin(0, 0, 36, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16,
                                    color = text_color,
                                    hjust = 0.5,
                                    margin = margin(32, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        legend.position = "none") +
  labs(title = "Founder crops proportion",
       caption = "Tidy Tuesday | 2023 | Week 16 | Founder Crops | plot by: @depauladiasleo | data from: @joeroe")


# Exportação -------------------------------------------------------------------



ggsave("W16_Founder_crops.png",
       width = 1368,
       height = 768,
       dpi = 100,
       unit = "px")

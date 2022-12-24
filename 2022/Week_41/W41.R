# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(ggdist)
library(showtext)
library(MetBrewer)


# Importação de dados ------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 41)


yarn <- tuesdata$yarn


# Faxina de dados ----------------------------------------------

top10 <- 
  yarn |> 
  group_by(yarn_company_name) |> 
  summarize(rate_avg = sum(rating_total)/sum(rating_count),
            rate_count = sum(rating_count)) |> 
  filter(rate_count > 10000) |> 
  arrange(desc(rate_avg)) |> 
  slice_head(n = 10)




df <- 
  yarn |> 
  filter(yarn_company_name %in% top10$yarn_company_name)


# Visualização -------------------------------------------------


## Títulos ---------------------------------------------------------------


títulos <- list(título = "Yarn manufacturers' rating according to Ravelry.com",
                subtítulo = "Manufacturer's with more than 10k ratings from users",
                legenda = "Tidy Tuesday 2022 #41 | Gráfico por: @depauladiasleo | Dados: Raverly.com")


## Paleta ----------------------------------------------------------------


bg_col <- "#E3DACD"

grid_col <- "#795F4C"

text_col <- "#453330"



## Fonts -----------------------------------------------------------------


font_title <- "Kanit"


font_text <- "Montserrat"


font_add_google(font_title, font_title)


font_add_google(font_text, font_text)


showtext_opts(dpi = 320)
showtext_auto()


## Gráficos --------------------------------------------------------------


  df |> 
  ggplot(aes(rating_average, 
             yarn_company_name,
             fill = yarn_company_name,
             color = yarn_company_name)) +
  stat_slab(scale = 0.75,
            alpha = 0.55,
            color = NA) +
  stat_dotsinterval(slab_color = "gray85", 
                    side = "bottom", scale = 0.7, 
                    show_interval = FALSE) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_fill_met_d("Signac") +
  scale_color_met_d("Signac") +
  theme_void() +
  theme(axis.text = element_text(size = 10, 
                                 color = text_col),
        axis.text.y = element_text(hjust = 1),
        panel.grid.major = element_line(color = grid_col,
                                        size = 0.1),
        plot.background = element_rect(fill = bg_col,
                                       color = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(family = font_title,
                                  size = 24,
                                  color = text_col),
        plot.subtitle = element_text(family = font_text,
                                     size = 18,
                                     color = "gray30",
                                     margin = margin(6, 0, 36, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(family = font_text,
                                    size = 14, 
                                    face = "bold",
                                    color = text_col,
                                    hjust = 0.5,
                                    margin = margin(36, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        legend.position = "none") +
  labs(
    title = títulos$título,
    subtitle = títulos$subtítulo,
    caption = títulos$legenda
  )


# Exportação ---------------------------------------------------


ggsave("TidyTuesday41.png", 
       width = 5120, height = 2880, dpi = 320, units = "px")

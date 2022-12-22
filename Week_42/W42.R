# Título: Week 42
# Script por: Leonardo Dias de Paula
# Última atualização: Outubro de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(ggstream)
library(cartography)
library(ragg)


# Importação de dados ------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 42)


dialogue_df <- tuesdata$stranger_things_all_dialogue


episodes_df <- tuesdata$episodes


# Faxina de dados ----------------------------------------------


df <- 
  episodes_df |> 
  group_by(season, written_by) |> 
  summarize(n_episodes = n())


# Visualização -------------------------------------------------



## Titles ----------------------------------------------------------------


títulos <- list(título = "Diversity of writers in Stranger Things",
                 subtítulo = "Episodes written by each author in a season",
                 legenda = "Tidy Tuesday 2022 #42 | Gráfico por: @depauladiasleo | Dados: {stringr_things}")



## Pallete ---------------------------------------------------------------


white <- "gray97"

bg_color <- "#1F2933"


## Plot ------------------------------------------------------------------


df |> 
  ggplot(aes(x = season,
             y = n_episodes,
             fill = written_by)) +
  geom_stream(color = "#616E7C",
              size = 0.1,
              extra_span = .2,
              true_range = "none",
              n_grid = 10000,
              bw = 1,
              sorting = "onset") +
  scale_fill_manual(name = "Writers, as presented in the stream", 
                    values = carto.pal("wine.pal")) +
  theme_void() +
  theme(
    panel.grid.major.x = element_line(linetype = 3,
                                      size = 0.25,
                                      color = "#616E7C"),
    axis.text.x = element_text(family = "Segoe UI",
                             size = 10, 
                             color = "#616E7C"),
    axis.title.x = element_text(family = "Segoe UI",
                                size = 10, 
                                color = "white",
                              #  face = "bold",
                                hjust = 1),
    plot.background = element_rect(fill = bg_color,
                                   color = bg_color),
    plot.title = element_text(family = "Segoe UI",
                              size = 24,
                              color = "gray95",
                              face = "bold"),
    plot.subtitle = element_text(family = "Segoe_UI",
                                 size = 17,
                                 color = "#616E7C",
                                 margin = margin(6, 0, 36, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe_UI",
                    size = 14,
                    color = "#616E7C",
                    margin = margin(36, 0, 0, 0, "pt"),
                    hjust = 0.5),
    plot.caption.position = "plot",
    plot.margin = margin(2, 2, 2, 2, "cm"),
    legend.text = element_text(family = "Segoe_UI",
                               color = white,
                               size = 8),
    legend.title = element_text(family = "Segoe_UI",
                                color = white,
                               size = 10,
                               face = "bold")) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)


## Plot 2 ----------------------------------------------------------------


episodes_df |>
  mutate(
    season_lbl = case_when(
      season == 1 ~ "1st Season",
      season == 2 ~ "2nd Season",
      season == 3 ~ "3rd Season",
      season == 4 ~ "4th Season",
    )
  ) |>
  ggplot(aes(y = written_by)) +
  geom_bar(fill = txt_col,
           #color = "#CC5C5C",
           #size = 0.25,
           width = 0.75) +
  #scale_fill_manual(name = "Writers, as presented in the stream",
  #                  values = carto.pal("wine.pal")) +
  theme_void() +
  theme(
    panel.grid.major.x = element_line(
      linetype = 3,
      size = 0.25,
      color = txt_col
    ),
    axis.text.x = element_text(
      family = "Segoe UI",
      size = 12,
      color = txt_col
    ),
    axis.text.y = element_text(
      family = "Segoe UI",
      size = 12,
      color = txt_col,
      hjust = 1
    ),
    axis.title.x = element_text(
      family = "Segoe UI",
      size = 12,
      color = white,
      #  face = "bold",
      hjust = 1
    ),
    plot.background = element_rect(fill = bg_color,
                                   color = bg_color),
    plot.title = element_text(
      family = "Segoe UI",
      size = 24,
      color = white,
      face = "bold"
    ),
    plot.subtitle = element_text(
      family = "Segoe_UI",
      size = 17,
      color = txt_col,
      margin = margin(6, 0, 36, 0, "pt")
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      family = "Segoe_UI",
      size = 14,
      color = txt_col,
      margin = margin(36, 0, 0, 0, "pt"),
      hjust = 0.5
    ),
    plot.caption.position = "plot",
    plot.margin = margin(2, 2, 2, 2, "cm"),
    legend.position = "none",
    strip.text = element_text(
      family = "Segoe_UI",
      face = "bold",
      size = 14,
      color = txt_col,
      hjust = 0,
      margin = margin(0, 0, 12, 0, "pt")
    )
  ) +
  labs(
    title = títulos$título,
    subtitle = títulos$subtítulo,
    caption = títulos$legenda
  ) +
  facet_wrap("season_lbl", ncol = 1,
             scales = "free_y")



# Exportação ---------------------------------------------------


ggsave(
  "TidyTuesday42.png",
  width = 6741,
  height = 3792,
  dpi = 320,
  units = "px"
)




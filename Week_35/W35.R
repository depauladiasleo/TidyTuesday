# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação ---------------------------------------------------

## preparando %Notin%


`%notin%` <- Negate(`%in%`)


## Bibliotecas -------------------------------------------------


library(tidyverse)
library(geofacet)
library(janitor)
library(ragg)


# Importação de dados ------------------------------------------


tt_data <- tidytuesdayR::tt_load(2022, week = 35)


df <- 
  tt_data$pell |> 
  clean_names()


# Faxina de dados ----------------------------------------------


df_tidy <- 
  df |> 
  group_by(state, year) |> 
  summarise(sum_award = sum(award),
            sum_recipient = sum(recipient))


# Visualização -------------------------------------------------



## Títulos -----------------------------------------------------------------


títulos <- 
  list(título = "pell grants",
       subtítulo = usefunc::str_wrap_break("Federal Pell Grants usually are awarded only to undergraduate students who display exceptional financial need and have not earned a bachelor's, graduate, or professional degree.", 105),
       legenda = "Tidy Tuesday 2022 #35 | Graph by: @depauladiasleo | Data: U.S. Department of Education")


## Palette ---------------------------------------------------------------


blue <- "#011536"


## Gráfico 1: geom_statebins ---------------------------------------------


df_tidy |> 
  filter(year == 2000) |> 
  ggplot() +
  geom_statebins(aes(state = state, 
                     fill = sum_award),
                 border_size = 1,
                 border_col = "gray90") +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "Segoe UI",
                                face = "bold",
                                size = 12,
                                color = blue,
                                margin = margin(r = 24, "pt")),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "gray90",
                                   color = "gray90"),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(family = "Segoe UI",
                              face = "bold",
                              size = 24,
                              color = "gray15"),
    plot.subtitle = element_text(family = "Segoe UI",
                                 size = 17,
                                 color = "gray15",
                                 margin = margin(t = 7, b = 24, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe UI",
                                size = 14,
                                color = "gray15",
                                margin = margin(t = 46),
                                hjust = 0.5),
    plot.caption.position = "plot",
    strip.text = element_text(family = "Segoe UI",
                              face = "Bold",
                              size = 10,
                              color = blue,
                              hjust = 0))



## Gráfico 2: geofacet ---------------------------------------------------


df_tidy |>
  filter(state %notin% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI")) |>
  ggplot(aes(x = year,
             y = sum_award)) +
  geom_area(fill = "#630000") +
  facet_geo(~ state, grid = "us_state_grid2", label = NULL) +
  scale_x_continuous(breaks = c(2000, 2017),
                     label = c("\'00", "\'17")) +
  scale_y_continuous(
    breaks = c(0, 4000000000),
    labels = scales::unit_format(
      prefix = "$",
      sep = " ",
      unit = "B",
      scale = 1e-9
    )
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = "Segoe UI",
      face = "bold",
      size = 12,
      color = blue,
      margin = margin(r = 24, "pt")
    ),
    axis.text = element_text(family  = "Montserrat",
                             color = "gray45",
                             size = 10),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "gray90",
                                   color = "gray90"),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(
      family = "Montserrat",
      face = "bold",
      size = 24,
      color = "gray15"
    ),
    plot.subtitle = element_text(
      family = "Segoe UI",
      size = 17,
      color = "gray15",
      margin = margin(t = 7, b = 24, "pt")
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      family = "Segoe UI",
      size = 14,
      color = "gray15",
      margin = margin(t = 46),
      hjust = 0.5
    ),
    plot.caption.position = "plot",
    strip.text = element_text(
      family = "Segoe UI",
      face = "bold",
      size = 10,
      color = blue,
      hjust = 0
    )
  ) +
  labs(
    title = títulos$título,
    subtitle = títulos$subtítulo,
    caption = títulos$legenda,
    x = "",
    y = "awards sum"
  )


# Exportação ---------------------------------------------------


ggsave("TidyTuesday35.png",
      width = 5688, height = 3792, dpi = 320, units = "px")


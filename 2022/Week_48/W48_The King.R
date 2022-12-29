# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(ggtext)
library(showtext)
library(tidyverse)
library(tidytuesdayR)
library(glue)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 48)

wcmatches <- tuesdata$wcmatches


# Faxina de dados --------------------------------------------------------------

df <-
  wcmatches |>
  filter(#stage %in% c("Round of 16", "Quarterfinals", "Semifinals", "Final", "Thid place"),
    home_team == "Brazil" | away_team == "Brazil") |>
  select(year:losing_team, date,-win_conditions) |>
  mutate(
    brasil = case_when(home_team == "Brazil" ~ home_team,
                       TRUE ~ away_team),
    other = case_when(home_team != "Brazil" ~ home_team,
                      TRUE ~ away_team),
    brasil_score = case_when(home_team == "Brazil" ~ home_score,
                             TRUE ~ away_score),
    other_score = case_when(home_team != "Brazil" ~ home_score,
                            TRUE ~ away_score)
  ) |>
  select(year, date, stage, brasil, brasil_score, other, other_score) |>
  pivot_longer(
    cols = c(brasil, other),
    names_to = "cntry",
    values_to = "team"
  ) |>
  mutate(
    score = case_when(team == "Brazil" ~ brasil_score,
                      TRUE ~ other_score),
    color = case_when(team == "Brazil" ~ "#DB9C09",
                      TRUE ~ "gray30")
  )




# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------

títulos <- list(título = "The king is gone, but its kingdom shall live",
                #subtitle = glue("<b><span style='color:#DB9C09'> Brasil</span></b> games in {df$year}'s World Cup"),
                caption = "Tidy Tuesday | 2022 | Week 48 | Plot by: @depauladiasleo | Data from: FIFA World Cup")

## Paleta de cores -------------------------------------------------------------


## Fontes ----------------------------------------------------------------------


open_sans <- "open sans"
kanit <-  "kanit"


font_add_google(open_sans)
font_add_google(kanit)


showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


copa <- function(df, ano) {
df |>
  filter(year == ano) |>
  ggplot(aes(y = date, x = score)) +
  ggtitle(label = títulos$título, 
          subtitle = glue("<b><span style='color:#DB9C09'> Brasil</span></b> games in {ano}'s World Cup")) +
  geom_segment(aes(
    x = brasil_score,
    xend = other_score,
    y = date,
    yend = date
  ),
  color = "gray") +
  geom_text(
    data = df |>
      filter(year == ano & team != "Brazil"),
    aes(
      label = team,
      x = score,
      y = date),
      color = "gray30",
      vjust = -1.25,
    hjust = 0.1,
    size = 4
  ) +
  geom_point(aes(color = color),
             size = 3) +
  scale_x_continuous(name = "goals\nscored",
                     limits = c(0, 7),
                     breaks = c(1, 2, 3, 4, 5, 6, 7)) +
  scale_y_date(expand = expansion(mult = .1)) +
  scale_color_identity() +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(family = kanit,
                                face = "bold",
                                size = 11,
                                color = "gray15",
                                hjust = 1),
    axis.text = element_text(family = open_sans,
                             color = "gray30",
                             size = 10),
    plot.title = element_text(family = kanit,
                              face = "bold",
                              size = 24,
                              color = "gray15"),
    plot.subtitle = element_markdown(family = kanit,
                                 size = 18,
                                 color = "gray30",
                                 margin = margin(6, 0, 24, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = open_sans,
                                size = 12,
                                color = "gray15",
                                margin = margin(44, 0, 0, 0, "pt"),
                                hjust = 0.5),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.background = element_rect(fill = "gray90")
  ) +
  labs(
    #title = títulos$título,
      # subtitle = títulos$subtitle,
       caption = títulos$caption)
}


copa(df, 2014)


# Exportação -------------------------------------------------------------------


ggsave("The king (2014).png",
       width = 4096,
       height = 2320,
       unit = "px",
       dpi = 320)

# Título: Gender in Peacekeeping
# Script por: Leonardo Dias de Paula
# Última atualização: Julho de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(ggtext)
library(ragg)


# Importação de dados ------------------------------------------


gender_full <- read_csv("full_gender.csv")


# Faxina de dados ----------------------------------------------


## Países na América do Sul ----------------------------------------------


sa_countries <- c("Argentina", "Bolivia", "Brazil",
                  "Chile", "Colombia", "Ecuador",
                  "Paraguay", "Peru", "Uruguay",
                  "Venezuela")


sa_países <- c("Argentina", "Bolívia", "Brasil",
               "Chile", "Colômbia", "Equador",
               "Paraguai", "Peru", "Uruguai")


## Summary -----------------------------------------------------------------


gender_pk_sa <-
  gender_full |> 
  select(Date, Contributor, 
         Individual_Police_Female,
         Individual_Police_Total, 
         Formed_Police_Units_Female, Formed_Police_Units_Total,
         Experts_on_Mission_Female,  Experts_on_Mission_Total,
         Troops_Female, Troops_Total) |> 
  filter(Contributor %in% sa_countries) |> 
  group_by(Contributor, Date) |> 
  summarize(across(.cols = c(Individual_Police_Female:Troops_Total), sum)) |> 
  mutate(Individual_police_rate = Individual_Police_Female/Individual_Police_Total,
         #Formed_police_rate = Formed_Police_Units_Female/Formed_Police_Units_Total,
         Experts_rate = Experts_on_Mission_Female/Experts_on_Mission_Total,
         Troops_rate = Troops_Female/Troops_Total) |> 
  select(!c(Individual_Police_Female:Troops_Total)) |> 
  group_by(Contributor) |> 
  summarize(across(.cols = Individual_police_rate:Troops_rate, mean, na.rm = TRUE)) |> 
  pivot_longer(Individual_police_rate:Troops_rate,
               names_to = "Contingent", values_to = "Female_rate") |> 
  mutate(Contingent = recode(Contingent, "Individual_police_rate" = "Policiais individuais",
                             "Experts_rate" = "Observadoras",
                             "Troops_rate" = "Tropas"),
         Contributor = recode(Contributor, "Argentina" = "Argentina",
                              "Bolivia" = "Bolívia",
                              "Brazil" = "Brasil",
                              "Chile" = "Chile",
                              "Colombia" = "Colômbia",
                              "Ecuador" = "Equador",
                              "Peru" = "Peru",
                              "Paraguay" = "Paraguai",
                              "Uruguay" = "Uruguai") |> 
           as_factor() |> fct_relevel(rev(sa_países)))


# Visualização -------------------------------------------------


## Paleta ------------------------------------------------------------------


un_blue <- "#009EDB"


# Títulos -----------------------------------------------------------------


títulos <- list(title = "Na América do Sul, a <span style = 'color:#009EDB;'> participação média</span> de mulheres nos contingentes de<br>Operações de Paz da ONU é pequena",
                subtitle = "Entre 2009 e 2020, nenhum país chegou próximo à paridade de gênero em suas contribuições",
                caption = "Tidy Tuesday 2022 #30 | Gráfico por: @depauladiasleo | Dados: International Peace Institute")


## Gráfico -----------------------------------------------------------------


gender_pk_sa |> 
  ggplot() +
  geom_point(aes(y = Contributor, x = Female_rate), 
             color = un_blue,
             size = 1.15) +
  geom_segment(aes(y = Contributor, yend = Contributor,
                   x = 0, xend = Female_rate), 
               color = un_blue,
               size = 0.9) +
  scale_x_continuous(limits = c(0, 0.5), 
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     labels = scales::label_percent()) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(family = "Segoe UI",
                               size = 12,
                               face = "bold",
                               color = "gray15"),
    panel.grid.major = element_line(color = "gray20",
                                    size = 0.05),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "gray90",
                                   color = "gray90"),
    plot.margin = margin(2, 2, 2, 2, unit = "cm"),
    plot.title = element_markdown(family = "Segoe UI",
                              size = 24,
                              color = "black",
                              lineheight = 1.2),
    plot.subtitle = element_markdown(family = "Segoe UI",
                                 size = 18,
                                 face = "plain",
                                 color = "gray20",
                                 margin = margin(t = 6, b = 24, unit = "pt"),
                                 lineheight = 1.15),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe UI",
                                size = 12,
                                face = "plain",
                                color = "gray20",
                                margin = margin(t = 24, unit = "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    strip.text = element_text(family = "Segoe UI",
                              size = 10,
                              color = "gray15",
                              face = "bold",
                              hjust = 0)
  ) +
  labs(title = títulos$title,
       subtitle = títulos$subtitle,
       caption = títulos$caption,
       x = "",
       y = "") +
  facet_wrap("Contingent", ncol = 3)


# Exportação ---------------------------------------------------


ggsave("TidyTuesday30_pk.png",
       width = 4800, height = 3600, dpi = 320, units = "px") # Check minor adjustments when saving again


ggsave("TidyTuesday30_story_pk.png",
       width = 3240, height = 5760, dpi = 320, units = "px")

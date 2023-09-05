# Título: TidyTuesday | 2023 | Week 34 | Refugees
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(geomtextpath)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 34)


df_full <- tuesdata$population


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_full |> 
  filter(coa %in% c("ARG", "BOL", "BRA", "CHL", "COL", "GUY", "PER",
                    "URU", "PAR", "SUR", "VEN")) |> 
  group_by(coa_name, coa, year) |> 
  summarize(refugees = sum(refugees),
            # returned_refugees = sum(returned_refugees),
            asylum_seekers = sum(asylum_seekers),
            idps = sum(idps),
            # returned_idps = sum(returned_idps),
            stateless = sum(stateless),
            ooc = sum(ooc),
            oip = sum(oip),
            hst = sum(hst)
            ) |> 
  pivot_longer(cols = refugees:hst, names_to = "type", values_to = "n") |> 
  mutate(type = case_when(type == "asylum_seekers" ~ "asylum seekers",
                          TRUE ~ type))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "A harsh reality",
                subtítulo = "Asylum seekers and refugees growth as reported in Brasil",
                legenda = "Tidy Tuesday | 2023 | Week 34 | plot by: @depauladiasleo | data from: {refugees}")


## Paleta de cores -------------------------------------------------------------


line_palette <- c("#FFFFFF", "#BBAB9B")


bg_color <- "#371722"


text_color <- "gray90"


title_color <- "gray99"




## Fontes ----------------------------------------------------------------------


open_sans <- "open sans"
font_add_google(open_sans)


showtext_opts(dpi = 200)
showtext_auto()

## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  filter(coa == "BRA",
         type %in% c("asylum seekers", "refugees")) |> 
  ggplot(aes(x = year, y = n,
             group = type,
             color = type)) +
  geom_textline(aes(label = type)) +
  scale_color_manual(values = line_palette) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)) +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(color = bg_color,
                                       fill = bg_color),
        plot.title = element_text(family = open_sans,
                                  size = 24,
                                  face = "bold",
                                  color = title_color,
                                  hjust = 0
                                  ),
        plot.subtitle = element_text(family = open_sans,
                                     size = 18,
                                     color = line_palette[[2]],
                                     hjust = 0,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(family = open_sans,
                                    size = 14,
                                    color = title_color,
                                    margin = margin(34, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(family = open_sans,
                                 size = 12,
                                 color = text_color),
        axis.title.y = element_text(family = open_sans,
                                    size = 12,
                                    # face = "bold",
                                    color = line_palette[[2]],
                                    lineheight = 1.2,
                                    vjust = 1)
        ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda,
       y = "displaced\nindividuals")


# Exportação -------------------------------------------------------------------



ggsave("W34.png",
       height = 1024*2,
       width = 1024*2*1.77,
       unit = "px",
       dpi = 200)

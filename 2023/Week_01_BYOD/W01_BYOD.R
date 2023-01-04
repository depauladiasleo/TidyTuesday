# Título: TidyTuesday | 2023 | Week 01 | BYOD
# Script por: @depauladiasleo
# Última atualização: janeiro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(ggtext)
library(tidyverse)
library(showtext)


# Importação de dados ----------------------------------------------------------


pk_contributions <- "C:/Users/depau/OneDrive/Documentos/Léo/Pesquisa/2022/2. Livro PUC-MG/df/full_data_2020.csv"


df <- read_csv(pk_contributions) |> 
  janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  filter(mission == "MINUSTAH",
  #       date > "2009-12-31"
  ) |> 
  select(date, contributor, total) |> 
  group_by(date) |> 
  summarize(total_contributions = sum(total))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


título <- list(título = "Haiti has suffered with centuries of antropocentric disasters",
               subtitle = "Although numerous in <b><span style='color:#009edb'>blue helmet military, police and specialized personel</span></b>,<br>Minustah failed to provide the country a horizon of peace",
               legenda = "TidyTuesday | 2023 | Week 01 | BYOD | Plot by: @depauladiasleo | Data from: UN Peacekeeping")



## Paleta de cores -------------------------------------------------------------


un_blue <- "#009edb"


txt_col <- "gray15"


## Fontes ----------------------------------------------------------------------


roboto <- "roboto"
font_add_google(roboto)


theme_set(theme_void(base_family = roboto))


showtext_opts(dpi = 320)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_area(aes(x = date, y  = total_contributions),
            fill = un_blue) +
  theme_void() +
  scale_y_continuous(breaks = c(5000, 10000),
                     labels = scales::number_format()) +
  theme(
    axis.text = element_text(size = 10,
                               color = txt_col),
    plot.background = element_rect(fill = "gray85",
                                   color = "gray85"),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    
    plot.title = element_text(face = "bold",
                              size = 24,
                              color = txt_col),
    plot.subtitle = element_markdown(size = 16,
                                     color = txt_col,
                                     lineheight = 1.25,
                                     margin = margin(8, 0, 24, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12,
                                color = txt_col,
                                hjust = 0.5,
                                margin = margin(24, 0 , 0, 0, "pt"))
  ) +
  labs(title = título$título,
       subtitle = título$subtitle,
       caption = título$legenda)



# Exportação -------------------------------------------------------------------


ggsave("W01.png",
       height = 3840,
       width = 6790,
       unit = "px",
       dpi = 320)

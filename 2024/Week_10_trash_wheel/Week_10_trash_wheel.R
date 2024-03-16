# Título: #TidyTuesday | 2024 | Week 10 | Trash Wheel
# Script por: @depauladiasleo
# Última atualização: março de 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2024, 10)


df <- tuesdata$trashwheel |> 
      janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  mutate(date = mdy(date),
         date = floor_date(date, "month")) |> 
  group_by(date) |> 
  summarize(across(weight:homes_powered, sum)) |> 
  mutate(month = month(date, label = TRUE, abbr = TRUE),
         year = year(date))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos = list(título = 'Casas que poderiam ser mantidas acesas\ncom os resíduos coletados pelo sistema\ndo Porto de Baltimore',
               subtítulo = "Cada residência consome, em média, 30 kW por dia",
               legenda = "Tidy Tuesday | Week 10 | Mr. Trash Wheel\ngraph by: @depauladiasleo | data from: Baltimore Harbour")


texto <- tibble(x = 1, y = 2026.6,
label = "O Sr. Trash Wheel é um mecanismo semiautônomo de coleta de lixo
instalado em cursos d'água de Baltimore, nos Estados Unidos. Ao invés
de buscar lixo atirado aos mares, este dispositivo coleta toneladas
que navegam por cursos como rios, correntes e outras saídas de água.
Ele foi projeado para suportar adversidades meteorológicas severas e 
funciona com energia hídrica e solar.")


## Paleta de cores -------------------------------------------------------------


col1 <- "#cbe7fe"

col2 <- "#3c1951"

col3 <- "#BF9BAC"

col4 <- "gray90"



text_col1 <- "#3c1951"

text_col2 <- "gray10"


## Fontes ----------------------------------------------------------------------


fraunces <- "Fraunces"
font_add_google(fraunces)


open_sans <- "Open Sans"
font_add_google(open_sans)


showtext_auto()

theme_set(theme_void(base_family = open_sans))


## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geom_tile(aes(x = month,
                y = year,
                fill = homes_powered), 
            linewidth = 0.75,
            color = col1) +
  geom_text(data = texto,
            aes(x = x, y = y,
                label = label), family = open_sans, size = 14,
            size.unit = "pt",
            color = text_col2,
            hjust = 0, 
            vjust = 1) +
  scale_fill_gradient(low = col3,
                      high = col2) +
  scale_y_continuous(limits = c(2013, 2027),
                     breaks = c(2014, 2023)) +
  guides(fill = guide_colorbar(title = "Residências",
                               barheight = unit(0.15, 'cm'),
                               barwidth = unit(15, 'cm'),
                               title.position = 'top',
                               ticks.colour = "gray95",
                               frame.colour = NA,
                               direction = 'horizontal')) +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(color = col1,
                                       fill = col1),
        legend.position = "top",
        plot.title = element_text(family = fraunces,
                                  size = 24,
                                  color = text_col1,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = text_col2,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    color = text_col2,
                                    margin = margin(32, 0, 0, 0, "pt"),
                                    lineheight = 1.2,
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.text = element_text(size = 12, 
                                 color = text_col2,
                                 face = "bold")) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)



# Exportação -------------------------------------------------------------------

ggview::ggview(width = 900,
               height = 1600,
               units = "px",
               dpi = 110)


ggsave("Week_10_Mr._Trash_Wheel.png",
       width = 900,
       height = 1600,
       units = "px",
       dpi = 110)

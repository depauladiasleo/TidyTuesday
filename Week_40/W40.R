# Título: TidyTuesday 2022 | Week 40
# Script por: Leonardo Dias de Paula
# Última atualização: Outubro de 2022.


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(ragg)


# Importação de dados ------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 40)


product_hunt <- tuesdata$product_hunt


# Faxina de dados ----------------------------------------------


cats <- c("(PRODUCTIVIY)|(USER EXPERIENCE)|(EDUCATION)|(POLITICS)")


df <- 
  product_hunt |> 
  filter(str_detect(category_tags, cats)) |> 
  mutate(date = as_date(release_date),
         month = lubridate::floor_date(as_date(release_date), unit = "month")) |> 
  select(id, name, month, release_date, category_tags)


# Visualização -------------------------------------------------



## Palette ---------------------------------------------------------------


color1 <- "#A99A85"


blue <- "#112A40"



## Títulos ---------------------------------------------------------------


títulos <- list(título = "Sociedade do Cansaço",
                subtítulo = "Lançamentos mensais de produtos de tecnologia voltados à produtividade",
                legenda = "Tidy Tuesday 2022 #40 | Gráfico por: @depauladiasleo | Dados: Product Hunt")



## Cansaço ---------------------------------------------------------------

cansaço <- tibble(x = as_date(c("2014-01-01", "2020-04-01")), y = c(1.35, 0.55),
                  label = c("'A sociedade do cansaço é a sociedade da auto-exploração. O sujeito explora a si mesmo até a exaustão.\nNeste processo, ele se auto-flagela, muitas vezes conduzindo-se à violência da própria destruição.\nUm projeto revela-se como projétil que o próprio sujeito dispara contra si.'\n(Byung Chul Han, The Burnout Society, 2015, p. 47)",
                            "Declaração da OMS sobre\no status de pandemia da covid-19"))


## Viz -------------------------------------------------------------------


df |> 
  filter(str_detect(category_tags, "PRODUCTIVITY")) |> 
  mutate(y = str_extract(category_tags, "PRODUCTIVITY") |> 
             str_to_sentence()) |> 
  ggplot(aes(x = month, y = y)) +
  geom_beeswarm(color = color1,
                groupOnX = FALSE,
                cex = 0.75,
                size = 0.75) +
  scale_x_date(breaks = as_date(c("2020-03-11")),
               labels = "") +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "Montserrat",
                             size = 10, 
                             color = color1),
    panel.grid.major.x = element_line(size = 0.2,
                                      color = "gray95",
                                      linetype = 3),
    plot.background = element_rect(fill = blue,
                                   color = blue),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(family = "Montserrat",
                              color = "gray95",
                              size = 24,
                              face = "bold"),
    plot.subtitle = element_text(family = "Montserrat",
                                 color = color1,
                                 size = 18,
                                 margin = margin(6, 0, 12, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Montserrat",
                                color = color1,
                                size = 12,
                            #   face = "bold",
                                margin = margin(36, 0, 0, 0, "pt"),
                            hjust = 0.5)
  ) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda) +
  geom_text(data = cansaço, aes(x = x, y = y, label = label),
            color = color1,
            size = 4,
            hjust = 0)
  
  


# Exportação ---------------------------------------------------


ggsave("TidyTuesday40.png", 
       width = 5120, height = 2880, dpi = 320, units = "px")

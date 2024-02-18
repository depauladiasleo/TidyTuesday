# Título: TidyTuesday | Semana 08 | ISRC Grants
# Script por: @depauladiasleo
# Última atualização: fevereiro 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(stm)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 8)


df <- tuesdata$isc_grants


# Faxina de dados --------------------------------------------------------------


## Unnesting and counting ------------------------------------------------------


df_word <- 
  df |> 
  select(-website) |> 
  unnest_tokens(word, title) |> 
  anti_join(get_stopwords())


df_word |> 
  count(word, proposed_by, sort = TRUE)


## Sparse ----------------------------------------------------------------------


df_sparse <- 
  df_word |> 
  count(word, proposed_by, sort = TRUE) |> 
  cast_sparse(proposed_by, word, n)


## Structural Topic Model ------------------------------------------------------


set.seed(123)


topic_model <- stm(df_sparse, K = 4)


summary(topic_model)


tidy(topic_model, matrix = "frex")

## Gamma ---------------------------------------------------------------------

## Looking for the probability of a project to pertain to one of the four topics


title_gamma <- 
  tidy(topic_model, matrix = "gamma", document_names = rownames(df_sparse))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos <- list(título = "Main topics of ISC grants",
                subtítulo = "Topic modelling for funded projects from 2016 to 2023",
                legenda = "Tidy Tuesday | Week 08 | ISC grants | graph by: @depauladiasleo | data from: R Consortium")



## Paleta de cores -------------------------------------------------------------


col1 <- "#1e51a4"

col2 <- "#102b57"

col3 <- "#061124"


title_col <- "gray10"

text_col <- "gray15"


bg_col <- "gray95"



## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


title_gamma |> 
  mutate(topic = case_match(topic,
                            1 ~ "Topic 1",
                            2 ~ "Topic 2",
                            3 ~ "Topic 3",
                            4 ~ "Topic 4")) |> 
  ggplot(aes(gamma, topic)) +
  geom_col(fill = col3,
           color = bg_col) + 
  facet_wrap("document") +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.background = element_rect(color = bg_col,
                                       fill = bg_col),
        plot.title = element_text(size = 24,
                                  color = col3,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18,
                                     color = title_col,
                                     margin = margin(6, 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 16,
                                    color = title_col,
                                    margin = margin(32, 0, 0, 0, "pt"),
                                    hjust = 0.5),
        plot.caption.position = "plot",
        axis.text.y = element_text(size = 10,
                                   color = text_col,
                                   hjust = 1),
        strip.text = element_text(size = 10,
                                  color = text_col,
                                  face = "bold",
                                  hjust = 0)) +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)


# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600*1.15,
               height = 900*1.15,
               units = "px",
               dpi = 110)


ggsave("Week_08_ISC.png",
       width = 1600*1.15,
       height = 900*1.15,
       units = "px",
       dpi = 110)

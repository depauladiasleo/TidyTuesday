# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(ggalluvial)
library(ggtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tt_load(2023, week = 29)


df_full <- tuesdata$detectors


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_full |> 
  mutate(kind_pt = case_match(kind, 
                           "AI" ~ "IA",
                           "Human" ~ "Pessoa"),
         pred_class_pt = case_match(.pred_class, 
                              "AI" ~ "IA",
                              "Human" ~ "Pessoa"),
         sub_kind_pt = case_when(kind_pt == "IA" ~ "IA",
                              kind_pt == "Pessoa" & native == "Yes" ~ "nativa",
                              TRUE ~ "não nativa"),
         correct = ifelse(kind == .pred_class, TRUE, FALSE)) |> 
  group_by(kind_pt, sub_kind_pt, pred_class_pt, correct) |> 
  summarize(freq = n())


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



títulos <- list(título = "Uma IA pode diferenciar texto gerado por outras IAs e textos elaborados por pessoas?",
                subtítulo = glue::glue("Nos meses recentes, a preocupação quanto à originalidade de textos cresceu em razão da difusão de ferramentas como o ChatGPT. Outras Inteligências Artificiais -- como Crossplag, GPTZero, HFOpenAI,<br>
                                       OriginalityAI, Quil, Sapling, e ZeroGPT -- passaram a ser usadas para conferir a originalidade dos textos. <b>Entretanto, usar estas ferramentas para classificá-los é <span style='color:{color2}'>impreciso.</span></b><br><br>
                                       Textos elaborados generativamente por IAs foram <span style='color:{color2}'><b>classificados incorretamente</span></b> como humanos em <b>69% dos casos</b>.<br>
E textos redigidos por pessoas não nativas de uma língua foram <b><span style='color:{color2}'>classificados incorretamente</span></b> como gerados por IA em <b>61% dos casos</b>"),
legenda = "Tidy Tuesday | 2023 | Semana 29 | Humano? | gráfico por: @depauladiasleo | inspiração: @nrennie35<br>
dados de: <i>GPT Detectors Are Biased Against Non-Native English Writers.</i> Weixin Liang, Mert Yuksekgonul, Yining Mao, Eric Wu, James Zou.")




## Paleta de cores -------------------------------------------------------------


white <- "gray90"

color1 <- "gray70"

color2 <- "#3E150F"

color3 <- "#0C383D"


palette <- c(color1, color2)


title_color <- "gray5"

text_color <- "gray15"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot(aes(y = freq, axis1 = pred_class_pt, axis2 = sub_kind_pt, axis3 = kind_pt)) +
  ggalluvial::geom_alluvium(aes(fill = correct),
                            width = 0.25, alpha = 0.8,
                            reverse = FALSE) +
  geom_stratum(width = 0.25, reverse = FALSE,
               color = white,
               fill = color3) +
  geom_text(
    stat = "stratum",
    reverse = FALSE, 
    aes(label = after_stat(stratum)),
    color = white,
    lineheight = 1.1,
    size = 5) +
  scale_fill_manual(values = rev(palette)) +
  scale_x_continuous(
    expand = c(.05, .05),
    breaks = c(1, 2, 3),
    labels = c("Classificado como:", "", "Elaborado por:")
  ) +
  coord_flip() +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = white,
                                       color = white),
        plot.title = element_text(size = 24,
                                  color = color3,
                                  face = "bold"),
        plot.subtitle = element_markdown(size = 14,
                                     color = title_color,
                                     margin = margin(10, 0, 24, 0, "pt"),
                                     lineheight = 1.25),
        plot.title.position = "plot",
        plot.caption = element_markdown(size = 14,
                                        color = text_color,
                                        margin = margin(22, 0, 0, 0, "pt"),
                                        hjust = 0.5,
                                        lineheight = 1.25),
        plot.caption.position = "plot",
        axis.text.y = element_text(size = 12,
                                 color = text_color,
                                 hjust = 1,
                                 face = "bold"),
        legend.position = "none") +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)



# Exportação -------------------------------------------------------------------


nflplotR::ggpreview(width = 1024*2.33,
                    height = 1024,
                    dpi = 120,
                    unit = "px")


ggsave("W29_Detectors.png",
       width = 1024*2.33,
       height = 1024,
       dpi = 120,
       unit = "px")

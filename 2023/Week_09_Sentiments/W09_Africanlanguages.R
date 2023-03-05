# Título: Tidy Tuesday | 2023 | Week 09 | African Languages sentiments
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(ggtext)
library(tidyverse)
library(tidytuesdayR)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, 9)


df_languages <- tuesdata$languages

df_lang_script <- tuesdata$language_scripts

df_lang_cnty <- tuesdata$language_countries

df_senti <- tuesdata$afrisenti


# Faxina de dados --------------------------------------------------------------


df <- 
  df_languages |> 
  left_join(df_lang_script, by = "language_iso_code") |> 
  full_join(df_lang_cnty)


sentiments <- 
  df_senti |> 
  filter(intended_use == "test") |> 
  group_by(language_iso_code, label) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(language_iso_code) |> 
  mutate(total = sum(n),
         percentual = round((n*100)/total, 0))


plot_data <- 
  df_languages |> 
  left_join(sentiments) |> 
  mutate(language = as_factor(language)) |> 
  arrange(desc(total)) |> 
  head(30)


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------

títulos <- list(título = "Sentimento de tuítes em línguas africanas",
                subtítulo = "Mais de 100 mil tuítes em 14 línguas africanas foram analisados.<br>Nesta figura, estão elencadas as dez línguas com mais tuítes.
                <br><br>Os sentimentos detectados foram classificados como <b><span style='color:#304848'>positivos</span></b>, 
                <b><span style='color:#304890'>negativos</span></b>, ou <b><span style='color:#5b5b5b'>neutros</span></b>.",
                legenda = "Tidy Tuesday | 2023 | Semana 09 | dados: AfriSenti | elaborado por: @depauladiasleo | inspiração: @nrennie35")

## Paleta de cores -------------------------------------------------------------


bg_col <- "gray95"


title_col <- "gray5"


txt_col <- "gray20"


## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------



plot_data |> 
  ggplot() +
  geom_col(aes(n, fct_reorder(language, total), 
               fill = label)) +
  nord::scale_fill_nord(palette = "baie_mouton") +
  geom_richtext(data = plot_data |> filter(label == "positive"),
                aes(x = 5, y = language,
                    label = glue::glue("{n} ({percentual}%)<br>positivo")),
                colour = bg,
                fill = NA,
                lineheight = 0.37,
                hjust = 0,
                size = 2.5,
                label.color = NA
                ) +
  geom_richtext(data = plot_data |> filter(label == "negative"),
                aes(x = total - 5, y = language,
                    label = glue::glue("{n} ({percentual}%)<br>negativo")),
                colour = bg_col,
                fill = NA,
                lineheight = 0.37,
                size = 2.5,
                label.color = NA,
                hjust = 1
  ) +
  theme_void() +
  theme(axis.text.y = element_text(size = 10,
                                   color = txt_col,
                                   hjust = 1),
        plot.background = element_rect(fill = bg_col,
                                       color = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(color = title_col,
                                  size = 24,
                                  face = "bold"),
        plot.subtitle = element_markdown(color = txt_col,
                                     size = 16,
                                     margin = margin(20, 0, 12, 0, "pt"),
                                     lineheight = 1.2),
        plot.title.position = "plot",
        plot.caption = element_text(color = title_col,
                                    size = 12,
                                    hjust = 0.5,
                                    margin = margin(24, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        legend.position = "none") +
  labs(title = títulos$título,
       subtitle = títulos$subtítulo,
       caption = títulos$legenda)
  




# Exportação -------------------------------------------------------------------


ggsave("W09_Africanlanguages.png",
       unit = "px",
       height = 768,
       width = 1368,
       dpi = 100)

# Título: Personagens em Dom Casmurro
# Script por: Leonardo Dias de Paula
# Última atualização:


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas -------------------------------------------------


library(tidyverse)
library(tidytext)
library(showtext)
library(MetBrewer)
library(ggstream)


# Importação de dados ------------------------------------------


dom_casmurro_url <- "https://www.gutenberg.org/cache/epub/55752/pg55752.txt"


dom_casmurro_texto <- read_lines(dom_casmurro_url)



## Descrição das linhas --------------------------------------------------


start_line <- 55


end_line <- 8523


# Faxina de dados ----------------------------------------------


## Personagens -------------------------------------------------


casmurro_persona <- "Bento Santiago|Bentinho|Dom Casmurro|Casmurro"
                      
capitu_persona <- "Capitú|Capitolina"

jose_dias_persona <- "José Dias"

maria_gloria_persona <- "D. Maria da Gloria Fernandes Santiago|D. Gloria|Maria da Gloria"

escobar_persona <- "Ezequiel de Sousa Escobar|Escobar"

sancha_persona <- "Sancha"

ezequiel_persona <- "Ezequiel A. de Santiago|Ezequiel"

capituzinha <- "Capituzinha"


persona_label <- c("Capitu", "D. Maria da Gloria", 
                   "Sancha", "Escobar", "José Dias",
                   "Ezequiel", "Bento, o Casmurro")


## As tibble -------------------------------------------------------------


casmurro_df <- 
  tibble(line = dom_casmurro_texto[start_line:end_line]) |> 
  mutate(line = str_squish(line),
         line_id = row_number()) |> 
  filter(line != "")             |>                                       # removes empty lines
  mutate(capitulo = str_extract(line, "^[:upper:]+[:punct:]*$") |>        # extracts chapter number
                    str_replace("[:punct:]", ""))   |>                    
  fill(capitulo, .direction = "down") |>                                  # fills chapter number in several lines
  mutate(casmurro = str_count(line, pattern = casmurro_persona),          # detects and counts citations for selected characters
         capitu = str_count(line, pattern = capitu_persona),              # too wordy, needs further attention 
         jose_dias = str_count(line, pattern = jose_dias_persona),
         maria_gloria = str_count(line, pattern = maria_gloria_persona),
         escobar = str_count(line, pattern = escobar_persona),
         sancha = str_count(line, pattern = sancha_persona),
         ezequiel = str_count(line, pattern = ezequiel_persona),
         capituzinha = str_count(line, pattern = capituzinha))


## Character citations in each chapter ---------------------------------

  
casmurro_count <-                                                         # creates a summary df for casmurro, counting the appearances
  casmurro_df |>                                                          # of selected characters in the novel in each chapter
  mutate(capitulo = as.roman(capitulo) |> as.numeric()) |> 
  group_by(capitulo) |> 
  summarize(casmurro = sum(casmurro),
            capitu = sum(capitu),
            jose_dias = sum(jose_dias),
            maria_gloria = sum(maria_gloria),
            escobar = sum(escobar),
            sancha = sum(sancha),
            ezequiel = sum(ezequiel),
          #  capituzinha = sum(capituzinha)
        ) |> 
  pivot_longer(casmurro:ezequiel, names_to = "character", values_to = "count")
  


# Visualização -------------------------------------------------


## Títulos -----------------------------------------------------


títulos <- list(title = "Obsessão: quantas vezes cada personagem é citada em Dom Casmurro", 
                subtitle = "Capitú é a personagem mais frequente ao longo de todo o romance de Machado de Assis",
                caption = "Tidy Tuesday 2022 | Gráfico por: @depauladiasleo | Texto em análise: Project Gutenberg",
                x = "Capítulos")


## Fontes ----------------------------------------------------------------


font_add_google("Montserrat")
font_add_google("Montserrat Alternates")


showtext.auto()


## Paleta de cores -------------------------------------------------------


blue <- "#01214d"


 # palette <- c("#54bebe", "#76c8c8", "#98d1d1",                First try on color pallete, using 'Pink Foam'.
 #             "#badbdb", "#dedad2", "#e4bcad", 
 #             "#df979e", "#d7658b", "#c80064") 


## Gráfico ---------------------------------------------------------------


casmurro_count |> 
  mutate(character = fct_relevel(character, "capitu", "maria_gloria", "sancha", "escobar", "jose_dias",
                                 "ezequiel", "casmurro")) |> 
  ggplot(aes(x = capitulo, y = count, fill = character)) +
  geom_stream(extra_span = .25,
              true_range = "none",
              bw = 0.65,
              color = "white",
              size = 0.25,
              sorting = "none") +
  scale_fill_manual(name = "", values = met.brewer("Hiroshige", 7),
                    aesthetics = "fill",
                    labels = persona_label) +
  scale_x_continuous(breaks = c(1, 50, 100, 146),
                     labels = c("I", "L", "C", "CXLVI")) +
  theme_minimal() +
  theme(
    plot.margin = margin(2, 2, 2, 2, unit = "cm"),
    plot.background = element_rect(fill = blue,
                                   color = blue),
    plot.title = element_text(family = "Montserrat",
                              size = 24,
                              color = "white",
                              face = "bold"),
    plot.subtitle = element_text(family = "Montserrat",
                                 size = 18,
                                 color = "gray90",
                                 margin = margin(t = 6, b = 24, unit = "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Montserrat",
                                size = 12,
                                color = "gray95",
                                hjust = 0.5,
                                margin = margin(t = 24, unit = "pt")),
    plot.caption.position = "plot",
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "white",
                                      size = 0.1,
                                      linetype = 2),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Montserrat",
                                size = 10,
                                color = "gray90",
                                hjust = 1.25),
    axis.text.x = element_text(family = "Montserrat",
                                size = 10,
                                color = "gray90",
                               margin = margin(t = 2, unit = "pt")),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(family = "Montserrat",
                               size = 12,
                               color = "gray95",
                               margin = margin(t = 6, b = 6, unit = "pt")),
    legend.box.margin = margin(t = 0, b = 24, unit = "pt"),
    legend.position = "top") +
  labs(
    title = títulos$title,
    subtitle = títulos$subtitle,
    caption = títulos$caption,
    x = títulos$x)
  
  
# Exportação ---------------------------------------------------


ggsave("TidyTuesday30.svg",
       width = 1080*3, height = 1080, dpi = 120, units = "px")

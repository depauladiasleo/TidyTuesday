# Título: TidyTuesday | 2024 | Week 06 | World Heritage
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(showtext)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 6)

df <- tuesdata$heritage


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df |> 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "n") |> 
  mutate(país = case_match(country,
                           "Denmark" ~ "Dinamarca",
                           "Norway" ~ "Noruega",
                           "Sweden" ~ "Suécia"))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


col_den <- "#C8102E"

col_nor <- "#00205B"

col_swe <- "#006AA7"


title_col <- "gray10"

text_col <- "gray20"

line_col <- "gray70"


## Fontes ----------------------------------------------------------------------


kanit <- "kanit"
font_add_google(kanit)


open_sans <- "open sans"
font_add_google(open_sans)

theme_set(theme_void(base_family = open_sans))


showtext_opts(dpi = 110)
showtext_auto()


## Gráfico ---------------------------------------------------------------------


## Gráfico 1 -------------------------------------------------------------------


gráfico1 <- 
df_tidy |> 
  ggplot() +
  geom_tile(aes(x = 8, y = 8, 
                color = país, 
                height = n, width = n),
            fill = NA, 
            linewidth = 1) +
  scale_color_manual(values = c(col_den, col_nor, col_swe)) +
  theme_void() +
  theme(plot.background = element_rect(color = "gray90",
                                       fill = "gray90"),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        strip.text = element_text(size = 14,
                                  color = title_col,
                                  face = "bold",
                                  hjust = 0),
        legend.position = "none",
        ) +
  facet_wrap("year")


## Gráfico 2 -------------------------------------------------------------------


gráfico2 <- 
df_tidy |> 
  ggplot() +
  annotate("curve", x = 4, y = 4,
           xend = 10, yend = 10, color = col_den) +
  annotate("curve", x = 5, y = 5,
           xend = 8, yend = 8, color = col_nor, curvature = -0.5) +
  annotate("curve", x = 13, y = 13,
           xend = 15, yend = 15, color = col_swe, curvature = -0.5) +
  geom_segment(aes(x = 0, y = 0,
                   xend = 15, yend = 15),
               linetype = 2,
               color = line_col) +
  geom_point(aes(x = n, y = n, size = 10*n, 
                color = país)) +
  scale_size_continuous(range = c(2, 4), guide = "none") +
  scale_x_continuous(name = "2022",
                     limits = c(0, 16), 
                     breaks = c(8, 10, 15)) +
  scale_y_continuous(name = "2004",
                     limits = c(0, 16), 
                     breaks = c(4, 5, 13)) +
  scale_color_manual(values = c(col_den, col_nor, col_swe)) +
  theme_void() +
  theme(plot.background = element_rect(color = "gray90",
                                       fill = "gray90"),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        panel.grid.major = element_line(color = line_col,
                                        linewidth = 0.25),
        axis.title.x = element_text(size = 14,
                                    color = title_col,
                                    face = "bold",
                                    hjust = 1),
        axis.title.y = element_text(size = 14,
                                    color = title_col,
                                    face = "bold",
                                    vjust = 1),
        axis.text = element_text(size = 12,
                                 # face = "bold",
                                 color = text_col,
                                 hjust = 0.5,
                                 vjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14,
                                   color = title_col,
                                   face = "bold"))



gráfico3 <- 
ggplot() +
  geom_text(aes(0, 0, 
                label = "1 conjunto, 2 visualizações"), 
            family = kanit,
            size = 6, 
            fontface = "bold",
            hjust = 0,
            color = title_col) +
  geom_text(aes(0, -3.5, 
                label = "Quantos patrimônios mundiais da humanidade há\nna Dinamarca, na Noruega e na Suécia?"),
            family = open_sans,
            lineheight = 1.15,
            size = 4, 
            color = title_col,
            hjust = 0) +
  geom_text(aes(0, -7.5, 
                label = "Tidy Tuesday | 2024\nSemana 06\nFonte: UNESCO World Heritage Sites\nGráfico: Leonardo Dias de Paula (@depauladiasleo)"), 
            family = open_sans, 
            lineheight = 1.15,
            size = 3.5, 
            hjust = 0,
            color = title_col) +
  scale_x_continuous(limits = c(-1, 11)) +
  scale_y_continuous(limits = c(-10, 2)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray90", color = NA)
  )


## Patchwork -------------------------------------------------------------------


gráfico2 + (gráfico1 / gráfico3)


# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1600,
               height = 900,
               units = "px",
               dpi = 110)


ggsave("Week_06_World_heritage.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

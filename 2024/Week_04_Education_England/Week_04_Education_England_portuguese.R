# Título: TidyTuesday | Week 04 | Education achievements in England
# Script por: @depauladiasleo
# Última atualização: fevereiro de 2024


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 4)


df <- tuesdata$english_education


# Faxina de dados --------------------------------------------------------------

df_tidy <- 
df |> 
  select(town11nm, population_2011, income_flag,
         university_flag, education_score) |> 
  filter(university_flag != is.na(university_flag),
         income_flag != is.na(income_flag))

  wilcox.test(df_tidy$education_score ~ df_tidy$university_flag)
  
  
  # Wilcoxon rank sum test with continuity correction
  # 
  # data:  df_tidy$education_score by df_tidy$university_flag
  # W = 35332, p-value = 0.1944
  # alternative hypothesis: true location shift is not equal to 0
  
  kruskal.test(df_tidy$education_score ~ df_tidy$university_flag)
  
  
  kruskal.test(df_tidy$education_score ~ df_tidy$income_flag)
  
  
  # Kruskal-Wallis rank sum test
  # 
  # data:  df_tidy$education_score by df_tidy$income_flag
  # Kruskal-Wallis chi-squared = 490.51, df = 3, p-value < 2.2e-16


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


  títulos <- list(título = "A instalação de universidades afeta a pontuação educacional de uma cidade?",
                  subttítulo = "Avaliação educacional de cidades inglesas em 2011, de acordo com a presença de campi universitários",
                  legenda = "Tidy Tuesday | 2024 | Semana 04 | Educação inglesa |\ngráfico por: @depauladiasleo | dados: The UK Office for National Statistics",
                  y = "Pontuação\neducacional")
  
  
  annotation <- tibble(x = 0, y = 10, label = "Apesar de visualmente diferentes,\num teste de Wilcoxon Rank Sum indica\numa diferença moderadamente significativa\nentre os dois grupos (valor de p: 0.1944).")
  

## Paleta de cores -------------------------------------------------------------


  col1 <- "#C18C5D"
  
  col2 <- "#495867"
  
  col3 <- "#CE796B"
  
  pal <- c(col1, col2)
  
  bg_col <- "gray90"
  
  title_col <- "gray10"
  
  text_col <- "gray15"
  
  
## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------

 
df_tidy |> 
    mutate(university_flag = case_match(university_flag,
                                         "No university" ~ "Sem universidade",
                                         "University" ~ "Com universidade",
                                         .default = university_flag)) |> 
ggplot(aes(x = factor(university_flag, levels = c("Sem universidade", "Com universidade")), 
           y = education_score,
           color = university_flag,
           fill = university_flag)) +
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(color = title_col,
      width = .15, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` or `outlier.alpha = 0` works as well
    ) +
    ## add dot plots from {ggdist} package
    ggdist::stat_dots(
      ## orientation to the left
      side = "left", 
      ## move geom to the left
      justification = 1.12, 
      ## adjust grouping (binning) of observations 
      binwidth = .25
    ) + 
    scale_color_manual(values = rev(pal)) +
    scale_fill_manual(values = rev(pal)) +
    theme_void() +
    theme(plot.background = element_rect(color = bg_col,
                                         fill = bg_col),
          plot.margin = margin(2, 2, 2, 2, "cm"),
          plot.title = element_text(size = 24,
                                    color =  title_col,
                                    face = "bold"),
          plot.subtitle = element_text(size = 18,
                                       color = title_col,
                                       margin = margin(6, 0, 24, 0, "pt")),
          plot.caption = element_text(size = 16,
                                      color = title_col,
                                      hjust = 0.5,
                                      margin = margin(20, 0, 0, 0, "pt")),
          plot.caption.position = "plot",
          plot.title.position = "plot",
          axis.title.y = element_text(size = 14,
                                      color = title_col,
                                      face = "bold", vjust = 1,
                                      hjust = 0),
          axis.text = element_text(size = 12,
                                   color = text_col),
          legend.position = "none") +
    #  ## remove white space on the sides
    # coord_cartesian(xlim = c(0.15, 2.5)) +
    labs(title = títulos$título,
         subtitle = títulos$subttítulo,
         caption = títulos$legenda,
         y = títulos$y) +
    annotate("text", x = annotation$x,
             y = annotation$y, label = annotation$label,
             color = text_col, hjust = 0)


df |> 
  select(town11nm, population_2011, income_flag,
         university_flag, education_score) |> 
  filter(income_flag != is.na(income_flag)) |> 
  ggplot(aes(x = income_flag, y = education_score)) +
  geom_violin()



# Exportação -------------------------------------------------------------------


ggview::ggview(width = 1800,
               height = 1012.5,
               units = "px",
               dpi = 110)


ggsave("Week_04_England_Education_portuguese.png",
       width = 1600,
       height = 900,
       units = "px",
       dpi = 110)

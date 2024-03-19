# Título:
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(patchwork)


# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2024, 12)


df <- tuesdata$mutant_moneyball |> 
  janitor::clean_names() |> 
  mutate(                                                                     
    member = str_replace_all(member, "([a-z])([A-Z])", "\\1 \\2"),
    member = str_to_title(member),
    member = ifelse(member == "Hank Mc Coy", "Hank McCoy", member)
  )  |> 
  mutate(
    name = case_match(member,                                                     
                      "John Proudstar"       ~ "Thunderbird",
                      "Shiro Yoshida"        ~ "Sunfire",
                      "Hank McCoy"           ~ "Beast",
                      "Bobby Drake"          ~ "Iceman",
                      "Charles Xavier"       ~ "Professor X",
                      "Jean Gray"            ~ "Phoenix",
                      "Logan Howlett"        ~ "Wolverine",
                      "Warren Worthington"   ~ "Angel",
                      "Scott Summers"        ~ "Cyclops",
                      "Eric Magnus"          ~ "Magneto",
                      "Jean Grey"            ~ "Phoenix",
                      "Alex Summers"         ~ "Havok",
                      "Lorna Dane"           ~ "Polaris",
                      "Ororo Munroe"         ~ "Storm",
                      "Kurt Wagner"          ~ "Nightcrawler",
                      "Peter Rasputin"       ~ "Colossus",
                      "Sean Cassidy"         ~ "Banshee",
                      "Kitty Pryde"          ~ "Shadowcat",
                      "Anna Marie Le Beau"   ~ "Rogue",
                      "Rachel Summers"       ~ "Askani",
                      "Alison Blaire"        ~ "Crystal",
                      "Longshot"             ~ "himself",
                      "Jonathan Silvercloud" ~ "Forge",
                      "Remy Le Beau"         ~ "Gambit",
                      "Jubilation Lee"       ~ "Jubilee",
                      "Lucas Bishop"         ~ "Bishop",
                      "Betsy Braddock"       ~ "Psylocke"
                      
    ),
    xmen = paste(member, "--", name)
  )


# Faxina de dados --------------------------------------------------------------


df_total <- 
  df |> 
  select(xmen, member, name,  total_issues) 


top10 <- df_total |> arrange(desc(total_issues)) |> 
  head(10)


df_decades <- 
  df |> 
  select(xmen, member, name, total_issues60s:total_issues90s) |> 
  pivot_longer(total_issues60s:total_issues90s,
               names_to = "decade", values_to = "n_issues") |> 
  mutate(decade = str_remove(decade, "total_issues"), 
         decade = case_match(decade,
                      "60s" ~ 1960,
                      "70s" ~ 1970,
                      "80s" ~ 1980,
                      "90s" ~ 1990)) 


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos_1 <- list(título = "Total issues")

títulos_2 <- list(título = "Number of appearances in each decade")


## Paleta de cores -------------------------------------------------------------


col1 <- "#052542"


bg_col <- "gray90"


title_col <- "gray10"

text_col <-  "gray15"


## Fontes ----------------------------------------------------------------------


open_sans <- "Open sans"
kanit     <- "Kanit"   

font_add_google(open_sans)
font_add_google(kanit)

showtext_auto()


theme_set(theme_void(base_family = open_sans))


## Gráfico ---------------------------------------------------------------------



### Gráfico 1 -- Totais ------------------------------------------------------


p1 <- 
df_total |> 
  mutate(xmen = as_factor(xmen)) |> 
  ggplot() +
  geom_point(aes(total_issues, fct_reorder(xmen, total_issues)),
           fill = col1,
           color = col1) +
  geom_segment(aes(x = 0, 
                   xend = total_issues,
                   y = fct_reorder(xmen, total_issues),
                   yend = fct_reorder(xmen, total_issues)),
               color = col1) +
  theme_void() +
  theme(plot.background = element_rect(color = bg_col,
                                       fill = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(size = 24, 
                                  family = kanit,
                                  color = title_col,
                                  face = "bold",
                                  margin = margin(0 , 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    family = open_sans,
                                    color = text_col,
                                    hjust = 0.5,
                                    margin = margin(34, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        axis.text.x = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col),
        axis.text.y = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col,
                                   hjust = 1),
        panel.grid.major.x = element_line(color = text_col,
                                            linetype = 3,
                                            linewidth = 0.1),
        panel.grid.minor.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1)) +
  labs(title = títulos_1$título)



### Decades --------------------------------------------------------------------


p2 <- 
df_decades |> 
  filter(xmen %in% top10$xmen) |> 
  ggplot(aes(x = decade, y = n_issues)) +
  geom_line(color = col1,
            linewidth = 0.75) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125)) +
  theme_void() +
  theme(plot.background = element_rect(color = bg_col,
                                       fill = bg_col),
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(size = 24, 
                                  family = kanit,
                                  color = title_col,
                                  face = "bold",
                                  margin = margin(0 , 0, 24, 0, "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14,
                                    family = open_sans,
                                    color = text_col,
                                    hjust = 0.5,
                                    margin = margin(34, 0, 0, 0, "pt")),
        plot.caption.position = "plot",
        axis.text.x = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col),
        axis.text.y = element_text(size = 12,
                                   family = open_sans,
                                   color = text_col,
                                   hjust = 1),
        panel.grid.major.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1),
        panel.grid.minor.x = element_line(color = text_col,
                                          linetype = 3,
                                          linewidth = 0.1),
        strip.text = element_text(family = kanit,
                                  size = 12,
                                  face = "bold",
                                  hjust = 0,
                                  margin = margin(0, 0, 8, 0, "pt")),
        panel.spacing = unit(0.75, "cm")) +
  facet_wrap("xmen", nrow = 2) +
  labs(title = títulos_2$título)




## Patchwork -------------------------------------------------------------------

p1+p2



# Exportação -------------------------------------------------------------------
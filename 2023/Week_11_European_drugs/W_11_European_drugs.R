# Título: Tidy Tuesday | 2023 | Week 11 | European drugs development
# Script por: @depauladiasleo
# Última atualização: julho de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)



# Importação de dados ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2023, 11)


df_drugs <- tuesdata$drugs


# Faxina de dados --------------------------------------------------------------


df_tidy <- 
  df_drugs |> 
  filter(category == "veterinary",
         authorisation_status == "authorised",
         marketing_authorisation_date >= as_date("2000-01-01")) |> 
  select(marketing_authorisation_date,
         species) |> 
  mutate(
    dogs = str_detect(species, "Dogs|dogs"),
    cats = str_detect(species, "Cats|cats"),
    chicken = str_detect(species, "Chicken|chicken"),
    pigs = str_detect(species, "Pigs|pigs"),
    sheep = str_detect(species, "Sheep|sheep")
  ) |> 
  select(-species) |> 
  pivot_longer(dogs:sheep, names_to = "species", values_to = "value") |> 
  arrange(marketing_authorisation_date) |>
  mutate(month = floor_date(marketing_authorisation_date, "month")) |> 
  group_by(species, month) |>
  summarize(total = sum(value)) |> 
  mutate(cum_total = cumsum(total)) 
  

# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------



## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


df_tidy |> 
  ggplot() +
  geomtextpath::geom_textline(aes(x = month,
                                  y = cum_total, 
                                  label = species, 
                                  colour = species, 
                                  group = species
                                  ),
                              stat = "smooth"
                              ) +
  theme_void() +
  theme(legend.position = "none")



# Exportação -------------------------------------------------------------------
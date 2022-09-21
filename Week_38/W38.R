# Título: Tidy Tuesday | Week 38
# Script por: Leonardo Das de Paula
# Última atualização: Setembro de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(sf)
library(spData)


# Importação de dados ------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 38)


df_full <- tuesdata$HydroWASTE_v10


iso_3166 <- read.csv("ISO_3166.csv")


world <- world |> 
  select(iso_a2, geom)


# Faxina de dados ----------------------------------------------


df <- 
  df_full |> 
  janitor::clean_names() |> 
  left_join(iso_3166, by = c("cntry_iso" = "alpha_3")) |> 
  select(waste_id, wwtp_name, status, 
         country, cntry_iso, alpha_2,
         region, sub_region, intermediate_region,
         lat_wwtp, lon_wwtp, pop_served, waste_dis, level) |> 
  filter(intermediate_region == "South America") |> 
  left_join(world, by = c("alpha_2" = "iso_a2")) |> 
  st_as_sf()




# Visualização -------------------------------------------------


df |> 
  ggplot() +
  geom_sf(color = "gray95",
          fill = "gray90") +
  geom_point(aes(x = lon_wwtp, y = lat_wwtp), 
             color = "orange",
             size = 0.1,
             alpha = 0.85) +
  theme_void()


# Exportação ---------------------------------------------------
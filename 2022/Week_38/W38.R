# Título: Tidy Tuesday | Week 38
# Script por: Leonardo Das de Paula
# Última atualização: Setembro de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Bibliotecas


library(tidyverse)
library(showtext)
library(viridis)
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



## Palette -----------------------------------------------------------------


teal1 <- "#004242"


teal2 <- "#198e8e"


orange <- "#f7c99b"


white <- "gray95"


## Fonts -----------------------------------------------------------------


font_add_google("Work Sans", "Work Sans")

showtext::showtext_opts(dpi = 320)

showtext_auto()


## Title -----------------------------------------------------------------


titles <- list(title = "Estações de tratamento d'água na América do Sul",
               caption = "Tidy Tuesday 2022 #38 | Gráfico por: @depauladiasleo | Dados: Macedo et al., 2022")


## Plot -----------------------------------------------------------------


df |> 
  ggplot() +
  geom_sf(color = "#004242",
          fill = "#198e8e",
          size = 0.1,
          alpha = 0.5) +
  geom_point(aes(x = lon_wwtp, y = lat_wwtp), 
             color = "#f7c99b",
             size = 0.1,
             alpha = 0.70) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#004242",
                                   color = "#004242"),
    plot.title = element_text(family = "Work Sans",
                              color = white,
                              size = 24,
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(0, 0, 36, 0, "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Work Sans",
                                color = white,
                                size = 14,
                                hjust = 0.5,
                                margin = margin(36, 0, 0, 0, "pt")),
    plot.caption.position = "plot",
    plot.margin = margin(2, 2, 2, 2, "cm")
  ) +
  labs(
    title = titles$title,
    caption = titles$caption
  )


# Exportação ---------------------------------------------------


ggsave("TidyTuesday38.png", 
       width = 3792, height = 3792, dpi = 320, units = "px")

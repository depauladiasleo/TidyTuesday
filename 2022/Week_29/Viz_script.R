# Título: TidyTuesday 2022 Week #29
# Script por: Leonardo Dias de Paula
# Última atualização: Julho de 2022


# Descrição:


# Preparação ---------------------------------------------------


## Pacotes



## Bibliotecas


library(tidyverse)
library(sf)
library(tmap)
library(spData)
library(ragg)
library(patchwork)



# # Textos ----------------------------------------------------------------


título <- "Uso excessivo de pesticidas nas lavouras de Brasil e Argentina"

subtítulo <- "Nas últimas décadas, o uso de agrotóxicos em ambos os países se destaca na região"

título2 <- "Consumo anual de pesticidas na lavoura em toneladas"

crédito <- "TidyTuesday 2022 #29 | Gráfico por: @depauladiasleo | Dados: NBER"


# Importação de dados ------------------------------------------


tech_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')


iso_3166 <- read_csv("ISO_3166.csv") |> 
  janitor::clean_names() |> 
  select(alpha_3, alpha_2, name, intermediate_region)


sa_geom <- world |> 
  filter(subregion == "South America") |> 
  select(iso_a2, geom)


# Faxina de dados ----------------------------------------------


sa_total_pesticides <- 
  tech_df |> 
  select(variable, label, iso3c, year, value) |> 
  filter(variable == "pest_total") |> 
  left_join(iso_3166, by = c("iso3c" = "alpha_3")) |>  
  left_join(sa_geom, by = c("alpha_2" = "iso_a2")) |> 
  filter(intermediate_region == "South America") |> 
  select(!c(iso3c, alpha_2, intermediate_region)) |>  
  st_as_sf()


# Visualização ------------------------------------------------------------


## Mapa de dois anos, usando como 'facets' -------------------------------


pest_maps <-
sa_total_pesticides |> 
  filter(year == min(year) | year == max(year)) |> 
  ggplot() +
  ggtitle(título,
          subtitle = subtítulo) +
  geom_sf(aes(fill = value), 
          color = "#4B634A", 
          size = 0.05) +
  NatParksPalettes::scale_fill_natparks_c("Arches2", 
                                          direction = -1) +
  scale_x_reverse() +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#4B634A", 
                                   color = NA),
    plot.margin = margin(1.5, 2, 0.5, 2, unit = "cm"),
    plot.title = element_text(family = "Segoe UI",
                              size = 24,
                              color = "gray95",
                              face = "bold"),
    plot.subtitle = element_text(family = "Segoe UI",
                                 size = 20,
                                 color = "gray95",
                                 face = "plain",
                                 margin = margin(t = 4, b = 24, unit = "pt")),
    panel.spacing = unit(1, "cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    strip.text = element_text(family = "Segoe UI", size = 14, 
                              face = "bold", 
                              color = "gray95",
                              hjust = 0,
                              margin = margin(t = 3, b = 7, unit = "pt"))
    ) +
  facet_grid(. ~ year)



## Gráfico de linhas para a série histórica --------------------------------


pest_linha <-
sa_total_pesticides |> 
  ggplot() +
  ggtitle(título2) +
  geom_text(
            data = sa_total_pesticides |> 
            filter(year == max(year),
                   name %in% c("Brazil", "Argentina")), 
            aes(year, value, label = name),
            color = "#BFACB4", 
            size = 3, 
            hjust = -0.1) +
  geom_line(aes(x = year, y = value, group = name), 
            color = "gray75", 
            size = 0.5) +
  geom_line(data = sa_total_pesticides |> filter(name %in% c("Brazil", "Argentina")),
            aes(x = year, y = value, group = name), 
            color = "#BFACB4", 
            size = 1.1) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#4B634A", 
                                   color = NA),
    plot.margin = margin(0.5, 2, 1.5, 2, 
                         unit = "cm"),
    plot.title = element_text(family= "Segoe UI",
                              size = 14,
                              color = "gray95",
                              face = "bold",
                              margin = margin(b = 22, unit = "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe UI",
                                size = 12, 
                                color = "gray95",
                                margin = margin(t = 24, unit = "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray95", 
                                      size = 0.1,
                                      linetype = 2),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Segoe UI",
                               size = 10,
                               color = "gray95",
                               margin = margin(t = 2, unit = "pt")),
        ) +
  labs(
    caption = crédito)





## Visualizações para o Instagram ----------------------------------------


### Títulos Instagram


título_insta <- "Uso excessivo de pesticidas\nnas lavouras de Brasil e Argentina"

subtítulo_insta <- "Nas últimas décadas, o uso de agrotóxicos\nem ambos os países se destaca na região"


### Mapa de dois anos, usando como 'facets' -------------------------------


pest_maps_insta <-
  sa_total_pesticides |> 
  filter(year == min(year) | year == max(year)) |> 
  ggplot() +
  ggtitle(título_insta,
          subtitle = subtítulo_insta) +
  geom_sf(aes(fill = value), 
          color = "#4B634A", 
          size = 0.05) +
  NatParksPalettes::scale_fill_natparks_c("Arches2", 
                                          direction = -1) +
  scale_x_reverse() +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#4B634A", 
                                   color = NA),
    plot.margin = margin(0.75, 1, 0.25, 1, unit = "cm"),
    plot.title = element_text(family = "Segoe UI",
                              size = 24,
                              color = "gray95",
                              face = "bold"),
    plot.subtitle = element_text(family = "Segoe UI",
                                 size = 20,
                                 color = "gray95",
                                 face = "plain",
                                 margin = margin(t = 4, b = 24, unit = "pt")),
    panel.spacing = unit(1, "cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    strip.text = element_text(family = "Segoe UI", size = 14, 
                              face = "bold", 
                              color = "gray95",
                              hjust = 0,
                              margin = margin(t = 3, b = 7, unit = "pt"))
  ) +
  facet_grid(. ~ year)



### Gráfico de linhas para a série histórica --------------------------------


pest_linha_insta <-
  sa_total_pesticides |> 
  ggplot() +
  ggtitle(título2) +
  geom_text(
    data = sa_total_pesticides |> 
      filter(year == max(year),
             name %in% c("Brazil", "Argentina")), 
    aes(year, value, label = name),
    color = "#BFACB4", 
    size = 3, 
    hjust = -0.1) +
  geom_line(aes(x = year, y = value, group = name), 
            color = "gray75", 
            size = 0.5) +
  geom_line(data = sa_total_pesticides |> filter(name %in% c("Brazil", "Argentina")),
            aes(x = year, y = value, group = name), 
            color = "#BFACB4", 
            size = 1.1) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#4B634A", 
                                   color = NA),
    plot.margin = margin(0.25, 1, 0.75, 1, 
                         unit = "cm"),
    plot.title = element_text(family= "Segoe UI",
                              size = 14,
                              color = "gray95",
                              face = "bold",
                              margin = margin(b = 22, unit = "pt")),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Segoe UI",
                                size = 12, 
                                color = "gray95",
                                margin = margin(t = 24, unit = "pt"),
                                hjust = 0.5),
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray95", 
                                      size = 0.1,
                                      linetype = 2),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Segoe UI",
                               size = 10,
                               color = "gray95",
                               margin = margin(t = 2, unit = "pt")),
  ) +
  labs(
    caption = crédito)



# Patchwork ---------------------------------------------------------------


pest_maps / pest_linha +
  plot_layout(heights = c(4,1))
  

## Instagram -------------------------------------------------------------


pest_maps_insta / pest_linha_insta +
  plot_layout(heights = c(4, 1))


# Exportação ---------------------------------------------------


write_excel_csv(iso_3166, "ISO_3166.csv")


ggsave("TidyTuesday29.png",
       width = 4800, height = 3600, dpi = 320, units = "px")


ggsave("TidyTuesday29_story.png",
       width = 3240, height = 5760, dpi = 320, units = "px") # Look the margins up

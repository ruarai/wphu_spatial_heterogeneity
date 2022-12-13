


library(tidyverse)
library(sp)

source("R/plot_theme.R")
source("R/population.R")


australia_shapefile <- strayr::read_absmap("aus2021") %>%
  sf::st_simplify(TRUE, 500)

lga_shapefile <- strayr::read_absmap("lga2021")

states_shapefile <- strayr::read_absmap("state2021")
vic_shapefile <- states_shapefile %>% 
  filter(state_name_2021 == "Victoria")


water_col <- ggokabeito::palette_okabe_ito(2) %>%
  shades::opacity(0.2)

wphu_lga_shapefile <- lga_shapefile %>%
  filter(lga_name_2021 %in% c("Brimbank", "Hobsons Bay", "Maribyrnong", "Melbourne",
                              "Melton", "Moonee Valley", "Moreland", "Wyndham"))


p_common <- list(
  plot_theme,
  theme(panel.border = element_blank(),
        panel.grid = element_blank())
)

ggplot() +
  
  geom_sf(fill = "white", colour = NA,
          data = australia_shapefile) +
  
  geom_sf(fill = "grey95", colour = "grey20",
          data = vic_shapefile) +
  
  geom_sf(fill = "white", colour = ggokabeito::palette_okabe_ito(5),
          data = wphu_lga_shapefile) +
  
  coord_sf(label_axes = "----",
           xlim = st_bbox(vic_shapefile)[c(1,3)],
           ylim = st_bbox(vic_shapefile)[c(2, 4)]) +
  p_common +
  
  theme(panel.background = element_rect(fill = water_col))



ggplot() +
  
  geom_sf(fill = "grey95", colour = "grey20",
          data = vic_shapefile) +
  
  
  geom_sf(fill = "white", colour = ggokabeito::palette_okabe_ito(5),
          size = 1,
          data = wphu_lga_shapefile) +
  
  p_common +
  
  coord_sf(label_axes = "----",
           ylim = c(-37.5, -38.01), xlim = c(144.4, 145.05)) +
  
  theme(panel.background = element_rect(fill = water_col))



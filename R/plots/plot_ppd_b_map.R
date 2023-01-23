

library(tidyverse)
library(sp)

source("R/plot_theme.R")
source("R/population.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))


draws_b <- spread_draws(model_fit$draws(), b[LGA]) %>%
  ungroup() %>%
  mutate(LGA = model_data$LGA_names[LGA])


mean_b <- draws_b %>%
  mutate(diff = b / b[LGA == "Melbourne (C)"]) %>%
  
  group_by(LGA) %>%
  summarise(mean_diff = mean(diff)) %>%
  
  mutate(lga_name_2021 = str_remove(LGA, " \\(C\\)"))


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



wphu_lga_data <- wphu_lga_shapefile %>%
  left_join(mean_b, by = "lga_name_2021")

ggplot() +
  
  geom_sf(fill = "grey95", colour = "white", size = 0.4,
          data = vic_shapefile) +
  
  
  geom_sf(aes(fill = mean_diff), colour = "white",
          size = 0.4,
          data = wphu_lga_data) +
  
  scale_fill_distiller(type = "seq", palette = "YlGn",
                       name = "") +
  
  p_common +
  
  coord_sf(label_axes = "----",
           ylim = c(-37.5, -38.01), xlim = c(144.4, 145.05)) +
  
  theme(panel.background = element_rect(fill = water_col),
        legend.position = "bottom")

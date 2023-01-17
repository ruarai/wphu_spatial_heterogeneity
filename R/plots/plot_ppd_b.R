

library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))


draws_b <- spread_draws(model_fit$draws(), b[LGA]) %>%
  ungroup() %>%
  mutate(LGA = model_data$LGA_names[LGA])


draws_b %>%
  mutate(diff = b / b[LGA == "Melbourne (C)"]) %>% 
  
  filter(LGA != "Melbourne (C)") %>% 
  
  group_by(LGA) %>% 
  mutate(LGA_mean = mean(diff)) %>%
  ungroup() %>%
  arrange(LGA_mean) %>%
  mutate(LGA = factor(LGA, unique(LGA))) %>% 
  
  ggplot() +
  ggridges::geom_density_ridges(aes(x = diff, y = LGA),
                                fill = ggokabeito::palette_okabe_ito(3) %>%
                                  shades::lightness(60),
                                colour = "black",
                                scale = 1.2) +
  
  xlab(NULL) +
  ylab(NULL) +
  
  coord_cartesian(clip = "off") +
  
  plot_theme +
  theme(panel.border = element_blank())



ggsave(
  "results/pdd_b_diff.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)

draws_b %>%
  
  group_by(LGA) %>% 
  mutate(LGA_mean = mean(b)) %>%
  ungroup() %>%
  arrange(LGA_mean) %>%
  mutate(LGA = factor(LGA, unique(LGA))) %>% 
  
  ggplot() +
  ggridges::geom_density_ridges(aes(x = b, y = LGA),
                                fill = ggokabeito::palette_okabe_ito(5) %>%
                                  shades::lightness(60),
                                colour = "black",
                                scale = 1.2) +
  
  xlab(NULL) +
  ylab(NULL) +
  
  coord_cartesian(clip = "off")  +
  
  plot_theme +
  theme(panel.border = element_blank())



ggsave(
  "results/pdd_b.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)

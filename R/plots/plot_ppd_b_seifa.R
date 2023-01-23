


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


seifa_sa1 <- strayr::get_seifa("sa1")

lga_correspond <- absmapsdata::get_correspondence_absmaps("sa1", 2016, "lga", 2018)


#[1] "irsed"  "irsead" "ier"    "ieo"   
seifa_LGA_range <- seifa_sa1 %>%
  mutate(sa1_11_code = as.character(sa1_11_code)) %>% 
  left_join(
    lga_correspond,
    by = c("sa1_11_code" = "SA1_MAINCODE_2016")
  ) %>%
  
  rename(LGA = LGA_NAME_2018) %>%
  filter(LGA %in% model_data$LGA_names) %>%
  
  filter(seifa_index == "ieo") %>%
  
  group_by(LGA, seifa_index) %>%
  summarise(
    score_median = median(score),
    score_lower = quantile(score, 0.25),
    score_upper = quantile(score, 0.75)
  )



draws_b %>%
  group_by(.draw) %>% 
  mutate(diff = b / b[LGA == "Melbourne (C)"]) %>% 
  ungroup() %>%
  
  #filter(LGA != "Melbourne (C)") %>% 
  
  group_by(LGA) %>% 
  mutate(LGA_mean = mean(diff)) %>%
  ungroup() %>%
  arrange(LGA_mean) %>%
  mutate(LGA = factor(LGA, unique(LGA))) %>%
  
  #filter(.draw %% 10 == 0) %>% 
  
  left_join(seifa_LGA_range, by = "LGA") %>% 
  
  group_by(seifa_index, LGA) %>%
  
  summarise(score = first(score_median),
            upper_score = first(score_upper),
            lower_score = first(score_lower),
            lower_diff = quantile(diff, 0.25),
            upper_diff = quantile(diff, 0.75),
            median_diff = median(diff)) %>% 
  
  ggplot() +
  
  geom_errorbar(aes(ymin = lower_score,ymax = upper_score, x = median_diff), width = 0.0035) + 
  geom_errorbarh(aes(xmin = lower_diff, xmax = upper_diff, y = score), height = 10) +
  geom_point(aes(x = median_diff, y = score)) +
  geom_point(aes(x = median_diff, y = score), size = 0.4, colour = ggokabeito::palette_okabe_ito(3)) +
  
  xlab(NULL) +
  
  ylab("Score") +
  
  plot_theme




ggsave(
  "results/pdd_b_seifa.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)


draws_b %>%
  group_by(.draw) %>% 
  mutate(diff = b / b[LGA == "Melbourne (C)"]) %>% 
  ungroup() %>%
  
  #filter(LGA != "Melbourne (C)") %>% 
  
  group_by(LGA) %>% 
  mutate(LGA_mean = mean(diff)) %>%
  ungroup() %>%
  arrange(LGA_mean) %>%
  mutate(LGA = factor(LGA, unique(LGA))) %>%
  
  #filter(.draw %% 10 == 0) %>% 
  
  left_join(wfh_table %>% rename(LGA = LGA_NAME19), by = "LGA") %>% 
  
  group_by(LGA) %>%
  
  summarise(score = median(SA2_WFH),
            lower_score = quantile(SA2_WFH, 0.25),
            upper_score = quantile(SA2_WFH, 0.75),
            lower_diff = quantile(diff, 0.25),
            upper_diff = quantile(diff, 0.75),
            median_diff = median(diff)) %>% 
  
  ggplot() +
  
  geom_errorbar(aes(ymin = lower_score,ymax = upper_score, x = median_diff), width = 0.0035) + 
  geom_errorbarh(aes(xmin = lower_diff, xmax = upper_diff, y = score)) +
  geom_point(aes(x = median_diff, y = score)) +
  geom_point(aes(x = median_diff, y = score), size = 0.4, colour = ggokabeito::palette_okabe_ito(3)) +
  
  xlab(NULL) +
  
  ylab("Proportion") +
  
  plot_theme


ggsave(
  "results/pdd_b_wfh.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)



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





sim_g_2 <- spread_draws(
  model_fit$draws(), gamma, mu_wt, b[LGA], I_log_0_wt[LGA]) %>%
  ungroup() %>%
  mutate(LGA = model_data$LGA_names[LGA]) %>%
  
  left_join(model_data$fit_data_tbl_wt, by = c("LGA")) %>%
  
  mutate(g = mu_wt * (1 - mobility * b) - gamma) 


sim_g_inc_diff <- sim_g_2 %>%
  
  group_by(.draw, date) %>%
  mutate(m_alt = mobility[LGA == "Melbourne (C)"]) %>% 
  
  ungroup() %>%
  filter(LGA != "Melbourne (C)") %>% 
  
  group_by(.draw, LGA) %>%
  arrange(date) %>% 
  mutate(g = mu_wt * (1 - mobility * b) - gamma,
         s = cumsum(g) + I_log_0_wt,
         
         g_alt = mu_wt * (1 - m_alt * b) - gamma,
         s_alt = cumsum(g_alt) + I_log_0_wt,
         
         diff = mean(exp(s) - exp(s_alt))) 

plot_data <- sim_g_inc_diff %>% 
  
  group_by(LGA) %>%
  mutate(mean_diff = mean(diff)) %>%
  ungroup() %>%
  arrange(mean_diff) %>%
  fix_LGA() %>% 
  mutate(LGA = factor(LGA, unique(LGA)))

plot_data %>% 
  
  ggplot() +
  
  ggridges::geom_density_ridges(aes(y = LGA, x = diff),
                                fill = ggokabeito::palette_okabe_ito(3) %>%
                                  shades::lightness(60),
                                colour = "black",
                                scale = 1.2) +
  geom_linerange(aes(xmin = 0, xmax = 2.85, y = LGA),
                 plot_data %>% group_by(LGA) %>% slice(1)) +
  
  xlab(NULL) + ylab(NULL) +
  
  plot_theme +
  
  coord_cartesian(xlim = c(0, NA))


ggsave(
  "results/pdd_b_incidence.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)


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


library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))


fit_data_alt <- model_data$fit_data_tbl_wt %>%
  group_by(LGA) %>%
  mutate(mobility_alt = lead(mobility, n = 7)) %>% 
  fill(mobility_alt)



draws_all <- spread_draws(
  model_fit$draws(), gamma, mu_wt, b[LGA], I_log_0_wt[LGA]) %>%
  ungroup() %>%
  mutate(LGA = model_data$LGA_names[LGA]) %>%
  
  left_join(fit_data_alt, by = c("LGA"))


sim_g <- draws_all %>%
  group_by(.draw, LGA) %>% 
  
  mutate(
    g = mu_wt * (1 - mobility * b) - gamma,
    g_alt = mu_wt * (1 - mobility_alt * b) - gamma,
    
    I = I_log_0_wt + cumsum(g),
    I_alt = I_log_0_wt + cumsum(g_alt)
  ) 


sim_g_summ <- sim_g %>%
  group_by(.draw, LGA) %>% 
  
  summarise(
    total_I = sum(exp(I)),
    total_I_alt = sum(exp(I_alt))
  )


ggplot(sim_g_summ) +
  geom_point(aes(x = LGA, y = total_I, colour = "Baseline")) +
  geom_point(aes(x = LGA, y = total_I_alt, colour = "Earlier reduction")) +
  
  plot_theme


sim_g_summ %>%
  group_by(LGA) %>% 
  mutate(mean_LGA = mean(1 - total_I_alt / total_I)) %>%
  fix_LGA() %>% 
  ungroup() %>%
  arrange(mean_LGA) %>%
  mutate(LGA = factor(LGA, unique(LGA))) %>% 

  ggplot() +
  ggridges::geom_density_ridges(
    aes(y = LGA, x = 1 - total_I_alt / total_I),
    fill = ggokabeito::palette_okabe_ito(3),
    colour = "black",
    scale = 1.2) +
  
  geom_linerange(aes(xmin = 0, xmax = 0.254, y = LGA),
                 sim_g_summ %>% fix_LGA() %>%  group_by(LGA) %>% slice(1)) +
  
  geom_linerange(aes(xmin = 0.542, xmax = 1, y = LGA),
                 sim_g_summ %>% fix_LGA() %>%  group_by(LGA) %>% slice(1)) +
  
  xlab(NULL) +
  ylab(NULL) +
  
  coord_cartesian(clip = "off",
                  xlim = c(0, 1)) +
  
  scale_x_continuous(labels = scales::label_percent(),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  
  plot_theme +
  theme(panel.border = element_blank())


ggsave(
  "results/counterfactual_mobility.png",
  bg = "white",
  width = 5, height = 3, dpi = 300
)  




make_quants <- . %>%
  
  pivot_wider(names_from = draw,
              names_prefix = "draw_",
              values_from = value) %>%
  make_results_quants(c(0.5, 0.9, 0.95, 0.99), col_prefix = "draw_")



I_quants <- sim_g %>% 
  mutate(I = exp(I)) %>% 
  ungroup() %>%
  select(date, LGA, draw = .draw, value = I) %>%
  make_quants() 


I_alt_quants <- sim_g %>% 
  mutate(I_alt = exp(I_alt)) %>% 
  ungroup() %>%
  select(date, LGA, draw = .draw, value = I_alt) %>%
  make_quants() 


ggplot() +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              
              fill = ggokabeito::palette_okabe_ito(5),
              I_quants) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              
              fill = ggokabeito::palette_okabe_ito(3),
              I_alt_quants) +
  
  facet_wrap(~LGA, scales = "free_y") +
  
  scale_alpha_manual(values = alpha_vals, name = "Credible interval",
                     labels = c("99" = "99%", "95" = "95%", "90" = "90%", "50" = "50%")) +
  
  xlab(NULL) + ylab(NULL) +
  
  plot_theme +
  
  theme(legend.position = "bottom")



ggsave(
  "results/counterfactual_mobility_time.png",
  bg = "white",
  width = 6, height = 4, dpi = 300
)



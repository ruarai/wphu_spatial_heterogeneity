


library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))



sim_g <- spread_draws(
  model_fit$draws(), gamma, mu_wt, log_f_pred_wt[t, LGA], b[LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  left_join(model_data$fit_data_tbl_wt, by = c("date", "t", "LGA")) %>%
  
  mutate(g = mu_wt * (1 - mobility * b) - gamma,
         g_adj = mu_wt * (1 - mobility * b) - gamma + log_f_pred_wt) 

sim_g_quants <- sim_g %>%
  select(date, LGA, draw = .draw, value = g) %>%
  make_quants()

sim_g_adj_quants <- sim_g %>%
  select(date, LGA, draw = .draw, value = g_adj) %>%
  make_quants()


LGAs_plot <- c("Brimbank (C)", "Hobsons Bay (C)", "Melton (C)", "Melbourne (C)")


ggplot()  +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(3),
              sim_g_adj_quants %>% filter(LGA %in% LGAs_plot)) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(1),
              sim_g_quants %>% filter(LGA %in% LGAs_plot)) +
  
  scale_alpha_manual(values = alpha_vals, name = "Credible interval",
                     labels = c("99" = "99%", "95" = "95%", "90" = "90%", "50" = "50%")) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~LGA) +
  
  xlab(NULL) + ylab(NULL) +
  
  plot_theme +
  
  theme(legend.position = "bottom")




ggsave(
  "results/ppderror_wt.png",
  bg = "white",
  width = 6, height = 4, dpi = 300
)



sim_g_delta <- spread_draws(model_fit$draws(), gamma, mu_delta, log_f_pred_delta[t, LGA], b[LGA], VE) %>%
  ungroup() %>%
  mutate(date = model_data$dates_delta[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  left_join(model_data$fit_data_tbl_delta, by = c("date", "t", "LGA")) %>%
  
  mutate(g = mu_delta * (1 - mobility * b) * (1 - p_second_dose * VE) - gamma,
         g_adj = mu_delta * (1 - mobility * b) * (1 - p_second_dose * VE) - gamma + log_f_pred_delta) 

sim_g_quants_delta <- sim_g_delta %>%
  select(date, LGA, draw = .draw, value = g) %>%
  make_quants()

sim_g_adj_quants_delta <- sim_g_delta %>%
  select(date, LGA, draw = .draw, value = g_adj) %>%
  make_quants()


case_growth <- model_data$fit_data_tbl_delta %>%
  arrange(LGA, date) %>% 
  group_by(LGA) %>%
  mutate(n_cases = zoo::rollmean(n_cases, 14, fill = NA),
         growth_rate = log(n_cases) - lag(log(n_cases)))


ggplot()  +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(5),
              sim_g_adj_quants_delta %>% filter(LGA %in% LGAs_plot)) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(6),
              sim_g_quants_delta %>% filter(LGA %in% LGAs_plot)) +
  
  scale_alpha_manual(values = alpha_vals, name = "Credible interval",
                     labels = c("99" = "99%", "95" = "95%", "90" = "90%", "50" = "50%")) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~LGA) +
  
  plot_theme +
  theme(legend.position = "bottom")



ggsave(
  "results/ppderror_delta.png",
  bg = "white",
  width = 6, height = 4, dpi = 300
)






sim_g_delta %>%
  group_by(.draw, LGA) %>%
  summarise(MAE = mean(abs(log_f_pred_delta))) %>%
  
  ggplot() +
  
  geom_point(aes(x = MAE, y = LGA),
             position = position_jitter(height = 0.25),
             size = 0.5) +
  
  xlab("Mean absolute error") +
  ylab(NULL) +
  
  coord_cartesian(xlim = c(0, NA)) +
  
  plot_theme


sim_g %>%
  group_by(.draw, LGA) %>%
  summarise(MAE = mean(abs(log_f_pred_wt))) %>%
  
  ggplot() +
  
  geom_point(aes(x = MAE, y = LGA),
             position = position_jitter(height = 0.25),
             size = 0.1, stroke = 0.2,
             colour = ggokabeito::palette_okabe_ito(5)) +
  
  xlab("Mean absolute error") +
  ylab(NULL) +
  
  coord_cartesian(xlim = c(0, NA))  +
  
  plot_theme


ggsave(
  "results/ppderror_mae_delta.png",
  bg = "white",
  width = 6, height = 4, dpi = 300
)


pred_error_mean <- sim_g %>%
  filter(LGA == "Melton (C)") %>%
  
  group_by(n_cases) %>%
  summarise(mean = mean(log_f_pred_wt))

sim_g %>%
  filter(LGA == "Brimbank (C)",
         .draw %% 10 == 0) %>%
  
  ggplot() +
  
  geom_point(aes(x = n_cases, y = log_f_pred_wt),
             size = 0.2, stroke = 0.1,
             position = position_jitter(width = 0.5)) +
  
  geom_point(aes(x = n_cases, y = mean),
             
             colour = ggokabeito::palette_okabe_ito(5),
             pred_error_mean) +
  
  geom_hline(yintercept = 0,
             colour = ggokabeito::palette_okabe_ito(1)) +
  
  plot_theme




error_quants_wt <- spread_draws(model_fit$draws(), n_cases = log_f_pred_wt[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, value = log_f_pred_wt) %>%
  make_quants()

ggplot() +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant),
              error_quants_wt %>% filter(LGA %in% LGAs_plot)) +
  
  scale_fill_manual(values = quant_fills, name = "Credible interval",
                    labels = c("99" = "99%", "95" = "95%", "90" = "90%", "50" = "50%")) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~LGA) +
  
  xlab(NULL) + ylab(NULL) +
  
  plot_theme +
  
  theme(legend.position = "bottom")



ggsave(
  "results/ppd_error_isolate_wt.png",
  bg = "white",
  width = 6, height = 4, dpi = 300
)




library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))





sim_g <- spread_draws(
  model_fit$draws(), gamma, mu_wt, log_f_pred_wt[t, LGA], b[LGA], I_log_0_wt[LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  left_join(model_data$fit_data_tbl_wt, by = c("date", "t", "LGA")) %>%
  
  mutate(g = mu_wt * (1 - mobility * b) - gamma,
         g_adj = mu_wt * (1 - mobility * b) - gamma + log_f_pred_wt) 



p1 <- sim_g %>%
  filter(.draw == 10,
         LGA == "Brimbank (C)") %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = g_adj, group = .draw),
            size = 0.7,
            colour = ggokabeito::palette_okabe_ito(5)) +
  
  geom_hline(yintercept = 0, size = 0.5) +
  coord_cartesian(ylim = c(-0.18, 0.18)) +
  
  xlab(NULL) + ylab(bquote(g[i](t))) +
  
  plot_theme


p2 <- sim_g %>%
  filter(.draw == 10,
         LGA == "Brimbank (C)") %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = I_log_0_wt + cumsum(g_adj), group = .draw),
            colour = ggokabeito::palette_okabe_ito(5),
            size = 0.7) +
  
  xlab(NULL) + ylab(expression(mu(t))) +
  
  plot_theme

p3 <- sim_g %>%
  filter(.draw == 10,
         LGA == "Brimbank (C)") %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = exp(I_log_0_wt + cumsum(g_adj)), group = .draw),
            colour = ggokabeito::palette_okabe_ito(5),
            size = 0.7)  +
  
  coord_cartesian(ylim = c(0, 100)) +
  
  xlab(NULL) + ylab(expression(e^mu(t))) +
  
  plot_theme


theta <- model_data$nb_theta

p4 <- sim_g %>%
  filter(.draw == 10,
         LGA == "Brimbank (C)") %>%
  
  mutate(pred_cases = exp(I_log_0_wt + cumsum(g_adj)),
         
         pred_var = pred_cases + (pred_cases) ^ 2 / theta,
         
         p = (pred_var - pred_cases) / (pred_var)) %>%
  
  expand_grid(quant = c(0.5, 0.9, 0.95, 0.99)) %>%
  
  mutate(pred_cases_lower = qnbinom(0.5 - quant / 2, size = theta, prob = 1 - p),
         pred_cases_upper = qnbinom(0.5 + quant / 2, size = theta, prob = 1 - p)) %>% 
  
  mutate(quant = quant * 100,
         quant = factor(quant, levels = c("50", "90", "95", "99")) %>% fct_rev()) %>% 
  
  ggplot() +
  
  geom_ribbon(aes(x = date, ymin = pred_cases_lower, ymax = pred_cases_upper, group = quant, fill = quant),
              #fill = ggokabeito::palette_okabe_ito(5),
              size = 0.7) +
  
  scale_fill_manual(values = quant_fills, name = "Credible interval",
                    labels = c("99" = "99%", "95" = "95%", "90" = "90%", "50" = "50%"))  +
  
  coord_cartesian(ylim = c(0, 100)) +
  
  xlab(NULL) + ylab(expression(n(t))) +
  
  plot_theme +
  theme(legend.position = "none")



cowplot::plot_grid(
  p1, p2, p3, p4,
  ncol = 1,
  align = "v", axis = "lr"
)




ggsave(
  "results/ppd_inf_trace.png",
  bg = "white",
  width = 4, height = 4.5, dpi = 300
)

library(tidyverse)
library(lubridate)
library(targets)

source("R/plot_theme.R")
source("R/population.R")


case_data <- tar_read(case_data)

non_WPHU_LGAs <- c("Hume (C)", "Whittlesea (C)", "Yarra Ranges (S)", "Cardinia (S)", "Casey (C)", "Mornington Peninsula (S)", 
                   "Frankston (C)", "Greater Dangenong (C)", "Knox (C)", "Monash (C)", "Kingston (C)", "Bayside (C)",
                   "Glen Eira (C)", "Whitehorse (C)", "Manningham (C)", "Maroondah (C)", "Yarra (C)", "Port Phillip (C)",
                   "Darebin (C)", "Banyule (C)", "Boroonda (C)", "Stonnington (C)")

non_WPHU_LGAs_pop <- 243901 + 229396 + 156068 + 118194 + 365239 + 168948 +
  139281 + 158208 + 159103 + 190397 + 158129 +101306 +
  148908 + 169346 + 124700 + 115043 + 90114 + 101942 + 
  148570 + 126236 + 167900 + 104703


total_pop <- population_LGAs$pop %>% sum()
total_pop_VIC <- 6503491
total_pop_without <- total_pop_VIC - total_pop

cumulative_cases <- case_data %>%
  count(date = date_specimen) %>% 
  complete(date = seq(min(date), max(date), "days"), fill = list(n = 0)) %>%
  mutate(n = cumsum(n) / total_pop) 


cumulative_cases_by_LGA <- case_data %>%
  count(date = date_specimen, LGA) %>% 
  complete(LGA, date = seq(min(date), max(date), "days"), fill = list(n = 0)) %>%
  left_join(population_LGAs, by = "LGA") %>% 
  group_by(LGA) %>% 
  mutate(n = cumsum(n) / pop) 


all_case_data <- read_csv("https://www.dhhs.vic.gov.au/ncov-covid-cases-by-lga-source-csv") %>%
  select(date = diagnosis_date, acquired, LGA = Localgovernmentarea)


outside_cumulative_cases <- all_case_data %>%
  filter(!(LGA %in% unique(case_data$LGA))) %>%
  count(date) %>% 
  complete(date = seq(min(date), max(date), "days"), fill = list(n = 0)) %>%
  mutate(n = cumsum(n) / total_pop_without) 


outside_cumulative_cases_metro <- all_case_data %>%
  filter((LGA %in% non_WPHU_LGAs)) %>%
  count(date) %>%
  mutate(n = cumsum(n) / non_WPHU_LGAs_pop) 

ggplot() +
  geom_step(aes(x = date, y = n * 100 - 0.03, colour = "within"),
            size = 0.9,
            cumulative_cases)  +
  geom_step(aes(x = date, y = n * 100 - 0.03, colour = "without"),
            size = 0.9,
            outside_cumulative_cases_metro) +
  
  coord_cartesian(xlim = c(ymd("2020-06-01"), ymd("2020-10-01")),
                  ylim = c(0, 0.75)) +
  plot_theme +
  
  
  annotate(
    "label", x = ymd("2020-09-01"), y = 0.08, label = "Rest of Melbourne", label.r = unit(0, "cm"),
  ) +
  annotate(
    "label", x = ymd("2020-09-01"), y = 0.72, label = "WPHU catchment", label.r = unit(0, "cm"),
  ) +
  
  
  scale_colour_manual(
    values = c("within" = ggokabeito::palette_okabe_ito(5),
               "without" = "black"),
    name = NULL
  ) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B"))) +
  
  
  theme(legend.position = "none") +
  
  xlab(NULL) +
  ylab("Cumulative cases (percentage)")


ggsave(
  "results/WPHU_burden_second_wave.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)




ggplot() +
  geom_step(aes(x = date, y = n * 100 - 0.7, colour = "within"),
            size = 0.9,
            cumulative_cases)  +
  geom_step(aes(x = date, y = n * 100 - 0.23, colour = "without"),
            size = 0.9,
            outside_cumulative_cases_metro) +
  
  coord_cartesian(xlim = c(ymd("2021-09-01"), ymd("2021-12-15")),
                  ylim = c(0, 3.5)) +
  plot_theme +
  
  
  annotate(
    "label", x = ymd("2021-11-15"), y = 0.5, label = "Rest of Melbourne", label.r = unit(0, "cm"),
  ) +
  
  annotate(
    "label", x = ymd("2021-11-15"), y = 2.8, label = "WPHU catchment", label.r = unit(0, "cm"),
  ) +
  
  
  scale_colour_manual(
    values = c("within" = ggokabeito::palette_okabe_ito(5),
               "without" = "black"),
    name = NULL
  ) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B"))) +
  
  
  theme(legend.position = "none") +
  
  xlab(NULL) +
  ylab("Cumulative cases (percentage)")


ggsave(
  "results/WPHU_burden_delta.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)




ggplot() +
  geom_step(aes(x = date, y = n * 100, colour = "LGA", group = LGA),
            size = 0.5,
            cumulative_cases_by_LGA %>% mutate(n = n - n[date == ymd("2020-06-01")])) +
  geom_step(aes(x = date, y = n * 100, colour = "within"),
            size = 0.9,
            cumulative_cases %>% mutate(n = n - n[date == ymd("2020-06-01")]))   +
  geom_step(aes(x = date, y = n * 100, colour = "without"),
            size = 0.9,
            outside_cumulative_cases_metro %>% mutate(n = n - n[date == ymd("2020-06-01")])) +
  
  coord_cartesian(xlim = c(ymd("2020-06-01"), ymd("2020-10-01")),
                  ylim = c(0, 1)) +
  plot_theme +

  
  scale_colour_manual(
    values = c("within" = ggokabeito::palette_okabe_ito(5),
               "LGA" = ggokabeito::palette_okabe_ito(1),
               "without" = "black"),
    name = NULL
  ) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B"))) +
  
  
  theme(legend.position = "none") +
  
  xlab(NULL) +
  ylab("Cumulative cases (percentage)")


ggsave(
  "results/WPHU_burden_second_wave_within.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)



ggplot() +
  geom_step(aes(x = date, y = n * 100, colour = "LGA", group = LGA),
            size = 0.5,
            cumulative_cases_by_LGA %>% mutate(n = n - n[date == ymd("2021-09-01")])) +
  geom_step(aes(x = date, y = n * 100, colour = "within"),
            size = 0.9,
            cumulative_cases %>% mutate(n = n - n[date == ymd("2021-09-01")]))   +
  geom_step(aes(x = date, y = n * 100, colour = "without"),
            size = 0.9,
            outside_cumulative_cases_metro %>% mutate(n = n - n[date == ymd("2021-09-01")])) +
  
  coord_cartesian(xlim = c(ymd("2021-09-01"), ymd("2021-12-15")),
                  ylim = c(0, 3.5)) +
  plot_theme +
  
  scale_colour_manual(
    values = c("within" = ggokabeito::palette_okabe_ito(5),
               "LGA" = ggokabeito::palette_okabe_ito(1),
               "without" = "black"),
    name = NULL
  ) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B"))) +
  
  
  theme(legend.position = "none") +
  
  xlab(NULL) +
  ylab("Cumulative cases (percentage)")


ggsave(
  "results/WPHU_burden_delta_within.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)


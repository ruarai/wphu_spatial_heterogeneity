

get_vaccination_data <- function(LGAs) {
  
  sa4_data <- read_csv("https://vaccinedata.covid19nearme.com.au/data/air_sa4.csv")
  
  sa4_west_pop <- 853054
  
  sa4_data_west <- sa4_data %>%
    filter(STATE == "VIC",
           SA4_NAME == "Melbourne - West") %>%
    
    select(date = DATE_AS_AT,
           count_approx = AIR_SA4_SECOND_DOSE_APPROX_COUNT,
           pop_2019 = ABS_ERP_2019_POPULATION)  %>%
    
    mutate(p_second_dose_sa4 = count_approx / sa4_west_pop)
  
  sa4_data_west %>%
    
    
    ggplot() +
    
    geom_line(aes(x = date, y = p_second_dose_sa4))
  
  
  source("R/population.R")
  
  lga_data <- read_csv("https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv")
  
  lga_data_adj <- lga_data %>%
    filter(ABS_NAME %in% LGAs) %>%
    
    select(LGA = ABS_NAME,
           date = DATE_AS_AT,
           count_approx = AIR_SECOND_DOSE_APPROX_COUNT,
           pop_2019 = ABS_ERP_2019_POPULATION) %>%
    
    left_join(population_LGAs, by = "LGA") %>%
    
    mutate(p_second_dose = count_approx / pop) %>%
    
    select(LGA, date, p_second_dose)
  
  
  lga_vacc_ORs <- lga_data_adj %>%
    filter(date == min(date)) %>%
    
    expand_grid(
      sa4_data_west  %>%
        filter(date == ymd("2021-08-29")) %>% 
        select(p_second_dose_sa4)
    ) %>%
    
    mutate(
      vacc_OR = qlogis(p_second_dose) - qlogis(p_second_dose_sa4)
    ) %>%
    
    select(LGA, vacc_OR)
  
  
  lga_vacc_imputed <- sa4_data_west  %>%
    filter(date < ymd("2021-08-29")) %>% 
    
    select(date, p_second_dose_sa4) %>% 
    
    expand_grid(lga_vacc_ORs) %>%
    
    mutate(p_second_dose = plogis(qlogis(p_second_dose_sa4) + vacc_OR)) %>%
    
    select(LGA, date, p_second_dose) %>%
    
    bind_rows(
      expand_grid(
        LGA = LGAs,
        date = ymd("2021-04-01"),
        p_second_dose = 0
      )
    ) %>%
    
    bind_rows(
      lga_data_adj
    ) %>%
    complete(LGA = LGAs, date = seq(min(date), max(date), "days")) %>% 
    
    arrange(LGA, date) %>%
    
    group_by(LGA) %>%
    mutate(p_second_dose = zoo::na.approx(p_second_dose)) %>%
    
    filter(date <= ymd("2022-01-01"))
}








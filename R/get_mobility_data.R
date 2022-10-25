

get_mobility_data <- function() {
  
  # mobility_raw <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
  # 
  # 
  # mobility_vic <- mobility_raw %>%
  #   filter(country_region == "Australia",
  #          sub_region_1 == "Victoria")
  
  mobility_vic <- fst::read_fst("data/mobility_report_raw_vic.fst")
  
  metro_LGAs_VIC <- tribble(
    ~google_name, ~LGA2020_name, 
    "City of Brimbank", "Brimbank (C)",
    "City of Hobsons Bay", "Hobsons Bay (C)", 
    "City of Maribyrnong", "Maribyrnong (C)",
    "City of Melbourne", "Melbourne (C)",
    "Melton City", "Melton (C)",
    "City of Moonee Valley", "Moonee Valley (C)",
    "City of Moreland", "Moreland (C)",
    "City of Wyndham", "Wyndham (C)",
  )
  
  
  
  mobility_vic %>%
    select(-c(country_region_code, country_region, sub_region_1, metro_area, iso_3166_2_code, census_fips_code, place_id)) %>%
    
    rename(google_name = sub_region_2) %>%
    drop_na(google_name) %>%
    left_join(metro_LGAs_VIC, by = "google_name") %>%
    
    drop_na(LGA2020_name) %>% 
    
    select(LGA = LGA2020_name, date, ends_with("change_from_baseline")) %>%
    
    pivot_longer(ends_with("change_from_baseline"),
                 names_to = "metric", values_to = "change") %>%
    
    mutate(metric = str_remove(metric, "_percent_change_from_baseline"))
  
}

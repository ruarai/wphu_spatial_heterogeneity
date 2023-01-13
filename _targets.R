library(targets)
library(tarchetypes)


options(tidyverse.quiet = TRUE)
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)




source("R/periods.R")
source("R/population.R")
source("R/plot_theme.R")


source("R/process_case_data.R")
source("R/plot_case_data.R")
source("R/get_mobility_data.R")

source("R/get_vaccination_data.R")

source("R/get_model_data.R")
source("R/fit_model.R")

list(
  tar_target(
    case_data_file,
    "data/as_received/2022-10-14_SH_new_boundary.xlsx"
  ),
  
  tar_target(
    case_data,
    process_case_data(case_data_file),
    format = "fst_tbl"
  ),
  
  tar_target(
    case_data_plots,
    plot_case_data(case_data),
    format = "file"
  ),
  
  
  tar_target(
    mobility_data,
    get_mobility_data(),
    format = "fst_tbl"
  ),
  
  tar_target(
    vaccination_data,
    
    get_vaccination_data(LGAs = unique(case_data$LGA))
  ),
  
  tar_target(
    model_data,
    
    get_model_data(
      case_data,
      mobility_data,
      vaccination_data
    )
  ),
  
  tar_target(
    model_file,
    "src/growth_rate_vacc_gp.stan",
    format = "file"
  ),
  
  tar_target(
    model,
    cmdstanr::cmdstan_model(model_file)
  ),
  
  tar_target(
    model_fit,
    fit_model(model, model_data),
    format = "file"
  )
)
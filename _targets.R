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

source("R/fit_reff.R")

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
    reff_fit,
    
    fit_reff(case_data)
  )
)
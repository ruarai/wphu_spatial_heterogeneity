

process_case_data <- function(case_data_file) {
  
  case_data <- readxl::read_xlsx(case_data_file) %>%
    mutate(date_symptom_onset = as_date(SymptomsOnsetDate),
           date_specimen = as_date(FirstSpecimenPositiveDate)) %>%
    
    select(date_symptom_onset, date_specimen, LGA)
  
  
  
  return(case_data)
}

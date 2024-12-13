# Purpose of script: Generate data for OBT training
# Author: Mathilde Mousset
# Date Created: 2024-12-12
# Email: mathilde.mousset@epicentre.msf.org




# Setup -----------------------------------------------

library(tidyverse)

# Path to the Measles standard setup I used
path_setup <- "D:/MSF/OutbreakTools - DEV/Measles EAF/MSLEAF23_standard_20241021/setup-MSL_STD_20241021.xlsb"



### Get distributions and re generate clean linelist / lab data OR import the rds below
# source(here::here("R", "measles_data.R")) # Output: "data/clean/measles_params.rds"
# source(here::here("R", "gen_linelist.R")) # Output: "data/clean/simulated_measles_ll.rds"
# source(here::here("R", "gen_lab_data.R"))

# read data (I I don't want or need to rerun the previous script)
sim_ll <- readRDS(here::here("data", "clean", "simulated_measles_ll.rds"))
lab    <- readRDS(here::here("data", "clean", "simulated_measles_lab_data.rds"))


# Probabilities for tweaking variables (arbitrary, I have no idea)
p_severe_pneumonia <- 0.1

p_transfered_msf <- 0.2      # There was no transfer outcome
p_transfered_other <- 0.1

p_dead_arrival <- 0.1        # Probab among deads

p_pregnant <- 0.3      
p_pregnant_uncertain <- 0.05


# Map the variables to the linelist --------------------------------

# Keep only the last test
lab <- lab %>% 
  arrange(id, date_test) %>% 
  slice_tail(by = id)

# Join and prepare the data
sim_ll_obt <- sim_ll %>% 
  select(-ct_value) %>% 
  left_join(lab, by = "id") %>% 
  mutate(
    sex_id = case_match(sex, 
                        "f" ~ "Female",
                        "m" ~ "Male"),
    age_unit = case_match(age_unit,
                          "months" ~ "Month",
                          "years" ~ "Year"),
    
    hospitalised_yn = if_else(is.na(hospitalisation), "No", "Yes"),
    date_hospitalisation_start = if_else(hospitalised_yn == "Yes", 
                                         date_admission,
                                         NA),
    
    TDR_malaria = case_match(malaria_rdt,
                             "positive" ~ "Positive",
                             "negative" ~ "Negative",
                             "inconclusive" ~ "Undetermined",
                             NA ~ "Test not performed"),
    
    fever = case_when(
      fever == 0 ~ "No",
      fever == 1 ~ "Yes"
    ),
    
    rash = case_when(
      rash == 0 ~ "No",
      rash == 1 ~ "Yes"
    ),
    
    cough = case_when(
      cough == 0 ~ "No",
      cough == 1 ~ "Yes"
    ),
    
    red_eye = case_when(
      red_eye == 0 ~ "No",
      red_eye == 1 ~ "Yes"
    ),
    
    encephalitis = case_when(
      encephalitis == 0 ~ "No",
      encephalitis == 1 ~ "Yes"
    ),
    
    pneumonia = case_when(
      pneumonia == 0 ~ "No",
      pneumonia == 1 ~ sample(x = c("Yes without serious signs",
                                    "Yes with severe signs"), 
                              size = nrow(sim_ll),
                              replace = TRUE,
                              prob = c(1 - p_severe_pneumonia,
                                       p_severe_pneumonia))
    ),
    
    outcome = case_when(
      outcome == "left against medical advice" ~ "Left against medical advice",
      
      hospitalised_yn == "No" & outcome == "recovered" ~ "Recoverred in community",
      hospitalised_yn == "Yes"  & outcome == "recovered" ~ "Discharged",
      
      hospitalised_yn == "No"  & outcome == "dead" ~  "Dead in community ", # Yes, space :(
      hospitalised_yn == "Yes" & outcome == "dead" ~ sample(
        x = c("Dead on arrival ", # yes, there is a fucking space at the end
              "Dead in facility"), 
        size = nrow(sim_ll),
        replace = TRUE,
        prob = c(p_dead_arrival, 1 - p_dead_arrival)),
      hospitalised_yn == "Yes" & is.na(outcome) ~ sample(
        x = c("Transferred (to an MSF facility)",
              "Transferred (to External Facility)",
              NA), 
        size = nrow(sim_ll),
        replace = TRUE,
        prob = c(p_transfered_msf,
                 p_transfered_other,
                 1 - p_transfered_msf - p_transfered_other)),
      
      !is.na(date_outcome) & is.na(outcome) ~ "Unknown",
      .default = outcome
    ),
    
    pregnancy_yn = case_when(
      sex_id    == "Male"      ~ NA,
      age_group != "15+ years" ~ NA,
      sex_id == "Female" ~ sample(
        x = c("Yes", "No", "Uncertain"), 
        size = nrow(sim_ll),
        replace = TRUE,
        prob = c(p_pregnant,
                 1 - p_pregnant - p_pregnant_uncertain,
                 p_pregnant_uncertain))
    ),
    
    final_classification = case_when(
      epi_classification == "confirmed" ~ "1-Lab confirmed",
      epi_classification == "probable" ~ "2-Epi-linked",
      epi_classification == "suspected" ~ "3-Clinically compatible"
    ),
    
    sample_yn = if_else(is.na(date_test), "No", "Yes"),
    
    sample_measles_result = case_when(
      lab_result == "negative" ~ "Negative",
      lab_result == "positive" ~ "Positive",
      sample_yn  == "No"       ~ "Test not performed"
    ),
    
    adm1_residence = paste0("TCD ", region)
    
  ) %>% 
  rename(
    case_id = id,
    facility_name = site,
    date_notification = date_admission,
    age_num = age,
    adm2_residence = sub_prefecture, 
    date_symptom_start = date_onset,
    patient_name = case_name,
    muac_adm = muac_cat,
    vacci_measles_yn = vacc_status,
    vacci_measles_doses = vacc_doses,
    date_hospitalisation_end = date_outcome,
    date_sample_occurred = date_test,
    sample_id = lab_id
  ) %>% 
  select(-c(sex, age_group, hospitalisation, muac, ct_value, 
            lab_result, malaria_rdt, epi_classification, region))

  

# Export ----------------------------------------------


# We create a fake data export that we will populate with our raw data 
export_prep <- obtdata::export_generate(
  x = path_setup,
  country = "TCD",
  n = 0, 
  range_date = as.Date(c("2022-01-01", "2024-01-01")),
  language = "English"
)


# Prepare the metadata 
export_prep$Info$value <- NA_character_
export_prep$Info$value[export_prep$Info$variable == "info_outbreak_code"] <- "MSL24"
export_prep$Info$value[export_prep$Info$variable == "info_organisation"] <- "MSF-OCP"
export_prep$Info$value[export_prep$Info$variable == "info_country"] <- "Chad - TCD"
export_prep$Info$value[export_prep$Info$variable == "info_country_code"] <- "TCD"
export_prep$Info$value[export_prep$Info$variable == "info_disease"] <- "Measles"
export_prep$Info$value[export_prep$Info$variable == "info_facility_name"] <- "Mandoul Hospital"

## add the LL to the export 
export_prep[["Linelist patients"]] <- sim_ll_obt

## Translation - only if required
# export_prep <- ll_translate(export_prep, from = "English", to = "Français")

# write to file
timestamp <- format(Sys.time(), '%Y%m%d-%H%M')

qxl::qxl(
  export_prep,
  file.path(paste0("simulated_data_obt_tchad_measles",
                   glue::glue("_{timestamp}.xlsx" ))),
  col_widths = 8
)











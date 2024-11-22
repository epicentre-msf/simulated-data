## ===
# Purpose of script: make dirty data from the clean simulated linelist
#
# Author: Hugo Soubrier
#
# Date Created: 2024-04-09
#
# Email: hugo.soubrier@epicentre.msf.org
# ===
#
#
# ===

# Load packages ---------------------------

pacman::p_load(
  rio, # import funcs
  simulist, # generate fake linelist
  epiparameter, # get epi parameters
  fitdistrplus, # fit best distribution
  sf, # work with spatial data
  fs, # work with path
  here, # create relative paths
  janitor, # data cleaning
  lubridate, # date handling
  tidyverse # data science
)

# If pacman fails:
# install.packages('simulist', 
# repos = c('https://epiverse-trace.r-universe.dev', 
#           'https://cloud.r-project.org'))


conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# read measles data
sim_clean <- readRDS(here::here("data", "clean", "simulated_measles_ll.rds"))

names(sim_clean)

sim_clean |> count(encephalitis)

sim_raw <- sim_clean |> 
  
  mutate(sex = case_when( 
    
    # Sex
    sex == "f" ~ sample(c("f", "female", "femme"), replace = TRUE, prob = c(.8, .1, .1), size = nrow(sim_clean)), 
    sex == "m" ~ sample(c("m", "male", "homme"), replace = TRUE, prob = c(.8, .1, .1), size = nrow(sim_clean))) , 
    
    #age_unit
    age_unit = case_when(age_unit == "years" ~ sample(c("years", NA), replace = TRUE, prob = c(.95, .05), size = nrow(sim_clean)), 
                         .default = age_unit),
    
    across(c(fever, rash, cough, red_eye, pneumonia, encephalitis), ~ case_when(
      .x == 1 ~sample(c(1, NA), replace = TRUE, size = nrow(sim_clean), prob = c(.9, .1)),
      .x == 0 ~sample(c(0, NA), replace = TRUE, size = nrow(sim_clean), prob = c(.85, .15))
    )
    ), 
    across(c(fever, rash, cough, red_eye, pneumonia, encephalitis), ~ case_match(.x, 1 ~ "Yes", 0 ~ "No")), 
    across(c(region, sub_prefecture), ~ str_to_upper(.x))
  ) 

#random row id
rows_id <- sample(1:nrow(sim_raw), replace = FALSE, size = 50)
rows_id_2 <- sample(1:nrow(sim_raw), replace = FALSE, size = 10)

sim_raw <- sim_raw |> 
  mutate(
    date_admission = case_when(row_number() %in% rows_id ~ format(date_admission, "%Y-%b-%d"), 
                               .default = as.character(date_admission)
    ), 
    date_onset = case_when(row_number() %in% rows_id_2 ~ date_onset - years(10), 
                           .default = date_onset)
  )

# remove variables 
sim_raw <- sim_raw |> 
  select(-c(ct_value, 
            case_name, 
            age_group, 
            epi_classification, 
            muac_cat)
  )

# rename variables 
sim_raw |> names()

sim_raw_final <- sim_raw |> 
  rename(
    `EpiID Number` = id, 
    `Sex patient` = sex, 
    `Age` = age, 
    `Age Units (months/years)` = age_unit, 
    `Date of onset of symptoms` = date_onset, 
    `Hospitalisation ("yes/no)` = hospitalisation, 
    `Date of Admission in structure` = date_admission, 
    `Date of Outcome` = date_outcome, 
    `Sub prefecture of residence` = sub_prefecture, 
    `Region of residence` = region, 
    `Participant had fever ?` = fever, 
    `Participant had rash ?` = rash, 
    `Participant had cough ?` = cough, 
    `Participant had red_eye ?` = red_eye, 
    `Participant had pneumonia ?` = pneumonia, 
    `Participant had encephalitis ?` = encephalitis, 
    `Middle Upper Arm Circumference (MUAC)` = muac, 
    `Vaccination status` = vacc_status, 
    `Vaccination dosage` = vacc_doses, 
    `Patient outcome (death/recovered/LAMA)` = outcome,
    `MSF site` = site, 
    `Malaria RDT` = malaria_rdt
  )

#export in Excel 
export(sim_raw_final, here::here("data", "final", "msf_linelist_moissala_2023-09-24.xlsx"))

export(sim_raw_final, here::here("data", "final", "msf_linelist_moissala_2023-09-24.csv"))

# # Keep only half of the outbreak
# sim_raw_sub <- sim_raw_final |> 
#   
#   filter(`Date of onset of symptoms` < "2023-06-11")
# 
# #export in Excel 
# export(sim_raw_sub, here::here("data", "final", "msf_linelist_moissala_2023-06-11.xlsx"))

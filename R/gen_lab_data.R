# ---------------------------
# Purpose of script: generate lab data to match the simulated linelist 
#
# Author: Hugo Soubrier
#
# Date Created: 2024-11-12
#
# Email: hugo.soubrier@epicentre.msf.org
# ---------------------------
# Notes:
# This script has to be run after the gen_linelist.R
#
#
# ---------------------------

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
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# read data
sim_ll <- readRDS(here::here("data", "clean", "moissala_linelist_clean_EN.rds"))

lab <- sim_ll |> 
  
  filter(epi_classification == "confirmed") |> 
  
  select(id, 
         outcome,
         date_onset,
         date_admission, 
         date_outcome, 
         sub_prefecture
  ) |> 
  
  #Add a lab_id
  mutate(lab_id  = paste0(toupper(str_extract(sub_prefecture, ".[a-z]{2}")), "-")) |> 
  
  mutate(.by = lab_id, 
         lab_id = paste0(lab_id, seq_along(lab_id) ) ) 

# See what's going on there
lab <- lab |> 
  mutate(
    delay_hosp = ifelse(!is.na(date_outcome), as.numeric(date_outcome - date_admission), sample(1:3, size = 1) ),
    delay_test = sample(1:delay_hosp, replace = TRUE, size = nrow(lab) ),
    date_test = date_admission + delay_test, 
    lab_result = case_when( 
      
      outcome == "recovered" ~ sample(c("negative", "positive", "inconclusive"), replace = TRUE, size = nrow(lab), prob = c(.1, .85, .05)), 
      
      outcome == "dead" ~ sample(c("negative", "positive", "inconclusive"), replace = TRUE, size = nrow(lab), prob = c(.1, .85, .05)),
      
      .default = sample(c("negative", "positive", "inconclusive"), replace = TRUE, size = nrow(lab), prob = c(.1, .85, .05))
    ), 
    ct_value = if_else(lab_result == "positive",  round(digits = 1, rnorm(nrow(lab), 27.3)), NA) ) 

inc <- lab |> filter(lab_result == "inconclusive")

inc <- inc |> 
  mutate(
    lab_id = paste0(lab_id, "_retest"),
    date_test = date_test + sample(1:4, size = nrow(inc), replace = TRUE), 
    lab_result = case_when( 
      
      outcome == "recovered" ~ sample(c("negative", "positive", "inconclusive"), replace = TRUE, size = nrow(inc), prob = c(.1, .89, .01)), 
      
      outcome == "dead" ~ sample(c("negative", "positive", "inconclusive"), replace = TRUE, size = nrow(inc), prob = c(.1, .89, .01)),
      
      .default = sample(c("negative", "positive", "inconclusive"), replace = TRUE, size = nrow(inc), prob = c(.1, .89, .01))
    ), 
    
    ct_value = if_else(lab_result == "positive",  rnorm(nrow(inc), 27.3), NA)
  )

#bind inconclusives back 
lab <- bind_rows(lab, inc) |> select(id, lab_id, date_test, ct_value, lab_result)

# rename some variables 
lab_raw <- lab |> 
  rename(
    `MSF Number ID` = id, 
    `Laboratory id` = lab_id, 
    ` Date of the test` =  date_test,
    `CT value` = ct_value, 
    `Final Test Result` = lab_result
  )

export(lab, here::here("data", "clean", "moissala_laboratory_clean_EN.rds"))

export(lab_raw, here::here("data", "final", "xlsx", "moissala_laboratory_EN.xlsx"))
export(lab_raw, here::here("data", "final", "csv", "moissala_laboratory_EN.csv"))
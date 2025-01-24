# Purpose of script: Generate dirty and clean data for Elisabeth's module
# Author: Mathilde Mousset
# Date Created: 23 january 2025
# Email: mathilde.mousset@epicentre.msf.org


# Setup -----------------------------------------------

pacman::p_load(
  rio,       # import funcs
  fitdistrplus, # fit best distribution
  sf,        # work with spatial data
  fs,        # work with path
  here,      # create relative paths
  janitor,   # data cleaning
  lubridate, # date handling
  tidyverse  # data science
)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")


# Import data -----------------------------------------

# Import the clean data prepared by Hugo on the main

# English version
df_en_raw <- read_rds(here::here("data", "clean", "simulated_measles_ll.rds"))

# French version
df_fr_raw <- read_rds(here::here("data", "clean", "simulated_measles_ll_fr.rds"))


# Prepare the clean data ------------------------------

# We prepare the "cleaned" version of the linelist to use in Excel.
# The dataset is pseudo-anonymised and simplified.


## English ---------------------------------------------


df_en <- df_en_raw %>% 
  
  # Pseudo-Anonymisation
  select(-full_name, -village_commune) %>% 
  
  # Divers
  mutate(
    length_stay = if_else(!is.na(date_admission) & !is.na(date_outcome),
                          date_outcome - date_admission, NA) %>% as.integer(),
    
    across(fever:encephalitis,
           ~ case_match(.x, 
                        .x == 1 ~ "yes",
                        .x == 0 ~ "no"))
  ) %>% 
  
  # Add date consultation
  mutate(

    date_consultation = case_when(
      hospitalisation == "no"  ~ date_onset + floor(runif(1, 0, 6.2)),
      is.na(hospitalisation)   ~ date_onset + floor(runif(1, 0, 6.2)),
      hospitalisation == "yes" ~ date_admission),
    
    .before = hospitalisation) %>% 
  
  # Add age in months
  mutate(    
    age_months = case_when(
      age_unit == "years" ~ age * 12,
      .default = age),
    
    .before = age_group) %>% 
  
  # Add age in years
  mutate(    
    age_years = age_months / 12,
    .before = age_group) %>% 
  
  # To aggregate by week
  mutate(
    week_consultation = floor_date(date_consultation, 
                                        unit = "week"),
    .before = hospitalisation) %>% 

  mutate(
    date_onset = as_date(date_onset),
    date_admission = as_date(date_admission),
    date_consultation = as_date(date_consultation),
    week_consultation = as_date(week_consultation),
    date_outcome = as_date(date_outcome)
    )  %>% 
  select(-date_admission, epi_classification) 


# Checks ordre dates
df_en %>% filter(date_consultation - date_onset < 0 )
df_en %>% filter(date_outcome - date_consultation < 0 )
df_en %>% filter(length_stay < 0)

# Check vaccination doses
df_en %>% 
  count(vacc_status,
        vacc_doses)

## French ---------------------------------------------


df_fr <- df_fr_raw %>% 
  
  # Pseudo-Anonymisation
  select(-nom, -village_commune) %>% 
  
  # Divers
  mutate(
    duree_sejour = if_else(!is.na(date_admission) & !is.na(date_sortie),
                           date_sortie - date_admission , NA) %>% as.integer(),
    
    across(fievre:encephalite ,
           ~ case_match(.x, 
                        .x == 1 ~ "oui",
                        .x == 0 ~ "non"))) %>% 
  
  # Add date consultation
  mutate(
    
    date_consultation = case_when(
      hospitalisation == "no"  ~ date_debut + floor(runif(1, 0, 6.2)),
      is.na(hospitalisation)   ~ date_debut + floor(runif(1, 0, 6.2)),
      hospitalisation == "yes" ~ date_admission),
    
    .before = hospitalisation) %>% 
  
  # Add age in months
  mutate(    
    age_mois = case_when(
      age_unite == "years" ~ age * 12,
      .default = age),
    .before = age_groupe) %>%
  
  # Add age_years
  mutate(    
    age_annees = age_mois / 12,
    .before = age_groupe) %>% 
  
  
  # To aggregate by week
  mutate(
    semaine_consultation = floor_date(date_consultation,
                                      unit = "week"),
    .before = hospitalisation
  ) %>% 
  mutate(
    date_debut = as_date(date_debut),
    date_admission = as_date(date_admission),
    date_consultation = as_date(date_consultation),
    semaine_consultation = as_date(semaine_consultation),
    date_sortie = as_date(date_sortie)
  )  %>% 
  
  select(-date_admission, -classification_epi)
  
  
# Checks ordre dates
df_fr %>% filter(date_consultation - date_debut < 0 )
df_fr %>% filter(date_sortie - date_consultation < 0 )
df_fr %>% filter(duree_sejour < 0)

# Check vaccination doses
df_en %>% 
  count(vacc_status,
        vacc_doses)


# Export ----------------------------------------------

path_save <- here::here("data", "final", "xlsx", "elisabeth")

export(list("data" = df_en), 
       file.path(path_save, "msf_linelist_moissala_measles_2023-09-24_en_clean.xlsx"))

export(list("data" = df_fr), 
       file.path(path_save, "msf_linelist_moissala_rougeole_2023-09-24_fr_propres.xlsx"))

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



# Dirty linelist -------------------------------------------

sim_raw <- sim_clean |>
 
  mutate(
    
    # Add missingness to date of onset
    date_onset = case_when(
      row_number() %in% sample(1:nrow(sim_clean), replace = FALSE, size = nrow(sim_clean) / 100) ~ NA,
      .default = date_onset
    ),

    # Encoding errors on sex
    sex = case_when(
      sex == "f" ~ sample(c("f", "female", "femme"), replace = TRUE, prob = c(.8, .1, .1), size = nrow(sim_clean)),
      sex == "m" ~ sample(c("m", "male", "homme"), replace = TRUE, prob = c(.8, .1, .1), size = nrow(sim_clean))
    ),

    # Encoding errors on outcome
    outcome = case_when(
      outcome == "recovered" ~ sample(c("recovered", "gueri", NA), replace = TRUE, prob = c(.8, .15, .05), size = nrow(sim_clean)),
      outcome == "dead" ~ sample(c("died", "death", "mort", "lost", NA), replace = TRUE, prob = c(.7, .1, .1, .05, .05), size = nrow(sim_clean))
    ),

    # Convert age to months and all age_units to months
    age = ifelse(age_unit == "years", age * 12, age),
    age_unit = case_match(age_unit, "years" ~ "months", .default = age_unit),

    # age_unit adding some NA
    # age_unit = case_when(age_unit == "years" ~ sample(c("years", NA), replace = TRUE, prob = c(.95, .05), size = nrow(sim_clean)),
    #                      .default = age_unit),


    # Add some missingness to symptoms + converting to "yes" / "no"
    across(c(fever, rash, cough, red_eye, pneumonia, encephalitis), ~ case_when(
      .x == 1 ~ sample(c(1, NA), replace = TRUE, size = nrow(sim_clean), prob = c(.9, .1)),
      .x == 0 ~ sample(c(0, NA), replace = TRUE, size = nrow(sim_clean), prob = c(.85, .15))
    )),
    across(c(fever, rash, cough, red_eye, pneumonia, encephalitis), ~ case_match(.x, 1 ~ "Yes", 0 ~ "No")),
    across(c(region, sub_prefecture), ~ str_to_upper(.x))
  )

# random row id
rows_id <- sample(1:nrow(sim_raw), replace = FALSE, size = 50)
rows_id_2 <- sample(1:nrow(sim_raw), replace = FALSE, size = 10)


# Mess up dates
sim_raw <- sim_raw |>
  
  mutate(
    date_admission = case_when(row_number() %in% rows_id ~ format(date_admission, "%Y-%b-%d"),
      .default = as.character(date_admission)
    ),
    date_onset = case_when(row_number() %in% rows_id_2 ~ date_onset + years(10),
      .default = date_onset
    ),
    date_outcome = as.numeric(date_outcome)
  )

# Add some duplicates
# 150 random row id which can be binds again to data
rows_id_3 <- slice_sample(sim_raw, n = 150)

# mismatch on age +/- 5 months and then lots of missing values
rows_id_no_fullmatch <- head(rows_id_3, n = 50) |> mutate(
  age = age + sample(-5:5, , replace = TRUE, size = 50),
  across(8:27,
    .fns = ~ {
      # Number of NAs to introduce in the column
      num_na <- round(.6 * length(.))
      # Randomly sample indices to introduce NAs
      na_indices <- sample(seq_along(.), num_na)
      .[na_indices] <- NA
      . # Return the modified column
    }
  )
)

# these are perfect duplicates
sim_raw <- bind_rows(sim_raw, filter(rows_id_3, !(id %in% rows_id_no_fullmatch$id))) |>
  bind_rows(rows_id_no_fullmatch)


# rename variables
sim_raw |> names()

sim_raw_final <- sim_raw |>
  rename(
    `EpiID Number` = id,
    `Sex patient` = sex,
    `Case Name` = full_name,
    `Age` = age,
    `Age Units (months/years)` = age_unit,
    `Date of onset of symptoms` = date_onset,
    `Hospitalisation ("yes/no)` = hospitalisation,
    `Date of Admission in structure` = date_admission,
    `Date of Outcome` = date_outcome,
    `Sub prefecture of residence` = sub_prefecture,
    `Region of residence` = region,
    `Village/Commune` = village_commune,
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
    `MSF site` = health_facility_name,
    `Malaria RDT` = malaria_rdt
  )

# remove variables
var_to_remove <- c(
  "age_group",
  "epi_classification",
  "muac_cat"
)

sim_raw_final_sub <- sim_raw_final |> select(-all_of(var_to_remove))

# export in Excel
export(sim_raw_final_sub, here::here("data", "final", "xlsx", "msf_linelist_moissala_2023-09-24.xlsx"))

export(sim_raw_final_sub, here::here("data", "final", "csv", "msf_linelist_moissala_2023-09-24.csv"))


# Dirtiness dictionary --------------------------------------------------
# Create the variable and dirtiness dictionary
# this is saved to project and is reused if existing

get_cat_values <- function(x) {
  char_var <- names(x)[sapply(x, function(col) class(col) %in% c("character", "factor"))]
  # remove full_names
  char_var <- char_var[!(char_var %in% c("full_name", "Case Name") )]

  x |>
    select(char_var) |>
    mutate(across(everything(), ~ as.character(.x))) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable_name",
      values_to = "categorical_values"
    ) |>
    summarise(
      .by = variable_name,
      categorical_values = paste(unique(categorical_values), collapse = " || "),
    )
}

get_n_NA <- function(x) {
  x |>
    mutate(across(everything(), ~ as.character(.x))) |>
    pivot_longer(,
      cols = everything(),
      names_to = "variable_name",
      values_to = "values"
    ) |>
    summarise(
      .by = variable_name,
      n_na = sum(is.na(values))
    )
}

path_dict <- here::here("data", "dictionary")

if (!fs::file_exists(here::here(path_dict, "measles_ll_full_dict_en.xlsx"))) {
  cat_clean <- get_cat_values(sim_clean)
  cat_raw <- get_cat_values(sim_raw_final)

  na_clean <- get_n_NA(sim_clean)
  na_raw <- get_n_NA(sim_raw_final)

  dict <- data.frame(
    clean_variable_name = names(sim_clean),
    raw_variable_name = names(sim_raw_final),
    clean_variable_type = sapply(sim_clean, class),
    raw_variable_type = sapply(sim_raw_final, class),
    description = NA,
    dirtiness = NA
  ) |>
    mutate(type = ifelse(clean_variable_name %in% var_to_remove, "to be calculated", "provided")) |>
    # add NA counts
    left_join(select(na_clean, clean_variable_name = variable_name, clean_n_na = n_na), by = join_by("clean_variable_name")) |>
    left_join(select(na_raw, raw_variable_name = variable_name, raw_n_na = n_na), by = join_by("raw_variable_name")) |>
    # add categories
    left_join(select(cat_clean, clean_variable_name = variable_name, clean_categorical_values = categorical_values), by = join_by("clean_variable_name")) |>
    left_join(select(cat_raw, raw_variable_name = variable_name, raw_categorical_values = categorical_values), by = join_by("raw_variable_name")) |>
    as_tibble()

  fs::dir_create(path_dict)

  rio::export(dict, here::here(path_dict, "measles_ll_full_dict_en.xlsx"))
} else {
  ll_dict <- rio::import(here::here(path_dict, "measles_ll_full_dict_en.xlsx")) |> as_tibble()

  # export a subset of dictionary for participants

  dict_sub <- ll_dict |>
    filter(type != "to be calculated") |>
    select(
      variable_name = raw_variable_name,
      variable_type = raw_variable_type,
      description
    )

  rio::export(dict_sub, here::here(path_dict, "measles_ll_dict_en.xlsx"))
}

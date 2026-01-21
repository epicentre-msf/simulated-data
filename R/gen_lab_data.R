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

  select(
    id,
    outcome,
    date_onset,
    date_admission,
    date_outcome,
    sub_prefecture
  ) |>

  #Add a lab_id
  mutate(
    lab_id = paste0(toupper(str_extract(sub_prefecture, ".[a-z]{2}")), "-")
  ) |>

  mutate(.by = lab_id, lab_id = paste0(lab_id, seq_along(lab_id)))

# See what's going on there
lab <- lab |>
  mutate(
    delay_hosp = ifelse(
      !is.na(date_outcome),
      as.numeric(date_outcome - date_admission),
      sample(1:3, size = 1)
    ),
    delay_test = sample(1:delay_hosp, replace = TRUE, size = nrow(lab)),
    date_test = date_admission + delay_test,
    lab_result = case_when(
      outcome == "recovered" ~ sample(
        c("negative", "positive", "inconclusive"),
        replace = TRUE,
        size = nrow(lab),
        prob = c(.1, .85, .05)
      ),

      outcome == "dead" ~ sample(
        c("negative", "positive", "inconclusive"),
        replace = TRUE,
        size = nrow(lab),
        prob = c(.1, .85, .05)
      ),

      .default = sample(
        c("negative", "positive", "inconclusive"),
        replace = TRUE,
        size = nrow(lab),
        prob = c(.1, .85, .05)
      )
    ),
    ct_value = if_else(
      lab_result == "positive",
      round(digits = 1, rnorm(nrow(lab), 27.3)),
      NA
    )
  )

inc <- lab |> filter(lab_result == "inconclusive")

inc <- inc |>
  mutate(
    lab_id = paste0(lab_id, "_retest"),
    date_test = date_test + sample(1:4, size = nrow(inc), replace = TRUE),
    lab_result = case_when(
      outcome == "recovered" ~ sample(
        c("negative", "positive", "inconclusive"),
        replace = TRUE,
        size = nrow(inc),
        prob = c(.1, .89, .01)
      ),

      outcome == "dead" ~ sample(
        c("negative", "positive", "inconclusive"),
        replace = TRUE,
        size = nrow(inc),
        prob = c(.1, .89, .01)
      ),

      .default = sample(
        c("negative", "positive", "inconclusive"),
        replace = TRUE,
        size = nrow(inc),
        prob = c(.1, .89, .01)
      )
    ),

    ct_value = if_else(lab_result == "positive", rnorm(nrow(inc), 27.3), NA)
  )

#bind inconclusives back
lab <- bind_rows(lab, inc) |>
  select(id, lab_id, date_test, ct_value, lab_result)

# rename some variables
lab_raw <- lab |>
  rename(
    `MSF Number ID` = id,
    `Laboratory id` = lab_id,
    ` Date of the test` = date_test,
    `CT value` = ct_value,
    `Final Test Result` = lab_result
  )

#* add the linelist data to lab (used in matching exercise)
lab_ll <- lab |>
  left_join(
    select(sim_ll, id, full_name, sex, age, date_onset),
    by = join_by(id)
  )

#* Add names variation
lab_names <- lab_ll |>
  select(lab_id, full_name)

# Create variations
set.seed(456)
lab_names <- lab_names |>
  mutate(
    variation_type = sample(
      c("high", "medium", "flip", "none", "missing"),
      n(),
      replace = TRUE,
      prob = c(0.20, 0.50, 0.15, 0.10, 0.05)
    ),
    wrong_lab_name = case_when(
      # Missing names
      variation_type == "missing" ~ NA_character_,

      # 20% high variation - significant typos, missing letters
      variation_type == "high" ~ str_replace_all(
        full_name,
        c(
          "a" = "e",
          "i" = "e",
          "Khadija" = "Kadija",
          "Hassan" = "asan",
          "Souleymane" = "Suleyman",
          "Moussa" = "Musa",
          "Nadja" = "N."
        )
      ),

      # 50% medium variation - minor typos, one letter change
      variation_type == "medium" ~ case_when(
        row_number() %% 4 == 0 ~ str_replace(full_name, "a$", "ah"),
        row_number() %% 4 == 1 ~ str_replace(full_name, "h", ""),
        row_number() %% 4 == 2 ~ str_replace(full_name, "i", "y"),
        TRUE ~ str_replace(full_name, "ou", "o")
      ),

      # 20% flip order
      variation_type == "flip" ~ paste(
        word(full_name, 2),
        word(full_name, 1)
      ),

      # 10% no change
      variation_type == "none" ~ full_name
    )
  ) |>
  select(lab_id, full_name, wrong_lab_name)

# join back
lab_ll_names <- lab_ll |>
  left_join(lab_names, by = join_by("lab_id", "full_name")) |>
  select(
    id,
    lab_id,
    date_test,
    ct_value,
    lab_result,
    full_name = wrong_lab_name,
    sex,
    age,
    date_onset
  ) |>
  #* add variation in record
  mutate(
    # Age variation: +/- 3 years (only 30% of records)
    age = if_else(
      runif(n()) < 0.30,
      pmax(0, age + sample(-1:3, n(), replace = TRUE)),
      as.numeric(age)
    ),

    # Sex variation: extremely rare swaps (1%)
    sex = if_else(
      runif(n()) < 0.01,
      if_else(sex == "m", "f", "m"),
      sex
    ),

    # Date of onset variation: +/- 2 days (only 30% of records)
    date_onset = if_else(
      runif(n()) < 0.20,
      date_onset + sample(-2:2, n(), replace = TRUE),
      date_onset
    )
  )


#* Add records not in the linelist
# Create fake lab records not in linelist

#* add names
#* Additional male names (different from the original set)
male_names_new <- c(
  "Abdelkerim",
  "Abdoulaye",
  "Adoum",
  "Alhadji",
  "Ammar",
  "Aziz",
  "Bachir",
  "Haroun",
  "Ibrahim",
  "Khalil",
  "Mahamat Ali",
  "Mustapha",
  "Omar",
  "Ousman",
  "Ramadan",
  "Saleh",
  "Tahir",
  "Zakaria"
)

#* Additional female names (different from the original set)
female_names_new <- c(
  "Achta",
  "Aisha",
  "Asma",
  "Aziza",
  "Fanta",
  "Fatma",
  "Hassana",
  "Houa",
  "Kadidja",
  "Mariama",
  "Maryam",
  "Naima",
  "Rokia",
  "Safiya",
  "Zara"
)

#* Additional surnames (different from the original set)
surnames_new <- c(
  "Abderahim",
  "Adoum",
  "Ahmat Saleh",
  "Bechir",
  "Daoud",
  "Fadoul",
  "Garba",
  "Hamid",
  "Haroun",
  "Issaka",
  "Khamis",
  "Mbaigoto",
  "Nadjita",
  "Nour",
  "Ousmane",
  "Ramadan",
  "Tahir",
  "Wardougou",
  "Yacoub",
  "Zakaria"
)

# Generate names for the 200 fake records (leave 2 as NA)
set.seed(123) # for reproducibility
fake_names <- c(
  NA_character_,
  NA_character_,
  paste(
    sample(c(male_names_new, female_names_new), 198, replace = TRUE),
    sample(surnames_new, 198, replace = TRUE)
  )
)

n_fake <- 200

fake_lab <- tibble(
  id = max(sim_ll$id) + seq_len(n_fake), # IDs not in linelist
  lab_id = paste0(
    sample(c("MAR", "FIR"), n_fake, replace = TRUE),
    "-",
    seq_len(n_fake)
  ),
  date_test = sample(
    seq(min(lab$date_test), max(lab$date_test), by = "day"),
    n_fake,
    replace = TRUE
  ),
  ct_value = sample(
    c(NA, round(rnorm(n_fake * 2, 27.3), 1)),
    n_fake,
    replace = TRUE
  ),
  lab_result = sample(
    c("negative", "positive", "inconclusive"),
    n_fake,
    replace = TRUE,
    prob = c(.3, .6, .1)
  ),
  full_name = sample(fake_names, n_fake, replace = FALSE), # use the fake names you created
  sex = sample(c("m", "f"), n_fake, replace = TRUE),
  age = sample(0:80, n_fake, replace = TRUE), # realistic age distribution
  date_onset = date_test - sample(1:10, n_fake, replace = TRUE) # onset before test
)

# Combine with existing lab_ll_names data
lab_ll_full <- bind_rows(lab_ll_names, fake_lab) |>
  arrange(date_test) |>
  as_tibble()

# Verify records not in linelist
lab_ll_full |>
  mutate(in_linelist = id %in% sim_ll$id) |>
  count(in_linelist)


export(
  lab_ll_full,
  here::here("data", "clean", "moissala_laboratory_clean_EN.rds")
)

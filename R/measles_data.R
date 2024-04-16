# Using real data to infer some distribution for a measles outbreak linelist

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

# Path to data
path_measles <- fs::path(here::here(Sys.getenv("SHAREPOINT_PATH"), "EAST-AFRICA-MEASLES-2023 - Documents", "2. Data", "data", "measles_east_africa_linelist_compiled_2024-04-12_160114.rds") )

# read data
dat <- readRDS(path_measles)

# Clean data ----------------------

dat_clean <- dat |>
  mutate(
    age_group = case_match(
      age_group,
      "0 - 5 mois" ~ "< 6 months",
      "0 - 5 months" ~ "< 6 months",
      "0 - 6 months" ~ "< 6 months",
      "6 - 8 mois" ~ "6 - 8 months",
      "9 - 11 mois" ~ "9 - 11 months",
      "1 - 4 ans" ~ "1 - 4 years",
      "12 - 59 months" ~ "1 - 4 years",
      "5 - 14 ans" ~ "5 - 14 years",
      "15 ans et plus" ~ "15+ years",
      .default = age_group
    ),
    age_group = as.character(
      age_group,
      c(
        "< 6 months",
        "6 - 8 months",
        "9 - 11 months",
        "1 - 4 years",
        "5 - 14 years",
        "15+ years"
      )
    )
  ) |>
  mutate(
    age_range = case_match(
      age_group,
      "< 6 months" ~ "0-1",
      "6 - 8 months" ~ "0-1",
      "9 - 11 months" ~ "0-1",
      "1 - 4 years" ~ "2-4",
      "5 - 14 years" ~ "5-14",
      "15+ years" ~ "15-40",
      .default = age_group
    ),
    age_group = fct_relevel(
      age_group,
      c(
        "< 6 months",
        "6 - 8 months",
        "9 - 11 months",
        "1 - 4 years",
        "5 - 14 years",
        "15+ years"
      )
    )
  )

export(dat_clean, here::here("data", "clean", "real_measles_data.rds"))

# Define parameters -------------------------------------------------------

# probability of infection upon contact
prob_infection <- 0.5

# create contact distribution - Poisson, mean 2, can't really find better
dist_contact <- epidist(
  disease = "Measles",
  pathogen = "Measles Virus",
  epi_dist = "contact distribution",
  prob_distribution = "pois",
  prob_distribution_params = c(mean = 2)
)

# distribution from onset to hospitalisation
ons_hosp_dist <- dat |>
  filter(hospitalised_yn == "Yes") |>
  select(date_notification, date_symptom_start, date_hospitalisation_start, date_hospitalisation_end) |>
  filter(!is.na(date_symptom_start), !is.na(date_hospitalisation_start)) |>
  mutate(ons_hosp = as.numeric(date_hospitalisation_start - date_symptom_start)) |>
  filter(ons_hosp < 50, ons_hosp > 0)

summary(ons_hosp_dist$ons_hosp)

# Try different distribution
gamma <- fitdist(ons_hosp_dist$ons_hosp, distr = "gamma", method = "mle")
weibull <- fitdist(ons_hosp_dist$ons_hosp, distr = "weibull", method = "mle")
negbin <- fitdist(ons_hosp_dist$ons_hosp, distr = "nbinom", method = "mle")

ons_hosp_dist |>
  ggplot() +
  geom_density(aes(x = ons_hosp), col = "darkred") +
  scale_x_discrete(breaks = seq(0, 50, 1)) +
  stat_function(fun = dgamma, args = list(shape = 1.9964291, rate = 0.4711589))

# Onset hospitalisation distribution
dist_ons_hosp <- epiparameter::epidist(
  disease = "Measles",
  epi_dist = "onset to hospitalisation",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1.9964291, rate = 0.4711589)
)

# Onset to death
hosp_out <- dat |>
  filter(hospitalised_yn == "Yes", !is.na(outcome), !is.na(date_hospitalisation_end), !is.na(date_hospitalisation_start)) |>
  select(date_notification, date_symptom_start, date_hospitalisation_start, date_hospitalisation_end) |>
  mutate(hosp_out_var = as.numeric(date_hospitalisation_start - date_symptom_start)) |>
  filter(hosp_out_var > 0, hosp_out_var < 80)

summary(hosp_out$hosp_out_var)

# Try different distribution
gamma <- fitdist(hosp_out$hosp_out_var, distr = "gamma", method = "mle")

hosp_out |>
  ggplot() +
  geom_density(aes(x = hosp_out_var), col = "darkred") +
  scale_x_continuous(breaks = seq(0, 50, 1)) +
  stat_function(fun = dgamma, args = list(shape = 1.9964291, rate = 0.4711589))

# Onset hospitalisation distribution
dist_hosp_out <- epiparameter::epidist(
  disease = "Measles",
  epi_dist = "hospitalisation to outcome",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1.9964291, rate = 0.4711589)
)

# Infectious period is -4 / +4 from/after symptoms
inf <- dat |>
  filter(hospitalised_yn == "Yes", !is.na(outcome), !is.na(date_symptom_start), !is.na(date_hospitalisation_end)) |>
  select(date_notification, date_symptom_start, date_hospitalisation_end) |>
  mutate(inf_time = as.numeric(date_hospitalisation_end - date_symptom_start) + 4) |>
  filter(inf_time > 0, inf_time < 30)

summary(inf$inf_time)

# Try different distribution
gamma <- fitdist(inf$inf_time, distr = "gamma", method = "mle")

inf |>
  ggplot() +
  geom_density(aes(x = inf_time), col = "darkred") +
  scale_x_continuous(breaks = seq(0, 50, 1)) +
  stat_function(fun = dgamma, args = list(shape = 8.8244367, rate = 0.8406069))

# create Measles infectious period
dist_infect_period <- epiparameter::epidist(
  disease = "Measles",
  epi_dist = "infectious period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 8.8244367, rate = 0.8406069)
)


# define an age structure
age_str <- dat_clean |>
  count(age_range) |>
  na.omit() |>
  mutate(p = round(n / sum(n), digits = 3)) |>
  rename(proportion = p) |>
  select(age_range, proportion)

# define hospitalisation based on age
age_hosp <- dat_clean |>
  mutate(age_range = case_match(
    age_group,
    "< 6 months" ~ "0-1",
    "6 - 8 months" ~ "0-1",
    "9 - 11 months" ~ "0-1",
    "1 - 4 years" ~ "2-4",
    "5 - 14 years" ~ "5-14",
    "15+ years" ~ "15-40",
    .default = age_group
  )) |>
  summarise(
    .by = age_range,
    n = n(),
    n_hosp = sum(hospitalised_yn == "Yes", na.rm = TRUE),
    risk = n_hosp / n
  ) |>
  mutate(age_limit = case_when(
    age_range == "0-1" ~ 0,
    age_range == "2-4" ~ 2,
    age_range == "5-14" ~ 5,
    age_range == "15-40" ~ 15
  )) |>
  na.omit() |>
  select(age_limit, risk)

# under 1 age structure because sim_linelist only takes integer
under_1_age_str <- dat_clean |>
  filter(age_group %in% c("< 6 months", "6 - 8 months", "9 - 11 months")) |>
  count(age_group) |>
  mutate(p = n / sum(n))

# Symptoms probabilities from data
sym_prob <- dat_clean |>
  drop_na(age_group) |>
  summarise(
    .by = age_group,
    n = n(),
    n_fever = sum(fever == "Yes", na.rm = TRUE),
    p_fever = round(digits = 3, n_fever / n),
    n_cough = sum(cough == "Yes", na.rm = TRUE),
    p_cough = round(digits = 3, n_cough / n),
    n_rash = sum(rash == "Yes", na.rm = TRUE),
    p_rash = round(digits = 3, n_rash / n),
    n_red_eye = sum(red_eye == "Yes", na.rm = TRUE),
    p_red_eye = round(digits = 3, n_red_eye / n),
    n_pneumonia = sum(pneumonia == "Yes with severe signs", na.rm = TRUE),
    p_pneumonia = round(digits = 3, n_pneumonia / n),
    n_encephalitis = sum(encephalitis == "Yes", na.rm = TRUE),
    p_encephalitis = round(digits = 4, n_encephalitis / n),
  ) |>
  select(age_group, contains("p_"))

# Malnutrition 
muac_prob <- dat_clean |>
  drop_na(age_group, muac_adm) |>
  count(age_group, muac_adm) |>
  mutate(
    .by = age_group,
    p = n / sum(n)
  )

# Vaccination status
vacc_prob <- dat_clean |>
  drop_na(age_group) |>
  count(
    age_group,
    vacci_measles_yn
  ) |>
  mutate(
    .by = age_group,
    p = n / sum(n)
  )

doses_prob <- dat_clean |>
  drop_na(age_group) |>
  filter(
    vacci_measles_yn %in% c("Yes - card", "Yes - oral"),
    vacci_measles_doses %in% c(NA, "Uncertain", "1 dose", "2 doses")
  ) |>
  count(
    age_group,
    vacci_measles_doses
  ) |>
  mutate(
    .by = age_group,
    p = n / sum(n)
  ) 

# Hospital length distribution
dist_hosp_length <- epiparameter::epidist(
  disease = "Measles",
  epi_dist = "hospitalisation length",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 2.2251860, rate = 0.8541434)
)



# list all parameters together 
measles_params <- list("dist_infect_period" = dist_infect_period,
                       "dist_hosp_out" = dist_hosp_out, 
                       "dist_ons_hosp" = dist_ons_hosp, 
                       "dist_contact" = dist_contact, 
                       "age_str" = age_str, 
                       "age_hosp" = age_hosp, 
                       "under_1_age_str" = under_1_age_str, 
                       "sym_prob" = sym_prob, 
                       "muac_prob" = muac_prob, 
                       "vacc_prob" = vacc_prob, 
                       "doses_prob" = doses_prob)

saveRDS(measles_params, here::here("data", "clean", "measles_params.rds"))
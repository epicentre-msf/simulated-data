# Purpose of script: fetch distributions from Measles data in the East Africa dashboard
#
# Author: Hugo Soubrier
#
# Date Created: 2024-11-12
#
# Email: hugo.soubrier@epicentre.msf.org


# Notes:



# Setup -----------------------------------------------

## Packages --------------------------------------------

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

# If pacman fails:
# install.packages('simulist', 
# repos = c('https://epiverse-trace.r-universe.dev', 
#           'https://cloud.r-project.org'))



## Paths -----------------------------------------------

# Path to East Africa dashboard data - you need to make sure it is sync to your drive
path_measles <- fs::dir_ls(here::here(Sys.getenv("SHAREPOINT_PATH"), 
                                      "EAST-AFRICA-MEASLES-2023 - Documents", 
                                      "2. Data", 
                                      "data"), 
                           regex = ".rds") |> 
  max()


## Get data ---------------------------------------

# read data
dat <- readRDS(path_measles)

# Clean data ----------------------
dat_clean <- dat |>
  
  #remove Yemen
  filter(ll_country != "YEM") |> 
  
  mutate(
    age_unit = case_match(age_unit, "Months" ~ "Month", .default = age_unit),
    age_num = if_else(age_num >= 12 & age_unit == "Month", ceiling(age_num/12), age_num ),
    age_unit = if_else(age_num >= 12 & age_unit == "Month", "Year", age_unit ),
    age_group = case_when(
      age_unit == "Day" ~ "< 6 months",
      age_num < 6 & age_unit == "Month" ~ "< 6 months",
      between(age_num, 6, 8) & age_unit == "Month" ~ "6 - 8 months",
      between(age_num, 9, 11) & age_unit == "Month" ~ "9 - 11 months",
      between(age_num, 1, 4) & age_unit == "Year" ~ "1 - 4 years",
      between(age_num, 5, 14) & age_unit == "Year" ~ "5 - 14 years",
      age_num >= 15 & age_unit == "Year" ~ "15+ years" 
    ),
    age_group = fct(
      age_group,
      c("< 6 months",
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
    )
  ) |> 
  filter(!is.na(age_group))

# Filter Chad out
dat_chad <- dat_clean |> 
  filter(ll_country == "TCD")

# Define parameters -------------------------------------------------------


## Contact distribution --------------------------------

# probability of infection upon contact
prob_infection <- 0.5

# create contact distribution - Poisson, mean 2, can't really find better
dist_contact <- epiparameter::epiparameter(
  disease  = "Measles",
  pathogen = "Measles Virus",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 2)
    )
)


## Onset to hospitalisation ----------------------------

# distribution from onset to hospitalisation
ons_hosp_dist <- dat_chad |>
  filter(hospitalised_yn == "Yes") |>
  select(date_notification, 
         date_symptom_start, 
         date_hospitalisation_start, 
         date_hospitalisation_end) |>
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
  stat_function(fun = dgamma, args = list(shape = gamma$estimate[1], rate = gamma$estimate[2]))
#stat_function(fun = dweibull, args = list(shape = 1.135107, scale = 3.246650))
#stat_function(fun = dnegbin, args = list(size = 2.402583, mu = 3.063271))

# Onset hospitalisation distribution
dist_ons_hosp <- epiparameter::epiparameter(
  disease = "Measles",
  epi_name = "onset to hospitalisation",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(gamma$estimate[1], 
                                 gamma$estimate[2])
  )

)

## Onset to death --------------------------------------
# Onset to death
hosp_out <- dat_chad |>
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
  stat_function(fun = dgamma, args = list(shape = gamma$estimate[1], rate = gamma$estimate[2] ))

# Onset hospitalisation distribution
dist_hosp_out <- epiparameter::epiparameter(
  disease = "Measles",
  epi_name = "hospitalisation to outcome",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(gamma$estimate[1], 
                                 gamma$estimate[2] )
  )
)

## Infectious period ---------------------------------------
# Infectious period is -4 / +4 from/after symptoms
inf <- dat_chad |>
  filter(hospitalised_yn == "Yes", 
         !is.na(outcome), 
         !is.na(date_symptom_start), 
         !is.na(date_hospitalisation_end)) |>
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
  stat_function(fun = dgamma, args = list(shape = gamma$estimate[1], rate = gamma$estimate[2]))

# create Measles infectious period
dist_infect_period <- epiparameter::epiparameter(
  disease = "Measles",
  epi_name = "infectious period",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(gamma$estimate[1], gamma$estimate[2])
  )
)

## Age structure -------------------------------------------

# define an age structure
age_str <- dat_clean |>
  count(age_range) |>
  na.omit() |>
  mutate(p = round(n / sum(n), digits = 3)) |>
  select(age_range, p)

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
    p = n_hosp / n
  ) |>
  mutate(age_limit = case_when(
    age_range == "0-1" ~ 0,
    age_range == "2-4" ~ 2,
    age_range == "5-14" ~ 5,
    age_range == "15-40" ~ 15
  )) |>
  na.omit() |>
  select(age_limit, p)

# under 1 age structure because sim_linelist only takes integer
under_1_age_str <- dat_clean |>
  filter(age_group %in% c("< 6 months", "6 - 8 months", "9 - 11 months")) |>
  count(age_group) |>
  mutate(p = n / sum(n))


## Symptoms --------------------------------------------------

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


## Malnutrition ---------------------------------------------

muac_prob <- dat_clean |>
  drop_na(age_group, muac_adm) |>
  count(age_group, muac_adm) |>
  mutate(
    .by = age_group,
    p = n / sum(n)
  )

## Vaccination status ---------------------------------------

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


# Hospital length ------------------------------------------
dist_hosp_length <- epiparameter::epiparameter(
  disease = "Measles",
  epi_name = "hospitalisation length",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(shape = 2.2251860, 
                                 rate = 0.8541434)
  )
)

# Gather and save ------------------------------------------

# list all parameters together
measles_params <- list(
  "prob_infection" = prob_infection,
  "dist_infect_period" = dist_infect_period,
  "dist_hosp_out" = dist_hosp_out,
  "dist_ons_hosp" = dist_ons_hosp,
  "dist_contact" = dist_contact,
  "age_str" = age_str,
  "age_hosp" = age_hosp,
  "under_1_age_str" = under_1_age_str,
  "sym_prob" = sym_prob,
  "muac_prob" = muac_prob,
  "vacc_prob" = vacc_prob,
  "doses_prob" = doses_prob
)


purrr::map(measles_params[c("age_str", "under_1_age_str") ], {
  
  ~ if(sum(.x$p) > 1 ) { stop(glue::glue("probabilities add up to more than 1 ! p : {round(digits = 2, sum(.x$p))}"))
  } else {print("all good with probabilities")} 
})

saveRDS(measles_params, here::here("data", "clean", "measles_params.rds"))

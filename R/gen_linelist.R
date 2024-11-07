# ===
# Purpose of script: generate a fake linelist of a measle outbreak in Moissala (Chad)
#
# Author: Hugo Soubrier

# ===
# Notes:
# We are using the data from the East Africa dashboard to estimate the distributions of several variables
# so that the outbreak remains close to reality
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
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# read measles data
# dat_clean <- readRDS(here::here("data", "clean", "real_measles_data.rds"))

# all params and distributions
measles_params <- readRDS(here::here("data", "clean", "measles_params.rds"))

# Generate the linelist -------------------------------------------------------

# probability of infection upon contact

set.seed(4999)
sim_ll <- sim_linelist(
  contact_distribution = measles_params$dist_contact,
  infect_period = measles_params$dist_infect_period,
  prob_infect = measles_params$prob_infection,
  onset_to_hosp = measles_params$dist_ons_hosp,
  onset_to_death = measles_params$dist_hosp_out,
  hosp_risk = rename(measles_params$age_hosp, risk = p),
  outbreak_start_date = as.Date("2022-08-13"),
  outbreak_size = c(100, 5000),
  population_age = rename(measles_params$age_str, proportion = p)
) |>
  as_tibble()

# Plot 
d <- sim_ll |> 
  mutate(epiweek = floor_date(date_onset, unit = "week")) |> 
  count(epiweek)

ggplot(data = d)+
  geom_col(aes(x = epiweek, y = n))

# Age variables -----------------------------------------------------------
sim_ll <- sim_ll |>
  # define age groups based on age structure that was given
  mutate(
    age_group = case_when(
      between(age, 0, 1) ~ "0 - 11 months",
      between(age, 2, 4) ~ "1 - 4 years",
      between(age, 5, 14) ~ "5 - 14 years",
      between(age, 15, 40) ~ "15+ years"
    ),
  ) |>
  # re sample the under 1y group
  mutate(
    age_group = if_else(
      age_group == "0 - 11 months",
      sample(
        measles_params$under_1_age_str$age_group,
        nrow(sim_ll),
        replace = TRUE,
        prob = measles_params$under_1_age_str$p
      ),
      age_group
    ),
    
    # give realistic age to under 1
    age = case_when(
      age_group == "< 6 months" ~ sample(1:5, nrow(sim_ll), replace = TRUE),
      age_group == "6 - 8 months" ~ sample(6:8, nrow(sim_ll), replace = TRUE),
      age_group == "9 - 11 months" ~ sample(9:11, nrow(sim_ll), replace = TRUE),
      .default = age
    ),
    age_unit = case_when(
      age_group %in% c(
        "< 6 months",
        "6 - 8 months",
        "9 - 11 months"
      ) ~ "months",
      .default = "years"
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
    ),
    # variable about hospitalisation
    # hospitalisation = if_else(is.na(date_admission), "no", "yes"),
    hospitalisation = sample(c(NA, "yes"), size = nrow(sim_ll), replace = TRUE, prob = c(.05, .95)),
  ) |>
  relocate(c(age, age_unit, age_group), .after = "sex") |>
  relocate(hospitalisation, .before = date_admission)

# Plots -------------------------------------------------------------------
ggplot(data = sim_ll) +
  geom_histogram(
    aes(
      x = date_onset,
      fill = age_group
    ),
    binwidth = 7
  ) +
  scale_x_date(breaks = "1 month")

ggplot(data = sim_ll) +
  geom_histogram(
    aes(
      x = date_onset,
      fill = hospitalisation
    ),
    binwidth = 7
  )


# Dates variable ----------------------------------------------------------
# we will make all cases hospitalised despite the output of sim_lis

sim_ll <- sim_ll |>
  mutate(date_admission = case_when(
    is.na(date_admission) ~ date_onset + sample(
      c(1:5, NA),
      size = nrow(sim_ll),
      replace = TRUE,
      prob = c(.2, .2, .2, .2, .19, .01)
    ),
    .default = date_admission
  ))


# Add Geographic Variables -----------------------------------------------------------

# import shapefiles of Chad
adm1 <- st_read(here::here("data", "gpkg", "GEO-EXPORT-TCD-2024-04-11.gpkg"), layer = "ADM1")
adm2 <- st_read(here::here("data", "gpkg", "GEO-EXPORT-TCD-2024-04-11.gpkg"), layer = "ADM2")

# Mandoul region of outbreak
mandoul <- filter(adm2, adm1_name == "Mandoul")

mapview::mapview(
  adm2,
  layer.name = "Adm2",
  alpha.regions = 0,
  label = "adm2_name"
) +
  mapview::mapview(
    mandoul,
    color = "darkred",
    lxd = 13,
    alpha.regions = .4,
    layer.name = "Mandoul",
    label = "adm2_name"
  )

# probability model of admin level - 2 prob based on dates
geo_dist <- data.frame(
  region = c("Mandoul", "Mandoul", "Mandoul", "Moyen Chari", "Moyen Chari", "Mandoul"),
  sub_prefecture = c("Moissala", "Bouna", "Bekourou", "Danamadji", "Koumogo", "Bedaya"),
  p = c(.25, .26, .21, .19, .01, 0.08),
  p2 = c(.65, .16, .01, .00, .00, 0.18)
)

sim_ll <- sim_ll |>
  mutate(
    sub_prefecture = case_when(
      date_onset < "2023-03-01" ~ sample(
        geo_dist$sub_prefecture,
        replace = TRUE,
        prob = geo_dist$p,
        size = nrow(sim_ll)
      ),
      .default = sample(
        geo_dist$sub_prefecture,
        replace = TRUE,
        prob = geo_dist$p2,
        size = nrow(sim_ll)
      )
    )
  ) |>
  left_join(select(geo_dist, region, sub_prefecture))


# Quick choropleth map
chor <- left_join(
  adm2,
  count(sim_ll, region, sub_prefecture),
  by = c("adm2_name" = "sub_prefecture")
) |>
  mutate(AR = round(digits = 3, n / adm2_pop * 1000))

mapview::mapview(
  chor,
  zcol = "n",
  label = "adm2_name"
)

# Add Symptoms variables  ----------------------------------------------------

sim_ll <- sim_ll |>
  # join the symptoms distributions
  left_join(measles_params$sym_prob, by = "age_group") |>
  mutate(
    fever = rbinom(n = nrow(sim_ll), size = 1, prob = p_fever),
    rash = rbinom(n = nrow(sim_ll), size = 1, prob = p_rash),
    cough = rbinom(n = nrow(sim_ll), size = 1, prob = p_cough),
    red_eye = rbinom(n = nrow(sim_ll), size = 1, prob = p_red_eye),
    pneumonia = rbinom(n = nrow(sim_ll), size = 1, prob = p_pneumonia),
    encephalitis = rbinom(n = nrow(sim_ll), size = 1, prob = p_encephalitis),
  ) |>
  select(-contains("p_"))

# Malnutrition
muac_cat <- unique(measles_params$muac_prob$muac_adm)

# Add malnutrition variable
sim_ll <- sim_ll |>
  mutate(
    muac_cat = case_when(
      age_group == "< 6 months" ~ sample(
        muac_cat,
        size = nrow(sim_ll),
        replace = TRUE,
        prob = filter(measles_params$muac_prob, age_group == "< 6 months")$p
      ),
      age_group == "6 - 8 months" ~ sample(
        muac_cat,
        size = nrow(sim_ll),
        replace = TRUE,
        prob = filter(
          measles_params$muac_prob,
          age_group == "6 - 8 months"
        )$p
      ),
      age_group == "9 - 11 months" ~ sample(
        muac_cat,
        size = nrow(sim_ll),
        replace = TRUE,
        prob = filter(measles_params$muac_prob, age_group == "9 - 11 months")$p
      ),
      age_group == "1 - 4 years" ~ sample(
        muac_cat,
        size = nrow(sim_ll),
        replace = TRUE,
        prob = filter(measles_params$muac_prob, age_group == "1 - 4 years")$p
      ),
      age_group == "5 - 14 years" ~ sample(
        muac_cat,
        size = nrow(sim_ll),
        replace = TRUE,
        prob = filter(measles_params$muac_prob, age_group == "5 - 14 years")$p
      ),
      age_group == "15+ years" ~ sample(
        muac_cat,
        size = nrow(sim_ll),
        replace = TRUE,
        prob = filter(measles_params$muac_prob, age_group == "15+ years")$p
      )
    ),
    
    # distribute a muac value
    muac = case_when(
      muac_cat == "Green (125+ mm)" ~ sample(125:250, replace = TRUE, size = nrow(sim_ll)),
      muac_cat == "Red (<115 mm)" ~ sample(60:114, replace = TRUE, size = nrow(sim_ll)),
      muac_cat == "Yellow (115 - 124 mm)" ~ sample(
        115:124,
        replace = TRUE,
        size = nrow(sim_ll)
      )
    )
  )

# Vaccination status
vacc_cat <- unique(measles_params$vacc_prob$vacci_measles_yn)
doses_cat <- unique(measles_params$doses_prob$vacci_measles_doses)

# add vaccination data
sim_ll <- sim_ll |>
  mutate(
    vacc_status = case_when(
      age_group == "< 6 months" ~ sample(vacc_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$vacc_prob, age_group == "< 6 months")$p),
      age_group == "6 - 8 months" ~ sample(vacc_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$vacc_prob, age_group == "6 - 8 months")$p),
      age_group == "9 - 11 months" ~ sample(vacc_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$vacc_prob, age_group == "9 - 11 months")$p),
      age_group == "1 - 4 years" ~ sample(vacc_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$vacc_prob, age_group == "1 - 4 years")$p),
      age_group == "5 - 14 years" ~ sample(vacc_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$vacc_prob, age_group == "5 - 14 years")$p),
      age_group == "15+ years" ~ sample(vacc_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$vacc_prob, age_group == "15+ years")$p)
    ),
    vacc_doses = if_else(
      vacc_status %in% c("Yes - card", "Yes - oral"),
      case_when(
        age_group == "< 6 months" ~ sample(doses_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$doses_prob, age_group == "< 6 months")$p),
        age_group == "6 - 8 months" ~ sample(doses_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$doses_prob, age_group == "6 - 8 months")$p),
        age_group == "9 - 11 months" ~ sample(doses_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$doses_prob, age_group == "9 - 11 months")$p),
        age_group == "1 - 4 years" ~ sample(doses_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$doses_prob, age_group == "1 - 4 years")$p),
        age_group == "5 - 14 years" ~ sample(doses_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$doses_prob, age_group == "5 - 14 years")$p),
        age_group == "15+ years" ~ sample(doses_cat, size = nrow(sim_ll), replace = TRUE, prob = filter(measles_params$doses_prob, age_group == "15+ years")$p)
      ),
      NA
    )
  )


# Build logit model for death  --------------------------------------------

# regression coef (log scale)

# Age
# < 6 months: 2.4
# 6 - 8 months: 2.3
# 9 - 11 months: 2.1
# 1 - 4 years: 1.38
# 5 - 14 years: 0.83
# 15+ years: Ref

# Malnutrition
# Severe: 1.25
# Medium: 0.53
# No: Ref

# Vaccination

# YES: -1.203973
# No vacc : Ref

# recode everything as 1/0

logit_death <- function(
    reg_age_1,
    reg_age_2,
    reg_age_3,
    reg_age_4,
    reg_age_5,
    reg_vacc,
    reg_muac_2,
    reg_muac_3,
    error
) {
  logit <- reg_age_1 * 2.4 +
    reg_age_2 * 2.1 +
    reg_age_3 * 2.1 +
    reg_age_4 * 1.38 +
    reg_age_5 * 0.83 +
    reg_vacc * -1.203973 +
    reg_muac_2 * 1.25 +
    reg_muac_3 * 0.53 +
    -3.9
  return(logit)
}

p_death <- sim_ll |>
  mutate(
    reg_age_1 = age_group == "< 6 months",
    reg_age_2 = age_group == "6 - 8 months",
    reg_age_3 = age_group == "9 - 11 months",
    reg_age_4 = age_group == "1 - 4 years",
    reg_age_5 = age_group == "5 - 14 years",
    reg_age_6 = age_group == "15+ years",
    reg_muac_1 = muac_cat == "Green (125+ mm)",
    reg_muac_2 = muac_cat == "Red (<115 mm)",
    reg_muac_3 = muac_cat == "Yellow (115 - 124 mm)",
    reg_vacc = vacc_status %in% c("Yes - card", "Yes - oral"),
    across(contains("reg_"), ~ as.numeric(.x)),
    reg_error = rnorm(nrow(sim_ll)),
    reg_logit_death = logit_death(
      reg_age_1,
      reg_age_2,
      reg_age_3,
      reg_age_4,
      reg_age_5,
      reg_vacc,
      reg_muac_2,
      reg_muac_3,
      .001
    ),
    reg_odds_death = exp(reg_logit_death),
    reg_p_death = reg_odds_death / (1 + reg_odds_death)
  ) |>
  select(contains("reg_"))

sim_ll <- sim_ll |>
  bind_cols("p_death" = p_death$reg_p_death) |>
  mutate(
    outcome = rbinom(n = nrow(sim_ll), size = 1, prob = p_death),
    outcome = as.logical(outcome)
  )

# prop of deaths generated
sim_ll |> tabyl(outcome)

# Test the Log regression -------------------------------------------------
prep_ll <- sim_ll |>
  mutate(
    age_group = fct_relevel(
      age_group,
      c(
        "15+ years",
        "< 6 months",
        "6 - 8 months",
        "9 - 11 months",
        "1 - 4 years",
        "5 - 14 years"
      )
    ),
    muac_cat = fct_relevel(
      muac_cat,
      c(
        "Green (125+ mm)",
        "Yellow (115 - 124 mm)",
        "Red (<115 mm)"
      )
    ),
    vacc_status = case_match(
      vacc_status,
      "Yes - card" ~ "Yes",
      "Yes - oral" ~ "Yes",
      "Uncertain" ~ NA,
      .default = vacc_status
    ),
    vacc_status = fct_relevel(
      vacc_status,
      c(
        "No",
        "Yes"
      )
    )
  )

# fit the logistic regression
mdl <- glm(outcome ~ fever + age_group + vacc_status + muac_cat, data = prep_ll, family = "binomial")

# view coeff
gtsummary::tbl_regression(
  mdl,
  exp = TRUE
)

# Hospital exit -----------------------------------------------------------

# randomly allocate a hospital length using a gamma
# hosp_length <- dat_clean |>
#   filter(hospitalised_yn == "Yes") |>
#   drop_na(date_hospitalisation_end) |>
#   drop_na(date_hospitalisation_start) |>
#   select(date_hospitalisation_start, date_hospitalisation_end) |>
#   mutate(delay = as.numeric(date_hospitalisation_end - date_hospitalisation_start)) |>
#   filter(delay > 0)
#
# ggplot(data = hosp_length) +
#   geom_density(aes(x = delay), binwidth = 1) +
#   stat_function(fun = dgamma, args = list(shape = 2.2251860, rate = 0.8541434))

# allocate hospital lenght
sim_ll <- sim_ll |>
  mutate(hosp_length = if_else(
    hospitalisation == "yes",
    round(digits = 0, rgamma(nrow(sim_ll), shape = 2.2251860, rate = 0.8541434)),
    NA
  )) |>
  # fix outcome and dates
  mutate(
    date_outcome = case_when(
      outcome & !is.na(date_death) ~ date_death,
      outcome & is.na(date_death) ~ date_admission + hosp_length,
      !outcome ~ date_admission + hosp_length,
      .default = NA
    )
  ) |>
  # add some variability to outcome
  mutate(outcome = case_when(
    outcome & hospitalisation == "yes" ~ sample(c("dead", "left against medical advice", NA), size = nrow(sim_ll), prob = c(.85, .02, .03), replace = TRUE),
    outcome & hospitalisation == "no" ~ sample(c("dead", NA), size = nrow(sim_ll), prob = c(.98, .02), replace = TRUE),
    !outcome ~ sample(c("recovered", "left against medical advice", NA), size = nrow(sim_ll), prob = c(.85, .02, .03), replace = TRUE)
  )) |>
  # create epi classification
  mutate(
    epi_classification = if_else(
      hospitalisation == "yes",
      sample(
        c("suspected", "probable", "confirmed"),
        size = nrow(sim_ll),
        replace = TRUE,
        prob = c(.5, .3, .2)
      ),
      sample(
        c("suspected", "probable"),
        size = nrow(sim_ll),
        replace = TRUE,
        prob = c(.7, .3)
      )
    )
  ) |>
  select(-c(hosp_length, p_death, case_type, date_death))


# Hospital data -----------------------------------------------------------

# Add hospitals

sim_ll <- sim_ll |>
  mutate(site = case_when(
    sub_prefecture == "Moissala" ~ "Moïssala Hospital",
    sub_prefecture == "Danamadji" ~ "Danamadji Hospital",
    sub_prefecture == "Bedaya" ~ "Bedaya Hospital",
    sub_prefecture == "Bekourou" ~ "Bekourou Hospital",
    sub_prefecture == "Bouna" ~ "Bouna Hospital",
    sub_prefecture == "Koumogo" ~ "Koumogo Hospital",
  )) |>
  relocate(site, .after = id)

ggplot(data = sim_ll) +
  geom_histogram(aes(x = date_onset)) +
  facet_wrap(~site)

# RDT Malaria  -----------------------------------------------------------

# incidence of Malaria in Moissala
# 684 cases per 1000 in 2021

# prev = incidence * duration disease
# say mean duration is 7 days (0.0192 year)
# prev in moissala is 684*0.0192 = 13,13 per 1000, so 1.3%
sim_ll <- sim_ll |>
  mutate(malaria_rdt = sample(
    c("positive", "negative", "inconclusive", NA),
    replace = TRUE,
    size = nrow(sim_ll),
    prob = c(0.013, 0.737, 0.15, 0.1)
  ))

# Onset date --------------------------------------------------------------
# make some onset date NA

# random rows id to make NA
rows_id <- sample(1:nrow(sim_ll), replace = FALSE, size = 300)

sim_ll <- sim_ll |>
  mutate(date_onset = case_when(
    row_number() %in% rows_id ~ NA,
    .default = date_onset
  )) |>
  select(-c(date_first_contact, date_last_contact))

# order variables
sim_ll <- sim_ll |> select(
  id,
  site,
  case_name,
  sex,
  age,
  age_unit,
  age_group,
  region,
  sub_prefecture,
  date_onset,
  hospitalisation,
  date_admission,
  ct_value,
  malaria_rdt,
  fever,
  rash,
  cough,
  red_eye,
  pneumonia,
  encephalitis,
  muac,
  muac_cat,
  vacc_status,
  vacc_doses,
  outcome,
  date_outcome,
  epi_classification
)

export(sim_ll, here::here("data", "clean", "simulated_measles_ll.rds"))

# Save shapefiles as .rds -------------------------------------------------

write_rds(adm1, "data/gpkg/chad_adm1.rds")
write_rds(adm2, "data/gpkg/chad_adm2.rds")

# Purpose of script: use the simulated linelist and add geographic parameters
#
# Author: Hugo Soubrier
#
# Date Created: 2024-12-22
#
## Load packages ---------------------------
pacman::p_load(
  rio, # import funcs
  sf, # work with spatial data
  here, # create relative paths
  janitor, # data cleaning
  lubridate, # date handling
  tidyverse # data science
)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

source("R/set_paths.R")


# Import data ------------------------------------------------------------

# import the linelist
sim_ll <- readRDS(here::here("data", "clean", "simulated_measles_ll.rds"))

# admin levels come from the geobase
adm1 <- readRDS(here::here(set_paths()$sharepoint_path, "OutbreakTools - GeoBase/TCD/TCD__ALL__20231122__124H/sf/TCD_adm1.rds"))
adm2 <- readRDS(here::here(set_paths()$sharepoint_path, "OutbreakTools - GeoBase/TCD/TCD__ALL__20231122__124H/sf/TCD_adm2.rds"))
# adm3 <- readRDS(here::here(set_paths()$sharepoint_path, "OutbreakTools - GeoBase/TCD/TCD__ALL__20231122__124H/sf/TCD_adm3.rds"))
adm4 <- readRDS(here::here(set_paths()$sharepoint_path, "OutbreakTools - GeoBase/TCD/TCD__ALL__20231122__124H/sf/TCD_adm4.rds"))

# health facilities come from the GIS
hf <- sf::st_read(
  dsn = fs::path(
    set_paths()$sharepoint_path,
    "OutbreakTools - GeoBase", "TCD", "resources", "shp", "GeoMSF", "MasterData", "MasterData.gdb"
  ),
  layer = "main_owner_hltfac_p_msf"
) |>
  select(pcode, name_fr, name_en, local_name, type_name, operated_by)

# prepare data -----------------------------------------------------------
mandoul_adm2 <- adm2 |> filter(adm1_name == "Mandoul")
# mandoul_adm3 <- adm3 |> filter(adm1_name == "Mandoul")
mandoul_adm4 <- adm4 |> filter(adm1_name == "Mandoul")

# keep only health facilities in mandoul
hf_mandoul <- hf[mandoul_adm2, ]

# Add Geographic Variables --------------------------------

# get date vector - have to go through this because date_onset in simulist is actually a datetime so can't remove duplicates with unique()
dates <- sim_ll |>
  mutate(date_onset = floor_date(date_onset, unit = "day")) |>
  distinct(date_onset) |>
  arrange(date_onset) |>
  pull(date_onset)

# Defines the outbreak dates for each of the admin2
out_dates_goundi <- c(as.Date("2023-08-19"), as.Date("2023-10-09"))
out_dates_koumra <- c(as.Date("2023-08-13"), as.Date("2023-12-01"))
out_dates_bedaya <- c(as.Date("2023-07-01"), as.Date("2023-12-01"))
out_dates_bedjondo <- c(as.Date("2023-06-08"), as.Date("2023-12-01"))
out_dates_bouna <- c(as.Date("2023-01-07"), as.Date("2023-11-28"))
out_dates_bekourou <- c(as.Date("2023-06-13"), as.Date("2023-10-28"))
out_dates_moissala <- c(as.Date("2022-08-01"), as.Date("2023-12-01"))

# check if a date is between dates of admin2
is_between <- function(date_check, out_date) {
  between(date_check, min(out_date), max(out_date))
}

# Here we create a df here we define the probability of being assigned a admin2 based on the day of the outbreak
date_prob <- data.frame(
  date = dates,
  index = rank(dates)
) |>
  as_tibble() |>
  mutate(
    check_moissala = is_between(date, out_dates_moissala),
    check_bekourou = is_between(date, out_dates_bekourou),
    check_bouna = is_between(date, out_dates_bouna),
    check_bedjondo = is_between(date, out_dates_bedjondo),
    check_bedaya = is_between(date, out_dates_bedaya),
    check_koumra = is_between(date, out_dates_koumra),
    check_goundi = is_between(date, out_dates_goundi)
  ) |>
  pivot_longer(contains("check"),
    names_to = "sub_prefecture",
    values_to = "check",
    names_prefix = "check_"
  ) |>
  mutate(sub_prefecture = str_to_sentence(sub_prefecture)) |> 
  mutate(
    .by = index,
    n_admin_out = sum(check),
    probability = case_when(
      !check ~ 0,
      n_admin_out == 1 & check ~ 1,
      n_admin_out > 1 & sub_prefecture == "moissala" & check ~ .6,
      n_admin_out > 1 & sub_prefecture != "moisala" & check ~ .4 / (n_admin_out - 1)
    )
  )

# assign the admin2 based on probability
sim_ll <- sim_ll |>
  mutate(date_onset = floor_date(date_onset, unit = "day")) |>
  left_join(date_prob,
    by = join_by(date_onset == date),
    relationship = "many-to-many"
  ) |>
  summarise(
    .by = 1:23,
    region = "Mandoul",
    sub_prefecture = sample(sub_prefecture, probability, size = 1, replace = TRUE)
  )

# visualise
ggplot(data = sim_ll) +
  geom_histogram(
    aes(
      x = date_onset,
      fill = sub_prefecture
    ),
    binwidth = 7
  ) +
  scale_x_date(date_breaks = "1 month") +
  facet_wrap(~sub_prefecture) +
  gghighlight::gghighlight() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#* Distribute cases in right village

# Define an epicenter point for each region - then draw a circle around and distribute points in villages around
epicenters <- data.frame(
  adm2 = c("Moissala", "Bouna", "Goundi", "Koumra", "Bedaya", "Bedjondo", "Bekourou"),
  epicentre_lat = c(8.34323, 8.41733011558516, 9.367655329026066, 8.908874493461946, 8.914899975059113, 8.672119168423363, 8.152893079978664),
  epicentre_lon = c(17.76326, 17.431073086773843, 17.413761518489963, 17.51068115843181, 17.8531999727768, 17.362304716154775, 17.548522959269945),
  radius = 40
)

# make epicenters as sf
epi_sf <- epicenters |> st_as_sf(
  coords = c("epicentre_lon", "epicentre_lat"),
  crs = raster::crs(adm4)
)

# add a ~ 20km buffer
buffer <- st_buffer(epi_sf, dist = 10000)

# visualise
mapview::mapview(
  filter(adm2, adm1_name == "Mandoul"),
  color = "darkred",
  alpha.regions = 0,
  layer.name = "Admin2",
  label = "adm2_name"
  # fill = FALSE
) +
  mapview::mapview(
    filter(adm4, adm1_name == "Mandoul"),
    color = "green",
    lxd = 0,
    layer.name = "Villages",
    alpha.regions = 1,
    label = "adm4_name"
  ) +

  mapview::mapview(
    buffer,
    color = "red",
    lxd = 0,
    layer.name = "Epicenters",
    alpha.regions = 1
  )


# get all villages and health zone that fall within each buffer zone
within_epi <- st_intersection(adm4, buffer)

mapview::mapview(within_epi)

# list of villages for each admin2
villages <- within_epi |>
  distinct(adm2, adm4_name) |>
  group_by(adm2) |>
  summarise(adm4_names = list(adm4_name))

# Join the linelist with the villages list, and then randomly assign an `adm4_name`
sim_ll <- sim_ll |>
  left_join(villages, by = join_by("sub_prefecture" == "adm2")) |>
  mutate(village_commune = sapply(adm4_names, function(villages) sample(villages, 1))) |>
  select(-adm4_names) |>
  relocate(c(region, sub_prefecture, village_commune), .after = age_group)

## Add Health facility data ----------------------------------------

mapview::mapview(
  hf_mandoul,
  color = "green",
  lxd = 0,
  layer.name = "Villages",
  alpha.regions = 1,
  label = "name_en"
) +
  mapview::mapview(
    buffer,
    color = "red",
    lxd = 0,
    layer.name = "Epicenters",
    alpha.regions = 1,
    label = "adm2"
  )

# keep hospitals that fall in the buffer zones
# get all villages and health zone that fall within each buffer zone
hf_within_epi <- st_intersection(hf_mandoul, buffer)

list_hf <- hf_within_epi |> 
  distinct(adm2, local_name) |> 
  filter(!is.na(local_name)) |> 
  group_by(adm2) |> 
  summarise(hf_local_name = list(local_name))

# Join the linelist with the villages list, and then randomly assign an `adm4_name`
sim_ll <- sim_ll |>
  left_join(list_hf, by = join_by("sub_prefecture" == "adm2")) |>
  mutate(
    health_facility_name = sapply(hf_local_name, function(x) sample(x, 1)), 
    #make sure no HF when hospitalisation is NA
    health_facility_name = as.character(case_when(is.na(hospitalisation) | hospitalisation == "no" ~ NA, .default = health_facility_name) )
  ) |>
  select(-hf_local_name) |>
  relocate(c(health_facility_name), .after = date_admission)
 
# Epicurve by different sites
ggplot(data = sim_ll) +
  geom_histogram(aes(x = date_onset)) +
  facet_wrap(~health_facility_name)

#* Save the data
export(sim_ll, here::here("data", "clean", "simulated_measles_ll.rds"))

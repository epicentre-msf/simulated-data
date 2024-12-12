# Purpose of script: Generate data for OBT training
# Author: Mathilde Mousset
# Date Created: 2024-12-12
# Email: mathilde.mousset@epicentre.msf.org


# Get distributions and generate clean linelist / lab data
source(here::here("R", "measles_data.R")) # Output: "data/clean/measles_params.rds"
source(here::here("R", "gen_linelist.R")) # Output: "data/clean/simulated_measles_ll.rds"

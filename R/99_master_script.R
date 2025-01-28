# Purpose of script: Generate raw and clean false data for Fetch
# Author: Mathilde Mousset
# Date Created: 2025-01-23
# Email: mathilde.mousset@epicentre.msf.org

# Notes: Hugo created the data and generating scripts, here
# I gathers all the commands to run it again faster.


# The data/clean contains the simulation parameters and the clean linelists in .RDS
# The data/final contains dirtied .xlsx and .csv files to use as raw materials

# The raw lab data is actually the same as the clean data, only
# the linelist was dirtied

# Get distributions 
source(here::here("R", "measles_data.R")) # Output: "data/clean/measles_params.rds"

# Generate the linelist
source(here::here("R", "gen_linelist.R"))     # Output: "data/clean/simulated_measles_ll.rds"
source(here::here("R", "gen_linelist_geo.R")) # Output: "data/clean/simulated_measles_ll.rds"

# Generate lab data
source(here::here("R", "gen_lab_data.R")) 
# Outputs: "data/clean/simulated_measles_lab_data.rds"
# And "data/final/msf_laboratory_moissala_2023-09-24.xlsx" (Raw, but not dirtied)

# Import, dirties, save:
source(here::here("R", "make_raw_linelist.R")) 
# Outputs: 
# "data/final/msf_linelist_moissala_2023-09-24.xlsx"
# "data/final/msf_msf_linelist_moissala_2023-09-24.csv"

# Translate all:
source(here::here("R", "linelist_fr_translation.R"))
# Outbputs:
# Clean linelist: "data/clean/simulated_measles_ll_fr.rds"
# Dirty linelist: "data/final/msf_linelist_moissala_2023-09-24_fr.xlsx"
# Dirty linelist: "data/final/msf_linelist_moissala_2023-09-24_fr.csv"
# Clean lab: "data/clean/simulated_measles_lab_data_fr.rds"
# Dirty lab (the same actually): "data/final/msf_laboratory_moissala_2023-09-24_fr.xlsx"

# Move stuff to the Repicentre.R
# ! NOT NEEDED as we use {episimdata} now
#source(here::here("R", "copy_data_to_repicentre.R"))

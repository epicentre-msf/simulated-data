# ---------------------------
# Purpose of script: Send data to the repicentre repository 
# Author: Hugo Soubrier
#
# Date Created: 2024-11-18
#
# Email: hugo.soubrier@epicentre.msf.org
# ---------------------------
# Notes:
#
# ---------------------------

# ! NOT NEEDED ANYMORE AS DATA ARE SENT IN EPISIMDATA

# user specific paths
user <- Sys.info()[["user"]]
paths <- list()
paths$user <- user
if (user == "hugzsoubrier") {
  
  paths$simulated_data_repo_path <- "/Users/hugzsoubrier/GitHub/simulated-data"
  paths$repicentre_repo_path <- "/Users/hugzsoubrier/GitHub/repicentre"
} else {
  
  stop("Enter your simulated_data and repicentre repository path here")

}

# check that the data folder exists in repicentre otherwise creates it
fs::dir_create(fs::path(paths$repicentre_repo_path, "data", "clean") ) 
fs::dir_create(fs::path(paths$repicentre_repo_path, "data", "raw"))
fs::dir_create(fs::path(paths$repicentre_repo_path, "data", "dictionary"))

# fetch only the simulated LL from the clean data folder in simulated_data repository 
clean_ll <- fs::dir_ls(fs::path(paths$simulated_data_repo_path, "data", "clean"), regexp = ".*/simulated_measles[^/]*\\.rds")

# fetch only the simulated LL from the clean data folder in simulated_data repository 
raw_dir <- fs::path(paths$simulated_data_repo_path, "data", "final")

# fetch only the simulated LL from the clean data folder in simulated_data repository 
dict_dir <- fs::path(paths$simulated_data_repo_path, "data", "dictionary")

# copy clean files
for(i in clean_ll){
  
  fs::file_copy(
    path = i, 
    new_path = fs::path(paths$repicentre_repo_path, "data", "clean"), 
    overwrite = TRUE
                )
}

#copy raw files 
  fs::dir_copy(
    path = raw_dir, 
    new_path = fs::path(paths$repicentre_repo_path, "data", "raw"), 
    overwrite = TRUE
  )

#copy dict dir 
fs::dir_copy(
    path = dict_dir, 
    new_path = fs::path(paths$repicentre_repo_path, "data", "dictionary"), 
    overwrite = TRUE
  )

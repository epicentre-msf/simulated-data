# ---------------------------
# Purpose of script: Send data to the repicentre repository
#
# Author: Hugo Soubrier
#
# Date Created: 2024-11-18
#
# Email: hugo.soubrier@epicentre.msf.org
# ---------------------------
# Notes:
#   
#
#
# ---------------------------

# user specific paths
user <- Sys.info()[["user"]]
paths <- list()
paths$user <- user
if (user == "hugzsoubrier") {
  paths$simulated_data_repo_path <- "/Users/hugzsoubrier/GitHub/simulated-data"
  paths$repicentre_repo_path <- "/Users/hugzsoubrier/GitHub/repicentre"
} else if (user == "M-MOUSSET") {
    paths$simulated_data_repo_path <- "D:/MATHILDE_UNPLUGGED/5_OTHER/simulated-data"
    paths$repicentre_repo_path <- "D:/MATHILDE_UNPLUGGED/5_OTHER/repicentre"
} else {
  stop("Enter your simulated_data and repicentre repository path here")
}

# check that the data folder exists in repicentre otherwise creates it
fs::dir_exists(fs::path(paths$simulated_data_repo_path, "data", "clean") ) 
fs::dir_exists(fs::path(paths$simulated_data_repo_path, "data", "raw"))

# fetch only the simulated LL from the clean data folder in simulated_data repository 
clean_ll <- fs::dir_ls(fs::path(paths$simulated_data_repo_path, "data", "clean"), 
                       regexp = ".*/simulated_measles[^/]*\\.rds")

# fetch only the simulated LL from the clean data folder in simulated_data repository 
raw_ll <- fs::dir_ls(fs::path(paths$simulated_data_repo_path, "data", "final"))

# copy clean files
for(i in clean_ll){
  
  fs::file_copy(
    path = i, 
    new_path = fs::path(paths$repicentre_repo_path, "data", "clean"), 
    overwrite = TRUE
                )
}

#copy raw files 
for(i in raw_ll){
  
  fs::file_copy(
    path = i, 
    new_path = fs::path(paths$repicentre_repo_path, "data", "raw"), 
    overwrite = TRUE
  )
  
}

# ---------------------------
# Purpose of script: Translate the clean and raw linelist generated previously
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

# Translate linelist ------------------------------

## Clean linelist ------------------------------

# read clean measles data
sim_clean <- readRDS(here::here("data", "clean", "simulated_measles_ll.rds"))

sim_clean |> names()

sim_clean_fr <- sim_clean |>
  # rename variable
  rename(
    nom = full_name,
    sexe = sex,
    age_unite = age_unit,
    age_groupe = age_group,
    sous_prefecture = sub_prefecture,
    date_debut = date_onset,
    rdt_palu = malaria_rdt,
    fievre = fever,
    eruption_cutanee = rash,
    toux = cough,
    yeux_rouges = red_eye,
    pneumonie = pneumonia,
    encephalite = encephalitis,
    pb = muac,
    pb_cat = muac_cat,
    statut_sortie = outcome, 
    date_sortie = date_outcome, 
    classification_epi = epi_classification
  ) |>
  # recode values
  mutate(
    age_unite = case_match(age_unite, "months" ~ "mois",
      "years" ~ "ans",
      "day" ~ "jour",
      .default = age_unite
    ),
    age_groupe = fct_relevel(
      case_match(age_groupe,
        "< 6 months" ~ "< 6 mois",
        "6 - 8 months" ~ "6 - 8 mois",
        "9 - 11 months" ~ "9 - 11 mois",
        "1 - 4 years" ~ "1 - 4 ans",
        "5 - 14 years" ~ "5 - 14 ans",
        "15+ years" ~ "15+ ans",
        .default = age_groupe
      ),
      c(
        "< 6 mois",
        "6 - 8 mois",
        "9 - 11 mois",
        "1 - 4 ans",
        "5 - 14 ans",
        "15+ ans"
      )
    ),
    hospitalisation = case_match(hospitalisation, "yes" ~ "oui", "no" ~ "non", .default = hospitalisation),
    vacc_status = case_match(vacc_status,
      "No" ~ "Non",
      "Uncertain" ~ "Incertain",
      "Yes - card" ~ "Oui - carte",
      "Yes - oral" ~ "Oui - oral",
      .default = vacc_status
    ),
    vacc_doses = case_match(vacc_doses,
      "Uncertain" ~ "Incertain",
      .default = vacc_doses
    ),
    pb_cat = case_match(pb_cat,
      "Green (125+ mm)" ~ "Vert (125+ mm)",
      "Red (<115 mm)" ~ "Rouge (<115 mm)",
      "Yellow (115 - 124 mm)" ~ "Jaune (115 - 124 mm)",
      .default = pb_cat
    ),
    statut_sortie = case_match(statut_sortie,
      "dead" ~ "deces",
      "recovered" ~ "gueri",
      "left against medical advice" ~ "sortie contre avis medical",
      .default = statut_sortie
    ),
    classification_epi = case_match(classification_epi,
      "confirmed" ~ "confirme",
      "suspected" ~ "suspect",
      "probable" ~ "probable",
      .default = classification_epi
    ),
    rdt_palu = case_match(rdt_palu,
      "inconclusive" ~ "inconclusif",
      "negative" ~ "negatif",
      "positive" ~ "positif",
      .default = rdt_palu
    )
  )

# save the fr clean version
saveRDS(sim_clean_fr, here::here("data", "clean", "simulated_measles_ll_fr.rds"))

## Raw linelist ----------------------------

# Load the raw linelist

sim_raw_final <- import(here::here("data", "final", "xlsx", "msf_linelist_moissala_2023-09-24.xlsx" )) |> as_tibble()

sim_raw_final_fr <- sim_raw_final |>
  rename(
    `Numero ID` = `EpiID Number`,
    `Sexe du patient` = `Sex patient`,
    `Nom du patient` = `Case Name`,
    `Age` = `Age`,
    `Unité d'Age (mois/ans)` = `Age Units (months/years)`,
    `Date de début des symptomes` = `Date of onset of symptoms`,
    `Hospitalisation (oui/non)` = `Hospitalisation ("yes/no)`,
    `Date d'admission à l'hopital` = `Date of Admission in structure`,
    `Date de statut final` = `Date of Outcome`,
    `Sous préfécture de résidence` = `Sub prefecture of residence`,
    `Région de résidence` = `Region of residence`,
    `Village ou Commune` = `Village/Commune`,
    `Patient à de la fièvre ?` = `Participant had fever ?`,
    `Patient à une éruption cutanée ?` = `Participant had rash ?`,
    `Patient à de la toux ?` = `Participant had cough ?`,
    `Patient à les yeux rouges ?` = `Participant had red_eye ?`,
    `Patient à une pneumonie ?` = `Participant had pneumonia ?`,
    `Patient à une encéphalite?` = `Participant had encephalitis ?`,
    `Perimètre brachial ? (PB)` = `Middle Upper Arm Circumference (MUAC)`,
    `Status de la vaccination` = `Vaccination status`,
    `Doses de vaccins` = `Vaccination dosage`,
    `Status final à la sortie` = `Patient outcome (death/recovered/LAMA)`,
    `Nom du site MSF` = `MSF site`,
    `Test rapide Paludisme` = `Malaria RDT`
  ) |>
  mutate(
    `Sexe du patient` = case_match(`Sexe du patient`, "m" ~ "h",
      .default = `Sexe du patient`
    ),
    `Unité d'Age (mois/ans)` = case_match(`Unité d'Age (mois/ans)`,
      "months" ~ "mois",
      "years" ~ "ans",
      .default = `Unité d'Age (mois/ans)`
    ),
    `Hospitalisation (oui/non)` = case_match(`Hospitalisation (oui/non)`,
      "yes" ~ "oui",
      "no" ~ "non",
      .default = `Hospitalisation (oui/non)`
    ),

    across(contains("Patient à"), ~ ifelse(.x == "Yes", "oui", "non") ),

    `Status de la vaccination` = case_match(`Status de la vaccination`,
      "No" ~ "Non",
      "Uncertain" ~ "Incertain",
      "Yes - oral" ~ "Oui - oral",
      "Yes - card" ~ "Oui - carte",
      .default = `Status de la vaccination`
    ),
    `Doses de vaccins` = case_match(`Doses de vaccins`,
      "Uncertain" ~ "Incertain",
      .default = `Doses de vaccins`
    ),
    `Status final à la sortie` = case_match(`Status final à la sortie`,
      "dead" ~ "deces",
      "recovered" ~ "gueri",
      "left against medical advice" ~ "sortie contre avis medical",
      .default = `Status final à la sortie`
    ),
    `Test rapide Paludisme` = case_match(`Test rapide Paludisme`,
      "negative" ~ "negatif",
      "inconclusive" ~ "inconclusif",
      "positive" ~ "positif",
      .default = `Test rapide Paludisme`
    ), 
    `Date de début des symptomes` = ymd(`Date de début des symptomes`)
  )
# save the raw data
saveRDS(sim_raw_final_fr, here::here("data", "final", "xlsx", "msf_listelineaire_rougeole_2023-09-24_fr.xlsx"))
saveRDS(sim_raw_final_fr, here::here("data", "final", "csv", "msf_listelineaire_rougeole_2023-09-24_fr.csv"))

# Translate lab data --------------------------------------------------------------

## Clean data ----------------------------------------------

# import lab clean data
lab_clean <- readRDS(here::here("data", "clean", "simulated_measles_lab_data.rds"))

lab_clean_fr <- lab_clean |>
  rename(
    id_labo = lab_id,
    valeur_ct = ct_value,
    resultat_labo = lab_result
  ) |>
  mutate(
    resultat_labo = case_match(resultat_labo,
      "positive" ~ "positif",
      "negative" ~ "negatif",
      "inconclusive" ~ "inconclusif",
      .default = resultat_labo
    )
  )

# save clean lab result
saveRDS(lab_clean_fr, here::here("data", "clean", "simulated_measles_lab_data_fr.rds"))

## Raw data ------------------------------------------------

# import raw lab data
lab_raw <- import(here::here("data", "final", "xlsx", "msf_laboratory_moissala_2023-09-24.xlsx")) |> as_tibble()

lab_raw_fr <- lab_raw |>
  rename(
    `Numero ID MSF` = `MSF Number ID`,
    `Numero Labo` = `Laboratory id`,
    `Date du test` = `Date of the test`,
    `Valeur CT` = `CT value`,
    `Résultat final du test` = `Final Test Result`
  ) |>
  mutate(
    `Résultat final du test` = case_match(`Résultat final du test`,
      "positive" ~ "positif",
      "inconclusive" ~ "inconclusif",
      "negative" ~ "negatif",
      .default = `Résultat final du test`
    )
  )

export(lab_raw_fr, here::here("data", "final",  "xlsx", "msf_laboratoire_moissala_2023-09-24_fr.xlsx"))
export(lab_raw_fr, here::here("data", "final",  "csv", "msf_laboratoire_moissala_2023-09-24_fr.csv"))

# Dirtiness dictionnary --------------------------------------------------
# Create the variable and dirtiness dictionary
# this is saved to project and is reused if existing

# remove variables
var_to_remove <- c(
  "age_groupe",
  "classification_epi",
  "pb_cat"
)

# First need to add three variables to sim_raw_final_fr so it has the same number than sim_clean_fr
sim_raw_final_fr <- sim_raw_final_fr |> mutate(
  age_group = NA, 
epi_classification = NA, 
muac_cat = NA) |> 
  relocate(age_group, .after = `Unité d'Age (mois/ans)` ) |> relocate(muac_cat, .after = `Perimètre brachial ? (PB)`)

get_cat_values <- function(x) {
  char_var <- names(x)[unlist(sapply(x, function(col) class(col) %in% c("character", "factor")))]
  # remove full_names
  char_var <- char_var[!(char_var %in% c("Nom du patient", "nom"))]

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
    pivot_longer(
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

if (!fs::file_exists(here::here(path_dict, "measles_ll_full_dict_fr.xlsx"))) {
  cat_clean <- get_cat_values(sim_clean_fr)
  cat_raw <- get_cat_values(sim_raw_final_fr)

  na_clean <- get_n_NA(sim_clean_fr)
  na_raw <- get_n_NA(sim_raw_final_fr)

  dict <- data.frame(
    clean_variable_name = names(sim_clean_fr),
    raw_variable_name = names(sim_raw_final_fr),
    clean_variable_type = unlist(sapply(sim_clean_fr, class)),
    raw_variable_type = unlist(sapply(sim_raw_final_fr, class)),
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

  rio::export(dict, here::here(path_dict, "measles_ll_full_dict_fr.xlsx"))
} else {
  ll_dict <- rio::import(here::here(path_dict, "measles_ll_full_dict_fr.xlsx")) |> as_tibble()

  # export a subset of dictionnary for participants

  dict_sub <- ll_dict |>
    filter(type != "to be calculated") |>
    select(
      nom_variable = raw_variable_name,
      type_variable = raw_variable_type,
      description
    )

  rio::export(dict_sub, here::here(path_dict, "rougeole_ll_dict_fr.xlsx"))
}

# Simulated-data
Use this repo to be used to generate simulated datasets for all kind of purposes within Epicentre (training, interviews, case-study ect...)

## Measles Outbreak in Moïssala, Chad

This datasets (~ 5000 cases) simulates an outbreak of Measles across Southern Chad. It is made of a main linelist and a corresponding laboratory dataset. 
The linelist is built so that `vacc_status`, `age_group`, `muac_cat` are associated with a `dead` `outcome`. 

Look at the `.Rmd` document for some ideas of analysis that can be done.

To replicate the data: 

1. `measles_data.R` creates a lists of different paramaters and distributions for a measle outbreak, using the Chad data from the [East Africa Surveillance dashboard](https://apps.epicentre-msf.org/secure/app/east-africa-measles)
2. `gen_linelist.R` uses [`{simulist}`](https://github.com/epiverse-trace/simulist) to generate a realistic linelist for the Measle outbreak. It then attributes symptoms and outcome based on a logistic regression model.
3.`gen_linelist_geo.R` uses the simulated linelist and adds geographic variable (Region, Sub-prefecture, Villages and Health facility) in a way that ressemble a spatially aggregated outbreak.
4. `gen_lab_data.R` uses the *clean simulated linelist* to generate a laboratory datasets of confirmed cases.
5. `make_raw_linelist.R` takes both the *simulated linelist* and the *laboratory data* to add some **dirtiness** and change the columns names. The data are then exported to `.xlsx` and `.csv`. A variable dictionnary summarising clean/raw data names, categorical values and number of missing values is also created and saved in `data/dictionnary/`
6. `linelist_fr_translation` translates both the clean and the raw data to french (variable names and categorical values), and creates the relevant variable dictionnary. 



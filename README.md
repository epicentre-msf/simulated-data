# Fake-data
This repo to be used to generate fake datasets for all kind of purposes

## Moïssala Measles Outbreak

This datasets (~ 5000 cases) simulates an outbreak of Measles across Southern Chad. It is made of a main linelist and a corresponding laboratory dataset. 
The linelist is built so that `vacc_status`, `age_group`, `muac_cat` are associated with a `dead` `outcome`. 

Look at the `.Rmd` document for some ideas of analysis that can be done.

To replicate the data: 

1. `measle_data.R` creates a lists of different paramaters and distributions for a measle outbreak, using the data from the [East Africa Surveillance dashboard](https://apps.epicentre-msf.org/secure/app/east-africa-measles)
2. `gen_linelist.R` uses [`{simulist}`](https://github.com/epiverse-trace/simulist) to generate a realistic linelist for the Measle outbreak. It then attributes symptoms and outcome based on a logistic regression model.
3. `gen_lab_data.R` uses the *clean simulated linelist* to generate a laboratory datasets of confirmed cases
4. `make_raw_linelist.R` takes both the *simulated linelist* and the *laboratory data* to add some **dirtiness** and change the columns names before exporting to `.xlsx`


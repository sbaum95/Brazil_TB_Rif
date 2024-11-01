# Estimating RR-TB burden using routine Xpert MTB/RIF data: An application to Brazil

This repository contains code necessary to produce findings associated with the manuscript "Surveillance for TB drug resistance using routine rapid diagnostic testing data: Methodological development and application in Brazil".  A pre-print version of this manuscript can be found on medrxiv: https://www.medrxiv.org/content/10.1101/2024.07.22.24310845v1 

## Description: 
This project uses routine Xpert MTB/RIF data to estimate levels and trends in Rifampin-resistant Tuberculosis (RR-TB) using a hierarchical generalized additive model in R. We apply this model to Brazil, using patient-level TB data from their National Disease Notification Registry (SINAN). 

## Table of Contents: 
*master_run_all.R* will run all sub-directories to prepare analytic datasets, run models, and produce figures and tables. Each sub-directory contains a *01_run_all.R* that will run components of that directory. Note that if running within sub-directory, you will need to manually set the version for which files to load and save (e.g. file_version_load, file_version_save). Otherwise, these can be set in *master_run_all.R*. 

- **01_data_processing** - *01_run_all.R* cleans SINAN data and prepares analytics datasets for model, by sourcing relevant scripts in folder. 

- **02_run_models** - *05_run_all.R* runs HGAM models and gets the associated fitted values, uncertainty intervals, and compiles modeled and observed estimates. Results are compiled at the national- and state-level by year and by quarter. 

- **03_make_figures_and_tables** - *01_run_all.R* reproduces figures and table in manuscript. *manuscript_calculations.R* produces values for in-text references of results. 


## Packages: 
See *dependencies.R* for packages necessary to run code. 

## Data Sources: 
- Patient-level TB data from Brazil's national Notifiable Diseases Registry, Sistema de Informação de Agravos de Notificação (SINAN)
- Municipality centroid shapefile (IBGE)
- National and state-level population denominators from 2010 Census (IBGE)
- Region associated with each state. 


## Notes: 

# Estimating RR-TB burden using routine Xpert MTB/RIF data: An application to Brazil

This repository contains code necessary to produce findings associated with the manuscript "". 

## Description: 
This project uses routine Xpert MTB/RIF data to estimate levels and trends in Rifampin-resistant Tuberculosis (RR-TB) using a hierarchical generalized additive model in R. We apply this model to Brazil, using patient-level TB data from their National Disease Notification Registry (SINAN). 

## Table of Contents: 
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
# epivet
Mixed scripts and functions for veterinary epidemiology

This repository is meant to gather code and example to be used in veterinary epidemiology.
The starting example is a work in dairy cow feet and legs disorders.

Topics covered include:
- calculation of time-at-risk ("cow-years")
- estimation of prevalence and incidence
- comparison of different diagnostic approaches

### operative guidelines

0. [extract_from_json.r](support_scripts/): read source json file (from vet app) and save it to `.RData` objects for further analysis:
    - `extracted_visite.RData` (vet visits)
    - `extracted_bovine.RData` (cows)
    - `extracted_status_bovine.RData` (cow records, i.e. milk, calvings etc.)
1. `get_at_risk_population.r`: script to subset the data based on the selected year of reference

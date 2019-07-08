Revisions: June 26

# WORK IN PROGRESS

# religion_lgbtq

Replication material for "African, Religious, and Tolerant? How Religious Diversity Shapes Attitudes Towards Sexual Minorities in Africa" (article under review)

## Authors
Sarah Dreier, James Long, and Stephen Winkler (authorship listed alphabetically) 

## Afrobarometer Data
We use the merged Afrobarometer Round 6 data, which is publicly available at: http://afrobarometer.org/data/merged-data. Follow the source R scripts (below) to download directly from the Afrobarometer website. 

## Build Scripts
- [1_create_herf.R](source/1_create_herf.R) reads in the Afrobarometer data from its web source [Afrobarometer.org](http://afrobarometer.org/data/merged-data), calculates Herfindahl scores for key "diversity" variables, saves the new dataset ("data") as: `data/afrob_with_herf.RData`, and produces statistics for Tables A.4 and A.5.

- [2_clean_afrobarometer.R](source/2_clean_afrobarometer.R) transforms and cleans variables used in the analysis, subsets those variables into a new dataset ("data"), and saves it as: `data/clean_afrobarometer.RData`.

## Analysis and Outputs
- [3_descriptive_stats.R](source/3_descriptive_stats.R) generates descriptive plots/tables:
    - Figure 1: Percent of respondents who dislike LGBTQ by country (DV)
    - Figure 2a & 2b: Distribution of dependent variable
    - Figure 3a & 3b: Distribution of independent variable
    - Table A.2: Spearman correlations of tolerance variables 
    - Table A.3: Descriptive statistics for model covariates
    
- [4_main_models.R](source/4_main_models.R) generates tables for main models and various replications of main models:
    - Table 1: Main OLS results
    - Table A.6: Main logit results
    - Table A.7: Main ordered probit results
    - Table A.8: Main OLS results with country coefficients
    - Table A.9: Replication of main OLS results with unbinned HHI of religion 
    - Table A.10: Replication of main logit results with unbinned HHI of religion
    - Table A.11: Replication of main OLS results with control for religious affiliation
    - Table A.12: Replication of main logit results with control for religious affiliation
    - Table A.22: Replication of main OLS results with religion HHI at region level rather than district
    - Table A.23: Replication of main logit results with religion HHI at region level rather than district
    
- [5_model_plots.R](source/5_model_plots.R) generates figures representing simulated probability of tolerating or opposing LGBTs based on respondent's district herfindahl score. Code requires functions from [multiplot_code_lc.R](source/multiplot_code_lc.R), which was developed by [Loren Collingwood](https://www.collingwoodresearch.com/). Figures include:
    - Figure 4: Main model (3) results, probability of tolerating LGBTs
    - Figure 5a & 5b: Main model (3, ordered) results, probability of strongly opposing or tolerating LGBTs
    - Table A.24: Main model (3) results, subsetted to districts with specific religious identities present
    - Figure A.24: Main model (3) results, subsetted to districts with specific religious identities present
    - Figure A.25: Main model (3) results among each distinct country surveyed by Afrobarometer. 

- [robust_ethnicity.R](source/robust_ethnicity.R) runs robustness checks:
    - Table A.13: Replace religious HHI with ethnicity HHI (OLS)
    - Table A.14: Replace religious HHI with ethnicity HHI (logit)
    - Table A.15: Effect of religious HHI on other social out-groups (OLS)
    - Table A.16: Effect of religious HHI on other social out-groups (logit)

- [robust_district_tolerance_control.R](source/robust_district_tolerance_control.R) creates district-level average of non-lgbt tolerance, runs main models with this control:
    - Table A.17: Replication of main OLS model with control for district-level social tolerance
    - Table A.18: Replication of main logit model with control for district-level social tolerance
    
- [robust_control_media.R](source/robust_control_media.R) replicates main models with controls for individual level media consumption:
    - Table A.20: Replication of main OLS model with control for media consumption
    - Table A.21: Replication of main logit model with control for media consumption
    
- [robust_region_fe.R](source/robust_region_fe.R) replicates main models with region and country fixed effects, creates:
    - Table A.19: Replication of main OLS model with region fixed effect

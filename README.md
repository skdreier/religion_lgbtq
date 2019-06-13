Revisions: June 12

# WORK IN PROGRESS

# religion_lgbtq

Replication material for "African, Religious, and Tolerant? How Religious Diversity Shapes Attitudes Towards Sexual Minorities in Africa" (article under review)

## Authors
Sarah Dreier, James Long, and Stephen Winkler. 

## Afrobarometer Data
We use the merged Afrobaormeter Round 6 data, which is publicly available at: http://afrobarometer.org/data/merged-data. Follow the source R scripts (below) to download directly from the Afrob website. Alternatively, download this dataset and save it in a folder within your directory. Make sure the downloaded dataset is named: `merged_r6_data_2016_36countries2.sav`.  

## Build Scripts
- `source/create_herf.R` reads in the Afrobarometer data from its web source [Afrobarometer.org](http://afrobarometer.org/data/merged-data), calculates Herfindahl scores for key "diversity" variables, saves the new dataset ("data") as: `data/afrob_with_herf.RData`, and produces statistics for Tables A.4 and A.5.

- `source/clean_afrobarometer.R` transforms and cleans variables used in the analysis, subsets those variables into a new dataset ("data"), and saves it as: `data/clean_afrobarometer.RData`.

## Analysis and Outputs
- `source/descriptive_stats.R` generates descriptive plots/tables:
    - Figure 1: Percent of respondents who dislike LGBTQ by country (DV)
    - Figure 2a & 2b: Distribution of dependent variable
    - Figure 3a & 3b: Distribution of independent variable
    - Table A.2: Spearman correlations of tolerance variables 
    - Table A.3: Descriptive statistics for model covariates **SW: This code doesn't run for me?**
    
- `source/main_models.R` generates tables for main models and various replications of main models:
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
*NOTE to STEPHEN*: Keep the chunk of code at end of this script here. I think it creates our main plots, but I can't clean it until we figure out our extractdata issue.

- `source/robust_ethnicity.R` runs robustness checks:
    - Table A.13: Replace religious HHI with ethnicity HHI (OLS)
    - Table A.14: Replace religious HHI with ethnicity HHI (logit)
    - Table A.15: Effect of religious HHI on other social out-groups (OLS)
    - Table A.16: Effect of religious HHI on other social out-groups (logit)
*NOTE to SARAH*: there is a chunk of code at end of this script (line 265) that I think can be deleted, but wanted to
check with you first to make sure it's not used to create tables A.24 and A.25. See my note in script for more. 

- `source/robust_district_tolerance_contorl.R` creates district-level average of non-lgbt tolerance, runs main models with this control:
    - Table A.17: Replication of main OLS model with control for district-level social tolerance
    - Table A.18: Replication of main logit model with control for district-level social tolerance
    
- `source/robust_control_media.R` replicates main models with controls for individual level media consumption:
    - Table A.20: Replication of main OLS model with control for media consumption
    - Table A.21: Replication of main logit model with control for media consumption
    
- `source/robust_region_fe.R` replicates main models with region and country fixed effects, creates:
    - Table A.19: Replication of main OLS model with region fixed effect

## NOTES REGARDING OLDER DOCUMENTS

### SCRIPTS WE MIGHT BE ABLE TO DELTE?
These are scripts that I think we might be able to delete from main repo. We shouldn't delete yet until we can confirm that everything replicates, but can you look at the ones with your name by it to see if you think we use it for anything? 

- Muslim Detail folder?: SARAH, do you remember what this folder was for? I can't recall.
- create_dhs_herf.R: SARAH, do you know if we use this? I think not.
- multiplot_code_lc.R: SARAH, do you know if we use this? Looks like it might be a dependency script for some function?
- post-apsr-robustness.R: SARAH, do you know if we use this? 
- post_CPS_Model_tests.R: STEPHEN will see if we use any tables from this in the paper. 
- robustness_relig_div*relig_tol.R STEPHEN will seee if we use any tables from this in paper. 

# old data
WE CAN DELETE THIS WHEN WE'RE SURE WE DON'T NEED IT
`afro_herf.csv` is the Afrobarometer dataset with new variables for Herf index
`afro_herf_subset.csv` is a subset of Afrobarometer data used in our models, plus Herf index
`afro_herf_subset2.csv` adds Herf index for binned religion variables 

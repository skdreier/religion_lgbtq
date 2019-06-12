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
*NOTE to SARAH*: there is a chunk of code at end of this script (starting around line 680) that creates plots.
I wasn't sure if we still need it for any of the plots in the paper. If not, let me know and I'll delete it. 

    
    

`source/robustness.R` runs robustness checks, creates:
    - STEPHEN WILL FILL IN

`source/robust_district_tolerance_contorl.R` creates district-level average of non-lgbt tolerance, runs main models with this control, creates:
    - STEPHEN WILL FILL IN
    
`source/robust_control_media.R` STEPHEN will fill in
    - STEPHEN WILL FILL IN

`source/plot_lgbt_country_Nov2017.R` SARAH fill in what this does
    - SARAH list what figure/table this outputs

`source/plots_SD_0817.R` SARAH fill in what this does
    - SARAH list what figures/table this outputs
    

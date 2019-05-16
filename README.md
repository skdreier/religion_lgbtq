# WORK IN PROGRESS

# religion_lgbtq
This repository will contain replication materials for a working paper ("African, Religious, and Tolerant: How religious diversity shapes attitudes toward sexual minorities.")


Replication material for "African, Religious, and Tolerant? How Religious Diversity Shapes Attitudes Towards Sexual Minorities in Africa" (article under review)

## Authors
Sarah Dreier, James Long, and Stephen Winkler. 

## Afrobarometer Data
We use the merged Afrobaormeter Round 6 data, which is publicly available at: http://afrobarometer.org/data/merged-data.
Download this dataset and save it in a folder called `data-raw` within your directory. Make sure the downloaded dataset is named: `merged_r6_data_2016_36countries2.sav`.  OR follow the scripts (below) to download directly from the Afrob website.s

## Build Scripts
- `source/create_herf.R` reads in the Afrobarometer data from its web source [Afrobarometer.org](http://afrobarometer.org/data/merged-data), creates Herfindahl variables, saves new dataset as: `data/afrob_with_herf.RData`.

- `source/clean_afrobarometer.R` transforms and cleans variables used in the analysis. Subsets those variables into a new dataset and saves it as: `data/clean_afrobarometer.csv`. (STEPHEN will clean this code)

## Analysis and Outputs
- `source/descriptive_stats.R` generates descriptive plots/tables:
    - Figure 2a & 2b: Distribution of dependent variable
    - Figure 3a & 3b: Distribution of independent variables
    - Table A.2: Spearman correlations of tolerance variables 
    - STEPHEN WILL ADD CODE FROM `source/descriptive_stats_table.R` into this script to condense 

`source/main_models.R` runs main models, creates:
    - STEPHEN WILL FILL IN

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
    

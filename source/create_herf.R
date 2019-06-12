###############################################
# Replication Code for:                       #
# Long, Dreier, Winkler (P&R)                 #
#                                             #
# CODE TO CREATE HERFINDAHL SCORES            #
# AFROBAORMETER DATA                          #
#                                             #
# Code lead: S. Dreier                        #
#                                             #
#  R version 3.6.0 (2019-04-26)               #
# DATE: 06/12/2019                            #
###############################################

# Content:
# - Load raw Afrobarometer data and drop countries where homosexuality attitudes are not surveyed
# - For herfindahl variables: relable missing/don't know/refused as "NA"
# - Recode and bin religious identities
# - Write/run Herfindahl function
# - Merge back to all Afro data

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################

# Function to check and install CRAN packages 
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Check to see if packages are installed, and then load them
packages<-c("Hmisc", "stringr", "DescTools", "dplyr")

# Load packages
suppressWarnings( check.packages(packages) )

##########################################
### LOAD AFROBAROMETER ROUND 6 DATASET ###
##########################################

suppressWarnings(
afro6 <- spss.get("http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav", use.value.labels=FALSE)
)

#####################################################################
### DROP COUNTRIES WHERE HOMOSEXUALITY ATTITUDES ARE NOT SURVEYED ###
### (Algeria, Egypt, Sudan)                                       ###
#####################################################################

afro <- subset(afro6, Q89C != 99) #99: "not asked in this country"

##############################################################
### CLEAN AND RECODE VARIABLES FOR HERFINDAHL CALCULATIONS ###
##############################################################

# Ethnicity
  afro$ethn <- afro$Q87 # Create new variable
  afro$ethn[afro$Q87 == -1 | afro$Q87 >= 9998] <- NA # 9998 (Refused), 9999 (DK), -1 (Missing) 

# Political ideology 
  afro$pol <- afro$Q90B # Create new variable
  afro$pol[afro$Q90B==-1 | afro$Q90B>9997] <- NA # 9998 (Refused), 9999 (DK), -1 (Missing) [9997 (NA) not removed]

# Religion
  afro$relig <- afro$Q98A # Create new variable
  afro$relig[afro$Q98A == -1 | afro$Q98A >= 9998] <- NA # 9998 (Refused), 9999 (DK), -1 (Missing)

# Religion Binned
  afro$relig_bin <- afro$relig
  
  # 0: No religion (none, agnostic, atheist)
  afro$relig_bin[afro$relig == 0 | afro$relig == 28 | afro$relig == 29 ] <- 0
  
  # 1: Christian misc (general, Orthodox, Coptic)
  afro$relig_bin[afro$relig == 1 | afro$relig == 3 | afro$relig == 4 ]   <- 1 
  
  # 2: Catholic
  afro$relig_bin[afro$relig == 2 ] <- 2 
  
  # 3: Anglican
  afro$relig_bin[afro$relig == 5 ] <- 3
  
  # 4: Protestant mainline/non-indep (Lutheran, Methodist, Presbyterian, Baptist, Quaker, Mennonite, 7th Adventist)
  afro$relig_bin[afro$relig > 5 & afro$relig <= 11 | afro$relig == 16] <- 4
  
  # 5: Renewalist (Evangelical, Pentecostal, Independent/AIC)
  afro$relig_bin[afro$relig == 12 | afro$relig == 13 | afro$relig == 14] <- 5 # Renewalist/Pentecostal
  
  # 6: Muslim (all)
  afro$relig_bin[afro$relig > 17 & afro$relig <= 24] <- 6
  
  # 7: Traditional / ethnic religion
  afro$relig_bin[afro$relig == 25 ] <- 7
  
  # 8: Hindu or Bahai
  afro$relig_bin[afro$relig == 26 | afro$relig == 27] <- 8
  
  # 9: Misc / other (Jehovah's Witness, Mormon, Other, and original codes 30-1260)
  afro$relig_bin[afro$relig == 15 | afro$relig == 17 | afro$relig == 9995 |
                 afro$relig > 29 & afro$relig <= 1260] <- 9

# Religion Binned: Muslim (2), Christian (1), Neither (0)
  afro$christ.mus <- afro$relig_bin 
  afro$christ.mus[afro$relig_bin==0 | afro$relig_bin > 6] <- 0 # neither
  afro$christ.mus[afro$relig_bin > 0 & afro$relig_bin < 6 ] <- 1 # Christian
  afro$christ.mus[afro$relig_bin==6] <- 2 # Muslim

# Christian binary
  afro$christian <- afro$christ.mus
  afro$christian[afro$christ.mus==2] <- 0

# Muslim binary
  afro$muslim <- afro$christ.mus
  afro$muslim[afro$christ.mus==1] <- 0
  afro$muslim[afro$christ.mus==2] <- 1

############################################
### PREPARE DATA FOR HERFINDAHL FUNCTION ###
############################################

# Make variables to split by region / county
afro$COUNTY <- afro$LOCATION.LEVEL.1 # rename county variable
afro$ctry_reg_cty <- paste(afro$COUNTRY, afro$REGION, afro$COUNTY, sep = "_") # create unique ID for each county
afro$ctry_reg_cty <- str_trim(afro$ctry_reg_cty) # remove dead space
afro$ctry_reg <- paste(afro$COUNTRY, afro$REGION, sep = "_") # create unique ID for each region

# Sort data by country (numeric), region (numeric) and county (alphabetical)
afro <- afro[order(afro$COUNTRY, afro$ctry_reg_cty),]

# Count unique countries, regions, and counties
length(unique(afro$COUNTRY)) # 33 unique countries
length(unique(afro$ctry_reg)) # 418 unique regions
length(unique(afro$ctry_reg_cty)) # 2095 unique counties

# Create list which splits regions or counties into their own dataframe
county <- split(afro, afro$ctry_reg_cty) # List split by county (2095 dataframes)
region <- split(afro, afro$ctry_reg) # List split by region (418 dataframes)

##################################
### DEFINE HERFINDAHL FUNCTION ###
##################################

herf <- function(data){
  
  # Ethnicity
    ethn <- data$ethn # assign ethnicity
    ethn_tab <- table(ethn) # count of each unique ethnicity in a given unit (region/county)
    herf_ethn <- DescTools::Herfindahl(ethn_tab) # calculate unit's ethnicity herf; herf only needs counts per category
    
    max_ethn <- which.max(ethn_tab)
    maxes_ethn <- as.numeric(names(max_ethn))
    maj_ethn <- ifelse(ethn == maxes_ethn, 1, 0) # indicate respondents who are an ethnic majority
  
  # Political ideology
    pol <- data$pol #assign political ideology
    pol_tab <- table(pol) # count of each unique pol ideology in a given unit
    herf_pol <- DescTools::Herfindahl( pol_tab ) # calculate unit's pol ideology herf
    
    max_pol <- which.max(pol_tab)
    maxes_pol <- as.numeric(names(max_pol))
    maj_pol <- ifelse(pol == maxes_pol, 1, 0) # indicate respondents who are a political majority 
    
  # Religion
    relig <- data$relig # assign religion
    relig_tab <- table(relig) # count of each unique relig in a given unit
    herf_relig <- DescTools::Herfindahl(relig_tab)  # calculate unit's relig herf
    
    max_relig <- which.max(relig_tab)
    maxes_relig <- as.numeric(names(max_relig))
    maj_relig <- ifelse(relig == maxes_relig, 1, 0) # indicate respondents who are a religious majority
    
  # Religion binned  
    relig_bin <- data$relig_bin # assign religion bin
    relig_bin_tab <- table(relig_bin) # count each unique binned religion in a given unit
    herf_relig_bin <- DescTools::Herfindahl(relig_bin_tab) # calculate unit's relig bin herf
    
    max_relig <- which.max(relig_bin_tab)
    maxes_relig <- as.numeric(names(max_relig))
    maj_relig_bin <- ifelse(relig_bin == maxes_relig, 1, 0) # indicate respondents who are a religious majority
    
  # Religion binned: Muslim/Christian
    christ_mus  <- data$christ.mus # assing religion bin christ/mus
    christ_mus_tab <- table(data$christ_mus)
    herf_christ_mus <- DescTools::Herfindahl(christ_mus_tab)
    
  RESPNO <- data$RESPNO # resondent number
  size_sample <- dim(data)[1] # count size of respondnets in unit
  
  return(data.frame(ethn, maj_ethn, herf_ethn, relig, maj_relig, herf_relig, pol, maj_pol, herf_pol, 
                    relig_bin, maj_relig_bin, herf_relig_bin, christ_mus, herf_christ_mus, 
                    RESPNO, size_sample)) 
}

#################################################
### CALCULATE HERFINDAHL BY COUNTY AND REGION ###
#################################################

out_reg <- lapply(region, herf) # Apply herfindahl function to region list
suppressWarnings( out_reg <- bind_rows(out_reg) ) # return region list to a single dataframe
names(out_reg) <- c("ethn", "maj_ethn_reg", "herf_ethn_reg", 
                    "relig", "maj_relig_reg", "herf_relig_reg", 
                    "polparty", "maj_pol_reg", "herf_pol_reg", 
                    "relig_bin", "maj_relig_bin_reg", "herf_relig_bin_reg", 
                    "christ_mus", #"maj_christ_mus_reg", 
                    "herf_christ_mus_reg", 
                    "RESPNO", "size_sample_reg") # define column names

out_cty <- lapply(county, herf) # Apply herfindahl function to county list
suppressWarnings( out_cty <- bind_rows(out_cty) ) # return county list to a single dataframe
names(out_cty) <- c("ethn", "maj_ethn_cty", "herf_ethn_cty", 
                    "relig", "maj_relig_cty", "herf_relig_cty",  
                    "polparty", "maj_pol_cty", "herf_pol_cty", 
                    "relig_bin", "maj_relig_bin_cty", "herf_relig_bin_cty", 
                    "christ_mus", #"maj_christ_mus_cty", 
                    "herf_christ_mus_cty", 
                    "RESPNO", "size_sample_cty") # define columns names

##############################################
### MERGE AND SAVE UPDATED DATASET LOCALLY ###
##############################################

data <- merge(afro, out_reg, by.x.y="RESPNO") # merge region with original dataset
data <- merge(data, out_cty, by.x.y="RESPNO") # merge county with original/region dataset

save(data, file = "data/afrob_with_herf.RData")

#TEST TEST
######################################################
########       END OF HERFINDAHL SCRIPT       ########
######################################################
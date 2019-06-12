###############################################
# Replication Code for:                       #
# Long, Dreier, Winkler (P&R)                 #
#                                             #
# CODE TO CLEAN & TRANSFORM AFROBAROMETER     #
# AFROBAORMETER DATA                          #
#                                             #
# Code lead: S. Winkler                       #
#                                             #
#  R version 3.6.0 (2019-04-26)               #
# DATE: 06/12/2019                            #
###############################################

# Content:
# - Load Afrobarometer dataset with added Herfindahl scores
# - Clean and transform variables used in analysis
# - Subset variables used in analysis into a new dataset
# - Save that subsetted dataset

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################

library(magrittr) #pipe function
library(dplyr) #transformations
library(forcats) #factor recoding

##################################
### LOAD AFROBAROMETER DATA W/ ###  
### HERFINDAHL SCORES ############
##################################

load(file="data/afrob_with_herf.RData")  # saved as "data"
afro <- data

#################################
### CLEAN/TRANSFORM VARIABLES ###
#################################

# Tolerance of Homosexuality (ordinal & logical)
afro %<>%
  mutate(sexuality = Q89C) # rename the ordinal version to something meaningful
afro$sexuality[afro$sexuality==9] <- NA # drop don't know (n = 710)
afro$sexuality[afro$sexuality==-1] <- NA # drop missing (n = 33)

afro %<>%
  dplyr::mutate(sexuality2 = as.factor(sexuality), # create a logical version of the variable
                sexuality2 = fct_recode(sexuality2,
                                        "0" = "1", # strongly dislike 
                                        "0" = "2", # and somewhat dislike are set to 0
                                        "1" = "3", # would not care
                                        "1" = "4", # somewhat like
                                        "1" = "5")) # and strongly like are set to 1

afro$tol_sex2 <- as.numeric(afro$sexuality2 ) -1 # Replicate this with a name that matches other tolerance variables
                                                 # this is used later in this clean script to create aggregate variables. 

# Religious Tolerance (ordinal and logical)
afro %<>%
  mutate(tol_relig = Q89A) # create meaningful name
afro$tol_relig[afro$tol_relig==9 | afro$tol_relig==-1] <- NA #drop don't know or missing (n = 232 + 24)

afro %<>%
  dplyr::mutate(tol_relig2 = as.factor(tol_relig), # create a logical version of the variable
                tol_relig2 = fct_recode(tol_relig2,
                                        "0" = "1",
                                        "0" = "2",
                                        "1" = "3",
                                        "1" = "4",
                                        "1" = "5"))
afro$tol_relig2 <- as.numeric(afro$tol_relig2 ) -1 #convert to numeric

# Ethnic Tolerance (ordinal and logical)
afro %<>%
  mutate(tol_ethnic = Q89B) # create meaningful name
afro$tol_ethnic[afro$tol_ethnic==9 | afro$tol_ethnic==-1] <- NA #drop don't know or missing (n = 234 + 34)

afro %<>%
  dplyr::mutate(tol_ethnic2 = as.factor(tol_ethnic), # create logical version of variable
                tol_ethnic2 = fct_recode(tol_ethnic2,
                                         "0" = "1",
                                         "0" = "2",
                                         "1" = "3",
                                         "1" = "4",
                                         "1" = "5"))
afro$tol_ethnic2 <- as.numeric(afro$tol_ethnic2 ) -1 #convert to numeric

# HIV/AIDS Tolerance (ordinal and logical)
afro %<>%
  mutate(tol_hiv = Q89D) # create meaningful name
afro$tol_hiv[afro$tol_hiv==9 | afro$tol_hiv==-1] <- NA #drop don't know or missing (n = 405 + 40)
afro %<>%
  dplyr::mutate(tol_hiv2 = as.factor(tol_hiv), #create logical version of the variable 
                tol_hiv2 = fct_recode(tol_hiv2,
                                      "0" = "1",
                                      "0" = "2",
                                      "1" = "3",
                                      "1" = "4",
                                      "1" = "5"))
afro$tol_hiv2 <- as.numeric( afro$tol_hiv2 ) -1

# Immigrant or foreign worker Tolerance (ordinal and logical)
afro %<>%
  mutate(tol_immig = Q89E) #create meaningful name 
afro$tol_immig[afro$tol_immig==9  | afro$tol_immig==-1] <- NA #drop don't know or missing (n = 428 + 45)
afro %<>%
  dplyr::mutate(tol_immig2 = as.factor(tol_immig), # create logical version of the variable
                tol_immig2 = fct_recode(tol_immig2,
                                        "0" = "1",
                                        "0" = "2",
                                        "1" = "3",
                                        "1" = "4",
                                        "1" = "5"))
afro$tol_immig2 <- as.numeric(afro$tol_immig2 ) -1 # convert to numeric 

# Tolerance aggregates
# Create a variable that aggregates an individual's tolerance of all social outgroups
# Then, for each out-group, create an aggregate for all other social outgroups. 
# This is used when we run placebo tests in the analysis and need to control for tolerance of all other outgroups.
afro$tol_all <- afro$tol_relig2 + afro$tol_ethnic2 + afro$tol_sex2 + afro$tol_hiv2 + afro$tol_immig2
afro$tol_noLGBT <- afro$tol_relig2 + afro$tol_ethnic2 + afro$tol_hiv2 + afro$tol_immig2
afro$tol_noRelig <- afro$tol_ethnic2 + afro$tol_sex2 + afro$tol_hiv2 + afro$tol_immig2
afro$tol_noEthnic <- afro$tol_relig2 + afro$tol_sex2 + afro$tol_hiv2 + afro$tol_immig2
afro$tol_noHiv <- afro$tol_relig2 + afro$tol_ethnic2 + afro$tol_sex2 + afro$tol_immig2
afro$tol_noImmig <- afro$tol_relig2 + afro$tol_ethnic2 + afro$tol_sex2 + afro$tol_hiv2

# Age
afro %<>%
  mutate(age = as.numeric(Q1))
afro$age[afro$age=="-1"] <- NA #drop missing (62)
afro$age[afro$age=="998"] <- NA #drop resfused (25)
afro$age[afro$age=="999"] <- NA #drop don't know (199)

# Gender
afro %<>%
  mutate(female = Q101 %in% 2)

# Education
afro %<>%
  mutate(education = Q97) # give it a meaningful name 
afro$education[afro$education=="-1"] <- NA #drop missing (n=43)
afro$education[afro$education=="99"] <- NA #drop don't know (n=86)
afro$education <- as.factor(afro$education)

# News from Radio
afro %<>%
  mutate(radio = Q12A)
afro$radio[afro$radio=="-1"] <- NA #missing (n=33)
afro$radio[afro$radio=="9"] <- NA #don't know (n=757)
afro$radio <- as.factor(afro$radio)

# News from TV
afro %<>%
  mutate(tv = Q12B)
afro$tv[afro$tv=="-1"] <- NA #missing (n=33)
afro$tv[afro$tv=="9"] <- NA #don't know (n=757)
afro$tv <- as.factor(afro$tv)

# News from Newspaper
afro %<>%
  mutate(newspaper = Q12C)
afro$newspaper[afro$newspaper=="-1"] <- NA #missing (n=33)
afro$newspaper[afro$newspaper=="9"] <- NA #don't know (n=757)
afro$newspaper <- as.factor(afro$newspaper)

# News from Internet
afro %<>%
  mutate(internet = Q12D)
afro$internet[afro$internet=="-1"] <- NA #missing (n=33)
afro$internet[afro$internet=="9"] <- NA #don't know (n=757)
afro$internet <- as.factor(afro$internet)

# Urban
afro %<>%
  mutate(urban = URBRUR) # give meaningful name
afro$urban[afro$urban=="3"] <- .5 # transform peri-urban so it works within numerical order (n=512, all Botswana)
afro$urban[afro$urban=="460"] <- NA # drop missing (n=88)
afro$urban[afro$urban=="1"] <- 1 #Urban
afro$urban[afro$urban=="2"] <- 0 #Rural

# Wealth
afro %<>%
  mutate(water_access = Q93A)
afro$water_access[afro$water_access=="-1"] <- NA #missing (n=61)
afro$water_access[afro$water_access=="1"] <- 1 #inside house
afro$water_access[afro$water_access=="2"] <- 1 #inside compound
afro$water_access[afro$water_access=="3"] <- 0 #outside compound
afro$water_access[afro$water_access=="9"] <- NA #don't know (n=148)
afro$water_access[afro$water_access=="98"] <- NA #refused (n=3)

# Religiosity
afro %<>%
  mutate(religiosity = Q98B)
afro$religiosity[afro$religiosity=="-1"] <- NA #missing (n=577)
afro$religiosity[afro$religiosity=="7"] <- 0 #has no religion
afro$religiosity[afro$religiosity=="9"] <- NA #don't know (n=293)
afro$religiosity[afro$religiosity=="98"] <- NA #(n=1)

# rename county to district, which is less likely to be confused with Country
afro <- dplyr::rename(afro, herf_relig_dist=herf_relig_cty)
afro <- dplyr::rename(afro, herf_relig_bin_dist=herf_relig_bin_cty)
afro <- dplyr::rename(afro, herf_ethn_dist=herf_ethn_cty)
afro <- dplyr::rename(afro, herf_pol_dist=herf_pol_cty)
afro <- dplyr::rename(afro, maj_relig_dist=maj_relig_cty)
afro <- dplyr::rename(afro, maj_relig_bin_dist=maj_relig_bin_cty)
afro <- dplyr::rename(afro, maj_ethn_dist=maj_ethn_cty)
afro <- dplyr::rename(afro, maj_pol_dist=maj_pol_cty)
afro <- dplyr::rename(afro, sample_size_dist=size_sample_cty)
afro <- dplyr::rename(afro, sample_size_reg=size_sample_reg)
afro <- dplyr::rename(afro, DISTRICT=COUNTY)

# Subset data to only what we need for analysis
data <-
  dplyr::select(afro, RESPNO, ethn, relig, relig_bin, COUNTRY, URBRUR, REGION, DISTRICT,
                herf_ethn_reg, herf_ethn_dist, maj_ethn_reg, maj_ethn_dist,
                herf_relig_reg, herf_relig_dist, maj_relig_reg, maj_relig_dist,
                herf_relig_bin_reg, herf_relig_bin_dist, maj_relig_bin_reg, maj_relig_bin_dist,
                herf_pol_reg, herf_pol_dist, maj_pol_reg, maj_pol_dist,
                sexuality, sexuality2, age, female, education, urban,
                water_access, religiosity, radio, tv, newspaper, internet, relig_bin,
                christian, muslim, christ.mus,
                tol_relig, tol_ethnic, sexuality, tol_hiv, tol_immig,
                tol_relig2, tol_ethnic2, tol_sex2, tol_hiv2, tol_immig2,
                tol_all, tol_noRelig, tol_noEthnic, tol_noLGBT,
                tol_noHiv, tol_noImmig, sample_size_dist, sample_size_reg
  )

###############################
###  SAVE SUBSETTED DATASET ###
###############################

save(data, file = "data/clean_afrobarometer.RData")

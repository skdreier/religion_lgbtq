###############################################
# Replication Code for:                       
# Long, Dreier, Winkler (P&R)                 
#           
# Code to replicate main models with a control
# for district-level social tolerance. 
# 
# OUTPUTS:
#   - Table A.17
#   - Table A.18

# AFROBAORMETER DATA                          
# R version 3.6.0 (2019-04-26)                
#                                             
# DATE: 06/17/2019                            
###############################################

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################

library(magrittr) #pipe function
library(dplyr) #transformations
library(multiwayvcov) #for clustered std err
library(lmtest) #coeftest for clustered std err
library(stargazer)
library(tidyr)

library(devtools) #simcf
#install_github("chrisadolph/simcf") # github source is compatible w recent R versions
library(simcf) #extractdata


############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

#####################################################
### CREATE DISTRICT-LEVEL AVERAGE OF             ###
### SOCIAL TOLERANCE (excluding lgbt tolerance)  ###
####################################################

# We already have a variable that is individual-level measure of all tolerance except LGBTs
# It's called 'tol_noLGBT'. To create this, we first turned all individual level tolerance questions into
# a binary variable to match how we measure our main DV (LGBT tol). So, each tolerance is either 0 or 1.
# Then, we combined all of these to create an index from 0 to 4 where 0 = not tolerant of any outgroups
# and 4 = tolerant of all outgroups (relig, ethnic, immigrant, HIV+)

# Here is a slice of that data to see what it looks like:
head(dplyr::select(myData, RESPNO, DISTRICT, tol_relig2, tol_ethnic2, tol_hiv2, tol_immig2, tol_noLGBT))

# Drop any persons who have tol_noLGBT = NA
# if we don't do this, we get a large amount of missingness in the district-level average of social tolerance
# because it will drop any district that has at least one person with tol_noLGBT = NA
myData <- myData %>% drop_na(tol_noLGBT)

# Now, create new variable that takes the average of `tol_noLGBT` by District
myData <- myData %>%
  group_by(DISTRICT) %>%
  mutate(tol_noLGBT_district = mean(tol_noLGBT))

# Create another variable that standardizes this on 0,1 scale
range01 <- function(x){(x-min(x))/(max(x)-min(x))} #function to standardize on 0,1
myData$tol_noLGBT_district_rescaled <- range01(myData$tol_noLGBT_district)

############################################### 
### REPLICATE MAIN MODELS WITH CONTROL FOR  ###
### DISTRICT-LEVEL AVERAGE SOCIAL TOLERANCE ### 
### TABLES A.17 and A.18                    ###
###############################################

# Model 1: Religious Herf 
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + 
  christian + muslim + 
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  tol_noLGBT_district + as.factor(ctry) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.1 <- lm(model, data = mdata)
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.1 <- glm(model, family=binomial, data=mdata) 
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 2: Majority Religion
model <- 
  sexuality2 ~ 
  maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  tol_noLGBT_district +
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.2 <- lm(model, data = mdata)
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata) 
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 


# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 +  
  tol_noLGBT_district + as.factor(ctry) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.3 <- lm(model, data = mdata) 
lm.result.3 <- coeftest(lm.result.3, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.3 <- glm(model, family=binomial, data=mdata)  
logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 4: Religious Herf + Maj Religion + Ethnic Herf + Maj Ethnic
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  tol_noLGBT_district + as.factor(ctry) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.5 <- lm(model, data = mdata) #save OLS result
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result.5 <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# create meaningful labels for the table
main.vars <- c("Religion HHI (district)", "Majority religion", "Ethnicity HHI", "Majority ethnicity",
               "Christian", "Muslim", "Female", "Age", "Education",
               "Water access", "Urban", "Religiosity", "Access to internet",
               "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance", "Immigrant tolerance",
               "District social tolerance"
)

# create latex table of ols result: Table A.17
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "ols_district_tolerance_control",
          keep.stat = c("rsq", "n"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (OLS) Addressing Selection into Tolerant Districts",
          omit= c("COUNTRY", "Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# create latex table of logit result: Table A.18
stargazer(logit.result.1, logit.result.2, logit.result.3,logit.result.5,
          no.space=TRUE, 
          label = "logit_district_tolerance_control",
          keep.stat = c("n", "aic"),
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (Logit) Addressing Selection into Tolerant Districts",
          omit= c("COUNTRY", "Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)



###########################################################################
########       END OF ROBUSTNESS (DISTRICT TOLERANCE) SCRIPT       ########
###########################################################################

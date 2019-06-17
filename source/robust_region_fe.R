###############################################
# Replication Code for:                       
# Long, Dreier, Winkler (P&R)                 
#                     
# Code to add region fixed effect to main models.
# 
# OUTPUTS:
#   - Table A.19

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

library(devtools) #simcf
#install_github("chrisadolph/simcf") # github source is compatible w recent R versions
library(simcf) #extractdata

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

##################################
### REPLICATE MAIN MODELS WITH ###
### A REGION FIXED EFFECT      ###
### TABLE: A.19                ###
##################################

# Model 1: Religious Herf 
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + 
  christian + muslim + 
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(REGION) # Add Region FE

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.1 <- lm(model, data = mdata)
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.1 <- glm(model, family=binomial, data=mdata )  
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 2: Majority Religion
model <- 
  sexuality2 ~ 
  maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(REGION) # Add Region FE

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
  as.factor(ctry) + as.factor(REGION) # Add Region FE

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.3 <- lm(model, data = mdata) 
lm.result.3 <- coeftest(lm.result.3, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.3 <- glm(model, family=binomial, data=mdata)  
logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 5: Religious Herf + Maj Religion + Ethnic Herf + Maj Ethnic
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(REGION) # Add Region FE

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.5 <- lm(model, data = mdata) 
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.5 <- glm(model, family=binomial, data=mdata) 
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# create labels for table
main.vars <- c("Religion HHI (district)", "Majority religion", "Ethnicity HHI", "Majority ethnicity",
               "Christian", "Muslim", "Female", "Age", "Education",
               "Water access", "Urban", "Religiosity", "Access to internet",
               "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance", "Immigrant tolerance"
)

# create latex table: Table A.19
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "ols_table",
          keep.stat = c("rsq", "n"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (OLS)",
          omit= c("COUNTRY", "REGION", "Constant"),
          notes = c("All models include country and region fixed effects. Standard errors are clustered at the district level.")
)


#############################################################################
########       END OF ROBUSTNESS (REGION FIXED EFFECTS) SCRIPT       ########
#############################################################################


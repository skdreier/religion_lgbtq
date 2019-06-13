###############################################
# Replication Code for:                       
# Long, Dreier, Winkler (P&R)                 
#                 
# Code to replicate main models with control
# for individual level media consumption.
#
# OUTPUTS:
#   - Table A.20
#   - Table A.21 

# AFROBAORMETER DATA                          
# R version 3.5.2 (2018-12-20)                
# DATE: 5/17/2019                             
###############################################

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################
library(magrittr) #pipe function
library(dplyr) #transformations
library(simcf) #extractdata
library(multiwayvcov) #for clustered std err
library(lmtest) #coeftest for clustered std err
library(stargazer)

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

#####################################
### REPLICATE MAIN MODELS WITH    ###
### CONTROL FOR MEDIA CONSUMPTION ###
#####################################

# Model 1: Religious Herf 
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + 
  christian + muslim + 
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(radio) + as.numeric(tv) + as.numeric(newspaper) + as.numeric(internet) +  
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) + 
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result.1 <- lm(model, data = mdata) #save OLS result
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result.1 <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Model 2: Majority Religion
model <- 
  sexuality2 ~ 
  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) +
  as.numeric(radio) + as.numeric(tv) + as.numeric(newspaper) + as.numeric(internet) + 
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result.2 <- lm(model, data = mdata) #save OLS result
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result.2 <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + 
  as.numeric(radio) + as.numeric(tv) + as.numeric(newspaper) + as.numeric(internet) + 
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result.3 <- lm(model, data = mdata) #save OLS result
lm.result.3 <- coeftest(lm.result.3, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result.3 <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Model 5: Religious Herf + Maj Religion + Ethnic Herf + Maj Ethnic
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) +
  as.numeric(radio) + as.numeric(tv) + as.numeric(newspaper) + as.numeric(internet) + 
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result.5 <- lm(model, data = mdata) #save OLS result
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result.5 <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# labels for latex table
main.vars <- c("Religion HHI (district)", "Majority religion", "Ethnicity HHI", "Majority ethnicity",
               "Christian", "Muslim", "Female", "Age", "Education",
               "Water access", "Urban", "Religiosity",
               "Radio", "TV", "Newspaper", "Internet",
               "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance", "Immigrant tolerance"
)

# create latex table for ols results: Table A.20
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "ols_table",
          keep.stat = c("rsq", "n"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (OLS)",
          omit= c("COUNTRY", "Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# create latex table for logit results: Table A.21
stargazer(logit.result.1, logit.result.2, logit.result.3, logit.result.5,
          no.space=TRUE, 
          label = "logit_appendix",
          keep.stat = c("n", "aic"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (Logit)",
          omit= c("COUNTRY", "Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

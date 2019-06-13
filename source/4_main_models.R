###############################################
# Replication Code for:                       #    
# Long, Dreier, Winkler (P&R)                 #              
#                                             #
# Code to run main models and various         #
# replications of the main models.            #
#                                             #
# OUTPUTS:                                    #
#   - Table 1                                 #
#   - Table A.8                               #
#   - Table A.6                               #
#   - Table A.7                               #
#   - Table A.9                               #
#   - Table A.10                              #
#   - Table A.11                              #
#   - Table A.12                              #
#   - Table A.22                              #
#   - Table A.23                              #
#                                             #
# AFROBAORMETER DATA                          #  
# R version 3.5.2 (2018-12-20)                #
# NOTE: Not compatible with newer R versions  #
#                                             #
# DATE: 06/12/2019                            # 
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
library(stargazer) #for latex output
library(MASS) #polr to run probit models

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data
myData$COUNTRY <- myData$ctry # replace original country number w recoded country name variable for main analyses

###################################
### RUN MAIN OLS & LOGIT MODELS ###
### CREATE TABLES 1, A.6, A.8 #####
###################################

# Model 1: Religious Herf 
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + 
  christian + muslim + 
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) + 
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) + 
  as.factor(COUNTRY) 

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #DISTRICT for district-clustered sd err; RESPNO for reference
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
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

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
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

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
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE)

lm.result.5 <- lm(model, data = mdata) 
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.5 <- glm(model, family=binomial, data=mdata) 
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Create meaningful labels that can be used in the table
main.vars <- c("Religion HHI (district)", "Majority religion", "Ethnicity HHI (district)", "Majority ethnicity",
          "Christian", "Muslim", "Female", "Age", "Education", "Water access", "Urban", "Religiosity", 
          "Access to internet", "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance", 
          "Immigrant tolerance"
          )

# Generate latex table for main OLS results (Table 1)
stargazer(lm.result.1, lm.result.2, lm.result.3,  lm.result.5,
          no.space=TRUE, 
          label = "ols_table",
          keep.stat = c("rsq", "n"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (LPM)",
          omit= c("COUNTRY", "Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
          )

# Generate another table for main OLS results that includes country coefficients (Table A.8)
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "ols_table_with_country",
          keep.stat = c("rsq", "n"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (OLS)",
          omit= c("Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# Generate latex table with logit results from main models (Table A.6)
stargazer(logit.result.1, logit.result.2, logit.result.3, logit.result.5,
          no.space=TRUE, 
          label = "logit_appendix",
          keep.stat = c("n", "aic"), #see note above
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (Logit)",
          omit= c("COUNTRY", "Constant"),
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

###################################
### RUN ORDERED PROBIT MODELS #####
### CREATE TABLE A.7 ##############
###################################

myData$sexuality <- as.factor(myData$sexuality) #make sure DV is factor
levels(myData$sexuality) # and that ordering is correct 1:5

# Function to cluster standard errors
c1   <- function(data,fm, cluster){ 
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <-ncol(data)-1 +length(unique(data[,]))
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

# Model 1: Religious Herf
model <- 
  sexuality ~ 
  herf_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) +
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, myData, na.rm=TRUE)

probit.result1 <- polr(model,
                       data = mdata,
                       method = "probit",
                       Hess = TRUE)

# Apply cluster function
# formula here is data, model, cluster. This code clusters at the district level
obs2 <- as.numeric(rownames(mdata, probit.result1, myData$DISTRICT)) 
probit.result1.cluster <- c1(mdata, probit.result1, myData$DISTRICT[obs2]) 

# Model 2: Majority Religion
model <- 
  sexuality ~ 
  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, na.rm=TRUE)

probit.result2 <- polr(model,
                       data = mdata,
                       method = "probit",
                       Hess = TRUE)

# Apply cluster function
obs2 <- as.numeric(rownames(mdata, probit.result2, mdata$DISTRICT)) 
probit.result2.cluster <- c1(mdata, probit.result2, myData$DISTRICT[obs2]) 

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality ~   
  herf_relig_bin_dist +
  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, na.rm=TRUE)

probit.result3 <- polr(model,
                       data = mdata,
                       method = "probit",
                       Hess = TRUE)

# Apply cluster function
obs2 <- as.numeric(rownames(mdata, probit.result3, mdata$DISTRICT)) 
probit.result3.cluster <- c1(mdata, probit.result3, myData$DISTRICT[obs2]) 

# Model 4: Religious Herf + Majority Religion + Ethnic Herf + Ethnic Majority
model <- 
  sexuality ~   
  herf_relig_bin_dist + maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, na.rm=TRUE) 

probit.result5 <- polr(model,
                       data = mdata,
                       method = "probit",
                       Hess = TRUE)

# Apply cluster function
obs2 <- as.numeric(rownames(mdata, probit.result5, mdata$DISTRICT)) 
probit.result5.cluster <- c1(mdata, probit.result5, myData$DISTRICT[obs2]) 

# Use variable lables from above (main.vars)

# Generate latex table with ordered probit results of main models (Table A.7)
stargazer(probit.result1.cluster, probit.result2.cluster, probit.result3.cluster, probit.result5.cluster,
          no.space=TRUE,
          label = "probit_models", 
          summary.stat = c("n"), nobs = TRUE, 
          dep.var.caption = "DV: Homosexual as Neighbor (0: Dislike, 5: Strongly Like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of District-Level Religious Diversity on LGBTQ Attitudes (Ordered Probit)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.") 
)

###################################################
### REPLICATE MAIN OLS & LOGIT RESULTS WITH     ###
### HHI CALCULATED ON UNBINNED RELIGIOUS GROUPS ###
### CREATE TABLE A.9 & A.10                     ###
###################################################

# Model 1: Religious Herf
model <- 
  sexuality2 ~ 
  herf_relig_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE)

lm.result.1 <- lm(model, data = mdata)
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.1 <- glm(model, family=binomial, data=mdata) 
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 2: Majority Religion
model <- 
  sexuality2 ~ 
  maj_relig_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE)

lm.result.2 <- lm(model, data = mdata) 
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata)  
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT))  

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ 
  herf_relig_dist +  maj_relig_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.3 <- lm(model, data = mdata) 
lm.result.3 <- coeftest(lm.result.3, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.3 <- glm(model, family=binomial, data=mdata)
logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 4: Religious Herf + Maj Religion + Ethnic Herf + Maj Ethnic
model <- 
  sexuality2 ~ 
  herf_relig_dist + maj_relig_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.5 <- lm(model, data = mdata) 
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.5 <- glm(model, family=binomial, data=mdata) 
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT))

# Create meaningful labels for table
unbinned.vars <- c("Religion HHI (unbinned)", "Majority religion (unbinned)", 
                   "Ethnicity HHI (district)", "Majority ethnicity",
                   "Christian", "Muslim", "Female", "Age", "Education",
                   "Water access", "Urban", "Religiosity", "Access to internet", 
                   "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance",
                   "Immigrant tolerance")

# Generate latex table for OLS results (Table A.9)
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "unbinned_ols",
          covariate.labels = unbinned.vars,
          keep.stat = c("n", "aic"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (OLS)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# Generate latex table for Logit results (Table A.10)
stargazer(logit.result.1, logit.result.2, logit.result.3, logit.result.5,
          no.space=TRUE, 
          label = "unbinned_logit",
          covariate.labels = unbinned.vars,
          keep.stat = c("n", "aic"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (Logit)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

###################################################
### REPLICATE MAIN OLS & LOGIT RESULTS WITH     ###
### CONTROL FOR ALL RELIGIONS INSTEAD OF JUST   ###
### CHRISTIAN AND MUSLIM                        ###
### CREATE TABLE A.11 & A.12                    ###
###################################################

# Model 1: Religious Herf
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + as.factor(relig)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.1 <- lm(model, data = mdata)
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.1 <- glm(model, family=binomial, data=mdata) 
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 2: Majority Religion
model <- 
  sexuality2 ~ 
  maj_relig_bin_dist +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + as.factor(relig)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.2 <- lm(model, data = mdata) 
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata) 
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + as.factor(relig)

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
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + as.factor(relig)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.5 <- lm(model, data = mdata) 
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.5 <- glm(model, family=binomial, data=mdata) 
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# create meaningful labels for the table
full_relig.vars <- c("Religion HHI (unbinned)", "Majority religion (unbinned)", 
                     "Ethnicity HHI (district)", "Majority ethnicity",
                     "Female", "Age", "Education",
                     "Water access", "Urban", "Religiosity", "Access to internet", 
                     "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance",
                     "Immigrant tolerance")

# Generate latex table of OLS results (Table A.11)
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "main_ols_full_relig",
          covariate.labels = full_relig.vars,
          keep.stat = c("n", "aic"),
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (OLS)",
          omit = "COUNTRY",
          notes = c("All models include country fixed effects. 
                    Standard errors are clustered at the district level.")
          )

# Generate latex table of Logit results (Table A.12)
stargazer(logit.result.1, logit.result.2, logit.result.3, logit.result.5,
          no.space=TRUE, 
          label = "main_logit_full_relig",
          keep.stat = c("n", "aic"), 
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          title = "Effect of District-Level Religious Diversity on LGBT Attitudes (Logit)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

###################################################
### REPLICATE MAIN OLS & LOGIT RESULTS WITH     ###
### HHI CALCULATED AT REGIONAL LEVEL            ###
### INSTEAD OF AT DISTRICT LEVEL                ###
### CREATE TABLE A.22 & A.23                    ###
###################################################

# Model 1: Religious Herf 
model <- 
  sexuality2 ~ 
  herf_relig_bin_reg + 
  christian + muslim + 
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) + 
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

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
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.2 <- lm(model, data = mdata) 
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata) 
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ 
  herf_relig_bin_reg +  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.3 <- lm(model, data = mdata) 
lm.result.3 <- coeftest(lm.result.3, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.3 <- glm(model, family=binomial, data=mdata) 
logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 4: Religious Herf + Maj Religion + Ethnic Herf + Maj Ethnic
model <- 
  sexuality2 ~ 
  herf_relig_bin_reg + maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + 
  as.numeric(water_access) + as.numeric(urban) + 
  as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + 
  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) 

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.5 <- lm(model, data = mdata) 
lm.result.5 <- coeftest(lm.result.5, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.5 <- glm(model, family=binomial, data=mdata)
logit.result.5 <- coeftest(logit.result.5, vcov. = function(x) cluster.vcov(x, ~ DISTRICT))

# Create meaningful labels for table
main.vars <- c("Religion HHI (region)", "Majority religion", "Ethnicity HHI (region)", "Majority ethnicity",
               "Christian", "Muslim", "Female", "Age", "Education",
               "Water access", "Urban", "Religiosity", "Access to internet",
               "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance", "Immigrant tolerance")

# Generate latex table of OLS results (Table A.22)
stargazer(lm.result.1, lm.result.2, lm.result.3, lm.result.5,
          no.space=TRUE, 
          label = "region_ols",
          summary.stat = c("n"), nobs = TRUE,
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of Region-Level Religious Diversity on LGBT Attitudes (OLS)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# Generate latex table of Logit results (Table A.23)
stargazer(logit.result.1, logit.result.2, logit.result.3, logit.result.5,
          no.space=TRUE, 
          label = "region_logit",
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          title = "Effect of Region-Level Religious Diversity on LGBT Attitudes (Logit)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

##########################################################################################################
##########################################################################################################
###################################### END? ##############################################################
##########################################################################################################

# Sarah: The code below looks like it's mostly for creating plots. Do we need it or can we delete? 

################################################################
##### Plotting Model 3 (Logit and Ordered Logit) ###############
################################################################

## plotting simulated results ##
source("~/Dropbox/Current_Projects/Af_LGBT_Tolerance_JL_SW_SD/source/multiplot_code_lc.R")
library(Zelig)
library(ZeligChoice)
library(magrittr)

setwd("~/Dropbox/Current_Projects/Af_LGBT_Tolerance_JL_SW_SD/plots/sims")

xvar <- "Inverse Religious HHI"
xlab_name <- "Inverse Religious HHI"

# Estimate: Dummy Model 3
model <-
  sexuality2 ~
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + sample_size_dist #remove sample size for main models

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

logit.result.3 <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

mdata$age <- as.numeric(mdata$age)
mdata$education <- as.numeric(mdata$education)
mdata$water_access <- as.numeric(mdata$water_access)
mdata$urban <- as.numeric(mdata$urban)
mdata$religiosity <- as.numeric(mdata$religiosity)
mdata$internet <- as.numeric(mdata$internet)
mdata$tol_relig2 <- as.numeric(mdata$tol_relig2)
mdata$tol_ethnic2 <- as.numeric(mdata$tol_ethnic2)
mdata$tol_hiv2 <- as.numeric(mdata$tol_hiv2)
mdata$tol_immig2 <- as.numeric(mdata$tol_immig2)

zmod <- zelig(model <-
                as.factor(sexuality2) ~
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim +
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(COUNTRY) + sample_size_dist #remove sample size for main models
              , data = mdata, model="logit")

# Set Range for 'continuous' variable
herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), max(mdata$herf_relig_bin_dist, na.rm=T), length.out=1000)
x.out <- setx(zmod, herf_relig_bin_dist = herf_range)
s.out <- Zelig::sim(zmod, x=x.out)

plotdata <- mv_extract(s.out)

#colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="")
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

# PID Covariate #
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + #ylim(.03,.08) + xlim(0,.9) +
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color = "blue") +
  geom_line(aes(y =low), linetype="dashed", color = "blue") +
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on toleration of sexual minorities (logit)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Pr (Tolerating LGBT neighbors)"  ) +
  
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
    axis.text.x = element_text(size=14), 
    axis.title.x = element_text(size=14), 
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=14)) +

  #annotate("text", x = .50, y=.0735, label = "Pr (Accepting LGBT Neighbors )\n95% CI", color = "blue") #+
  ggsave("model3_dec.png", device="png")

dummy.plot.3 <- zmod
dummy.dcse.3 <- logit.result.3

###################
# ORDERED MODEL 3 # 
###################

# recode var for interpretation (ordered model): 1-3
myData$sexuality_ordered <- myData$sexuality 
myData$sexuality_ordered[myData$sexuality==4 | myData$sexuality_ordered==5] <- 3 

model <- 
  as.factor(sexuality_ordered) ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) #+ sample_size_dist #remove sample size for main models

mdata <- extractdata(model, myData, na.rm=TRUE) 

#####
ordered.result3.dcse <- polr(model,data = mdata, method = "probit", Hess = TRUE)

#Apply cluster function
obs2 <- as.numeric(rownames(mdata, ordered.result3.dcse, mdata$DISTRICT)) #forumula here is data, model, cluster. So in this example I am clustering on District
ordered.result3.dcse <- c1(mdata, ordered.result3.dcse, myData$DISTRICT[obs2]) 

mdata$age <- as.numeric(mdata$age)
mdata$education <- as.numeric(mdata$education)
mdata$water_access <- as.numeric(mdata$water_access)
mdata$urban <- as.numeric(mdata$urban)
mdata$religiosity <- as.numeric(mdata$religiosity)
mdata$internet <- as.numeric(mdata$internet)
mdata$tol_relig2 <- as.numeric(mdata$tol_relig2)
mdata$tol_ethnic2 <- as.numeric(mdata$tol_ethnic2)
mdata$tol_hiv2 <- as.numeric(mdata$tol_hiv2)
mdata$tol_immig2 <- as.numeric(mdata$tol_immig2)

zmod <- zelig(model <- 
                as.factor(sexuality_ordered) ~ 
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim + 
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 +
                as.factor(COUNTRY) #+ sample_size_dist #remove sample size for main models
              , data = mdata, model="oprobit")#"ologit")

# Set Range for 'continuous' variable
herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), 1, length.out=100)
x.out <- setx(zmod, herf_relig_bin_dist = herf_range)
s.out <- Zelig::sim(zmod, x=x.out)
s.out

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 3),] # only looking at people who strongly oppose LGBT neighbors
plotdata2 <- plotdata[seq(2,nrow(plotdata), 3),] # only looking at people who oppose LGBT neighbors
plotdata3 <- plotdata[seq(3,nrow(plotdata), 3),] # only looking at people who DON'T OPPOSE LGBT neighbors
colnames(plotdata2) <- paste(colnames(plotdata2),col="_2", sep="")
colnames(plotdata3) <- paste(colnames(plotdata3),col="_3", sep="")

plot_data_full <- data.frame(plotdata1, plotdata2, plotdata3, herf_relig_bin_dist = herf_range)

# Pr strongly oppose
  ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) + #ylim(.84,.92) + xlim(0,1) +
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color="red") + 
  geom_line(aes(y =low), linetype="dashed", color="red") + 
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = "on attitudes toward sexual minorities (ordered probit)",
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( Strongly opposing sexual minorities as neighbors )"  
  ) +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=17),
        axis.text.x = element_text(size=20), 
        axis.title.x = element_text(size=19), 
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=17)) +
  annotate("text", x = .55, y=.915, label = "Pr (Strongly Opposing LGBT Neighbors)", color="red", size=5.5) +
  ggsave("model3_ordered_p_st_oppose_dec.png", device="png")

# # Pr all oppose (not used in paper)
# ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + #ylim(.84,.92) + xlim(0,1) +
#   geom_line(aes(y =mean + mean_2)) + 
#   geom_line(aes(y =high + high_2), linetype="dashed", color="red") + 
#   geom_line(aes(y =low + low_2), linetype="dashed", color="red") + 
#   theme_minimal() +
#   labs(
#     title = "Effect of Religious Diversity",
#     subtitle = paste("on attitudes toward sexual minorities (Model 3, ordered logit)"),
#     x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
#     , y = ""
#     #, y = "Pr( Strongly opposing sexual minorities as neighbors )"  
#   ) +
#   annotate("text", x = .7, y=.962, label = "Pr (Opposing LGBT Neighbors)\n95% CI", color="red") +
#   ggsave("model3_ordered_all_oppose.png", device="png")

# Don't Oppose
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) + # xlim(0,.9) + ylim(.039,.09) + 
  geom_line(aes(y =mean_3)) + 
  geom_line(aes(y =high_3), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_3), linetype="dashed", color="blue") + 
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on attitudes toward sexual minorities (ordered probit)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( Not opposing sexual minorities as neighbors )"  
  ) +
  theme(plot.title = element_text(size=22),
      plot.subtitle = element_text(size=17),
      axis.text.x = element_text(size=20), 
      axis.title.x = element_text(size=19), 
      axis.text.y = element_text(size=20),
      axis.title.y = element_text(size=17)) +
    annotate("text", x = .38, y=.08, label = "Pr (Not Opposing LGBT Neighbors)", color="blue", size=5.5) +
  ggsave("model3_ordered_p_tolerate_dec.png", device="png")
# 
# # PR Oppose (not strongly), Don't Oppose (not used in paper)
# ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + ylim(.025,.09) + xlim(0,1) +
#   geom_line(aes(y =mean_2)) + 
#   geom_line(aes(y =high_2), linetype="dashed", color="orange") + 
#   geom_line(aes(y =low_2), linetype="dashed", color="orange") + 
#   theme_minimal() +
#   geom_line(aes(y =mean_3)) + 
#   geom_line(aes(y =high_3), linetype="dashed", color="blue") + 
#   geom_line(aes(y =low_3), linetype="dashed", color="blue") + 
#   theme_minimal() +
#   labs(
#     title = paste("Effect of", xvar, sep=" "),
#     subtitle = paste("on attitudes toward sexual minorities (Model 3, Ordered)"),
#     x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
#     , y = ""
#     #, y = "Pr( given attitude on sexual minorities as neighbors )"  
#   ) +
#   annotate("text", x = .6, y=.09, label = "Pr (Not Opposing LGBT Neighbors)", color="blue") +
#   annotate("text", x = .45, y=.035, label = "Pr (Opposing LGBT Neighbors,\nbut not strongly)", color="orange") 
#   # + ggsave("model3_ordered_c.png", device="png")

ordered.plot.3 <- zmod
ordered.dcse.3 <- ordered.result3.dcse

labs <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
logit.plot <-   c(0.5031, (0.1047), 4.807, 1.53e-06)
logit.dcse <-   c(0.5031, (0.1757), 2.864, 4.19e-03)
ordered.plot <- c(0.4360, (0.0876), "", "")
ordered.dcse <- c(0.4360, (0.1618), 2.696, 7.03e-03)

x <- t(rbind(labs, logit.dcse, logit.plot, ordered.plot, ordered.dcse))
x1 <- x[1:2,]

library(xtable)
xtable(x1)

########################
# ORDERED MODEL 3: 1-5 #
########################

myData$sexuality_ordered <- myData$sexuality 

model <- 
  as.factor(sexuality_ordered) ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + sample_size_dist #remove sample size for main models

mdata <- extractdata(model, myData, na.rm=TRUE) 

ordered.result3.dcse <- polr(model,data = mdata, method = "logistic", Hess = TRUE)

#Apply cluster function
obs2 <- as.numeric(rownames(mdata, ordered.result3.dcse, mdata$DISTRICT)) #forumula here is data, model, cluster. So in this example I am clustering on District
ordered.result3.dcse <- c1(mdata, ordered.result3.dcse, myData$DISTRICT[obs2]) 

mdata$age <- as.numeric(mdata$age)
mdata$education <- as.numeric(mdata$education)
mdata$water_access <- as.numeric(mdata$water_access)
mdata$urban <- as.numeric(mdata$urban)
mdata$religiosity <- as.numeric(mdata$religiosity)
mdata$internet <- as.numeric(mdata$internet)
mdata$tol_relig2 <- as.numeric(mdata$tol_relig2)
mdata$tol_ethnic2 <- as.numeric(mdata$tol_ethnic2)
mdata$tol_hiv2 <- as.numeric(mdata$tol_hiv2)
mdata$tol_immig2 <- as.numeric(mdata$tol_immig2)

zmod <- zelig(model <- 
                as.factor(sexuality_ordered) ~ 
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim + 
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 +
                as.factor(COUNTRY) + sample_size_dist #remove sample size for main models
              , data = mdata, model="ologit")

# Set Range for 'continuous' variable
herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), 1, length.out=100)
x.out <- setx(zmod, herf_relig_bin_dist = herf_range)
s.out <- Zelig::sim(zmod, x=x.out)
s.out

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 5),] # only looking at people who strongly oppose LGBT neighbors
plotdata2 <- plotdata[seq(2,nrow(plotdata), 5),] # only looking at people who oppose LGBT neighbors
plotdata3 <- plotdata[seq(3,nrow(plotdata), 5),] # only looking at people who don't care
plotdata4 <- plotdata[seq(4,nrow(plotdata), 5),] # only looking at people who like
plotdata5 <- plotdata[seq(5,nrow(plotdata), 5),] # only looking at people who strongly like
plotdata_tol <- plotdata3 + plotdata4 + plotdata5 

colnames(plotdata2) <- paste(colnames(plotdata2),col="_2", sep="")
colnames(plotdata_tol) <- paste(colnames(plotdata_tol),col="_tol", sep="")
plot_data_full <- data.frame(plotdata1, plotdata2, plotdata_tol, herf_relig_bin_dist = herf_range)

# Pr strongly oppose
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + #ylim(.84,.92) + xlim(0,1) +
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color="red") + 
  geom_line(aes(y =low), linetype="dashed", color="red") + 
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = "on attitudes toward sexual minorities (ordered logit)",
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( Strongly opposing sexual minorities as neighbors )"  
  ) +
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +
  annotate("text", x = .55, y=.915, label = "Pr (Strongly Opposing LGBT Neighbors)", color="red", size=4.5) +
  ggsave("model3_ordered5_st_oppose_dec.png", device="png")

# Don't Oppose
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + #ylim(.039,.09) + xlim(0,1) +
  geom_line(aes(y =mean_tol)) + 
  geom_line(aes(y =high_tol), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_tol), linetype="dashed", color="blue") + 
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on attitudes toward sexual minorities (ordered logit)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( Not opposing sexual minorities as neighbors )"  
  ) +
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +
  annotate("text", x = .4, y=.092, label = "Pr (Not Opposing LGBT Neighbors)", color="blue", size=4.5) +
  ggsave("model3_ordered5_tolerate_dec.png", device="png")
# 

ordered.plot <- zmod
ordered.dcse <- ordered.result3.dcse

labs <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
logit.plot <-   c(0.5031, (0.1047), 4.807, 1.53e-06)
logit.dcse <-   c(0.5031, (0.1757), 2.864, 4.19e-03)
ordered.plot <- c(0.4098, (0.0844), "", "")
ordered.dcse <- c(0.4098, (0.155), 2.6436, 8.2e-03)

x <- t(rbind(labs, logit.dcse, logit.plot, ordered.plot, ordered.dcse))
x1 <- x[1:2,]

library(xtable)
xtable(x1)

################################################################
##### Plotting Model 5 (Logit and Ordered Logit) ###############
##### CURRENTLY NOT IN PAPER ###################################
################################################################

## plotting simulated results ##
source("~/Dropbox/Current_Projects/Af_LGBT_Tolerance_JL_SW_SD/source/multiplot_code_lc.R")
library(Zelig)
library(ZeligChoice)
library(magrittr)

setwd("~/Dropbox/Current_Projects/Af_LGBT_Tolerance_JL_SW_SD/plots/sims")

xvar <- "Inverse Religious HHI"
xlab_name <- "Inverse Religious HHI"

# Estimate: Dummy Model 5
model <-
  sexuality2 ~
  herf_relig_bin_dist +  maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + sample_size_dist #remove sample size for main models

mdata <- extractdata(model, myData, na.rm=TRUE)

mdata$age <- as.numeric(mdata$age)
mdata$education <- as.numeric(mdata$education)
mdata$water_access <- as.numeric(mdata$water_access)
mdata$urban <- as.numeric(mdata$urban)
mdata$religiosity <- as.numeric(mdata$religiosity)
mdata$internet <- as.numeric(mdata$internet)
mdata$tol_relig2 <- as.numeric(mdata$tol_relig2)
mdata$tol_ethnic2 <- as.numeric(mdata$tol_ethnic2)
mdata$tol_hiv2 <- as.numeric(mdata$tol_hiv2)
mdata$tol_immig2 <- as.numeric(mdata$tol_immig2)

zmod <- zelig(model <-
                as.factor(sexuality2) ~
                herf_relig_bin_dist +  maj_relig_bin_dist +
                herf_ethn_dist + maj_ethn_dist + 
                christian + muslim +
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(COUNTRY) + sample_size_dist #remove sample size for main models
              , data = mdata, model="logit")

# Set Range for 'continuous' variable
herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), max(mdata$herf_relig_bin_dist, na.rm=T), length.out=100)
x.out <- setx(zmod, herf_relig_bin_dist = herf_range)
s.out <- Zelig::sim(zmod, x=x.out)

plotdata <- mv_extract(s.out)

#colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="")
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

# PID Covariate #
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + ylim(.03,.075) + xlim(0,.9) +
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color="red") +
  geom_line(aes(y =low), linetype="dashed", color="red") +
  theme_minimal() +
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on attitudes toward sexual minorities (Model 5, Logit)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Acceptance of LGBT neighbors"  ) +
  annotate("text", x = .7, y=.07, label = "95% CI", color="red") +
  ggsave("model5.png", device="png")

# recode var for interpretation (ordered model)
myData$sexuality_ordered <- myData$sexuality 
myData$sexuality_ordered[myData$sexuality==4 | myData$sexuality_ordered==5] <- 3 

model <- 
  as.factor(sexuality_ordered) ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY) + sample_size_dist #remove sample size for main models

mdata <- extractdata(model, myData, na.rm=TRUE) 

mdata$age <- as.numeric(mdata$age)
mdata$education <- as.numeric(mdata$education)
mdata$water_access <- as.numeric(mdata$water_access)
mdata$urban <- as.numeric(mdata$urban)
mdata$religiosity <- as.numeric(mdata$religiosity)
mdata$internet <- as.numeric(mdata$internet)
mdata$tol_relig2 <- as.numeric(mdata$tol_relig2)
mdata$tol_ethnic2 <- as.numeric(mdata$tol_ethnic2)
mdata$tol_hiv2 <- as.numeric(mdata$tol_hiv2)
mdata$tol_immig2 <- as.numeric(mdata$tol_immig2)

zmod <- zelig(model <- 
                as.factor(sexuality_ordered) ~ 
                herf_relig_bin_dist +  maj_relig_bin_dist +
                herf_ethn_dist + maj_ethn_dist + 
                christian + muslim + 
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 +
                as.factor(COUNTRY) + sample_size_dist #remove sample size for main models
              , data = mdata, model="ologit")

# Set Range for 'continuous' variable
herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), 1, length.out=100)
x.out <- setx(zmod, herf_relig_bin_dist = herf_range)
s.out <- Zelig::sim(zmod, x=x.out)

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 3),] # only looking at people who strongly oppose LGBT neighbors
plotdata2 <- plotdata[seq(2,nrow(plotdata), 3),] # only looking at people who oppose LGBT neighbors
plotdata3 <- plotdata[seq(3,nrow(plotdata), 3),] # only looking at people who DON'T OPPOSE LGBT neighbors
colnames(plotdata2) <- paste(colnames(plotdata2),col="_2", sep="")
colnames(plotdata3) <- paste(colnames(plotdata3),col="_3", sep="")

plot_data_full <- data.frame(plotdata1, plotdata2, plotdata3, herf_relig_bin_dist = herf_range)

# Pr strongly oppose
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + ylim(.85,.93) + xlim(0,1) +
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color="red") + 
  geom_line(aes(y =low), linetype="dashed", color="red") + 
  theme_minimal() +
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on attitudes toward sexual minorities (Model 5, Ordered)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( Strongly opposing sexual minorities as neighbors )"  
  ) +
  annotate("text", x = .65, y=.915, label = "Pr (Strongly Opposing LGBT Neighbors)\n95% CI", color="red") +
  ggsave("model5_ordered_a.png", device="png")

# PR Oppose (not strongly), Don't Oppose
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + ylim(.035,.085) + xlim(0,1) +
  geom_line(aes(y =mean_3)) + 
  geom_line(aes(y =high_3), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_3), linetype="dashed", color="blue") + 
  theme_minimal() +
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on attitudes toward sexual minorities (Model 5, Ordered)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( Not opposing sexual minorities as neighbors )"  
  ) +
  annotate("text", x = .54, y=.080, label = "Pr (Not Opposing LGBT Neighbors)\n95% CI", color="blue") +
  ggsave("model5_ordered_b.png", device="png")

# PR Oppose (not strongly), Don't Oppose
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + ylim(.025,.09) + xlim(0,1) +
  geom_line(aes(y =mean_2)) + 
  geom_line(aes(y =high_2), linetype="dashed", color="orange") + 
  geom_line(aes(y =low_2), linetype="dashed", color="orange") + 
  theme_minimal() +
  geom_line(aes(y =mean_3)) + 
  geom_line(aes(y =high_3), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_3), linetype="dashed", color="blue") + 
  theme_minimal() +
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on attitudes toward sexual minorities (Model 5, Ordered)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" ")
    , y = ""
    #, y = "Pr( given attitude on sexual minorities as neighbors )"  
  ) +
  annotate("text", x = .6, y=.09, label = "Pr (Not Opposing LGBT Neighbors)", color="blue") +
  annotate("text", x = .45, y=.035, label = "Pr (Opposing LGBT Neighbors,\nbut not strongly)", color="orange") +
  ggsave("model5_ordered_c.png", device="png")


#### Spare / duplicate plot code ####

# 
# model <- 
#   sexuality2 ~ 
#   herf_relig_bin_dist +  maj_relig_bin_dist +
#   christian + muslim +
#   female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
#   as.numeric(tol_noLGBT) + 
#   as.factor(COUNTRY) # + sample_size_dist #remove sample size for main models
# 
# mdata <- extractdata(model, #extract data to only include what is in the model
#                      myData, 
#                      extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
#                      na.rm=TRUE) 
# 
# lm.result.3 <- lm(model, data = mdata) #save OLS result
# lm.result.3 <- coeftest(lm.result.3, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
# lm.result.3 #OLS result w/ DCSE
# logit.result.3 <- glm(model, family=binomial, data=mdata) #save logit result 
# logit.result.3 <- coeftest(logit.result.3, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result
# logit.result.3 # Logit result w/ DCSE
# # n: 47,034
# 
# mdata$age <- as.numeric(mdata$age)
# mdata$education <- as.numeric(mdata$education)
# mdata$water_access <- as.numeric(mdata$water_access)
# mdata$urban <- as.numeric(mdata$urban)
# mdata$religiosity <- as.numeric(mdata$religiosity)
# mdata$internet <- as.numeric(mdata$internet)
# mdata$tol_noLGBT <- as.numeric(mdata$tol_noLGBT)
# 
# zmod <- zelig(model <- 
#                 as.factor(sexuality2) ~ 
#                 herf_relig_bin_dist +  maj_relig_bin_dist +
#                 christian + muslim + 
#                 female + age + education + water_access + urban + religiosity + internet + tol_noLGBT + 
#                 as.factor(COUNTRY) + sample_size_dist #remove sample size for main models
#         , data = mdata, model="logit")
# 
# # Set Range for 'continuous' variable
# herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), max(mdata$herf_relig_bin_dist, na.rm=T), length.out=100)
# x.out <- setx(zmod, herf_relig_bin_dist = herf_range)
# s.out <- Zelig::sim(zmod, x=x.out)
# s.out
# 
# plotdata <- mv_extract(s.out)
# #plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with low trust
# #plotdata4 <- plotdata[seq(4,nrow(plotdata), 4),] # only looking at people with high trust
# 
# #colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="")
# plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)
# 
# xvar <- "Inverse HHI (Religion)"
# xlab_name <- "Inverse HHI (Religion)"
# 
# # PID Covariate #
# ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + ylim(.02,.085) + xlim(0,.9) +
#   geom_line(aes(y =mean)) + 
#   geom_line(aes(y =high), linetype="dashed", color="red") + 
#   geom_line(aes(y =low), linetype="dashed", color="red") + 
#   #geom_line(aes(y =qt.1), linetype="dashed", color="red") + 
#   #geom_line(aes(y =qt.3), linetype="dashed", color="red") + 
#   theme_minimal() +
#   #geom_ribbon(aes(ymin=low, ymax=high), alpha=0.1) +
#   #geom_ribbon(aes(ymin=qt.1, ymax=qt.3), alpha=0.6) +
#   #geom_line(aes(y =mean_4)) + 
#   #geom_line(aes(y =high_4), linetype="dashed", color="blue") + 
#   #geom_line(aes(y =low_4), linetype="dashed", color="blue") + 
#   #geom_line(aes(y =qt.1_4), linetype="dashed", color="blue") + 
#   #geom_line(aes(y =qt.3_4), linetype="dashed", color="blue") + 
#   theme_minimal() +
#   #geom_ribbon(aes(ymin=low_4, ymax=high_4), alpha=0.1) +
#   #geom_ribbon(aes(ymin=qt.1_4, ymax=qt.3_4), alpha=0.6) + 
#   labs(
#     title = paste("Effect of", xvar, sep=" "),
#     subtitle = paste("on attitudes toward sexual minorities (Model 3"),
#     x = paste(xlab_name, "(95% CI)", sep=" "),
#     y = "E( Accepting sexual minorities as neighbors )"  ) +
# annotate("text", x = .7, y=.08, label = "95% CI", color="red") 
# #annotate("text", x = 12, y=.05, label = "DV: High Trust")
# ggsave("plots/sims/model3.png", device="png")



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
# R version 3.6.0 (2019-04-26)                #
# NOTE: Not compatible with newer R versions  #
#                                             #
# DATE: 06/17/2019                            # 
###############################################

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################

library(magrittr) #pipe function
library(dplyr) #transformations
library(multiwayvcov) #for clustered std err
library(lmtest) #coeftest for clustered std err
library(stargazer) #for latex output
library(MASS) #polr to run probit models
library(devtools) #simcf

install_github("chrisadolph/simcf") # github source is compatible w recent R versions
library(simcf) #extractdata

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

###################################
### RUN MAIN OLS & LOGIT MODELS ###
### CREATE TABLES 1, A.6, A.8 #####
###################################

# Model 1: Religious Herf 
model <- 
  sexuality2 ~ herf_relig_bin_dist + 
  christian + muslim + 
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
  sexuality2 ~ maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.2 <- lm(model, data = mdata) 
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata) 
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ herf_relig_bin_dist +  
  maj_relig_bin_dist + christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
  as.factor(ctry)

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
          omit= c("ctry", "Constant"),
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
          omit= c("ctry", "Constant"),
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
  sexuality ~ herf_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, na.rm=TRUE)

probit.result1 <- polr(model, data = mdata, method = "probit", Hess = TRUE)

# Apply cluster function
# formula here is data, model, cluster. This code clusters at the district level
obs2 <- as.numeric(rownames(mdata, probit.result1, myData$DISTRICT)) 
probit.result1.cluster <- c1(mdata, probit.result1, myData$DISTRICT[obs2]) 

# Model 2: Majority Religion
model <- 
  sexuality ~ maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) 

mdata <- extractdata(model, myData, na.rm=TRUE)

probit.result2 <- polr(model, data = mdata, method = "probit", Hess = TRUE)

# Apply cluster function
obs2 <- as.numeric(rownames(mdata, probit.result2, mdata$DISTRICT)) 
probit.result2.cluster <- c1(mdata, probit.result2, myData$DISTRICT[obs2]) 

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality ~ herf_relig_bin_dist +
  maj_relig_bin_dist + christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, na.rm=TRUE)

probit.result3 <- polr(model, data = mdata, method = "probit", Hess = TRUE)

# Apply cluster function
obs2 <- as.numeric(rownames(mdata, probit.result3, mdata$DISTRICT)) 
probit.result3.cluster <- c1(mdata, probit.result3, myData$DISTRICT[obs2]) 

# Model 4: Religious Herf + Majority Religion + Ethnic Herf + Ethnic Majority
model <- 
  sexuality ~   
  herf_relig_bin_dist + maj_relig_bin_dist +
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, na.rm=TRUE) 

probit.result5 <- polr(model, data = mdata, method = "probit", Hess = TRUE)

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
          omit= "ctry",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.") 
)

###################################################
### REPLICATE MAIN OLS & LOGIT RESULTS WITH     ###
### HHI CALCULATED ON UNBINNED RELIGIOUS GROUPS ###
### CREATE TABLE A.9 & A.10                     ###
###################################################

# Model 1: Religious Herf
model <- 
  sexuality2 ~ herf_relig_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE)

lm.result.1 <- lm(model, data = mdata)
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result.1 <- glm(model, family=binomial, data=mdata) 
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 2: Majority Religion
model <- 
  sexuality2 ~ maj_relig_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE)

lm.result.2 <- lm(model, data = mdata) 
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata)  
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT))  

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ herf_relig_dist +  
  maj_relig_dist + christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
          omit= "ctry",
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
          omit= "ctry",
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
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(relig)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.1 <- lm(model, data = mdata)
lm.result.1 <- coeftest(lm.result.1, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.1 <- glm(model, family=binomial, data=mdata) 
logit.result.1 <- coeftest(logit.result.1, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 2: Majority Religion
model <- 
  sexuality2 ~ 
  maj_relig_bin_dist +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(relig)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result.2 <- lm(model, data = mdata) 
lm.result.2 <- coeftest(lm.result.2, vcov. = function(x) cluster.vcov(x, ~DISTRICT))
logit.result.2 <- glm(model, family=binomial, data=mdata) 
logit.result.2 <- coeftest(logit.result.2, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Model 3: Religious Herf + Majority Religion
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(relig)

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
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry) + as.factor(relig)

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
          omit = "ctry",
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
          omit= "ctry",
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
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
  as.factor(ctry)

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
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

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
          omit= "ctry",
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
          omit= "ctry",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

######################################################
########       END OF MAIN MODEL SCRIPT       ########
###########################################@@@@@@#####

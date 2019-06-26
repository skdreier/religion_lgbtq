###############################################
# Replication Code for:                       #
# Long, Dreier, Winkler (P&R)                 #
#                                             #
# Code for various robustness checks          #
#                                             #
# OUTPUTS:                                    #
#   - Table A.13                              #
#   - Table A.14                              #
#   - Table A.15                              #
#   - Table A.16                              #
#                                             #
# AFROBAORMETER DATA                          #
# R version 3.6.0 (2019-04-26)                #
#                                             #
# DATE: 06/17/2019                            #
###############################################

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################
library(magrittr) #pipe function
library(dplyr) #transformations
library(forcats) #factor recoding
library(stringr) #str_detect
library(broom) #for clustered std err
library(multiwayvcov) #for clustered std err
library(lmtest) #coeftest for clustered std err
library(stargazer)
library(MASS) #polr to run probit models
library(ggplot2)

library(devtools) #simcf
#install_github("chrisadolph/simcf") # github source is compatible w recent R versions
library(simcf) #extractdata

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

#################################################################
### Effect of Religious Diversity on Other Tolerance Measures ###
### To show uniquness of DV (LGBT)                            ###
### Tables: A.15 and A.16                                     ### 
#################################################################

# Main model with LGBT as DV
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC1 <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata) 
logit.result.RC1 <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Religion as DV
model <- 
  tol_relig2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_ethnic2 + as.numeric(sexuality2) + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC1a <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata) 
logit.result.RC1a <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Ethniciy as DV
model <- 
  tol_ethnic2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + as.numeric(sexuality2) + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC1b <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata) 
logit.result.RC1b <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# HIV/AIDS as DV
model <- 
  tol_hiv2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + as.numeric(sexuality2) + tol_immig2 + 
  as.factor(ctry)


mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC1c <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata) 
logit.result.RC1c <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Immigration as DV
model <- 
  tol_immig2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + as.numeric(sexuality2) + tol_hiv2 + 
  as.factor(ctry)


mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC1d <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata)
logit.result.RC1d <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# create meaningful labels for latex table
tol.vars <- c("Religion HHI (district)", "Majority religion", 
              "Christian", "Muslim", "Female", "Age", "Education",
              "Water access", "Urban", "Religiosity", "Access to internet", 
              "Religious tolerance", "Ethnic tolerance", "LGBT tolerance",
              "HIV+ tolerance", "Immigrant tolerance")

# create latex table for ols results: Table A.15
stargazer(lm.result.RC1, lm.result.RC1a, lm.result.RC1b, lm.result.RC1c, lm.result.RC1d, 
          no.space=TRUE, 
          label = "rc1_table",
          dep.var.caption = "DV: Tolerance toward neighbors (0: dislike, 1: like/don't care)",
          dep.var.labels.include = FALSE,
          covariate.labels = tol.vars,
          column.labels=c("LGBT", "Relig", "Ethnic", "HIV/AIDS", "Immig/Foreign"),
          title = "Effect of District-Level Religious Diversity on Tolerance (OLS)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# create latex table for logit results: Table A.16
stargazer(logit.result.RC1, logit.result.RC1a, logit.result.RC1b, logit.result.RC1c, logit.result.RC1d, 
          no.space=TRUE, 
          label = "rc1_table_logit",
          dep.var.caption = "DV: Tolerance toward neighbors (0: dislike, 1: like/don't care)",
          dep.var.labels.include = FALSE,
          covariate.labels = tol.vars,
          column.labels=c("LGBT", "Relig", "Ethnic", "HIV/AIDS", "Immig/Foreign"),
          title = "Effect of District-Level Religious Diversity on Tolerance (Logit)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

########################################################
### Effect of Ethnic Diversity HHI on LGBT Tolerance ### 
### To show uniquness of Religious Diversity         ###
### Tables A.13 and A.14                             ###
########################################################

# Original model with Religious HHI as explanatory variable
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC2 <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata) 
logit.result.RC2 <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# Ethnic HHI as explanatory variable
model <- 
  sexuality2 ~ 
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + age + education + water_access + urban + religiosity + 
  internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
  as.factor(ctry)

mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 

lm.result <- lm(model, data = mdata) 
lm.result.RC2b <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) 
logit.result <- glm(model, family=binomial, data=mdata) 
logit.result.RC2b <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) 

# create meaningful labels for table 
div.vars <- c("Religion HHI (district)", "Majority religion", 
              "Ethnicity HHI (district)", "Majority ethnicity", 
              "Christian", "Muslim", "Female", "Age", "Education",
              "Water access", "Urban", "Religiosity", "Access to internet", 
              "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance",
              "Immigrant tolerance")

# Generate latex table with OLS results (Table A.13)
stargazer(lm.result.RC2, lm.result.RC2b,
          no.space=TRUE, 
          label = "rc2_table",
          dep.var.caption = "DV: Tolerance toward LGBT neighbors (0: dislike, 1: like/don't care)",
          dep.var.labels.include = FALSE,
          covariate.labels = div.vars,
          title = "Effect of District-Level Diversity on LGBT Tolerance (OLS)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

# Generate latex table with Logit result (Table A.14)
stargazer(logit.result.RC2, logit.result.RC2b,
          no.space=TRUE, 
          label = "rc2_table_logit",
          dep.var.caption = "DV: Tolerance toward LGBT neighbors (0: dislike, 1: like/don't care)",
          dep.var.labels.include = FALSE,
          covariate.labels = div.vars,
          title = "Effect of District-Level Diversity on LGBT Tolerance (Logit)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

##################################################################
########       END OF ROBUSTNESS (ETHNICITY) SCRIPT       ########
##################################################################
###############################################
# Replication Code for:                       
# Long, Dreier, Winkler (P&R)                 
#          
# Code for various robustness checks.
#
# OUTPUTS:
#   - Table A.13
#   - Table A.14
#   - Table A.15
#   - Table A.16

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
library(forcats) #factor recoding
library(stringr) #str_detect
library(simcf) #extractdata
library(broom) #for clustered std err
library(multiwayvcov) #for clustered std err
library(lmtest) #coeftest for clustered std err
library(stargazer)
library(MASS) #polr to run probit models
library(ggplot2)

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
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC1 <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC1 <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Religion as DV
model <- 
  tol_relig2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_ethnic2) +  as.numeric(sexuality2) +  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +  
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC1a <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC1a <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Ethniciy as DV
model <- 
  tol_ethnic2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) +  as.numeric(sexuality2) +  as.numeric(tol_hiv2) + as.numeric(tol_immig2) +  
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC1b <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC1b <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# HIV/AIDS as DV
model <- 
  tol_hiv2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) +  as.numeric(sexuality2) + as.numeric(tol_immig2) +  
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC1c <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC1c <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Immigration as DV
model <- 
  tol_immig2 ~ 
  herf_relig_bin_dist + maj_relig_bin_dist +
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) +  as.numeric(sexuality2) +  as.numeric(tol_hiv2) +  
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC1d <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC1d <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

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
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC2 <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC2 <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

# Ethnic HHI as explanatory variable
model <- 
  sexuality2 ~ 
  herf_ethn_dist + maj_ethn_dist + 
  christian + muslim +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) +
  as.numeric(internet) + 
  as.numeric(tol_relig2) + as.numeric(tol_ethnic2) + as.numeric(tol_hiv2) + as.numeric(tol_immig2) +
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     myData, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 

lm.result <- lm(model, data = mdata) #save OLS result
lm.result.RC2b <- coeftest(lm.result, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
logit.result <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.RC2b <- coeftest(logit.result, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result

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




##########################################################################################################
##########################################################################################################
###################################### END? ##############################################################
##########################################################################################################

# Sarah: Do you know if you used the code below to create tables A.24 and A.25? It doesn't look like it to me.
# and I think those tables were probably created within a script that creates the corresponding Figure A.23. 
# I'm keeping this code here for now but we can delete it once we confirm it's not used to create tables A.23 & A.25

################
# Robustness 3:
# Herf of Christ/Muslim
# To show it's not just about Muslim v. Christian 
################

##################################################################################################
# Subset to only exclusively Muslim or exclusively Christian or exclusively "Other"
##################################################################################################

### CODE BREAKS HERE! 

summary(myData$herf_relig_christ_mus)
table(myData$christ_mus)

sub <- subset(myData, myData$herf_relig_christ_mus==1) # people in exclusively Christ, Mus or Other districts
sub.christian <- subset(sub, sub$relig_bin > 0 & sub$relig_bin < 6) # people living in 100 % Christian districts
sub.muslim <- subset(sub, sub$relig_bin==6) # people living in 100 % Muslim districts

## RUN MODELS
#  Herf + Majority

#  Christian Only 
model <- 
  sexuality2 ~ 
  herf_relig_dist + maj_relig_dist +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_noLGBT) + 
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     sub.christian, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 
lm.result.sub <- lm(model, data = mdata) #save OLS result
lm.result.sub <- coeftest(lm.result.sub, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
christ.all.lm <- lm.result.sub 
logit.result.sub <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.sub <- coeftest(logit.result.sub, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result
christ.all.logit <- logit.result.sub

# Muslim Only
mdata <- extractdata(model, #extract data to only include what is in the model
                     sub.muslim,
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 
lm.result.sub <- lm(model, data = mdata) #save OLS result
lm.result.sub <- coeftest(lm.result.sub, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
muslim.all.lm <- lm.result.sub
logit.result.sub <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.sub <- coeftest(logit.result.sub, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result
muslim.all.logit <- logit.result.sub

stargazer(
  christ.all.lm, christ.all.logit, muslim.all.lm,  muslim.all.logit, 
  no.space=TRUE, 
  keep.stat = c("n", "aic"), #see note below
  dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: like or don't care)",
  dep.var.labels.include = FALSE,
  column.labels=c("All Christian (OLS)", "All Christian (Logit)", "All Muslim (OLS)", "All Muslim (Logit)"),
  title = "Effect of Relig Diversity in all-Christian or all-Muslim Districts on LGBT Tolerance",
  omit= "COUNTRY",
  notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)

#  Herf * Majority (INTERACTION MODEL)
#  Christian Only 
model <- 
  sexuality2 ~ 
  herf_relig_dist * maj_relig_dist +
  female + as.numeric(age) + as.numeric(education) + as.numeric(water_access) + as.numeric(urban) + as.numeric(religiosity) + as.numeric(internet) +
  as.numeric(tol_noLGBT) + 
  as.factor(COUNTRY)

mdata <- extractdata(model, #extract data to only include what is in the model
                     sub.christian, 
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 
lm.result.sub <- lm(model, data = mdata) #save OLS result
lm.result.sub <- coeftest(lm.result.sub, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
christ.all.lm <- lm.result.sub 
logit.result.sub <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.sub <- coeftest(logit.result.sub, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result
christ.all.logit <- logit.result.sub

# Muslim Only
mdata <- extractdata(model, #extract data to only include what is in the model
                     sub.muslim,
                     extra = ~DISTRICT + RESPNO, #add DISTRICT so we can get district-clustered sd err, and RESPNO for reference
                     na.rm=TRUE) 
lm.result.sub <- lm(model, data = mdata) #save OLS result
lm.result.sub <- coeftest(lm.result.sub, vcov. = function(x) cluster.vcov(x, ~DISTRICT)) #add DCSE to OLS
muslim.all.lm <- lm.result.sub
logit.result.sub <- glm(model, family=binomial, data=mdata) #save logit result 
logit.result.sub <- coeftest(logit.result.sub, vcov. = function(x) cluster.vcov(x, ~ DISTRICT)) #add DCSE to logit result
muslim.all.logit <- logit.result.sub

stargazer(
  christ.all.lm, christ.all.logit, muslim.all.lm,  muslim.all.logit, 
  no.space=TRUE, 
  keep.stat = c("n", "aic"), #see note below
  dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: like or don't care)",
  dep.var.labels.include = FALSE,
  column.labels=c("All Christian (OLS)", "All Christian (Logit)", "All Muslim (OLS)", "All Muslim (Logit)"),
  title = "Effect of Relig Diversity in all-Christian or all-Muslim Districts on LGBT Tolerance (Interaction)",
  omit= "COUNTRY",
  notes = c("All models include country fixed effects. Standard errors are clustered at the district level.")
)



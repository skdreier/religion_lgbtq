###############################################
# Replication Code for:                       #    
# Dreier, Long, Winkler (P&R)                 #              
#                                             #
# Code to plot simulations for main models    #
#                                             #
# OUTPUTS:                                    #
#   - Figure 4                                #
#   - Figure 5A and 5B                        #
#   - Figures A.23 (Religious subset plots)   #
#   - Table A.24 (Religious subset tables)    #
#   - Table A.25 (Religious subset tables)    #
#   - Figures A.25 (Country plots)            #
#                                             #
# AFROBAORMETER DATA                          #  
# R version 3.6.0 (2019-04-26)                #
#                                             #
# DATE: 06/25/2019                            # 
###############################################

rm(list=ls())

source("source/multiplot_code_lc.R")
library(ZeligChoice)
library(ggplot2)
library(dplyr)

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

#####################################
### ESTABLISH VARIABLES FOR PLOTS ###    
### AFROBAROMETER DATA ##############
#####################################

# Establish DV axis names for all plots 
xvar <- xlab_name <- "Inverse Religious HHI"

####################################################
### FIGURE 4 #######################################    
### MODEL 3 (TABLE 6.A) ############################
### Simulation: Probability of tolerating LGBTs) ###
### AFROBAROMETER DATA #############################
####################################################

# Estimate model
zmod <- zelig(model <-
                as.factor(sexuality2) ~ herf_relig_bin_dist + 
                maj_relig_bin_dist + christian + muslim +
                female + age + education + water_access + urban + religiosity + 
                internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(ctry), 
                data = myData, model="logit")

# Set herf range for continuous variable (actual), calculate values accordingly
herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), 
                  max(myData$herf_relig_bin_dist, na.rm=T), length.out=1000)
suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

# Extract data to plot simulated values
plotdata <- mv_extract(s.out)
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

### Figure 4: Simulated values for Model 3 (probability of tolerating LGBTs) ###
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + 
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color = "blue") +
  geom_line(aes(y =low), linetype="dashed", color = "blue") +
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on toleration of sexual minorities (logit)"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Pr (Tolerating LGBT neighbors)"  
    ) +
  
  theme(
    plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)
    ) +
  
  ggsave("figures/4_model3_toleration.png", device="png")


#############################################################
### FIGURE 5A and 5B  ####################################### 
### MODEL 3: Ordered DV #####################################
### 5A Simulation: Probability of strongly opposing LGBTs ###
### 5B Simulation: Probability of tolerating LGBTs ##########
### AFROBAROMETER DATA ######################################
#############################################################

# recode DV for interpretation (ordered model): 1-3
myData$sexuality_ordered <- myData$sexuality 
myData$sexuality_ordered[myData$sexuality==4 | myData$sexuality_ordered==5] <- 3 

# Estimate model
zmod <- zelig(model <- 
                as.factor(sexuality_ordered) ~ herf_relig_bin_dist +  
                maj_relig_bin_dist + christian + muslim + 
                female + age + education + water_access + urban + religiosity + 
                internet + tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 +
                as.factor(ctry),
                data = myData, model="oprobit")

# Set herf range for continuous variable (hypothetical), calculate values accordingly
herf_range2 <- seq(0,1,length.out=1000)
suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range2) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

# Extract data and prepare to plot simulated values
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 3),] # people who strongly oppose LGBT neighbors
plotdata3 <- plotdata[seq(3,nrow(plotdata), 3),] # people who DON'T OPPOSE LGBT neighbors
colnames(plotdata3) <- paste(colnames(plotdata3),col="_3", sep="")
plot_data_full <- data.frame(plotdata1, plotdata3, herf_relig_bin_dist = herf_range2)

### Figure 5A: Simulated values for Model 3 ordered (probability of strongly opposing LGBTs) ###
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color="red") + 
  geom_line(aes(y =low), linetype="dashed", color="red") + 
  theme_minimal() +
  
  labs(
        title = "Effect of Religious Diversity",
        subtitle = "on attitudes toward sexual minorities (ordered probit)",
        x = paste(xlab_name, "(0 = homogeneous)", sep=" "), y = "" 
        ) +
  
  theme(
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=17),
        axis.text.x = element_text(size=20), 
        axis.title.x = element_text(size=19), 
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=17)
        ) +
  
  annotate("text", x = .55, y=.915, label = "Pr (Strongly Opposing LGBT Neighbors)", 
           color="red", size=5.5) +
  
  ggsave("figures/5a_model3_ordered_oppose.png", device="png")

### Plot Figure 5B: Simulated values for Model 3 ordered (probability of tolerating LGBTs) ###
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) +  
  geom_line(aes(y =mean_3)) + 
  geom_line(aes(y =high_3), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_3), linetype="dashed", color="blue") + 
  theme_minimal() +
  
  labs(
        title = "Effect of Religious Diversity",
        subtitle = paste("on attitudes toward sexual minorities (ordered probit)"),
        x = paste(xlab_name, "(0 = homogeneous)", sep=" "), y = ""
       ) +
  
  theme(
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=17),
        axis.text.x = element_text(size=20), 
        axis.title.x = element_text(size=19), 
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=17)
        ) +
  
  annotate("text", x = .38, y=.08, label = "Pr (Not Opposing LGBT Neighbors)", 
           color="blue", size=5.5) +
  
  ggsave("figures/5b_model3_ordered_tolerate.png", device="png")


########################################################################
### TABLE A.24 #########################################################   
### MODEL 3: Probability of tolerating LGBTs ###########################
### among districts that have specific religious identities present ####
### AFROBAROMETER DATA #################################################
########################################################################

### Create variables and data subsets to run analyses ###

# Create dummies for relig identities of interest
myData %<>%
  dplyr::mutate(
    prot_evan = ifelse( relig_bin==5, 1, 0),
    muslim2 = ifelse( relig_bin==6, 1, 0),
    catholic = ifelse( relig_bin==2, 1, 0),
    prot_main = ifelse( relig_bin==4, 1, 0)
  )

# Create unique identifiers for each district and remove dead space
myData$ctry_reg_dist <- paste(myData$COUNTRY, myData$REGION, myData$DISTRICT, sep = "_")
myData$ctry_reg_dist <- str_trim(myData$ctry_reg_dist)

# Split each district into its own dataset
district <- split(myData, myData$ctry_reg_dist)

# Function to identify districts that have protestant, catholic, or muslim respondents 
spec_relig_present <- function(data){
  
  prot_evan <- ifelse( sort(data$prot_evan, decreasing=TRUE)[1]==1,1,0 )
  muslim2 <- ifelse( sort(data$muslim2, decreasing=TRUE)[1]==1,1,0 )
  catholic <- ifelse( sort(data$catholic, decreasing=TRUE)[1]==1,1,0 )
  prot_main <- ifelse( sort(data$prot_main, decreasing=TRUE)[1]==1,1,0 )
  all_present <- ifelse( prot_evan==1 & muslim2==1 & catholic==1 & prot_main==1, 1, 0 ) 
    
  dist <- data$ctry_reg_dist[1]
  
  return(data.frame(prot_evan, muslim2, catholic, prot_main, all_present, 
                    dist, stringsAsFactors = F) )
}

# Apply function to list of district datasets
out_dist <- lapply(district, spec_relig_present)
out_dist <- bind_rows(out_dist)
names(out_dist) <- c("prot_evan_present", "muslim_present", "catholic_present", 
                     "prot_main_present", "all_relig_present", "ctry_reg_dist")

# Merge district-level data onto individual-level data
data_base <- merge(myData, out_dist, by.x="ctry_reg_dist")

# Create individual-level datasets subsetted to districts with specific identities present 
prot_evan_present_data <- subset(data_base, prot_evan_present==1)
prot_main_present_data <- subset(data_base, prot_main_present==1)
catholic_present_data <- subset(data_base, catholic_present==1)
muslim_present_data <- subset(data_base, muslim_present==1)
all_present_data <- subset(data_base, all_relig_present==1)

### Estimate models for each data subset (for table outputs) ###

# Define model
model <- 
  sexuality2 ~ 
  herf_relig_bin_dist +  maj_relig_bin_dist +
  christian + muslim + 
  female + age + education + water_access + urban + religiosity + internet +
  tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 +
  as.factor(ctry) 

# Estimate base model
mdata <- extractdata(model, myData, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 
logit.3.base <- glm(model, family=binomial, data=mdata) 

# Estimate model: Protestant evangelicals present
mdata <- extractdata(model, prot_evan_present_data, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 
logit.3.evan <- glm(model, family=binomial, data=mdata) 

# Estimate model: Protestant mainlines present
mdata <- extractdata(model, prot_main_present_data, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 
logit.3.main <- glm(model, family=binomial, data=mdata) 

# Estimate model: Catholics present
mdata <- extractdata(model, catholic_present_data, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 
logit.3.catholic <- glm(model, family=binomial, data=mdata) 

# Estimate model: Muslims present
mdata <- extractdata(model, muslim_present_data, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 
logit.3.muslim <- glm(model, family=binomial, data=mdata) 

# Estimate model: Evangelicals, mainlines, Catholics, and Muslims present
mdata <- extractdata(model, all_present_data, extra = ~DISTRICT + RESPNO, na.rm=TRUE) 
logit.3.all <- glm(model, family=binomial, data=mdata) 

### Output table of model estimates for data subsets ###

# Define variable names
main.vars <- c("Religion HHI (district)", "Majority religion", 
               "Christian", "Muslim", "Female", "Age", "Education",
               "Water access", "Urban", "Religiosity", "Access to internet",
               "Religious tolerance", "Ethnic tolerance", "HIV+ tolerance", "Immigrant tolerance")

stargazer(logit.3.base, logit.3.evan, logit.3.main, logit.3.catholic, 
          logit.3.muslim, logit.3.all,
          no.space=TRUE, label = "relig_subsets",
          keep.stat = c("n", "aic"), #see note above
          dep.var.caption = "DV: Homosexual as Neighbor (0: dislike, 1: don't care or like)",
          dep.var.labels.include = FALSE,
          covariate.labels = main.vars,
          column.labels=c("All Districts", "Evangelicals", "Mainlines", 
                          "Catholics", "Muslims", "All Present"),
          title = "Model 3 (Logit) among districts with the following groups present: 
          Evangelicals (2), Mainlines (3), Catholics (4), Muslims (5), and all religious categories (6)",
          omit= "ctry", notes = c("Standard errors are clustered at the district level.")
)


########################################################################
### FIGURES A.24 #######################################################
### MODEL 3: Probability of tolerating LGBTs ###########################
### among districts that have specific religious identities present ####
### AFROBAROMETER DATA #################################################
########################################################################

### Figure A.23a: Protestant Evangelicals present in district ###

# Estimate model
zmod <- zelig(model <-
                as.factor(sexuality2) ~
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim +
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(COUNTRY), data = prot_evan_present_data, model="logit")

# Set herf range (actual) for specific data subset, calculate values accordingly
herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), 
                  max(myData$herf_relig_bin_dist, na.rm=T), length.out=1000)

suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

plotdata <- mv_extract(s.out)
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

# Plot Figure A.23a
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) +
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color = "blue") +
  geom_line(aes(y =low), linetype="dashed", color = "blue") +
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on toleration of sexual minorities (logit)\namong districts with Protestant Evangelicals"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Pr (Tolerating LGBT neighbors)"  ) +
  
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +

  ggsave("figures/relig_subsets/23a_model3_evangelical.png", device="png")


### Figure A.23b: Protestant Mainlines present in district ###

# Estimate model
zmod <- zelig(model <-
                as.factor(sexuality2) ~
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim +
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(COUNTRY), data = prot_main_present_data, model="logit")

herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), 
                  max(myData$herf_relig_bin_dist, na.rm=T), length.out=1000)

suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

plotdata <- mv_extract(s.out)
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

### Plot Figure A.23b
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) +
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color = "blue") +
  geom_line(aes(y =low), linetype="dashed", color = "blue") +
  theme_minimal() +
    
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on toleration of sexual minorities (logit)\namong districts with Mainline Protestants"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Pr (Tolerating LGBT neighbors)"  ) +
  
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +

  ggsave("figures/relig_subsets/23b_model3_mainline.png", device="png")

  
### Figure A.23c: Catholics present in district ###
  
# Estimate model
zmod <- zelig(model <-
                as.factor(sexuality2) ~
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim +
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(COUNTRY), data = catholic_present_data, model="logit")

herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), 
                  max(myData$herf_relig_bin_dist, na.rm=T), length.out=1000)

suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

plotdata <- mv_extract(s.out)
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

# Plot Figure A.23c
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) +
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color = "blue") +
  geom_line(aes(y =low), linetype="dashed", color = "blue") +
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on toleration of sexual minorities (logit)\namong districts with Catholics"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Pr (Tolerating LGBT neighbors)"  ) +
  
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +
  
  ggsave("figures/relig_subsets/23c_model3_catholic.png", device="png")


### Figure A.23d: Muslims present in district ###

# Estimate model
zmod <- zelig(model <-
                as.factor(sexuality2) ~
                herf_relig_bin_dist +  maj_relig_bin_dist +
                christian + muslim +
                female + age + education + water_access + urban + religiosity + internet + 
                tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 + 
                as.factor(COUNTRY), data = muslim_present_data, model="logit")

herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), 
                  max(myData$herf_relig_bin_dist, na.rm=T), length.out=1000)

suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

plotdata <- mv_extract(s.out)
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

# Plot Figure A.23d
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) + 
  geom_line(aes(y =mean)) +
  geom_line(aes(y =high), linetype="dashed", color = "blue") +
  geom_line(aes(y =low), linetype="dashed", color = "blue") +
  theme_minimal() +
  labs(
    title = "Effect of Religious Diversity",
    subtitle = paste("on toleration of sexual minorities (logit)\namong districts with Muslims"),
    x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
    y = "Pr (Tolerating LGBT neighbors)"  ) +
  
  theme(plot.title = element_text(size=16),
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +
  
  ggsave("figures/relig_subsets/23d_model3_muslim.png", device="png")


######################################################################
### FIGURES A.25 #####################################################    
### MODEL 3 Simulation: Probability of tolerating LGBTs by country ###
### AFROBAROMETER DATA ###############################################
######################################################################

### Model to loop over for most countries in analysis ###
model <- 
  as.factor(sexuality2) ~ herf_relig_bin_dist + 
  maj_relig_bin_dist + christian + muslim +
  female + age + education + water_access + urban + religiosity + internet +
  tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 

# Drop countries whose religious demographics make Model 3 over-determined
data_sub <- subset(myData, ctry!="Morocco" & ctry!="Mali" & ctry!="Niger" & ctry!="Sao Tome and Principe" & ctry!="Tunisia" & ctry!="Senegal" & ctry!="Sierra Leone") 

# Loop over each country to estimate model and plot simulation
countrylist <- unique(data_sub$ctry)
n <- length(countrylist)

for (i in 1:length(countrylist)) {
  
  currcty <- countrylist[i]
  
  mdata_0 <- subset(data_sub, ctry==currcty)
  mdata <- extractdata(model, mdata_0, na.rm=TRUE) 
  zmod <- zelig(model, mdata, model="logit")
  
  herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), max(mdata$herf_relig_bin_dist, na.rm=T), length.out=1000)
  suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
  suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )
  
  plotdata <- mv_extract(s.out)
  plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)
  
  ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) + 
    geom_line(aes(y =mean)) +
    geom_line(aes(y =high), linetype="dashed", color = "blue") +
    geom_line(aes(y =low), linetype="dashed", color = "blue") +
    theme_minimal() +
    
    labs(
      title = paste(currcty, "Effect of Religious Diversity", sep=": "),
      subtitle = paste("on toleration of sexual minorities (logit)"),
      x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
      y = "Pr (Tolerating LGBT neighbors)"  
    ) +
    
    theme(
      plot.title = element_text(size=16),
      plot.subtitle = element_text(size=15),
      axis.text.x = element_text(size=14), 
      axis.title.x = element_text(size=14), 
      axis.text.y = element_text(size=14),
      axis.title.y = element_text(size=14)
    ) +
    
    ggsave( paste("figures/ctry_plots/", currcty, "_model3.png", sep=""), device="png" )
  
}

### Model for countries omitted above (removes Christian and Muslim binary variables) ###
model <- 
  as.factor(sexuality2) ~ herf_relig_bin_dist + 
  maj_relig_bin_dist + #christian + muslim +
  female + age + education + water_access + urban + religiosity + internet +
  tol_relig2 + tol_ethnic2 + tol_hiv2 + tol_immig2 

# Subset to relevant countries
data_sub2 <- subset(myData, ctry=="Mali" | ctry=="Niger" | 
                      ctry=="São Tomé and Príncipe" | ctry=="Tunisia" | ctry=="Senegal" | 
                      ctry=="Sierra Leone") # Morocco omitted

# Loop over each country to estimate model and plot simulation
countrylist <- unique(data_sub2$ctry)
n <- length(countrylist)

for (i in 1:length(countrylist)) {
  currcty <- countrylist[i]
  
  mdata_0 <- subset(data_sub2, ctry==currcty)
  mdata <- extractdata(model, mdata_0, na.rm=TRUE) 
  zmod <- zelig(model, mdata, model="logit")
  
  herf_range <- seq(min(mdata$herf_relig_bin_dist, na.rm=T), max(mdata$herf_relig_bin_dist, na.rm=T), length.out=1000)
  suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
  suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )
  
  plotdata <- mv_extract(s.out)
  plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)
  
  ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist))  + #ylim(.03,.08) + xlim(0,.9) +
    geom_line(aes(y =mean)) +
    geom_line(aes(y =high), linetype="dashed", color = "blue") +
    geom_line(aes(y =low), linetype="dashed", color = "blue") +
    theme_minimal() +
    
    labs(
      title = paste(currcty, "Effect of Religious Diversity", sep=": "),
      subtitle = paste("on toleration of sexual minorities (logit)"),
      x = paste(xlab_name, "(0 = homogeneous)", sep=" "),
      y = "Pr (Tolerating LGBT neighbors)"  
    ) +
    
    theme(
      plot.title = element_text(size=16),
      plot.subtitle = element_text(size=15),
      axis.text.x = element_text(size=14), 
      axis.title.x = element_text(size=14), 
      axis.text.y = element_text(size=14),
      axis.title.y = element_text(size=14)
    ) +
    
    ggsave( paste("figures/ctry_plots/", currcty, "_model3.png", sep=""), device="png" )
  
}

################################################
########       END OF PLOT SCRIPT       ########
################################################
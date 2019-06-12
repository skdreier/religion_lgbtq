###################################################
# Replication Code for:                           #
# Long, Dreier, Winkler (P&R)                     #
#                                                 #
# CODE TO GENERATE DESCRIPTIVE TABLES AND FIGURES #
# AFROBAROMETER DATA                              #
#                                                 #
# OUTPUTS:                                        #
#   - Figure 1                                    #
#   - Figure 2a & 2b                              #                        
#   - Figure 3a & 3b                              #
#   - Table A.1 info                              #
#   - Table A.2                                   #
#   - Table A.3                                   #
#                                                 #
# Coders: S. Winkler and S. Dreier                #
#                                                 #
# R version 3.6.0 (2019-04-26)                    #
# DATE: 06/12/2019                                #
###################################################

rm(list=ls())

#####################
### LOAD PACKAGES ###
#####################

library(ggplot2)
library(dplyr)   
library(magrittr)
library(forcats)
library(stargazer)
library(scales)
library(xtable)

##########################
### LOAD SUBSET OF     ###
### AFROBAROMETER DATA ###
##########################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

###########################################################
### FIGURE 1: PERCENT DISLIKE LGBTQ NEIGHBOR BY COUNTRY ###
### DV DESCRIPTIVE STATISTICS #############################
###########################################################

# Label country names and reformat vectors for plot
myData %<>%
  dplyr::mutate(ctry = as.factor(COUNTRY), #create logical version of the variable 
                ctry = fct_recode(ctry,
                                  "Benin" = "2",
                                  "Botswana" = "3",
                                  "Burkina Faso" = "4",
                                  "Burundi"  = "5",
                                  "Cameroon"  = "6",
                                  "Cape Verde"  = "7",
                                  "Cote d'Ivoire"  = "8",
                                  "Gabon"  = "10",
                                  "Ghana"  = "11",
                                  "Guinea"  = "12",
                                  "Kenya"  = "13",
                                  "Lesotho"  = "14",
                                  "Liberia"  = "15",
                                  "Madagascar"  = "16",
                                  "Malawi"  = "17",
                                  "Mali"  = "18",
                                  "Mauritius"  = "19",
                                  "Morocco"  = "20",
                                  "Mozambique"  = "21",
                                  "Namibia"  = "22",
                                  "Niger"  = "23",
                                  "Nigeria"  = "24",
                                  "Sao Tome & Principe"  = "25",
                                  "Senegal"  = "26",
                                  "Sierra Leone"  = "27",
                                  "South Africa"  = "28",
                                  "Swaziland"  = "30",
                                  "Tanzania" = "31",
                                  "Togo" = "32",
                                  "Tunisia" = "33",
                                  "Uganda" = "34",
                                  "Zambia" = "35",
                                  "Zimbabwe" = "36"  ),
                ctry = as.character(ctry),
                sexuality2 = as.numeric(sexuality2)-1
  )

# Omit NAs on sexuality2 for mean calculations
meanData <- na.omit(dplyr::select(myData, ctry, sexuality2))

# Create "All survey" variable to calculate survey mean
all_survey <- meanData
all_survey$ctry <- "SURVEY MEAN"
meanData <- rbind(meanData, all_survey)

# Split dataset by country
country_list <- split(meanData, meanData$ctry)

# Function to caluclate country means/CI
mean_func <- function(data) {
  n = length(data$sexuality2)
  sigma = sd(data$sexuality2)
  sem = sigma/sqrt(n)
  E = qnorm(.975)*sem 
  xbar = 1-mean(data$sexuality2, na.rm=T)
  xbar + c(-E, 0, E)
}

# Apply function to each country in country_list
country_list_means <- lapply(country_list, mean_func)

# Collapse list into single dataset
data_out <- t(bind_cols(country_list_means))

# Prepare dataset for plotting
plot_data <- data.frame( rownames(data_out), data_out, row.names=NULL, stringsAsFactors = F)
colnames(plot_data) <- c("ctry", "lower", "mean", "upper")
sorted <- plot_data[ rev(order(plot_data$mean)) ,]
row.names(sorted) = c(1:34)

# PLOT FIGURE 1: Percent who would dislike having an LGBTQ neighbor (by country)
pdf(file="figures/1_dv_ctry.pdf",width=5,height=5)
par(mai=c(.7,1,0.35,1.2))
plot(sorted$mean, nrow(sorted):1, type="p", xlim =c(0,1),
     pch=16, cex=.7, yaxt="n", xaxt="n", ylab="", xlab="", bty="n", cex.main=.9, cex.axis=.9)
abline(v= sorted$mean[sorted$ctry=="SURVEY MEAN"], col="grey")
for (i in 1:length(sorted$ctry)){
  segments(x0= sorted$lower[i], y0=abs(i-length(sorted$ctry))+1, 
           x1= sorted$upper[i], y1=abs(i-length(sorted$ctry))+1  )
}
points(sorted$mean[sorted$ctry=="SURVEY MEAN"], 9, pch=16, cex=.7)
axis(4, at=1:34, labels = rev(sorted$ctry), las=1, cex.axis=.6) 
axis(1, at=c(seq(0,1,.2)), labels = c("0%", "20%", "40%", "60%", "80%", "100%"), las=1, cex.axis=.8) 
text(.69, 33, labels="mean\n response", cex=.6, col="grey")
dev.off()

###################################
### FIGURE 2A: DISTRIBUTION OF ####
### UNBINNED DEPENDENT VARIABLE ###
###################################

# Invert Herf so that E(0:1) where 1 is more heterogenous district
myData$herf_ethn_reg <- 1.00 - myData$herf_ethn_reg
myData$herf_ethn_dist <- 1.00 - myData$herf_ethn_dist
myData$herf_relig_reg <- 1.00 - myData$herf_relig_reg
myData$herf_relig_dist <- 1.00 - myData$herf_relig_dist
myData$herf_relig_bin_reg <- 1.00 - myData$herf_relig_bin_reg
myData$herf_relig_bin_dist <- 1.00 - myData$herf_relig_bin_dist
myData$herf_pol_reg <- 1.00 - myData$herf_pol_reg
myData$herf_pol_dist <- 1.00 - myData$herf_pol_dist

myData %<>% 
  mutate(sexuality4 = as.factor(sexuality), #Create new variable
         sexuality4 = fct_recode(sexuality4,#and rename responses so they are easier to identify in plot
                                 "Strongly\nDislike" = "1",
                                 "Somewhat\nDislike" = "2",
                                 "Don't\nCare" = "3",
                                 "Somewhat\nLike" = "4",
                                 "Strongly\nLike" = "5"))

dv_plot2 <- #plot for ordinal dependent variable (Figure 2a)
  ggplot(data = subset(myData, !is.na(sexuality4)), aes(x = as.factor(sexuality4))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("") + ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  theme(axis.text.x = element_text(size=16), 
        axis.text.y = element_text(size=16)) +
  theme_bw()

ggsave(filename = "figures/2a_dv_distribution.png", #Save it to directory
       plot = dv_plot2)

###################################
### FIGURE 2B: DISTRIBUTION OF ####
### BINNED DEPENDENT VARIABLE #####
###################################

myData %<>% 
  mutate(sexuality3 = as.factor(sexuality2), #Create new variable 
         sexuality3 = fct_recode(sexuality3, #and rename responses so they are easier to identify in plot
             "Strongly or Somewhat\nDislike" = "0",
             "Strongly or Somewhat\nLike or Don't Care" = "1"))

dv_plot1 <- #plot for logical dependent variable (Figure 2b) 
  ggplot(data = subset(myData, !is.na(sexuality3)), aes(x = as.factor(sexuality3))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("") + ylab("") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  theme(axis.text.x = element_text(size=16), 
        axis.text.y = element_text(size=16)) +
  theme_bw()
  
ggsave(filename = "figures/2b_dv_distribution_binned.png", #Save it to directory
       plot = dv_plot1)

#####################################
### FIGURE 3A: DISTRIBUTION OF ######
### UNBINNED RELIGIOUS HERFINDAHL ###
#####################################

sub <- # subset to only the unique districts
  unique(data.frame(myData$DISTRICT, myData$herf_relig_dist, myData$herf_relig_bin_dist))

iv_plot1 <- # plot independent variable based on original religion categories (Figure 3a)
  ggplot(data = sub, aes(x = myData.herf_relig_dist)) +
  geom_histogram() + 
  xlab("Inverse Religious HHI (0 = homogeneous)\n(original religion categories)") +
  ylab("") +
  theme(axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) + 
  theme_bw()

ggsave(filename = "figures/3a_iv_distribution.png", #Save it to directory
       plot = iv_plot1)

#####################################
### FIGURE 3B: DISTRIBUTION OF ######
### BINNED RELIGIOUS HERFINDAHL #####
#####################################

iv_plot2 <- # plot independent variable based on binned religion categories (Figure 3b)
  ggplot(data = sub, aes(x = myData.herf_relig_bin_dist)) +
  geom_histogram() + 
  xlab("Inverse Religious HHI (0 = homogeneous)\n(binned religion categories)") +
  ylab("") +
  theme(axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14), 
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14)) +
  theme_bw()

ggsave(filename = "figures/3b_iv_distribution_binned.png", #Save it to directory
       plot = iv_plot2)

####################################
### TABLE A.1: LIST OF COUNTRIES ###
### INCLUDED/OMITTED IN ANALYSIS ###
####################################

print(c("COUNTRIES INCLUDED (Q89C ASKED):", unique(myData$ctry)))
print(c("COUNTRIES EXCLUDED (Q89C NOT ASKED):", "Algeria", "Egypt", "Sudan"))

#######################################
### TABLE A.2: COVARIANCE MATRIX OF ###
### ALL SOCIAL TOLERANCE VARAIBLES ####
#######################################

tolerance_data <- dplyr::select(myData, tol_relig, tol_ethnic, sexuality,
                         tol_hiv, tol_immig)

tolerance_cov_table <-
  cor(tolerance_data, use = "pairwise.complete.obs", method = "spearman") 

row_names <- c("Religion", "Ethnicity", "Homosexual", "HIV", "Immigrant") #create meaningful row/col names
col_names <- c("Religion", "Ethnicity", "Homosexual", "HIV", "Immigrant")
rownames(tolerance_cov_table) <- row_names
colnames(tolerance_cov_table) <- col_names

stargazer(tolerance_cov_table, title="Spearman Correlation Matrix of Tolerance Battery") # latex output

####################################
### TABLE A.3: DESCRIPTIVE STATS ###
### FOR MODEL COVARIATES ###########
####################################

## STEPHEN: My code breaks here... not sure why? ##

table_data <- myData[,c("sexuality2", # setup table
                        "herf_relig_bin_dist", "maj_relig_bin_dist",
                        "herf_ethn_dist", "maj_ethn_dist",  
                        "christian", "muslim",  
                        #Controls
                        "tol_relig2", "tol_ethnic2", "tol_hiv2", "tol_immig2",
                        "female", "age", "education", "urban", "religiosity",
                        "water_access", "internet",
                        #Robustness
                        "sexuality",
                        "herf_relig_bin_reg", "maj_relig_bin_reg",
                        "herf_ethn_reg", "maj_ethn_reg",
                        "herf_relig_dist", "maj_relig_dist")]

# Function to generate summary statistics
stat_sum <- function(vec) {
  out <- c ( min(vec, na.rm=T), mean(vec, na.rm=T), max(vec, na.rm=T), sd(vec, na.rm=T)
  )
  return(out)
}

# Apply function to stats
tab_sum <- round ( t ( apply(table_data, 2, stat_sum) ), 2)

# Label Columns 
colnames(tab_sum) <- c("Min", "Mean", "Max", "S.D.")

# Label Rows w var names
row.names(tab_sum) <- c(
  "LGBT Tolerance (0=intolerant)", 
  "Religious HHI (district)", "Religious Majority (district, 0=minority)",
  "Ethnic HHI (district)", "Ethnic Majority (district, 0=minority)",  
  "Christian (0=non-Christian)", "Muslim (0=non-Muslim)",  
  
  "Religious Tolerance (0=intolerant)", "Ethnic Tolerance (0=intolerant)", 
  "HIV Tolerance (0=intolerant)", "Immigrant Tolerance (0=intolerant)",
  "Female (0=male)", "Age", "Education Level", "Urban (0=rural)", "Religiosity",
  "Water Access", "Internet Access",
  "LGBT Tolerance (scale)", 
  "Religious HHI (region)", "Religious Majority (region)",
  "Ethnic HHI (region)", "Ethnic Majority (region)",
  "Raw Religious HHI (district)", "Raw Religious Majority (district)"
)

xtable(tab_sum) #latex output 


#############################################################
########       END OF DESCRIPTIVE STATS SCRIPT       ########
#############################################################

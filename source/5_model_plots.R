###############################################
# Replication Code for:                       #    
# Dreier, Long, Winkler (P&R)                 #              
#                                             #
# Code to plot simulations for main models    #
#                                             #
# OUTPUTS:                                    #
#   - Figure 4                                #
#   - Figure 5A and 5B                        #
#                                             #
# AFROBAORMETER DATA                          #  
# R version 3.6.0 (2019-04-26)                #
#                                             #
# DATE: 06/17/2019                            # 
###############################################

rm(list=ls())

source("source/multiplot_code_lc.R")
library(ZeligChoice)
library(ggplot2)

############################
### LOAD CLEAN SUBSET OF ###    
### AFROBAROMETER DATA #####
############################

load(file="data/clean_afrobarometer.RData") # saved as "data"
myData <- data

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

# Set Range for continuous variable, calculate values accordingly
herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), max(myData$herf_relig_bin_dist, na.rm=T), length.out=1000)
suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

# Extract data to plot simulated values
plotdata <- mv_extract(s.out)
plot_data_full <- data.frame(plotdata, herf_relig_bin_dist = herf_range)

# Plot Figure 4: Simulated values for Model 3 (probability of tolerating LGBTs)
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

# Set range
herf_range <- seq(min(myData$herf_relig_bin_dist, na.rm=T), 1, length.out=100)
suppressWarnings( x.out <- setx(zmod, herf_relig_bin_dist = herf_range) )
suppressWarnings( s.out <- Zelig::sim(zmod, x=x.out) )

# Extract data and prepare to plot simulated values
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 3),] # people who strongly oppose LGBT neighbors
plotdata3 <- plotdata[seq(3,nrow(plotdata), 3),] # people who DON'T OPPOSE LGBT neighbors
colnames(plotdata3) <- paste(colnames(plotdata3),col="_3", sep="")
plot_data_full <- data.frame(plotdata1, plotdata3, herf_relig_bin_dist = herf_range)

# Plot Figure 5A: Simulated values for Model 3 ordered (probability of strongly opposing LGBTs)
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
  
  annotate("text", x = .55, y=.915, label = "Pr (Strongly Opposing LGBT Neighbors)", color="red", size=5.5) +
  ggsave("figures/5a_model3_ordered_oppose.png", device="png")

# Plot Figure 5B: Simulated values for Model 3 ordered (probability of tolerating LGBTs)
ggplot(data=plot_data_full, aes(x = herf_relig_bin_dist)) + # xlim(0,.9) + ylim(.039,.09) + 
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
  
  annotate("text", x = .38, y=.08, label = "Pr (Not Opposing LGBT Neighbors)", color="blue", size=5.5) +
  ggsave("figures/5a_model3_ordered_tolerate.png", device="png")


################################################
########       END OF PLOT SCRIPT       ########
################################################

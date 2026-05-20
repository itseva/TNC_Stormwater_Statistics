# This script takes the chemical of concern (COC) data with landscape predictors, and cleans up
# the data in preparation for individual constituent modeling

# v4 differs from v3 in that additional daymet precip columns are generated, and plots of antecedant
#   dry days/ precip with season are included.
#   Daymet precip data are sqrt transformed, ADD data are log+1 transformed; both data sets are then standardized
# v5 takes new set of predictors (from July 7, 2021), and does away with unstandardized predictors

# previous revision was July 21, 2021.  Latest revision doesn't change anything, just saves standardization values for daymet precip

# Author:  Eva Dusek Jennings
# Revised: Jan 22, 2025
#          May 1, 2025 - add predictors for Zinc: greenery_bareEarth, not_green, not_greenBE
#          Jun 2, 2025 - add predictor roads (road_density)
#          Jun 6, 2025 - add predictors (sqrt_paved, sqrt_CO2_cmv_rail, log_CO2_cmv_rail)
#          Jun 17, 2025 - add predictors for zinc
#          Dec 17, 2025 - try new trees predictor for greenery
#----------------------------------------------

library(gtools)
library(PerformanceAnalytics)
library(EnvStats)  #note the objects that are masked from other packages...
library(readr)  #for read_csv
library(knitr)  #for kable
library(rmdformats)
#library(hrbrthemes)
#library(tidyverse)
library(showtext)
library(kableExtra)
#library(MCMCglmm)
library(bayesplot)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(here)

source(here("..", "functions", "HighstatLibV10.R")) #Zuur library, incl panel.smooth2, VIF


#-------------------------------------#
#  Load S8 Data & Spatial Predictors  #
#-------------------------------------#

s8data <- read.csv(here("..", "data", "s8data.csv")) %>% #cleaned s8 data generated in clean_s8_data_v3.R
  dplyr::filter(! (location_id %in% c("POSOUTFALL_60",  #remove POS (unrepresentative); PIE_HDR, PIE_LDR (sampled in stream)
                                      "PIEHIRES_OUT",                               
                                      "PIELORES_OUT")))                              
  
sp_final <- read.csv(here("..", "processed_data", "spatial_predictors_standardized.csv"))

params <- c('Zinc - Water - Total',
            'Copper - Water - Total',
#            'Nitrite-Nitrate - Water - Dissolved',
            'Total Phosphorus - Water - Total',
            'Total Suspended Solids - Water - Total',
#            'Total PAH - Water - Total',
#            'Lead - Water - Total',
#            'CPAH - Water - Total',
#            'HPAH - Water - Total',
#            'Total Phthalate - Water - Total',
            "Total Kjeldahl Nitrogen - Water - Total" #,
#            "Zinc - Water - Dissolved" 
)


#make a function for scatter plots
scatter_cocs <- function(df.coc, title) {
  p <- ggplot(df.coc, aes(1, result)) + geom_jitter() + labs(
    title = title,
    subtitle = "Data collected 2009-2013",
    caption =
      " Data source: Ecology, 2015",
    x = "Observations"
  )
  p + facet_wrap( ~ parameter, scales = 'free')+theme(axis.title.x=element_blank(),
                                                      axis.text.x=element_blank(),
                                                      axis.ticks.x=element_blank())
}


#------------------------------------------#
#  1. Plot COC data and look for outliers  #
#------------------------------------------#

# params2 <- c('Zinc - Water - Total',
#             'Copper - Water - Total',
# #            'Nitrite-Nitrate - Water - Dissolved',
#             'Total Phosphorus - Water - Total',
#             'Total Suspended Solids - Water - Total',
#             "Zinc - Water - Dissolved" )


#this set of scatterplots shows three obvious outliers: one each for nitrite/nitrate, phosphorus, and TSS
scatter_cocs(s8data[which(s8data$parameter %in% params),],'All Observations')

#remove outliers  
outlierParams <- c("Total Kjeldahl Nitrogen - Water - Total") #, "Nitrite-Nitrate - Water - Dissolved")  #new version, with PIE_HDR and PIE_LDR gone
#outlierParams <- c("Total Suspended Solids - Water - Total", "Total Phosphorus - Water - Total", "Nitrite-Nitrate - Water - Dissolved")
#This removes the highest values 
outlierVals <-
  top_n(group_by(s8data[which(s8data$parameter %in% outlierParams), ], parameter), 1, result)$result
s8data <- s8data %>%
  group_by(parameter) %>%
  slice(which(!(
    parameter %in% outlierParams & result %in% outlierVals
  ))) %>%
  ungroup()

#replot
scatter_cocs(s8data[which(s8data$parameter %in% params),],'All Observations - Outliers Removed')
#note: there is one high data point for kjeldahl nitrogen, which may cause problems.  remove it later if so...


#--------------------#
# Log-transform data #
# Generate Q-Q plots #
#--------------------#

#make a function for scatter plots
qqplot_cocs <- function(df.coc, title) {
  p <- ggplot(df.coc, aes(sample=log(result))) + stat_qq() + stat_qq_line(aes(color="grey")) + 
    theme(legend.position="none") + labs(   #### make this into a qq-plot!
      title = title,
      subtitle = "Data collected 2009-2013",
      caption =
        " Data source: Ecology, 2015",
      x = "Observations"
  )
  p + facet_wrap( ~ parameter, scales = 'free')+theme(axis.title.x=element_blank(),
                                                      axis.text.x=element_blank(),
                                                      axis.ticks.x=element_blank())
}

#this set of scatterplots shows three obvious outliers: one each for nitrite/nitrate, phosphorus, and TSS
qqplot_cocs(s8data[which(s8data$parameter %in% params),],'Log-Normal Q-Q Plots')

# par(mfrow=c(3,2))
# x <- s8data[which(s8data$parameter %in% params[1]),]$result
# qqPlot(x, y=NULL, "norm")
# qqPlot(x, y=NULL, "lnorm", estimate.params=T)
# qqPlot(x, y=NULL, "gamma", estimate.params=T)
# qqPlot(x, y=NULL, "gammaAlt", estimate.params=T)
# qqPlot(x, y=NULL, "logis", estimate.params=T)
# qqPlot(x, y=NULL, "lnormAlt", estimate.params=T)
# 
# 
# 
# #zinc
# shapiro.test(s8data[which(s8data$parameter %in% params[1]),]$result)$p.val
# shapiro.test(log(s8data[which(s8data$parameter %in% params[1]),]$result))$p.val
# 
# #copper
# shapiro.test(log(s8data[which(s8data$parameter %in% params[2]),]$result))$p.val
# 
# #TSS
# shapiro.test(log(s8data[which(s8data$parameter %in% params[4]),]$result))$p.val
# 
# #Phosphorus
# shapiro.test(log(s8data[which(s8data$parameter %in% params[3]),]$result))$p.val
# 
# #TKN
# shapiro.test(log(s8data[which(s8data$parameter %in% params[5]),]$result))$p.val
# 
# 
# #make a function for scatter plots
# qqplot_cocs <- function(df.coc, title) {
#   p <- ggplot(df.coc, aes(sample=(result))) + stat_qq() + stat_qq_line(aes(color="grey")) + 
#     theme(legend.position="none") + labs(   #### make this into a qq-plot!
#       title = title,
#       subtitle = "Data collected 2009-2013",
#       caption =
#         " Data source: Ecology, 2015",
#       x = "Observations"
#     )
#   p + facet_wrap( ~ parameter, scales = 'free')+theme(axis.title.x=element_blank(),
#                                                       axis.text.x=element_blank(),
#                                                       axis.ticks.x=element_blank())
# }
# 
# #this set of scatterplots shows three obvious outliers: one each for nitrite/nitrate, phosphorus, and TSS
# qqplot_cocs(s8data[which(s8data$parameter %in% params),],'Normal Q-Q Plots')
# 
# 
# 
# 

#------------------------------------------#
#  Merge COC data with Spatial Predictors  #
#------------------------------------------#

#predictors that we will use in the model
z_final <- sp_final 

#merge COC data with spatial predictor data based on [watershed] location
s8data_sp <- dplyr::left_join(s8data, z_final, by=c("location_id"="location") )


#----------------------------------------#
#  Now lets look at precip, ADD, season  #
#----------------------------------------#

z <- s8data %>%
  dplyr::mutate(agency=as.factor(agency),
                season=as.factor(season),
                month=as.factor(month)) %>%
  dplyr::select(c(daymet_precip, daymet_2day, daymet_3day, daymet_5day, daymet_7day,
                  daymet_10day, daymet_14day, daymet_21day, daymet_28day,
                  antecedant_dry_days, agency, season, month, mPrecip))


#plot data to visualize; can skip this if inspection has already been completed
if(F) {
  pairs(z, lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)
  
  z2 <- z %>%
    dplyr::select(-c(daymet_2day, daymet_5day, daymet_10day))
  pairs(z2, lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)
  #looks like the best precip predictors are daymet, 3day, 7day, 14day, 21day and 28day
  #  the longer time periods may capture season and month (look at plots rather than numbers)
  
  par(mfrow=c(6,4))
  hist(s8data$daymet_28day)           
  hist(log(s8data$daymet_28day))
  hist(sqrt(s8data$daymet_28day))
  hist((s8data$daymet_28day)^(1/3))
  #hist((s8data$daymet_28day)^(1/4))
  hist(s8data$daymet_21day)           
  hist(log(s8data$daymet_21day))
  hist(sqrt(s8data$daymet_21day))
  hist((s8data$daymet_21day)^(1/3))
  hist(s8data$daymet_14day)           
  hist(log(s8data$daymet_14day))
  hist(sqrt(s8data$daymet_14day))
  hist((s8data$daymet_14day)^(1/3))
  hist(s8data$daymet_7day)           
  hist(log(s8data$daymet_7day))
  hist(sqrt(s8data$daymet_7day))
  hist((s8data$daymet_7day)^(1/3))
  hist(s8data$daymet_3day)           
  hist(log(s8data$daymet_3day))
  hist(sqrt(s8data$daymet_3day))
  hist((s8data$daymet_3day)^(1/3))
  hist(s8data$daymet_precip)           
  hist(log(s8data$daymet_precip))
  hist(sqrt(s8data$daymet_precip))
  hist((s8data$daymet_precip)^(1/3))
  #daymet precip data should be square-root transformed, then standardized!
  
  #monthly precip - not sure what to do with this... Try all options?
  par(mfrow=c(2,2))
  hist(s8data$mPrecip)
  hist(sqrt(s8data$mPrecip))
  hist(log(s8data$mPrecip))
  hist((s8data$mPrecip)^(1/3))
  
  
  par(mfrow=c(1,3))
  hist(s8data$antecedant_dry_days)
  hist(log(s8data$antecedant_dry_days + 1))
  hist(sqrt(s8data$antecedant_dry_days))
  #antecedant dry days data can be log+1-transformed, or square-root transformed
}


#make the lowest daymet_2day, 3day and 5day values = 0  (no idea why some are < 0)
s8data_sp$daymet_2day[which(s8data_sp$daymet_2day<0)] <- 0
s8data_sp$daymet_3day[which(s8data_sp$daymet_3day<0)] <- 0
s8data_sp$daymet_5day[which(s8data_sp$daymet_5day<0)] <- 0

#modify s8data_sp for precip and ADD transformations; also standardize these data
s8data_sp <- s8data_sp %>%
  dplyr::mutate(daymet_precipCR=(daymet_precip)^(1/3),  #cubic root 
                daymet_3dayCR=(daymet_3day)^(1/3),
                daymet_7dayCR=(daymet_7day)^(1/3),
                daymet_14dayCR=(daymet_14day)^(1/3),
                daymet_21dayCR=(daymet_21day)^(1/3),
                daymet_28dayCR=(daymet_28day)^(1/3),
                daymet_precipSR=(daymet_precip)^(1/2),  #square root
                daymet_3daySR=(daymet_3day)^(1/2),
                daymet_7daySR=(daymet_7day)^(1/2),
                daymet_14daySR=(daymet_14day)^(1/2),
                daymet_21daySR=(daymet_21day)^(1/2),
                daymet_28daySR=(daymet_28day)^(1/2),
                daymet_14dayLog=log(daymet_14day),  #log
                daymet_21dayLog=log(daymet_21day),
                daymet_28dayLog=log(daymet_28day),
                daymet_28dayFR=(daymet_28day)^(1/4),  #fourth root
                mPrecipLog=log(mPrecip),
                mPrecipSR=sqrt(mPrecip),
                mPrecipCR=(mPrecip)^(1/3),
                antecedant_dry_days=log(antecedant_dry_days+1)) %>%
  dplyr::mutate(daymet_precip_std=(daymet_precip - mean(daymet_precip))/sd(daymet_precip),
                daymet_3day_std=(daymet_3day - mean(daymet_3day))/sd(daymet_3day),
                daymet_7day_std=(daymet_7day - mean(daymet_7day))/sd(daymet_7day),
                daymet_14day_std=(daymet_14day - mean(daymet_14day))/sd(daymet_14day),
                daymet_21day_std=(daymet_21day - mean(daymet_21day))/sd(daymet_21day),
                #try some alternates for 28-day precip
                daymet_28dayLog_std=(daymet_28dayLog - mean(daymet_28dayLog))/sd(daymet_28dayLog),
                daymet_28daySR_std=(daymet_28daySR - mean(daymet_28daySR))/sd(daymet_28daySR),
                daymet_28dayCR_std=(daymet_28dayCR - mean(daymet_28dayCR))/sd(daymet_28dayCR),
                daymet_28dayFR_std=(daymet_28dayFR - mean(daymet_28dayFR))/sd(daymet_28dayFR),
                antecedant_dry_days_std=(antecedant_dry_days - mean(antecedant_dry_days))/sd(antecedant_dry_days)) %>%
  dplyr::select(-c(daymet_2day, daymet_5day, daymet_10day))


#save mean & sd of the daymet precip data for later use
std_precip <- s8data_sp %>%
  select(daymet_precip, daymet_14day, daymet_21day) %>%
  gather(key="dur", value="precip_mm", 1:3) %>%
  group_by(dur) %>%
  summarize(mean=mean(precip_mm), 
            sd=sd(precip_mm))
  
write.csv(std_precip, here("..", "processed_data", "daymet_precip_standardization_values.csv"), row.names=FALSE)


#----------------------#
#  Lets examine COC's  #  
#----------------------#

# What percentage of data are ND for each COC?
num_samples <- num_nd <- rep(NA, length(params))
for(i in 1:length(params)) {
  num_samples[i] <- length(which(s8data_sp$parameter == params[i]))
  num_nd[i] <- length(which(s8data_sp$parameter == params[i] & s8data_sp$nondetect_flag==TRUE))
}
percent_nd <- round(num_nd/num_samples, 2)
names(percent_nd) <- params      #Zinc, Lead, Copper, Phosphorus, TSS and Nitrite/Nitrate are <0.02
                                 #Total PAH=0.30, Total Kjeldahl Nitrogen=0.10


#----------------------------------------------#
#  Save COC Dataframe with Spatial Predictors  #
#----------------------------------------------#

write.csv(s8data_sp, here("..", "processed_data", "s8data_with_spatial_predictors.csv"), row.names=FALSE)


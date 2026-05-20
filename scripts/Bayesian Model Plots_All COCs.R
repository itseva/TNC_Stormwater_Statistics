# This script calls functions to plot Bayesian model outputs for each COC.  See script "Bayesian
#   Model Plot Functions.R" for functions.

# Author: Eva Dusek Jennings
# Revised: Nov 29, 2022
# Revised: Apr 22, 2025 -- updated Zinc to use greenery model rather than sqrt_traffic + paved
# Revised: Oct 21, 2025 -- updated Zinc to use not_greenBE model
#-------------------------------------------------------------------------------------------

source("Bayesian Model Plot Functions.R")  #script with plotting functions for Bayesian figures
source("Bayesian Credibility Interval Function.R")  #script with credibility interval (& CI plotting) functions

library(cowplot)
library(ggpubr)

theme_set(theme_tidybayes() + panel_border() + grids())

#read in standardized values; this will be used in plots of prediction intervals vs raw data
std.vals <- read.csv(file="../processed_data/spatial_predictor_standardization_values.csv")
colnames(std.vals) <- c("predictor", "mean", "sd")

#read in backtransform powers for spatial predictors, used for plotting raw spatial predictor values
xform.pwr <- read.csv("../processed_data/spatial_predictor_backtransform_power.csv", header=TRUE)

# #read (raw) watershed reductions (PSAU version) csv from stormwaterheatmap GitHub page
# wr_psau <- read.csv("https://raw.githubusercontent.com/stormwaterheatmap/TNC_Stormwater_Statistics/main/data/watershed_reductions/psau-predictors.csv")
# wr <- wr_psau[-which(wr_psau$AU_ID==0),] %>%
#   mutate(sqrt_traffic=traffic)   #### get rid of this once we sort out what "traffic" actually means!


#----------------#
#  Total Copper  #
#----------------#

#load data 
load(file="../results/Bayesian_Copper.RData")  #Bayesian model
load(file="../results/Frequentist_Copper Models.RData")  #frequentist mixed effects model, for coc2 dataframe
#load(file="../results/Copper_80_CI_wr_psau.Rdata")  #credibility intervals for all Puget Sound watersheds

###   PLOT 1   ###
# posterior densities and chain traces for important predictors; max of 5 predictors per page
#get_variables(Cu.brm)
plot(Cu.brm, variable=c("b_Intercept", "b_rain", "b_summer1", "b_sqrt_traffic", "b_devAge2"))  #recognizable covariates
plot(Cu.brm, variable=c("b_sigma_Intercept", "sd_agency__Intercept", "sd_agency:location__Intercept",
                        "sd_location__sigma_Intercept", "nu"))   #covariates dealing with variability

###   PLOT 2   ###
# Observed vs Estimated - colored dots (by location) with thin grey lines = 95% interval                 
obs.vs.pred.1(Cu.brm, Cu.coc2, "Copper")

###   PLOT 3   ###
#Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot(Cu.brm, Cu.coc2, "Copper", "sqrt_traffic", "Reds")
p2 <- obsPredPlot(Cu.brm, Cu.coc2, "Copper", "devAge2", "Greens")
p3 <- obsPredPlot(Cu.brm, Cu.coc2, "Copper", "rain", "Blues")
p4 <- obsPredPlot2(Cu.brm, Cu.coc2, "Copper", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

obsPredPlot(Cu.brm, Cu.coc2, "Copper", "dry", "Oranges")  #are outlier points explained by antecedant dry days? (they're not)

###   PLOT 4   ###
#Credible Intervals vs Raw Data plots  
credInt.vs.rawPreds.2preds(Cu.coc2, Cu.brm, "Total Copper", preds=c("sqrt_traffic", "devAge2"))

###   PLOT 5   ###
#Global Intercept (+/- 95% CI) with Distributions of Location-Specific Intercepts
plotIntercepts.global.location(Cu.brm, xlims=c(0.7, 4.2)) 

# ###   PLOT 6   ###
# #plot transformed & standardized median values (black) with upper/ lower 80% CI's 
# plotBayesianCIs(Cu.CI, "Copper", c("sqrt_traffic", "devAge2"))
# 
# ###   PLOT 7   ###
# #plot raw median values (black) with upper/ lower 80% CI's
# plotBayesianCIs.raw(Cu.CI, "Copper", c("sqrt_traffic", "devAge2"))

#remove large objects from memory
rm(Cu.brm, Cu.CI, Cu.coc2) 


#-------#
#  TSS  #
#-------#

#load data 
load(file="../results/Bayesian_TSS.RData")  #Bayesian model
load(file="../results/Frequentist_TSS Models.RData")  #frequentist mixed effects model, for coc2 dataframe
#load(file="../results/TSS_80_CI_wr_psau.Rdata")  #credibility intervals for all Puget Sound watersheds

###   PLOT 1   ###
# posterior densities and chain traces for important predictors; max of 5 predictors per page
#get_variables(TSS.brm)
plot(TSS.brm, variable=c("b_Intercept", "b_rain", "b_sqrt_traffic", "b_devAge2"))  #recognizable covariates
plot(TSS.brm, variable=c("b_sigma_Intercept", "sd_agency__Intercept", "sd_agency:location__Intercept",
                        "sd_location__sigma_Intercept"))   #covariates dealing with variability

###   PLOT 2   ###
# Observed vs Estimated - colored dots (by location) with thin grey lines = 95% interval                 
obs.vs.pred.1(TSS.brm, TSS.coc2, "TSS")

###   PLOT 3   ###
#Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot(TSS.brm, TSS.coc2, "TSS", "sqrt_traffic", "Reds")
p2 <- obsPredPlot(TSS.brm, TSS.coc2, "TSS", "devAge2", "Greens")
p3 <- obsPredPlot(TSS.brm, TSS.coc2, "TSS", "rain", "Blues")
grid.arrange(p1, p2, p3, nrow=2, ncol=2)

###   PLOT 4   ###
#Credible Intervals vs Raw Data plots  
credInt.vs.rawPreds.2preds(TSS.coc2, TSS.brm, "Total Suspended Solids", preds=c("sqrt_traffic", "devAge2"))

###   PLOT 5   ###
#Global Intercept (+/- 95% CI) with Distributions of Location-Specific Intercepts
plotIntercepts.global.location(TSS.brm, xlims=c(8.8,12))

# ###   PLOT 6   ###
# #plot transformed & standardized median values (black) with upper/ lower 80% CI's 
# plotBayesianCIs(TSS.CI, "TSS", c("sqrt_traffic", "devAge2"))
# 
# ###   PLOT 7   ###
# #plot raw median values (black) with upper/ lower 80% CI's
# plotBayesianCIs.raw(TSS.CI, "TSS", c("sqrt_traffic", "devAge2"))

#remove large objects from memory
rm(TSS.brm, TSS.CI, TSS.coc2) 


#--------------------#
#  Total Phosphorus  #
#--------------------#

#load data 
load(file="../results/Bayesian_Phosphorus.RData")  #Bayesian model
load(file="../results/Frequentist_Total Phosphorus Models.RData")  #frequentist mixed effects model, for coc2 dataframe
#load(file="../results/Phosphorus_80_CI_wr_psau.Rdata")  #credibility intervals for all Puget Sound watersheds

###   PLOT 1   ###
# posterior densities and chain traces for important predictors; max of 5 predictors per page
#get_variables(P.brm)
plot(P.brm, variable=c("b_Intercept", "b_rain", "b_summer1", "b_sqrt_CO2_road"))  #recognizable covariates
plot(P.brm, variable=c("b_sigma_Intercept", "sd_agency__Intercept", "sd_agency:location__Intercept",
                        "sd_location__sigma_Intercept", "nu"))   #covariates dealing with variability

###   PLOT 2   ###
# Observed vs Estimated - colored dots (by location) with thin grey lines = 95% interval                 
obs.vs.pred.1(P.brm, P.coc2, "Phosphorus")

###   PLOT 3   ###
#Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot(P.brm, P.coc2, "Phosphorus", "sqrt_CO2_road", "Reds")
p.blank <- textGrob("")
p2 <- obsPredPlot(P.brm, P.coc2, "Phosphorus", "rain", "Blues")
p3 <- obsPredPlot2(P.brm, P.coc2, "Phosphorus", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
lay <- rbind(c(1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA),  #custom layout to make up for long predictor name (sqrt_CO2_road plot was narrower to accomodate the name in the legend)
             c(2,2,2,2,2,2,2,3,3,3,3,3,3,3))
grid.arrange(p1, p2, p3, layout_matrix=lay)

###   PLOT 4   ###
#Credible Intervals vs Raw Data plots  
credInt.vs.rawPreds.1pred(P.coc2, P.brm, "Total Phosphorus", preds="sqrt_CO2_road")

###   PLOT 5   ###
#Global Intercept (+/- 95% CI) with Distributions of Location-Specific Intercepts
plotIntercepts.global.location(P.brm, xlims=c(2.4,6.8)) 

# ###   PLOT 6   ###
# #plot transformed & standardized median values (black) with upper/ lower 80% CI's 
# plotBayesianCIs(P.CI, "Phosphorus", "sqrt_CO2_road")
# 
# ###   PLOT 7   ###
# #plot raw median values (black) with upper/ lower 80% CI's
# plotBayesianCIs.raw(P.CI, "Phosphorus", "sqrt_CO2_road")

#remove large objects from memory
rm(P.brm, P.CI, P.coc2) 


#--------------#
#  Total Zinc  #     #### NOTE: waiting on update to psau_wr (including sqrt_CO2_tot) so that Zn CI's can be run
#--------------#

#load data 
load(file="../results/Bayesian_TotalZinc_not_greenBE.RData")  #Bayesian model
load(file="../results/Frequentist_Total Zinc Models_notGreenBE.RData")  #frequentist mixed effects model, for coc2 dataframe

###   PLOT 1   ###
# posterior densities and chain traces for important predictors; max of 5 predictors per page
#get_variables(totZn.brm)
plot(totZn.brm, variable=c("b_Intercept", "b_rain", "b_summer1", "b_sqrt_CO2_transport"), bins=100)
plot(totZn.brm, variable=c("b_not_greenBE", "b_rain:not_greenBE"), bins=100)
plot(totZn.brm, variable=c("b_sigma_Intercept", "sd_agency__Intercept", "sd_agency:location__Intercept",
                         "sd_location__sigma_Intercept", "nu"), bins=100)   #covariates dealing with variability
# plot(totZn.brm, variable=c("b_Intercept", "b_rain", "b_summer1", "b_sqrt_traffic"))
# plot(totZn.brm, variable=c("b_paved", "b_rain:paved"))  #recognizable covariates
# plot(totZn.brm, variable=c("b_sigma_Intercept", "sd_agency__Intercept", "sd_agency:location__Intercept",
#                            "sd_location__sigma_Intercept", "nu"))   #covariates dealing with variability


###   PLOT 2   ###
# Observed vs Estimated - colored dots (by location) with thin grey lines = 95% interval                 
obs.vs.pred.1(totZn.brm, totZn.coc2, "Total Zinc")

###   PLOT 3   ###
#Observed vs Predicted plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot(totZn.brm, totZn.coc2, "Total Zinc", "not_greenBE", "Reds")
p2 <- obsPredPlot(totZn.brm, totZn.coc2, "Total Zinc", "sqrt_CO2_transport", "Greens")
p3 <- obsPredPlot(totZn.brm, totZn.coc2, "Total Zinc", "rain", "Blues")
p4 <- obsPredPlot2(totZn.brm, totZn.coc2, "Total Zinc", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
# grid.arrange(p1, p3, p4, nrow=2, ncol=2)
# lay <- rbind(c(1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA),  #custom layout to make up for long predictor name (sqrt_CO2_road plot was narrower to accomodate the name in the legend)
#              c(3,3,3,3,3,3,3,4,4,4,4,4,4,4))
# grid.arrange(p1, p3, p4, layout_matrix=lay)

grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)


# p1 <- obsPredPlot(totZn.brm, totZn.coc2, "Total Zinc", "sqrt_traffic", "Reds")
# p2 <- obsPredPlot(totZn.brm, totZn.coc2, "Total Zinc", "paved", "Greens")
# p3 <- obsPredPlot(totZn.brm, totZn.coc2, "Total Zinc", "rain", "Blues")
# p4 <- obsPredPlot2(totZn.brm, totZn.coc2, "Total Zinc", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
# grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

###   PLOT 4   ###
#Credible Intervals vs Raw Data plots  
credInt.vs.rawPreds.2preds(totZn.coc2, totZn.brm, "Total Zinc", preds=c("not_greenBE", "sqrt_CO2_transport"))
#credInt.vs.rawPreds.2preds(totZn.coc2, totZn.brm, "Total Zinc", preds=c("sqrt_traffic", "paved"))

#credInt.vs.rawPreds.2preds(TSS.coc2, TSS.brm, "Total Suspended Solids", preds=c("sqrt_traffic", "devAge2"))


###   PLOT 5   ###
#Global Intercept (+/- 95% CI) with Distributions of Location-Specific Intercepts
plotIntercepts.global.location(totZn.brm, xlims=c(2.4, 5.6)) 

# ###   PLOT 6   ###
# #plot transformed & standardized median values (black) with upper/ lower 80% CI's 
# plotBayesianCIs(totZn.CI, "totZn", c("greenery"))
# plotBayesianCIs(totZn.CI, "totZn", c("sqrt_traffic", "devAge2"))
# 

# ###   PLOT 7   ###
# #plot raw median values (black) with upper/ lower 80% CI's
# plotBayesianCIs.raw(totZn.CI, "totZn", c("sqrt_traffic", "devAge2"))

#remove large objects from memory
rm(totZn.brm, #totZn.CI, 
   totZn.coc2)


#---------------------------#
#  Total Kjeldahl Nitrogen  #        #######  Note: the two censored data points (detection limit = 30ppm) seem suspicious, given that
#---------------------------#                       all other samples at those locations have much higher values.  both were collected on 11/29/2010

#best Bayesian model uses censored methods WITHIN Bayesian context, and has Student t-distribution for errors, and
#  agency as a variance covariate

#load data 
load(file="../results/Bayesian_TotalKjeldahlNitrogen.RData")  #Bayesian model
load(file="../results/Frequentist_Total Kjeldahl Nitrogen Models_censtat.RData")  #frequentist mixed effects model, for coc2 dataframe
#load(file="../results/TotalKjeldahlNitrogen_80_CI_wr_psau.Rdata")  #credibility intervals for all Puget Sound watersheds

###   PLOT 1   ###
# posterior densities and chain traces for important predictors; max of 5 predictors per page
#get_variables(TKN.brm)
plot(TKN.brm, variable=c("b_Intercept", "b_rain", "b_summer1", "b_sqrt_traffic", "b_devAge2"))  #recognizable covariates
plot(TKN.brm, variable=c("b_sigma_Intercept", "sd_agency__Intercept", "sd_agency:location__Intercept",
                         "sd_agency__sigma_Intercept", "nu"))   #covariates dealing with variability

###   PLOT 2   ###
# Observed vs Estimated - colored dots (by location) with thin grey lines = 95% interval; censored data shown in triangles
#obs.vs.pred.cen(TKN.brm, TKN.coc2, "TKN")
obs.vs.pred.DL(TKN.brm, TKN.coc2, "TKN")    #this one is probably a better choice, as it doesn't mix frequentist ROS-obtained values with 
                                            #  the Bayesian-generated censored values (which we may not be able to extract?)

###   PLOT 3   ###
#Observed vs Predicted plots for multiple predictors, with colors showing a selected predictor.
#  uncensored points are shown as filled circles, while censored points are shown as filled triangles.
#  censored points are shown at the detection limit
p1 <- obsPredPlot.cen(TKN.brm, TKN.coc2, "TKN", "sqrt_traffic", "Reds")
p2 <- obsPredPlot.cen(TKN.brm, TKN.coc2, "TKN", "devAge2", "Greens")
p3 <- obsPredPlot.cen(TKN.brm, TKN.coc2, "TKN", "rain", "Blues")
p4 <- obsPredPlot2.cen(TKN.brm, TKN.coc2, "TKN", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

###   PLOT 4   ###
#Credible Intervals vs Raw Data plots  
credInt.vs.rawPreds.2preds(TKN.coc2, TKN.brm, "TKN", preds=c("sqrt_traffic", "devAge2"))

###   PLOT 5   ###
#Global Intercept (+/- 95% CI) with Distributions of Location-Specific Intercepts
plotIntercepts.global.location(TKN.brm, xlims=c(5.7,7.5))

# ###   PLOT 6   ###
# #plot transformed & standardized median values (black) with upper/ lower 80% CI's 
# plotBayesianCIs(TKN.CI, "TKN", c("sqrt_traffic", "devAge2"))
# 
# ###   PLOT 7   ###
# #plot raw median values (black) with upper/ lower 80% CI's
# plotBayesianCIs.raw(TKN.CI, "TKN", c("sqrt_traffic", "devAge2"))

#remove large objects from memory
rm(TKN.brm, TKN.CI, TKN.coc2) 


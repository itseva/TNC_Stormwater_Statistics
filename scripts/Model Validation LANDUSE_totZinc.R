# This script builds on the script "Model Validation v3.R" by including a model validation of the
#   Zinc data using the LANDUSE categories (COM, IND, LDR, HDR)

# In line 196 (building mv.full_res_pred) we remove the data points where 1-day precip was 0 for the day

# Author: Eva Dusek Jennings
# Date:   Oct 21, 2025
#------------------------------------------------------------------------------------

rm(list=ls(all=T))

library(ggplot2)
library(ggpubr)
library(tidyr)
library(brms)
library(dplyr)
library(cowplot)


#load prepared model validation data; run script with functions required for this project
load("../data/model_validation_prepped_data.RData")
source("Model Validation Functions.R")  #run script with functions that are used in this script

#load RData file with info from Bayesian LANDUSE analysis for totZinc (including coc2 objects), using the brms model with Location Names from Model Validation
load(file="../results/Best Fit Bayesian LANDUSE Model Using Model Validation Location Names_totZinc.RData")

#read in model validation predictor data, and scaling/centering values. validation predictor data are raw, and require transformation and scaling/centering
vp <- read.csv(file="../data/validation landuses.csv")  #in the csv, make sure columns are numbers, with no commas to denote thousands!
#stdvals <- read.csv(file="../processed_data/spatial_predictor_standardization_values.csv")


#----------------------------------#
#  Prep Validation Predictor Data  #
#----------------------------------#

head(vp)
#head(stdvals)

# #these situations don't logically make sense.  Fix them!
# vp$dev_1975_1990[which(vp$Name=="WSDOT_I5_MP210.85")] <- 1   #this square of highway is fully paved, but image file indicates that dev_1975_1990=0.36 and all other dev's=0.  Fix it!
# vp$dev_pre_1975[which(vp$Name=="WSDOT_I5_MP197.35")] <- 1   #this square of highway is fully paved, but image file indicates that dev_pre_1975=0.89 and all other dev's=0.  Fix it!
# vp$imperv_ground[which(vp$Name=="MLK Street Sweep_SS5")] <- mean(vp$imperv_ground[which(vp$Name %in% c("MLK Street Sweep_SS2", "MLK Street Sweep_SS3", "MLK Street Sweep_SS4"))])  
# #set SS5 paved to mean of SS2, SS3, SS4; all should be 100%, but are not...  Maybe tree cover is obscuring it in the satelite photo?

#prep the validation predictors by calculating devAge and transforming where applicable.  Also, remove locations where predictors make no sense
vp.mod <- vp %>%
  mutate(landuse = case_when(S8_equiv=="LDR" ~ "1.LDR",
                             S8_equiv=="HDR" ~ "2.HDR",
                             S8_equiv=="COM" ~ "3.COM",
                             S8_equiv=="IND" ~ "4.IND") ) %>%
#  filter(! Name== "I5-Bridge_v2") %>%   #remove the wrong version of the I5 Bridge polygon (the right one is a sub-section of the v2 one, per e-mail from Dylan Ahearn Nov 15, 2024)
  mutate(Location = sub("^[^_]*_", "", Name) ) %>%  #only remove the project name (before the first "_" in the Name column))
  mutate(Location = case_when(Project=="Tacoma S8C" ~ paste("OF", Location, sep=""),  #add "OF" (outfall) in front of Tac S8C location names
                              TRUE ~ Location)) %>%
  select(Project, Location, S8_equiv, landuse) %>%
  #remove predictors that seem highly out of whack with the actual situation
  filter(! Location=="520-W",  #520-W should be fully paved and was built in 1963. The image file indicates paved=0 and devAge2=0 (unpaved, undeveloped)
         ! Location=="SR5_Maytown"   #SR5-Maytown should be on a highway, but the image file indicates sqrt_traffic=0 and paved=NA
  )

#set order of levels of Location for the validation predictors
Loc_levels <- vp.mod$Location[c(1:6, 8:11, 16:24, 12:15, 25:29, 7)]
vp.mod$Location <- factor(vp.mod$Location, levels=Loc_levels)  #make Location a factor with a set order of levels

#comp_res projects: "MLK Street Sweep", "Tacoma S8C", "Lake WA PFAS", "Hwy DB", "WSDOT", "I5 Bridge"
#grab_res projects: "Coupeville", "LahtiDr", "GardenEd", "MasonCo"

vp.std <- vp.mod  #because the landuses don't need to be standardized, set vp.std equal to vp.mod; this prevents having to edit lots of code below.


# vp.std <- vp.mod %>%
#   mutate(sqrt_CO2_road=(sqrt_CO2_road - stdvals$mean[which(stdvals$X %in% "sqrt_CO2_road")])/stdvals$sd[which(stdvals$X %in% "sqrt_CO2_road")],
#          sqrt_CO2_cmv_rail=(sqrt_CO2_cmv_rail - stdvals$mean[which(stdvals$X %in% "sqrt_CO2_cmv_rail")])/stdvals$sd[which(stdvals$X %in% "sqrt_CO2_cmv_rail")],
#          sqrt_traffic=(sqrt_traffic - stdvals$mean[which(stdvals$X %in% "sqrt_traffic")])/stdvals$sd[which(stdvals$X %in% "sqrt_traffic")],
#          devAge2=(devAge2 - stdvals$mean[which(stdvals$X %in% "devAge2")])/stdvals$sd[which(stdvals$X %in% "devAge2")],
#          paved=(paved - stdvals$mean[which(stdvals$X %in% "paved")])/stdvals$sd[which(stdvals$X %in% "paved")],
#          greenery_bareEarth=(greenery_bareEarth - stdvals$mean[which(stdvals$X %in% "greenery_bareEarth")])/stdvals$sd[which(stdvals$X %in% "greenery_bareEarth")],
#          not_greenBE=(not_greenBE - stdvals$mean[which(stdvals$X %in% "not_greenBE")])/stdvals$sd[which(stdvals$X %in% "not_greenBE")]
#   )

# #summary table of min/ max for each predictor
# vp.std %>%
#   summarize(across(-c(Project, Location), range)) %>%
#   mutate(value = c("min", "max"), .before = 1)


# #make a new object with capped traffic and paved values.  Traffic max is 14.75, paved max is 3.078
# cap_level <- 2
# #cap_level_paved <- 1
# vp.cap <- vp.std %>%
#   mutate(sqrt_traffic = case_when(sqrt_traffic > cap_level ~ cap_level,
#                                   TRUE ~ sqrt_traffic),
#          paved = case_when(paved > cap_level ~ cap_level, #cap_level_paved,
#                            TRUE ~ paved),
#          sqrt_CO2_road = case_when(sqrt_CO2_road > cap_level ~ cap_level,
#                                    TRUE ~ sqrt_CO2_road),
#          sqrt_CO2_cmv_rail = case_when(sqrt_CO2_cmv_rail > cap_level ~ cap_level,
#                                        TRUE ~ sqrt_CO2_cmv_rail),
#          devAge2 = case_when(devAge2 > cap_level ~ cap_level,
#                              TRUE ~ devAge2),
#          greenery_bareEarth = case_when(greenery_bareEarth > cap_level ~ cap_level,
#                                         TRUE ~ greenery_bareEarth),
#          not_greenBE = case_when(not_greenBE > cap_level ~ cap_level,
#                                  TRUE ~ not_greenBE))
# 

vp.cap <- vp.std #because the landuses don't need to be capped, set vp.cap equal to vp.std; this prevents having to edit lots of code below.


#-----------------------------------#
#  Landscape Predictor Value Plots  #
#-----------------------------------#

#plot validation predictor values for landuse
ggplot() + 
  geom_point(aes(x=Location, y=landuse, colour="landuse"), data=vp.std) +  #add ln-transformed data points from this study
  ylab("standardized predictor values") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))



#-------------------------------------------------#
#  Add Predictor Values to Model Validation Data  #
#-------------------------------------------------#

comp_res.mod <- comp_res %>%
  mutate(Location = case_when(Project=="I5 Bridge" ~ "I5 Bridge",
                              TRUE ~ Location)) %>%
  filter(! is.na(Result),  #remove all results that are "NA"
         ! Project=="Hwy DB")

#comp_res.mod[which(comp_res.mod$Project=="I5 Bridge"),]         

#add predictor values to the comp_res and grab_res dataframes
comp_res_pred <- left_join(x=comp_res.mod, y=vp.cap, join_by(Project, Location) )
grab_res_pred <- left_join(x=grab_res, y=vp.cap, join_by(Project, Location) )
grab_res_4plus_pred <- left_join(x=grab_res_4plus, y=vp.cap, join_by(Project, Location) )
grab_res_3_pred <- left_join(x=grab_res_3, y=vp.cap, join_by(Project, Location) )

# #which locations are missing some predictor data
# unique(comp_res_pred$Location[which(is.na(comp_res_pred$paved))] )
# unique(grab_res_pred$Location[which(is.na(grab_res_pred$paved))] )
# 
# #get rid of any rows that don't have predictor data
# comp_res_pred <- comp_res_pred[!is.na(comp_res_pred$paved),]
# grab_res_pred <- grab_res_pred[!is.na(grab_res_pred$paved),]
# grab_res_4plus_pred <- grab_res_4plus_pred[!is.na(grab_res_4plus_pred$paved),]
# comp_res_3_pred <- grab_res_3_pred[!is.na(grab_res_3_pred$paved),]


# #all model validation data, including both composite samples AND discrete samples
# mv.dat.full <- rbind(comp_res_pred, grab_res_pred) %>%
#   mutate(agency=NA,
#          result=log(Result)) %>%
#   rename(location=Location)

#dataframe of the predictors for each location; does not include stormwater outfall data (but, unlike vp.cap, DOES include analyte!)
mv.full_res_pred <- rbind(comp_res_pred, grab_res_pred) %>%
  mutate(Project = as.factor(Project),
         Location = factor(Location, levels=Loc_levels),  #set order of levels of Location for the validation predictors
         location = Location,
         result = log(Result)) %>%
  mutate(agency = case_when(Project=="Tacoma S8C" ~ "Tacoma",
                            TRUE ~ Project),
         agency = as.factor(agency)) %>%
  filter(!daymet_precip_std < -1.48)  #For model validation, remove the (Tacoma) samples that were collected when standardized 1-day precip < -1.48 (0mm rainfall)


mv.full_preds <- mv.full_res_pred %>%
  group_by(Project, agency, Location, location, Analyte) %>%
  select(Project, agency, Location, location, Analyte, landuse, S8_equiv) %>%
  ungroup() %>%
  distinct()

# #dataframe of the predictors for each location; does not include stormwater outfall data (but, unlike vp.cap, DOES include analyte!)
# mv.full_preds <- mv.full_res_pred %>%
#   group_by(Project, Location, Analyte) %>%
#   select(Project, Location, Analyte, sqrt_CO2_road, devAge2, sqrt_traffic, paved) %>%
#   ungroup() %>%
#   distinct()

# #delete later
# Zn.mv.full <- mv.full_res_pred %>%
#   filter(Analyte=="Total Zinc") %>%
#   filter(!Result > 600)
# write.csv(Zn.mv.full, "Zinc model validation data for Teddy.csv")

#---------------------------------------#
#  Model Validation Data Frames by COC  #
#---------------------------------------#

Zn.mv.dat <- mv.full_res_pred %>% 
  filter(Analyte=="Total Zinc") %>% 
  mutate(rain=daymet_14day_std,
         summer = as.factor(summer),
         rawResult = Result) %>%
  select(Project, agency, Location, location, Analyte, Unit, rawResult, result, summer, rain, landuse, S8_equiv)



#-------------------------------------------#
#  Fitting vs. Model Validation Data Plots  #
#-------------------------------------------#

ggplot() + 
  geom_point(aes(x=Location, y=rawResult, colour="ModelValidationData"), data=Zn.mv.dat) +  #add raw model validation data 
  geom_point(aes(x=location, y=exp(result), colour="FittingData"), data=totZn.coc2) +  #add raw fitting data
  scale_color_manual(values = c(ModelValidationData = "purple", FittingData = "orange")) +
  ylab("Total Zinc (ug/kg)") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))



#----------------------------#
#  Find and Remove Outliers  #
#----------------------------#

colors <- c("red", "orange", "yellow", "green", "darkgreen", "blue", "purple", "magenta")

#Cleveland Dot Plots - looking for outliers.
dotchart(Zn.mv.dat$result, groups = Zn.mv.dat$Project, pch = 19, col = colors[as.numeric(Zn.mv.dat$Project)],
         xlab = "concentration", main = "Cleveland Dotplot: Zinc")

#Remove outliers from each COC
Zn.mv.dat2 <- Zn.mv.dat %>% filter(!result > 8)  #two super high outliers at Tac OF235



#---------------------------------------#
#  Color Palettes / Plotting Specifics  #
#---------------------------------------#

orangePalette=c(#"#d94701", 
  "#fd8d3c", "#fdbe85", "#feedde")
#orangePalette=c("#d94701", "#fd8d3c", "#fdbe85", "#feedde")

#custom manual color scale for all model validation locations
myColors <- c("#B4443C", "#B8523D", "#C26247", "#CC0033", "#DB94A7", "#D2799A", "#99FFFF", 
              "#C85B86", "#d94701", "#fd8d3c", "#fdbe85", "#CC99FF", "#9966FF", "#6600FF", 
              "purple4", "#A17636", "#C2A547", "#006633", "#297A44", "#37A442", "#518A2E", 
              "#66CC33", "#A2BF40", "#99FF00", "#0099FF", "#0033FF", "#3784A4", "#3660A1", 
              "#323A95")
names(myColors) <- vp.std$Location


#----------------------------------------------------------------#
#  Bar charts with Prediction Intervals & Data Plotted Atop PIs  #
#----------------------------------------------------------------#

#the my.coc2 is only to obtain summer and rainfall information (nothing else, such as location, is used)
#mv.full_Cu_PI <- make_PIs(my.brm=Cu.brm, my.coc2=Cu.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Copper"), c("agency", "location", "sqrt_traffic", "devAge2")])

mv.full_Zn_PI_noPB <- make_PIs(my.brm=LU_newNames_totZn.brm, my.coc2=totZn.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Zinc" & mv.full_preds$location!="PerkinsBluff"), c("agency", "location", "landuse")])

mv.full2 <- plot_PIs_raw(mv.full_Zn_PI_noPB, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),])


mv.full2

mv.full2 + ylim(-50, 1010)  #cut out high zinc PIs & data

#boxplots showing model validation data atop prediction intervals
mv.box2 <- plot_PIs_boxplot_raw(mv.full_Zn_PI_noPB, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], box.width=0.2)
mv.box2

#-----------------------------------#
#  Median per Location Obs vs Pred  #
#-----------------------------------#

# obs.vs.pred.median.mv(Cu.brm, "Copper", Cu.mv.dat2, myColors)
p2.Zn <- obs.vs.pred.median.mv(LU_newNames_totZn.brm, "Zinc", Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], myColors) + xlim(2.9, 6.2) + ylim(2.9, 6.2) #remove Perkins Bluff
p2.Zn


#---------------#
#  Obs vs Pred  #
#---------------#

# obs.vs.pred.mv(Cu.brm, "Copper", Cu.mv.dat2, myColors)
obs.vs.pred.mv(LU_newNames_totZn.brm, "Zinc", Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], myColors) 





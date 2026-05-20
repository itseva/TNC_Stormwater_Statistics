# This script takes model validation test data in the form of dataframes of composite 
#   samples and of grab samples collected on multiple occasions over time, and compares 
#   the test data to model predictions fitted to the S8 data.

# In line 196 (building mv.full_res_pred) we remove the data points where 1-day precip was 0 for the day

# Author: Eva Dusek Jennings
# Date: Apr 3, 2025
#       Apr 28, 2025 - use greenery_bareEarth
#       Oct 21, 2025 - use not_greenBE (based on percent tree cover)
#       Jan 6, 2026 - use new not_greenBE, based on TNC tree cover predictor
#------------------------------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(tidyr)
library(brms)
library(dplyr)
library(cowplot)


#load prepared model validation data; run script with functions required for this project
load("../data/model_validation_prepped_data.RData")
source("Model Validation Functions.R")  #run script with functions that are used in this script

#source("Model Validation_EIM Data.R")  #run script that preps EIM data
#source("Model Validation_Other Data.R")  #run script that preps other data (including MLK street sweeping data)
#source("Model Validation_Data Prep.R")  #run script that generates comp_res and grab_res dataframes from composite data and EIM grab data

#load RData files with info from Bayesian analyses (including coc2 objects), using the brms models with Location Names from Model Validation
load(file="../results/Best Fit Bayesian Models Using Model Validation Location Names_Zinc_notGreenBE.RData")

#read in model validation predictor data, and scaling/centering values. validation predictor data are raw, and require transformation and scaling/centering
vp <- read.csv(file="../data/validation_sheds_reduced_01_16_2025_mod.csv")  #in the csv, make sure columns are numbers, with no commas to denote thousands!
stdvals <- read.csv(file="../processed_data/spatial_predictor_standardization_values.csv")


#----------------------------------#
#  Prep Validation Predictor Data  #
#----------------------------------#

head(vp)
head(stdvals)

#these situations don't logically make sense.  Fix them!
vp$dev_1975_1990[which(vp$Name=="WSDOT_I5_MP210.85")] <- 1   #this square of highway is fully paved, but image file indicates that dev_1975_1990=0.36 and all other dev's=0.  Fix it!
vp$dev_pre_1975[which(vp$Name=="WSDOT_I5_MP197.35")] <- 1   #this square of highway is fully paved, but image file indicates that dev_pre_1975=0.89 and all other dev's=0.  Fix it!
vp$imperv_ground[which(vp$Name=="MLK Street Sweep_SS5")] <- mean(vp$imperv_ground[which(vp$Name %in% c("MLK Street Sweep_SS2", "MLK Street Sweep_SS3", "MLK Street Sweep_SS4"))])  
   #set SS5 paved to mean of SS2, SS3, SS4; all should be 100%, but are not...  Maybe tree cover is obscuring it in the satelite photo?

#prep the validation predictors by calculating devAge and transforming where applicable.  Also, remove locations where predictors make no sense
vp.mod <- vp %>%
  mutate(devAge = 4*dev_pre_1975 + 3*dev_1975_1990 + 2*dev_1990_2000 + 1*dev_2000_2014,
         devAge2 = devAge^2,
         sqrt_CO2_road = sqrt(CO_emissions_onroad),
         sqrt_CO2_cmv_rail = sqrt(CO_emissions_cmv + CO_emissions_rail),
         sqrt_CO2_transport = sqrt(CO_emissions_onroad + CO_emissions_rail + CO_emissions_cmv),
         sqrt_traffic = sqrt(traffic),
         greenery = grass_low_veg + shrub_med_veg + tnc_tree_cover,  #low, med and high veg all from TNC 
         #greenery = grass_low_veg + shrub_med_veg + percent_tree_cover/100,  #low, med and high veg (using percent tree cover)
         greenery_bareEarth = greenery + bare_earth,
         not_greenBE = 1 - greenery_bareEarth,
         sqrt_area_m2 = sqrt(area_m2)) %>%
  rename(paved = imperv_ground) %>%
  filter(! Name== "I5-Bridge_v2") %>%   #remove the wrong version of the I5 Bridge polygon (the right one is a sub-section of the v2 one, per e-mail from Dylan Ahearn Nov 15, 2024)
  filter(! Project=="Hwy DB") %>%    #remove the Highway DB sites.  I wasn't able to find actual locations for these, so guessed...
  mutate(Location = sub("^[^_]*_", "", Name) ) %>%  #only remove the project name (before the first "_" in the Name column))
  mutate(Location = case_when(Project=="Tacoma S8C" ~ paste("OF", Location, sep=""),  #add "OF" (outfall) in front of Tac S8C location names
                          TRUE ~ Location)) %>%
  select(Project, Location, sqrt_CO2_road, sqrt_CO2_cmv_rail, greenery_bareEarth, sqrt_area_m2,
         sqrt_CO2_transport, paved, devAge2, sqrt_traffic, not_greenBE) %>%
  #remove predictors that seem highly out of whack with the actual situation
  filter(! Location=="520-W",  #520-W should be fully paved and was built in 1963. The image file indicates paved=0 and devAge2=0 (unpaved, undeveloped)
         ! Location=="SR5_Maytown"   #SR5-Maytown should be on a highway, but the image file indicates sqrt_traffic=0 and paved=NA
  )

#per the MLK street sweeping QAPP, all locations should be 100% paved.  Here, set greenery_bareEarth equal to 1-paved at that site (it is way too high otherwise; probably trees overhanging pavement)
#vp.mod$greenery_bareEarth[which(vp.mod$Location=="SS5")] <- 1 - vp.mod$paved[which(vp.mod$Location=="SS5")]
vp.mod$not_greenBE[which(vp.mod$Location=="SS5")] <- vp.mod$paved[which(vp.mod$Location=="SS5")]

#set order of levels of Location for the validation predictors
Loc_levels <- vp.mod$Location[c(1:6, 8:11, 16:24, 12:15, 25:29, 7)]
vp.mod$Location <- factor(vp.mod$Location, levels=Loc_levels)  #make Location a factor with a set order of levels

#comp_res projects: "MLK Street Sweep", "Tacoma S8C", "Lake WA PFAS", "Hwy DB", "WSDOT", "I5 Bridge"
#grab_res projects: "Coupeville", "LahtiDr", "GardenEd", "MasonCo"

vp.std <- vp.mod %>%
  mutate(sqrt_CO2_road=(sqrt_CO2_road - stdvals$mean[which(stdvals$X %in% "sqrt_CO2_road")])/stdvals$sd[which(stdvals$X %in% "sqrt_CO2_road")],
         sqrt_CO2_cmv_rail=(sqrt_CO2_cmv_rail - stdvals$mean[which(stdvals$X %in% "sqrt_CO2_cmv_rail")])/stdvals$sd[which(stdvals$X %in% "sqrt_CO2_cmv_rail")],
         sqrt_CO2_transport=(sqrt_CO2_transport - stdvals$mean[which(stdvals$X %in% "sqrt_CO2_transport")])/stdvals$sd[which(stdvals$X %in% "sqrt_CO2_transport")],
         sqrt_traffic=(sqrt_traffic - stdvals$mean[which(stdvals$X %in% "sqrt_traffic")])/stdvals$sd[which(stdvals$X %in% "sqrt_traffic")],
         devAge2=(devAge2 - stdvals$mean[which(stdvals$X %in% "devAge2")])/stdvals$sd[which(stdvals$X %in% "devAge2")],
         paved=(paved - stdvals$mean[which(stdvals$X %in% "paved")])/stdvals$sd[which(stdvals$X %in% "paved")],
         greenery_bareEarth=(greenery_bareEarth - stdvals$mean[which(stdvals$X %in% "greenery_bareEarth")])/stdvals$sd[which(stdvals$X %in% "greenery_bareEarth")],
         not_greenBE=(not_greenBE - stdvals$mean[which(stdvals$X %in% "not_greenBE")])/stdvals$sd[which(stdvals$X %in% "not_greenBE")]
  )

#summary table of min/ max for each predictor
vp.std %>%
  summarize(across(-c(Project, Location), range)) %>%
  mutate(value = c("min", "max"), .before = 1)


#make a new object with capped traffic and paved values.  Traffic max is 14.75, paved max is 3.078
cap_level <- 2
cap_level_traffic <- 2
#cap_level_paved <- 1
vp.cap <- vp.std %>%
  mutate(sqrt_traffic = case_when(sqrt_traffic > cap_level_traffic ~ cap_level_traffic,
                                  TRUE ~ sqrt_traffic),
         paved = case_when(paved > cap_level ~ cap_level, #cap_level_paved,
                           TRUE ~ paved),
         sqrt_CO2_road = case_when(sqrt_CO2_road > cap_level ~ cap_level,
                                   TRUE ~ sqrt_CO2_road),
         sqrt_CO2_cmv_rail = case_when(sqrt_CO2_cmv_rail > cap_level ~ cap_level,
                           TRUE ~ sqrt_CO2_cmv_rail),
         sqrt_CO2_transport = case_when(sqrt_CO2_transport > cap_level ~ cap_level,
                                       TRUE ~ sqrt_CO2_transport),
         devAge2 = case_when(devAge2 > cap_level ~ cap_level,
                           TRUE ~ devAge2),
         greenery_bareEarth = case_when(greenery_bareEarth > cap_level ~ cap_level,
                              TRUE ~ greenery_bareEarth),
         not_greenBE = case_when(not_greenBE > cap_level ~ cap_level,
                                 TRUE ~ not_greenBE))



#-----------------------------------#
#  Landscape Predictor Value Plots  #
#-----------------------------------#

# plot(vp.std$sqrt_traffic, ylim=c(-5, 15), ylab="standardized sqrt_traffic", xlab="", main="sqrt_traffic values at model validation locations", xaxt="n")
# abline(h=c(-3, 3), col="red")
# abline(h=c(-2, 2), col="orange")
# 
# plot(vp.std$paved, ylim=c(-4, 4), ylab="standardized paved", xlab="", main="paved values at model validation locations", xaxt="n")
# abline(h=c(-3, 3), col="red")
# abline(h=c(-2, 2), col="orange")
# 
# plot(vp.std$sqrt_CO2_road, ylim=c(-4, 4), ylab="standardized sqrt_CO2_road", xlab="", main="sqrt_CO2_road values at model validation locations", xaxt="n")
# abline(h=c(-3, 3), col="red")
# abline(h=c(-2, 2), col="orange")

#plot validation predictor values for each standardized predictor (sqrt_traffic, devAge2, paved, sqrt_CO2_road)
ggplot() + 
  geom_point(aes(x=Location, y=sqrt_CO2_road, colour="sqrt_CO2_road"), data=vp.std) +  #add ln-transformed data points from this study
  #geom_point(aes(x=Location, y=sqrt_CO2_cmv_rail, colour="sqrt_CO2_cmv_rail"), data=vp.std) +  #add ln-transformed data points from this study
  geom_point(aes(x=Location, y=sqrt_CO2_transport, colour="sqrt_CO2_transport"), data=vp.std) +  #add ln-transformed data points from this study
  geom_point(aes(x=Location, y=sqrt_traffic, colour="sqrt_traffic"), data=vp.std) +  #add ln-transformed data points from this study
  geom_point(aes(x=Location, y=paved, colour="paved"), data=vp.std) +  #add ln-transformed data points from this study
  geom_point(aes(x=Location, y=devAge2, colour="devAge2"), data=vp.std) +  #add ln-transformed data points from this study
  #geom_point(aes(x=Location, y=greenery_bareEarth, colour="greenery_bareEarth"), data=vp.std) +
  geom_point(aes(x=Location, y=not_greenBE, colour="not_greenBE"), data=vp.std) +
  scale_color_manual(values = c(sqrt_CO2_road = "purple", sqrt_CO2_transport = "navy", sqrt_traffic = "red", paved="cadetblue", devAge2="orange", #greenery_bareEarth="green", 
                                not_greenBE="gray")) +
  ylab("standardized predictor values") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

vp1 <- ggplot() + 
  geom_point(aes(x=Location, y=sqrt_CO2_road), data=vp.std, colour="purple", size=3) +  #add ln-transformed data points from this study
  geom_hline(yintercept=c(-2, 2), color="purple", linetype="dashed", size=0.8) +
  xlab("") +   #("Location") +
  ylab("std sqrt_CO2_road") + ylim(-2.7, 2.7) +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill = "gray95", color = "black") )

vp2 <- ggplot() + 
  geom_point(aes(x=Location, y=sqrt_traffic), data=vp.std, colour="red", size=3) +  #add ln-transformed data points from this study
  geom_hline(yintercept=c(-2, 2), color="red", linetype="dashed", size=0.8) +
  xlab("") +   #("Location") +
  ylab("std sqrt_traffic") +  ylim(-2.7, 20) +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill = "gray95", color = "black") )

# vp3 <- ggplot() + 
#   geom_point(aes(x=Location, y=paved), data=vp.std, colour="cadetblue", size=2) +  #add ln-transformed data points from this study
#   xlab("Location") +
#   ylab("standardized paved") +
#   theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
#         axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

vp4 <- ggplot() + 
  geom_point(aes(x=Location, y=devAge2), data=vp.std, colour="orange", size=3) +  #add ln-transformed data points from this study
  geom_hline(yintercept=c(-2, 2), color="orange", linetype="dashed", size=0.8) +
  xlab("") +   #("Location") +
  ylab("std devAge2") + ylim(-2.7, 2.7) +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill = "gray95", color = "black") )

# vp5 <- ggplot() + 
#   geom_point(aes(x=Location, y=greenery_bareEarth), data=vp.std, colour="green", size=2) +  #add ln-transformed data points from this study
#   xlab("Location") +
#   ylab("standardized greenery_bareEarth") +
#   theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
#         axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

vp6 <- ggplot() + 
  geom_point(aes(x=Location, y=not_greenBE), data=vp.std, colour="gray", size=3) +  #add ln-transformed data points from this study
  geom_hline(yintercept=c(-2, 2), color="gray", linetype="dashed", size=0.8) +
  xlab("") +   #("Location") +
  ylab("std not_greenBE") + ylim(-2.7, 2.7) +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill = "gray95", color = "black") )

vp7 <- ggplot() + 
  geom_point(aes(x=Location, y=sqrt_CO2_transport), data=vp.std, colour="navy", size=3) +  #add ln-transformed data points from this study
  geom_hline(yintercept=c(-2, 2), color="navy", linetype="dashed", size=0.8) +
  xlab("") +   #("Location") +
  ylab("std sqrt_CO2_transport") + ylim(-2.7, 2.7) +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.background = element_rect(fill = "gray95", color = "black") )

ggarrange(vp1, vp2, vp4, vp6, vp7, nrow=3, ncol=2)


#-------------------------------------------------#
#  Add Predictor Values to Model Validation Data  #
#-------------------------------------------------#

comp_res.mod <- comp_res %>%
  mutate(Location = case_when(Project=="I5 Bridge" ~ "I5 Bridge",
                              TRUE ~ Location)) %>%
  filter(! is.na(Result),  #remove all results that are "NA"
         ! Project=="Hwy DB")


#add predictor values to the comp_res and grab_res dataframes
comp_res_pred <- left_join(x=comp_res.mod, y=vp.cap, join_by(Project, Location) )
grab_res_pred <- left_join(x=grab_res, y=vp.cap, join_by(Project, Location) )
grab_res_4plus_pred <- left_join(x=grab_res_4plus, y=vp.cap, join_by(Project, Location) )
grab_res_3_pred <- left_join(x=grab_res_3, y=vp.cap, join_by(Project, Location) )

#which locations are missing some predictor data
unique(comp_res_pred$Location[which(is.na(comp_res_pred$paved))] )
unique(grab_res_pred$Location[which(is.na(grab_res_pred$paved))] )

#get rid of any rows that don't have predictor data
comp_res_pred <- comp_res_pred[!is.na(comp_res_pred$paved),]
grab_res_pred <- grab_res_pred[!is.na(grab_res_pred$paved),]
grab_res_4plus_pred <- grab_res_4plus_pred[!is.na(grab_res_4plus_pred$paved),]
comp_res_3_pred <- grab_res_3_pred[!is.na(grab_res_3_pred$paved),]


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
  filter(!daymet_precip_std < -1.48) %>% #For model validation, remove the (Tacoma) samples that were collected when standardized 1-day precip < -1.48 (0mm rainfall)
  filter(daymet_precip > 0)

# #what does the s8 fitting data look like in terms of daily rainfall (based on Daymet estimates)
# s8 <- read.csv(file="../processed_data/s8data_with_spatial_predictors.csv")
# s8 %>% 
#   filter(daymet_precip < 1) %>% 
#   group_by(loc) %>% 
#   count()  #how many <1mm precip samples at each location?
# 
# # how does that compare to model validation data?
# mv.full_res_pred %>%
#   filter(daymet_precip < 1) %>%
#   group_by(Location) %>%
#   count()  #how many <1mm precip samples at each location?
# ## In comparing the S8 fitting data to the model validation data, there are similar percentages of samples at rainfall levels 
# #    between 1mm and 6mm in both data sets.  Conclude that we should just get rid of the data where daymet_precip <= 0.

mv.full_preds <- mv.full_res_pred %>%
  group_by(Project, agency, Location, location, Analyte) %>%
  select(Project, agency, Location, location, Analyte, sqrt_CO2_road, sqrt_CO2_cmv_rail, sqrt_CO2_transport, devAge2, sqrt_traffic, paved, greenery_bareEarth, not_greenBE, sqrt_area_m2) %>%
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

Cu.mv.dat <- mv.full_res_pred %>% 
  filter(Analyte=="Total Copper") %>% 
  mutate(rain=daymet_21day_std,
         summer = as.factor(summer),
         rawResult = Result) %>%
  select(Project, agency, Location, location, Analyte, Unit, rawResult, result, summer, rain, sqrt_traffic, devAge2, sqrt_area_m2)

Zn.mv.dat <- mv.full_res_pred %>% 
  filter(Analyte=="Total Zinc") %>% 
  mutate(rain=daymet_14day_std,
         summer = as.factor(summer),
         rawResult = Result) %>%
  select(Project, agency, Location, location, Analyte, Unit, rawResult, result, summer, rain, sqrt_traffic, paved, not_greenBE, greenery_bareEarth, sqrt_CO2_cmv_rail, sqrt_CO2_transport, sqrt_area_m2)

TSS.mv.dat <- mv.full_res_pred %>% 
  filter(Analyte=="Total Suspended Solids") %>% 
  mutate(rain=daymet_precip_std,
         rawResult = Result) %>%
  select(Project, agency, Location, location, Analyte, Unit, rawResult, result, rain, sqrt_traffic, devAge2, sqrt_area_m2)
  
P.mv.dat <- mv.full_res_pred %>% 
  filter(Analyte=="Total Phosphorus") %>% 
  mutate(rain=daymet_21day_std,
         summer = as.factor(summer),
         rawResult = Result) %>%
  select(Project, agency, Location, location, Analyte, Unit, rawResult, result, summer, rain, sqrt_CO2_road, sqrt_area_m2)

TKN.mv.dat <- mv.full_res_pred %>% 
  filter(Analyte=="Total Kjeldahl Nitrogen") %>% 
  mutate(rain=daymet_14day_std,
         summer = as.factor(summer),
         rawResult = Result) %>%
  select(Project, agency, Location, location, Analyte, Unit, rawResult, result, summer, rain, sqrt_traffic, devAge2, sqrt_area_m2)

# mv.dat.Cu <- mv.full_res_pred %>% filter(Analyte=="Total Copper") %>% mutate(rain=daymet_21day_std)
# mv.dat.Zn <- mv.full_res_pred %>% filter(Analyte=="Total Zinc") %>% mutate(rain=daymet_14day_std)
# mv.dat.TSS <- mv.full_res_pred %>% filter(Analyte=="Total Suspended Solids") %>% mutate(rain=daymet_precip_std)
# mv.dat.P <- mv.full_res_pred %>% filter(Analyte=="Total Phosphorus") %>% mutate(rain=daymet_21day_std)
# mv.dat.TKN <- mv.full_res_pred %>% filter(Analyte=="Total Kjeldahl Nitrogen") %>% mutate(rain=daymet_14day_std)

# #Copper model validation data
# Cu.mv.dat <- mv.dat.Cu %>%
#   select(Project, location, result, summer, rain, sqrt_traffic, devAge2) %>%
#   mutate(Project = as.factor(Project),
#          agency = Project,
#          agency = case_when(agency=="Tacoma S8C" ~ "Tacoma",
#                             TRUE ~ agency),
#          agency = as.factor(agency),
#          location = case_when(location=="OF237B" ~ "TAC_HDR",
#                               location=="OF235" ~ "TAC_COM",
#                               location=="OF245" ~ "TAC_IND",
#                               TRUE ~ location),
#          location = as.factor(location),
#          summer = as.factor(summer))
# 
# #Zinc model validation data
# Zn.mv.dat <- mv.dat.Zn %>%
#   select(Project, location, result, summer, rain, sqrt_traffic, paved) %>%
#   mutate(Project = as.factor(Project),
#          location = as.factor(location),
#          summer = as.factor(summer))
# 
# #TSS model validation data
# TSS.mv.dat <- mv.dat.TSS %>%
#   select(Project, location, result, summer, rain, sqrt_traffic, devAge2) %>%
#   mutate(Project = as.factor(Project),
#          location = as.factor(location),
#          summer = as.factor(summer))
# 
# #TKN model validation data
# TKN.mv.dat <- mv.dat.TKN %>%
#   select(Project, location, result, summer, rain, sqrt_traffic, devAge2) %>%
#   mutate(Project = as.factor(Project),
#          location = as.factor(location),
#          summer = as.factor(summer))
# 
# #Phosphorus model validation data
# P.mv.dat <- mv.dat.P %>%
#   select(Project, location, result, summer, rain, sqrt_CO2_road) %>%
#   mutate(Project = as.factor(Project),
#          location = as.factor(location),
#          summer = as.factor(summer))
# 


#-------------------------------------------#
#  Fitting vs. Model Validation Data Plots  #
#-------------------------------------------#

#zinc - compare range for fitting data and for model validation data
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

colors <- c("red", "orange", "yellow", "green", "darkgreen", "blue", "purple", "magenta", "brown")

#Cleveland Dot Plots - looking for outliers.
dotchart(Cu.mv.dat$result, groups = Cu.mv.dat$Project, 
         pch = 19, color = colors[as.numeric(Cu.mv.dat$Project)],
         xlab = "concentration", main = "Cleveland Dotplot: Copper", ann=TRUE)
dotchart(Zn.mv.dat$result, groups = Zn.mv.dat$Project, pch = 19, col = colors[as.numeric(Zn.mv.dat$Project)],
         xlab = "concentration", main = "Cleveland Dotplot: Zinc")
dotchart(TSS.mv.dat$result, groups = TSS.mv.dat$Project, pch = 19, col = colors[as.numeric(TSS.mv.dat$Project)],
         xlab = "concentration", main = "Cleveland Dotplot: TSS")
dotchart(TKN.mv.dat$result, groups = TKN.mv.dat$Project, pch = 19, col = colors[as.numeric(TKN.mv.dat$Project)],
         xlab = "concentration", main = "Cleveland Dotplot: TKN")
dotchart(P.mv.dat$result, groups = P.mv.dat$Project, pch = 19, col = colors[as.numeric(P.mv.dat$Project)],
         xlab = "concentration", main = "Cleveland Dotplot: Phosphorus")

#Remove outliers from each COC
Cu.mv.dat2 <- Cu.mv.dat %>% filter(!result < -2)  #one super low outlier at Tac OF237B -- ALREADY REMOVED ABOVE w/NO RAIN SAMPLES
Zn.mv.dat2 <- Zn.mv.dat %>% filter(!result > 8)  #two super high outliers at Tac OF235
TSS.mv.dat2 <- TSS.mv.dat %>% filter(!result > 13.5) %>% filter(!result < 7)  #one high outliers at MLK SS4; two really low outliers where daymet_precip <=1mm
TKN.mv.dat2 <- TKN.mv.dat %>% filter(!result > 10)  #one super high outlier at Hwy SR9_MP17.92
P.mv.dat2 <- P.mv.dat %>% filter(!result > 9)  #one super high outlier at Hwy SR9_MP17.92



#---------------------------------------#
#  Color Palettes / Plotting Specifics  #
#---------------------------------------#

orangePalette=c("#d94701", 
  "#fd8d3c", "#fdbe85", "#feedde")
#orangePalette=c("#d94701", "#fd8d3c", "#fdbe85", "#feedde")

#custom manual color scale for all model validation locations
myColors <- c("#000000", "#000000", "#000000", "#000000", "#E69F00", "#E69F00", "#CC79A7", "#E69F00", 
              "#56B4E9", "#56B4E9", "#56B4E9", "#009E73", "#009E73", "#009E73", "#009E73", 
              "#D55E00", "#D55E00", "#0072B2", "#0072B2", "#0072B2", "#0072B2", "#0072B2", 
              "#0072B2", "#0072B2", "#CC79A7", "#CC79A7", "#CC79A7", "#CC79A7", "#CC79A7")
names(myColors) <- vp.std$Location

# [1] BoatLaunch      CoupevilleWharf FrontAndMain    PerkinsBluff    W1              W2              I5 Bridge       LahtiSwale     
# [9] KIR-CW          N.MER           REN-JC          SS2             SS3             SS4             SS5             HS-033         
# [17] NB-020          OF230           OF235           OF237A          OF237B          OF243           OF245           OF254          
# [25] I5_MP197.27     I5_MP197.35     I5_MP210.71     I5_MP210.85     SR9_MP17.92    


#custom manual symbols for model validation locations
mySymbols <- c(16, 15, 17, 18,  #coupeville
               15, 16, 15, 17,  #garden ed (1-2), I5-bridge, lahti swale
               15, 16, 17,  #lake WA
               15, 16, 17, 18,  #MLK way
               15, 16,  #hood canal
               15, 16, 17, 18, 3, 8, 6,  #tacoma
               16, 17, 18, 8, 3)  #WSDOT
names(mySymbols) <- vp.std$Location

# Okabe-Ito color palette
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2"
# [7] "#D55E00" "#CC79A7" "#999999"
# 



# #custom (rainbow) manual color scale for all model validation locations
# myColors <- c("#B4443C", "#B8523D", "#C26247", "#CC0033", "#DB94A7", "#D2799A", "#99FFFF", 
#               "#C85B86", "#d94701", "#fd8d3c", "#fdbe85", "#CC99FF", "#9966FF", "#6600FF", 
#               "purple4", "#A17636", "#C2A547", "#006633", "#297A44", "#37A442", "#518A2E", 
#               "#66CC33", "#A2BF40", "#99FF00", "#0099FF", "#0033FF", "#3784A4", "#3660A1", 
#               "#323A95")
# names(myColors) <- vp.std$Location


#----------------------------------------------------------------#
#  Bar charts with Prediction Intervals & Data Plotted Atop PIs  #
#----------------------------------------------------------------#

theme_set(theme_bw())
#theme_set(theme_gray())  #undo above


#the my.coc2 is only to obtain summer and rainfall information (nothing else, such as location, is used)
#mv.full_Cu_PI <- make_PIs(my.brm=Cu.brm, my.coc2=Cu.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Copper"), c("agency", "location", "sqrt_traffic", "devAge2")])

mv.full_Cu_PI_noPB <- make_PIs(my.brm=Cu.brm, my.coc2=Cu.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Copper" & mv.full_preds$location!="PerkinsBluff"), c("agency", "location", "sqrt_traffic", "devAge2")])
mv.full_Zn_PI_noPB <- make_PIs(my.brm=totZn.brm, my.coc2=totZn.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Zinc" & mv.full_preds$location!="PerkinsBluff"), c("agency", "location", "sqrt_CO2_transport", "not_greenBE")])
#mv.full_Zn_PI_noPB <- make_PIs(my.brm=totZn.brm, my.coc2=totZn.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Zinc" & mv.full_preds$location!="PerkinsBluff"), c("agency", "location", "sqrt_traffic", "sqrt_CO2_cmv_rail", "greenery_bareEarth")])
#mv.full_Zn_PI <- make_PIs(my.brm=totZn.brm, my.coc2=totZn.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Zinc"), c("agency", "location", "sqrt_traffic", "paved")])
mv.full_TSS_PI <- make_PIs(my.brm=TSS.brm, my.coc2=TSS.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Suspended Solids"), c("agency", "location", "sqrt_traffic", "devAge2")])
mv.full_TKN_PI <- make_PIs(my.brm=TKN.brm, my.coc2=TKN.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Kjeldahl Nitrogen"), c("agency", "location", "sqrt_traffic", "devAge2")])
mv.full_P_PI <- make_PIs(my.brm=P.brm, my.coc2=P.coc2, wr.preds=mv.full_preds[which(mv.full_preds$Analyte=="Total Phosphorus"), c("agency", "location", "sqrt_CO2_road")])

mv.full1 <- plot_PIs_raw(mv.full_Cu_PI_noPB, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),])
mv.full2 <- plot_PIs_raw(mv.full_Zn_PI_noPB, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),])
mv.full3 <- plot_PIs_raw(mv.full_TSS_PI, TSS.mv.dat2)
mv.full4 <- plot_PIs_raw(mv.full_TKN_PI, TKN.mv.dat2)
mv.full5 <- plot_PIs_raw(mv.full_P_PI, P.mv.dat2)
ggarrange(mv.full1, mv.full2, mv.full3, mv.full4, mv.full5)

mv.full1
mv.full2
mv.full3
mv.full4
mv.full5

mv.full1 + ylim(-10, 150)   #cut out high copper PIs & data
mv.full2 + ylim(-50, 1010)  #cut out high zinc PIs & data
mv.full3 + ylim(-50, 1e+06)
mv.full4 + ylim(-50, 10000)
mv.full5 + ylim(-50, 2500)  #cut out the super high phosphorus data points

mv.full2 + ylim(0, 600)  #cut out high zinc PIs & data


#boxplots showing model validation data atop prediction intervals
# plot_PIs_boxplot_raw(mv.full_Cu_PI, mv.full_res_pred[which(mv.full_res_pred$Analyte=="Total Copper"),])
# plot_PIs_boxplot_raw(mv.full_Zn_PI, mv.full_res_pred[which(mv.full_res_pred$Analyte=="Total Zinc"),]) + ylim(-50, 1500)
# plot_PIs_boxplot_raw(mv.full_TSS_PI, mv.full_res_pred[which(mv.full_res_pred$Analyte=="Total Suspended Solids"),])
# plot_PIs_boxplot_raw(mv.full_TKN_PI, mv.full_res_pred[which(mv.full_res_pred$Analyte=="Total Kjeldahl Nitrogen"),]) + ylim(-50, 10000)
# plot_PIs_boxplot_raw(mv.full_P_PI, mv.full_res_pred[which(mv.full_res_pred$Analyte=="Total Phosphorus"),]) +ylim(-50, 1600)

mv.box1 <- plot_PIs_boxplot_raw(mv.full_Cu_PI_noPB, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], box.width=0.2, line.width=12) + theme(plot.title=element_blank())
mv.box2 <- plot_PIs_boxplot_raw(mv.full_Zn_PI_noPB, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], box.width=0.2, line.width=12) + theme(plot.title=element_blank())
#mv.box1 <- plot_PIs_boxplot_raw(mv.full_Zn_PI, Zn.mv.dat2, box.width=0.2) + ylim(-50, 3200)
mv.box3 <- plot_PIs_boxplot_raw(mv.full_TSS_PI, TSS.mv.dat2, box.width=0.2, line.width=10) + theme(plot.title=element_blank())
mv.box4 <- plot_PIs_boxplot_raw(mv.full_TKN_PI, TKN.mv.dat2, box.width=0.15, line.width=18) + ylim(-50, 10000) + theme(plot.title=element_blank())
mv.box5 <- plot_PIs_boxplot_raw(mv.full_P_PI, P.mv.dat2, box.width=0.2, line.width=13.5) + theme(plot.title=element_blank()) + ylim(0, 1520)

mv.box2 + ylim(0, 600)
ggarrange(mv.box1, mv.box2, mv.box3, mv.box4, mv.box5)

#-----------------------------------#
#  Median per Location Obs vs Pred  #
#-----------------------------------#

# obs.vs.pred.median.mv(Cu.brm, "Copper", Cu.mv.dat2, myColors)
p1.Cu <- obs.vs.pred.median.mv(Cu.brm, "Copper", Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], myColors, mySymbols) + xlim(0,4) + ylim(0,4)  #remove Perkins Bluff
p2.Zn <- obs.vs.pred.median.mv(totZn.brm, "Zinc", Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], myColors, mySymbols) + xlim(2.5, 5.6) + ylim(2.5, 5.6) #remove Perkins Bluff
p3.TSS <- obs.vs.pred.median.mv(TSS.brm, "TSS", TSS.mv.dat2, myColors, mySymbols) + xlim(8, 12) + ylim(8, 12)
p4.TKN <- obs.vs.pred.median.mv(TKN.brm, "TKN", TKN.mv.dat2, myColors, mySymbols) + xlim(6.7, 7.7) + ylim(6.7, 7.7)
p5.P <- obs.vs.pred.median.mv(P.brm, "Phosphorus", P.mv.dat2, myColors, mySymbols) + xlim(3.5, 5.7) + ylim(3.5, 5.7)

p1.Cu
p2.Zn
p3.TSS
p4.TKN
p5.P

ggarrange(p1.Cu, p2.Zn, p3.TSS, p4.TKN, p5.P)


#--------------------------------#
#  Median per Area, Obs vs Pred  #
#--------------------------------#

# obs.vs.pred.median.mv(Cu.brm, "Copper", Cu.mv.dat2, myColors)
p1.Cu.area <- obs.vs.pred.median.area.mv(Cu.brm, "Copper", Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),]) + xlim(0,4) + ylim(0,4)  #remove Perkins Bluff
p2.Zn.area <- obs.vs.pred.median.area.mv(totZn.brm, "Zinc", Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),]) + xlim(2.5, 6.2) + ylim(2.5, 6.2) #remove Perkins Bluff
p3.TSS.area <- obs.vs.pred.median.area.mv(TSS.brm, "TSS", TSS.mv.dat2) + xlim(8, 12) + ylim(8, 12)
p4.TKN.area <- obs.vs.pred.median.area.mv(TKN.brm, "TKN", TKN.mv.dat2) + xlim(6, 8) + ylim(6, 8)
p5.P.area <- obs.vs.pred.median.area.mv(P.brm, "Phosphorus", P.mv.dat2) + xlim(3.5, 6) + ylim(3.5, 6)

ggarrange(p1.Cu.area, p2.Zn.area, p3.TSS.area, p4.TKN.area, p5.P.area)


#---------------#
#  Obs vs Pred  #
#---------------#

# obs.vs.pred.mv(Cu.brm, "Copper", Cu.mv.dat2, myColors)
obs.vs.pred.mv(Cu.brm, "Copper", Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], myColors, mySymbols) + xlim(-1.5,5.1) + ylim(-1.5,5.1)
obs.vs.pred.mv(totZn.brm, "Zinc", Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], myColors, mySymbols) + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
obs.vs.pred.mv(TSS.brm, "TSS", TSS.mv.dat2, myColors, mySymbols) + xlim(7.2, 13.5) + ylim(7.2, 13.5)
obs.vs.pred.mv(TKN.brm, "TKN", TKN.mv.dat2, myColors, mySymbols) + xlim(6, 9.2) + ylim(6, 9.2)
obs.vs.pred.mv(P.brm, "Phosphorus", P.mv.dat2, myColors, mySymbols) + xlim(2.3, 8.3) + ylim(2.3, 8.3)


#------------------------------------------#
#  Obs vs Pred showing a select Predictor  #
#------------------------------------------#

# #Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
# p1 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2, "Copper", "sqrt_traffic", "Reds")
# p2 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2, "Copper", "devAge2", "Greens")
# p3 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2, "Copper", "rain", "Blues")
# p4 <- obsPredPlot2.mv(Cu.brm, Cu.mv.dat2, "Copper", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
# ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)

#Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], "Copper", "sqrt_traffic", "Reds") + xlim(-1.5,5.1) + ylim(-1.5,5.1)
p2 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], "Copper", "devAge2", "Greens") + xlim(-1.5,5.1) + ylim(-1.5,5.1)
p3 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], "Copper", "rain", "Blues") + xlim(-1.5,5.1) + ylim(-1.5,5.1)
p4 <- obsPredPlot2.mv(Cu.brm, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], "Copper", "summer", paletteCol=c("#fdbe85", "#d94701")) + xlim(-1.5,5.1) + ylim(-1.5,5.1)  #oranges for summer palette
p5 <- obsPredPlot.mv(Cu.brm, Cu.mv.dat2[-which(Cu.mv.dat2$location=="PerkinsBluff"),], "Copper", "sqrt_area_m2", "Purples") + xlim(-1.5,5.1) + ylim(-1.5,5.1)
ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)
p5

#Observed vs Predicted plots for multiple predictors, with colors showing a selected predictor.  
#p1 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "greenery_bareEarth", "Greens") + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
p1 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "not_greenBE", "Greens") + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
p2 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "sqrt_CO2_transport", "Reds") + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
p3 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "rain", "Blues") + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
p4 <- obsPredPlot2.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "summer", paletteCol=c("#fdbe85", "#d94701")) + xlim(0.6, 6.5) + ylim(0.6, 6.5)   #oranges for summer palette
#p5 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "sqrt_CO2_cmv_rail", "Purples") + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
p5 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2[-which(Zn.mv.dat2$location=="PerkinsBluff"),], "Total Zinc", "sqrt_area_m2", "Purples") + xlim(0.6, 6.5) + ylim(0.6, 6.5) 
ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)
p5

# #Observed vs Predicted plots for multiple predictors, with colors showing a selected predictor.  
# p1 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2, "Total Zinc", "sqrt_traffic", "Reds")
# p2 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2, "Total Zinc", "paved", "Greens")
# p3 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2, "Total Zinc", "rain", "Blues")
# p4 <- obsPredPlot2.mv(totZn.brm, Zn.mv.dat2, "Total Zinc", "summer", paletteCol=c("#fdbe85", "#d94701"))  #oranges for summer palette
# p5 <- obsPredPlot.mv(totZn.brm, Zn.mv.dat2, "Total Zinc", "sqrt_area_m2", "Purples")
# ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)

#Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot.mv(TSS.brm, TSS.mv.dat2, "TSS", "sqrt_traffic", "Reds") + xlim(7.2, 13.5) + ylim(7.2, 13.5)
p2 <- obsPredPlot.mv(TSS.brm, TSS.mv.dat2, "TSS", "devAge2", "Greens") + xlim(7.2, 13.5) + ylim(7.2, 13.5)
p3 <- obsPredPlot.mv(TSS.brm, TSS.mv.dat2, "TSS", "rain", "Blues") + xlim(7.2, 13.5) + ylim(7.2, 13.5)
p4 <- obsPredPlot.mv(TSS.brm, TSS.mv.dat2, "TSS", "sqrt_area_m2", "Purples") + xlim(7.2, 13.5) + ylim(7.2, 13.5)
ggarrange(p1, p2, p3, nrow=2, ncol=2)
p4

#Observed vs Predicted plots for multiple predictors, with colors showing a selected predictor.
p1 <- obsPredPlot.mv(TKN.brm, TKN.mv.dat2, "TKN", "sqrt_traffic", "Reds") + xlim(6, 9.2) + ylim(6, 9.2)
p2 <- obsPredPlot.mv(TKN.brm, TKN.mv.dat2, "TKN", "devAge2", "Greens") + xlim(6, 9.2) + ylim(6, 9.2)
p3 <- obsPredPlot.mv(TKN.brm, TKN.mv.dat2, "TKN", "rain", "Blues") + xlim(6, 9.2) + ylim(6, 9.2)
p4 <- obsPredPlot2.mv(TKN.brm, TKN.mv.dat2, "TKN", "summer", paletteCol=c("#fdbe85", "#d94701")) + xlim(6, 9.2) + ylim(6, 9.2)  #oranges for summer palette
p5 <- obsPredPlot.mv(TKN.brm, TKN.mv.dat2, "TKN", "sqrt_area_m2", "Purples") + xlim(6, 9.2) + ylim(6, 9.2)
ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)
p5

#Observed vs Estimated plots for multiple predictors, with colors showing a selected predictor.  
p1 <- obsPredPlot.mv(P.brm, P.mv.dat2, "Phosphorus", "sqrt_CO2_road", "Reds") + xlim(2.3, 8.3) + ylim(2.3, 8.3)
p.blank <- text_grob("")
p2 <- obsPredPlot.mv(P.brm, P.mv.dat2, "Phosphorus", "rain", "Blues") + xlim(2.3, 8.3) + ylim(2.3, 8.3)
p3 <- obsPredPlot2.mv(P.brm, P.mv.dat2, "Phosphorus", "summer", paletteCol=c("#fdbe85", "#d94701")) + xlim(2.3, 8.3) + ylim(2.3, 8.3)  #oranges for summer palette
p4 <- obsPredPlot.mv(P.brm, P.mv.dat2, "Phosphorus", "sqrt_area_m2", "Purples") + xlim(2.3, 8.3) + ylim(2.3, 8.3)
lay <- rbind(c(1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA),  #custom layout to make up for long predictor name (sqrt_CO2_road plot was narrower to accomodate the name in the legend)
             c(2,2,2,2,2,2,2,3,3,3,3,3,3,3))
ggarrange(p1, p2, p3, layout_matrix=lay)
p4







#----------------------------   OLD CODE BELOW, JUST FOR REFERENCE  -----------------------------------


# 
# #-------------------------#
# #  Composite Sample Data  #
# #-------------------------#
# 
# #dataframe of the predictors for each location; does not include stormwater outfall data (but, unlike vp.cap, DOES include analyte!)
# comp_preds <- comp_res_pred %>%
#   group_by(Project, Location, Analyte) %>%
#   select(Project, Location, Analyte, sqrt_CO2_road, devAge2, sqrt_traffic, paved) %>%
#   ungroup() %>%
#   distinct()
# 
# comp_Cu_PI <- make_PIs(my.brm=Cu.brm, my.coc2=Cu.coc2, wr.preds=comp_preds[which(comp_preds$Analyte=="Total Copper"), c("Location", "sqrt_traffic", "devAge2")])
# comp_Zn_PI <- make_PIs(my.brm=totZn.brm, my.coc2=totZn.coc2, wr.preds=comp_preds[which(comp_preds$Analyte=="Total Zinc"), c("Location", "sqrt_traffic", "paved")])
# comp_TSS_PI <- make_PIs(my.brm=TSS.brm, my.coc2=TSS.coc2, wr.preds=comp_preds[which(comp_preds$Analyte=="Total Suspended Solids"), c("Location", "sqrt_traffic", "devAge2")])
# comp_TKN_PI <- make_PIs(my.brm=TKN.brm, my.coc2=TKN.coc2, wr.preds=comp_preds[which(comp_preds$Analyte=="Total Kjeldahl Nitrogen"), c("Location", "sqrt_traffic", "devAge2")])
# comp_P_PI <- make_PIs(my.brm=P.brm, my.coc2=P.coc2, wr.preds=comp_preds[which(comp_preds$Analyte=="Total Phosphorus"), c("Location", "sqrt_CO2_road")])
# 
# comp1 <- plot_PIs_raw(comp_Cu_PI, comp_res_pred[which(comp_res_pred$Analyte=="Total Copper"),])
# comp2 <- plot_PIs_raw(comp_Zn_PI, comp_res_pred[which(comp_res_pred$Analyte=="Total Zinc"),]) + ylim(-50, 1500)
# comp3 <- plot_PIs_raw(comp_TSS_PI, comp_res_pred[which(comp_res_pred$Analyte=="Total Suspended Solids"),])
# comp4 <- plot_PIs_raw(comp_TKN_PI, comp_res_pred[which(comp_res_pred$Analyte=="Total Kjeldahl Nitrogen"),]) + ylim(-50, 10000)
# comp5 <- plot_PIs_raw(comp_P_PI, comp_res_pred[which(comp_res_pred$Analyte=="Total Phosphorus"),]) + ylim(-50, 2500)
# ggarrange(comp1, comp2, comp3, comp4, comp5)
# 
# comp1
# comp2
# comp3
# comp4
# comp5
# 
# comp1 + ylim(-10, 150)   #cut out high copper PIs & data
# comp2 + ylim(-50, 3200)  #cut out high zinc PIs & data
# comp3 + ylim(-50, 1e+06)
# comp4 + ylim(-50, 10000)
# comp5 + ylim(-50, 2500)  #cut out the super high phosphorus data points
# 
# 
# 
# #######  ALL ITEMS BELOW STILL INCLUDE THE OUTLIERS!  #########
# 
# #--------------------#
# #  Grab Sample Data  #   ##### NEED TO THINK ABOUT -- Since these are grab samples, which metric should we use?  How should we approach the fact that
# #--------------------#       # we would expect to see much higher variability here than with composite samples?
# 
# #dataframe of the predictors for each location; does not include stormwater outfall data (but, unlike vp.cap, DOES include analyte!)
# grab_preds <- grab_res_pred %>%
#   group_by(Project, Location, Analyte) %>%
#   select(Project, Location, Analyte, sqrt_CO2_road, devAge2, sqrt_traffic, paved) %>%
#   ungroup() %>%
#   distinct()
# 
# grab_preds[which(grab_preds$Analyte=="Total Kjeldahl Nitrogen"),]
# 
# grab_Cu_PI <- make_PIs(my.brm=Cu.brm, my.coc2=Cu.coc2, wr.preds=grab_preds[which(grab_preds$Analyte=="Total Copper"), c("Location", "sqrt_traffic", "devAge2")])
# grab_Zn_PI <- make_PIs(my.brm=totZn.brm, my.coc2=totZn.coc2, wr.preds=grab_preds[which(grab_preds$Analyte=="Total Zinc"), c("Location", "sqrt_traffic", "paved")])
# grab_TSS_PI <- make_PIs(my.brm=TSS.brm, my.coc2=TSS.coc2, wr.preds=grab_preds[which(grab_preds$Analyte=="Total Suspended Solids"), c("Location", "sqrt_traffic", "devAge2")])
# #grab_TKN_PI <- make_PIs(my.brm=TKN.brm, my.coc2=TKN.coc2, wr.preds=grab_preds[which(grab_preds$Analyte=="Total Kjeldahl Nitrogen"), c("Location", "sqrt_traffic", "devAge2")])
# grab_P_PI <- make_PIs(my.brm=P.brm, my.coc2=P.coc2, wr.preds=grab_preds[which(grab_preds$Analyte=="Total Phosphorus"), c("Location", "sqrt_CO2_road")])
# 
# grab1 <- plot_PIs_raw(grab_Cu_PI, grab_res_pred[which(grab_res_pred$Analyte=="Total Copper"),])
# grab2 <- plot_PIs_raw(grab_Zn_PI, grab_res_pred[which(grab_res_pred$Analyte=="Total Zinc"),])
# grab3 <- plot_PIs_raw(grab_TSS_PI, grab_res_pred[which(grab_res_pred$Analyte=="Total Suspended Solids"),])
# #grab4 <- plot_PIs_raw(grab_TKN_PI, grab_res_pred[which(grab_res_pred$Analyte=="Total Kjeldahl Nitrogen"),])
# grab5 <- plot_PIs_raw(grab_P_PI, grab_res_pred[which(grab_res_pred$Analyte=="Total Phosphorus"),])
# ggarrange(grab1, grab2, grab3, grab5)
# 
# grab1
# grab2
# grab3
# #grab4
# grab5
# 
# 
# #----------------------------#
# #  S8 Data Used for Fitting  #
# #----------------------------#
# 
# #dataframe of the predictors for each location; does not include stormwater outfall data (but, unlike vp.cap, DOES include analyte!)
# s8fit_preds_A <- Cu.coc2 %>%  #any COC can be used -- its just for predictors (not for analyte data)
#   group_by(location, agency) %>%
#   select(location, agency, sqrt_CO2_road, devAge2, sqrt_traffic) %>%
#   ungroup() %>%
#   distinct()
# 
# s8fit_preds_B <- totZn.coc2 %>%
#   group_by(location, agency) %>%
#   select(location, agency, paved) %>%
#   ungroup() %>%
#   distinct()
# 
# s8fit_preds <- full_join(s8fit_preds_A, s8fit_preds_B, by=c("location", "agency")) %>%
#   rename(Location = location,
#          Agency = agency)
# 
# 
# s8fit_Cu_PI <- make_PIs(my.brm=Cu.brm, my.coc2=Cu.coc2, wr.preds=s8fit_preds[, c("Location", "sqrt_traffic", "devAge2")])
# s8fit_Zn_PI <- make_PIs(my.brm=totZn.brm, my.coc2=totZn.coc2, wr.preds=s8fit_preds[, c("Location", "sqrt_traffic", "paved")])
# s8fit_TSS_PI <- make_PIs(my.brm=TSS.brm, my.coc2=TSS.coc2, wr.preds=s8fit_preds[, c("Location", "sqrt_traffic", "devAge2")])
# s8fit_TKN_PI <- make_PIs(my.brm=TKN.brm, my.coc2=TKN.coc2, wr.preds=s8fit_preds[, c("Location", "sqrt_traffic", "devAge2")])
# s8fit_P_PI <- make_PIs(my.brm=P.brm, my.coc2=P.coc2, wr.preds=s8fit_preds[, c("Location", "sqrt_CO2_road")])
# 
# s8fit1 <- plot_PIs_raw(s8fit_Cu_PI, Cu.coc2 %>% mutate(Result=exp(result)) %>% rename(Location=location)) + ggtitle("Total Copper")
# s8fit2 <- plot_PIs_raw(s8fit_Zn_PI, totZn.coc2 %>% mutate(Result=exp(result)) %>% rename(Location=location)) + ggtitle("Total Zinc")
# s8fit3 <- plot_PIs_raw(s8fit_TSS_PI, TSS.coc2 %>% mutate(Result=exp(result)) %>% rename(Location=location)) + ggtitle("Total Suspended Solids")
# s8fit4 <- plot_PIs_raw(s8fit_TKN_PI, TKN.coc2 %>% mutate(Result=exp(result)) %>% rename(Location=location)) + ggtitle("Total Kjeldahl Nitrogen")
# s8fit5 <- plot_PIs_raw(s8fit_P_PI, P.coc2 %>% mutate(Result=exp(result)) %>% rename(Location=location)) + ggtitle("Total Phosphorus")
# ggarrange(s8fit1, s8fit2, s8fit3, s8fit4, s8fit5)
# 
# s8fit1
# s8fit2
# s8fit3
# s8fit4
# s8fit5
# 
# #summary table of min/ max for each predictor
# s8fit_preds %>%
#   summarize(across(-c(Agency, Location), range)) %>%
#   mutate(value = c("min", "max"), .before = 1)
# 
# 
# 
# 
# #-------------------------------------------------------------#
# #  Compare Tacoma Rainfall: S8 Data vs Model Validation Data  #
# #-------------------------------------------------------------#
# 
# rain.tac.s8 <- TSS.coc2 %>%
#   filter(agency=="Tacoma") %>%
#   select(location, rain, result)
# 
# rain.tac.mv <- TSS.mv.dat2 %>%
#   filter(Project=="Tacoma S8C") %>%
#   select(location, result, rain)
#   
# par(mfrow=c(1,1))
# plot(rain.tac.s8$rain ~ rain.tac.s8$result, col="purple", pch=16, xlab="ln(TSS)", ylab="std 1-day rainfall", main="Tacoma Sites", xlim=c(6,13), ylim=c(-2, 4))
# points(rain.tac.mv$rain ~ rain.tac.mv$result, col="orange", pch=16)
# legend("topleft", c("s8 fitting data", "model validation data"), col=c("purple", "orange"), pch=16)
# 
# rain.tac.mv[which(rain.tac.mv$rain < -1.3),] #these are the samples with precip=0mm; its about 43 samples. 
# #standardization values for 1-day precip are: mean=10.9374 sd=7.375
# -1.483009*7.37515+10.9374  #rain = -1.483009 is the equivalent of precip = 0mm.  Maybe remove all samples that are < -1.48 for 1-day rain?
# # For model validation, try removing the Tacoma data that was collected when 1-day precip < -1.48
# 
# 
# 




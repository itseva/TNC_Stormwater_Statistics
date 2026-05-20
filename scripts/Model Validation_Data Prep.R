# This script takes ingested model validation data from "Model Validation_EIM Data.R" and from 
# "Model Validation_Other Data.R", and distills the data into the correct columns, Location
# names, date formats, etc.  Precipitation data are added to the validation data at the end
# of the script, and standardized precip is added as well (for use as the model-specific "rain" 
# predictor).

# Author: Eva Dusek Jennings
# Update: Jan 22, 2025
#------------------------------------------------------------------------------------

library(tidyr)
library(ggpubr) #for ggarrange
library(daymetr)  #for adding precip to the model validation dataset
library(lubridate)  #for yday, month and year functions


#run scripts that prepare data and contain functions required for this project
source("Model Validation_EIM Data.R")  #run script that preps EIM data
source("Model Validation_Other Data.R")  #run script that preps other data (including MLK street sweeping data)
p_std <- read.csv(file="../processed_data/daymet_precip_standardization_values.csv")

#----------------------------------#
#  Flow or Time Composite Samples  #
#----------------------------------#

#distill each dataset into just the columns we need:
#   Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude

#MLK street sweeping data
mlk_distilled <- mlk_res %>%
  mutate(Project="MLK Street Sweep",
         Date=as.POSIXct(Date, format="%m/%d/%y"),
         NDFlag=case_when(Detect_Flag=="Y" ~ FALSE,
                          TRUE ~ TRUE)) %>%
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude)

#Tacoma S8C data
tac_distilled <- tac_res %>%
  mutate(Project="Tacoma S8C",
         Date=as.POSIXct(Field_Collection_Start_Date_Time, format="%m/%d/%Y %H:%M:%S"),
         Location = case_when(Location=="OF237A_NEW" ~ "OF237A",
                              Location=="OF237B_FD1" ~ "OF237B",
                              Location=="OF245_MH390" ~ "OF245",
                              Location=="OF243_FD23" ~ "OF243",
                              TRUE ~ Location)) %>%
  filter(Analyte %in% c("Copper", "Zinc", "Total Phosphorus", "Total Suspended Solids")) %>%
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude)

#Lake Washington TSS data
lkwa_distilled <- lkwa_res %>%
  mutate(Project="Lake WA PFAS",
         Date=as.POSIXct(Field_Collection_Start_Date_Time, format="%m/%d/%Y %H:%M:%S") ) %>%
  filter(! Location %in% c("BEL-SHORE", "KEN-SWAMP", "KIR-63RD", "REN-CMU")) %>%   #remove locations with only 1 sample (they were only sampled for TSS)
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude)

#National Highway Database data
hwy_distilled <- hwy_res %>%
  mutate(Project="Hwy DB",
         Location=paste(Location, HwyMP, sep=" "),
         NDFlag=case_when(NDFlag=="False" ~ FALSE,
                          TRUE ~ TRUE)) %>%
  filter(Analyte %in% c("Total Copper", "Total Zinc", "Phosphorus, unfiltered", "Total Suspended Solids")) %>%
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude)
         
#WSDOT highway data
ws14_distilled <- ws14_res %>%
  mutate(Project="WSDOT",
         Date=as.POSIXct(Sample_Date, format="%m/%d/%y"),
         NDFlag=FALSE) %>%    #see notes in "Model Validation_Other Data" about .xlsx file & how it doesn't match the final report.
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude)                            

#Ship Canal Test Facility (I-5 Bridge) data
i5br_distilled <- i5br_res %>%
  mutate(Project="I5 Bridge",
         NDFlag=case_when(Flag==NA ~ FALSE,
                          Flag=="J" ~ FALSE)) %>%
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude)


#composite sample results - all together in 1 dataframe
comp_res <- rbind(mlk_distilled, tac_distilled, lkwa_distilled, hwy_distilled, ws14_distilled, i5br_distilled)
unique(comp_res$Analyte)

#standardize analyte names: "Total Copper", "Total Zinc", "Total Suspended Solids", "Total Phosphorus", "Total Kjeldahl Nitrogen"
comp_res <- comp_res %>%
  mutate(Analyte=case_when(Analyte %in% c("Copper", "Copper Total") ~ "Total Copper",
                           Analyte %in% c("Zinc", "Zinc Total") ~ "Total Zinc",
                           Analyte %in% c("Solids, Total Suspended", "TSS") ~ "Total Suspended Solids",
                           Analyte %in% c("Phosphorus, Total", "Phosphorus, unfiltered") ~ "Total Phosphorus",
                           Analyte=="Nitrogen, Total Kjeldahl" ~ "Total Kjeldahl Nitrogen",
                           TRUE ~ Analyte),
         Location=case_when(Location=="WA SR-525 Chambered Vault MP 4.1" ~ "SR525_ChamberedVault",
                            Location=="WA SR-8 MP 16 MP 16" ~ "SR8_MP16",
                            Location=="WA SR-405 Closed Wet Vault Inlet MP 26" ~ "SR405_ClosedVaultInlet",
                            Location=="WA SR-5 Dry Pond 1 MP 188.1" ~ "SR5_DryPond1",
                            Location=="WA SR-101 MP 363 MP 363" ~ "SR101_MP363",
                            Location=="WA I-5 MP 106 MP 106" ~ "I5_MP106",
                            Location=="WA SR-5 Indian Creek MP 106" ~ "SR5_IndianCreek",
                            Location=="WA SR-525 Wet Pond Inlet MP 3.3" ~ "SR525_WetPondInlet",
                            Location=="WA SR-5 Maytown MP 96.2" ~ "SR5_Maytown",
                            Location=="WA SR-405 Open Wet Vault Inlet MP 29.5" ~ "SR405_OpenVaultInlet",
                            Location=="WA SR-405 Vortech Monitoring MP 24.5" ~ "SR405_Vortech",
                            Location=="WA SR-167 Ecology Embankment MP 16.4" ~ "SR167_Embankment",
                            TRUE ~ Location), # )#,
         year=year(Date),  #these will be used for matching precip DF to comp_res DF
         yday=yday(Date)
  )


#plot results for each COC, for 4+ samples per location in a dark color, and 3 samples per location in a lighter color
comp1 <- ggplot() +
  geom_point(aes(x=Location, y=Result), data=comp_res[which(comp_res$Analyte=="Total Copper"),], color="red") +
  ggtitle("Total Copper") + xlab("Location") + ylab("Copper, ug/l") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5))

comp2 <- ggplot() +
  geom_point(aes(x=Location, y=Result), data=comp_res[which(comp_res$Analyte=="Total Zinc"),], color="orange2") +
  ggtitle("Total Zinc") + xlab("Location") + ylab("Zinc, ug/l") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5))

comp3 <- ggplot() +
  geom_point(aes(x=Location, y=Result), data=comp_res[which(comp_res$Analyte=="Total Suspended Solids"),], color="darkgreen") +
  ggtitle("Total Suspended Solids") + xlab("Location") + ylab("TSS, ug/l") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5))

comp4 <- ggplot() +
  geom_point(aes(x=Location, y=Result), data=comp_res[which(comp_res$Analyte=="Total Phosphorus"),], color="blue") +
  ggtitle("Total Phosphorus") + xlab("Location") + ylab("Phosphorus, ug/l") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5))

comp5 <- ggplot() +
  geom_point(aes(x=Location, y=Result), data=comp_res[which(comp_res$Analyte=="Total Kjeldahl Nitrogen"),], color="goldenrod") +
  ggtitle("Total Kjeldahl Nitrogen") + xlab("Location") + ylab("TKN, ug/l") +
  theme(plot.title=element_text(face="bold", size=14, hjust=0.5))

ggarrange(comp1, comp2, comp3, comp4, comp5)




#------------------------------------#
#  Grab Samples from Studies on EIM  #
#------------------------------------#

#coalesce EIM data from grab samples into one db
coupe_res$Project <- "Coupeville"
lahti_res$Project <- "LahtiDr"
ge_res$Project <- "GardenEd"
mc_res$Project <- "MasonCo"

#projects where we can't find info on catchment/watershed locations
#lkmer_res$project <- "Lake Meridian"
#wc_res$project <- "White Center"

#projects that don't have enough samples
#cal_res$project <- "Caldart Ave"
#lwr_res$project <- "Lower White River"
#ic_res$project <- "Indian Creek"
#hi_res$project <- "Henderson Inlet"
#lduw_res$project <- "Lower Duwamish"

eim_res <- rbind(coupe_res, lahti_res, ge_res, mc_res) %>%
  filter(Result_Parameter_Name %in% c("Copper", "Zinc", "Total Phosphorus", "Phosphorus", "Total Suspended Solids")) %>%
  mutate(Result_Parameter_Name = case_when(Result_Parameter_Name=="Phosphorus" ~ "Total Phosphorus",
                                           Result_Parameter_Name=="Copper" ~ "Total Copper",
                                           Result_Parameter_Name=="Zinc" ~ "Total Zinc",
                                           TRUE ~ Result_Parameter_Name))

#what units are used for each analyte?  Do we need to convert any units to ug/L?
unique(eim_res$Result_Value_Unit)  #some units in mg/l, some in ug/l.  Will need to convert mg/l to ug/l

#are there any results at or below the detection limit? (indicating ND result)
unique(eim_res$Result_Data_Qualifier)  #data qualifiers include: "U", "J", "JL";  U=undetected; J=estimated; JL=?? (it's only one sample)
which(eim_res$Result_Data_Qualifier %in% c("U"))  #U indicates that the contaminant was undetected
eim_res$Result_Parameter_Name[which(eim_res$Result_Data_Qualifier %in% c("U", "UJ"))]  #looks like most ND's are from TSS
unique(eim_res$Result_Suspect_or_Rejected_Flag) #no data are suspected or rejected

#fix up the dataframe by unifying units, marking ND data, and renaming columns for consistency with other datasets
#distill EIM data into just the columns we need:
#   Project, Location, Location_Name, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude
grab_res <- eim_res %>%
  mutate(Location=Location_Name,
         Location = case_when(Location=="BOAT LAUNCH" ~ "BoatLaunch",
                              Location=="COUPEVILLE WHARF" ~ "CoupevilleWharf",
                              Location=="FRONT AND MAIN" ~ "FrontAndMain",
                              Location=="PERKINS BLUFF" ~ "PerkinsBluff",
                              Location=="WOODLAKE ROAD 1 (NEAR AVALON CT)" ~ "W1",
                              Location=="WOODLAKE ROAD 2 (NEAR BROOKEDGE CT)" ~ "W2",
                              Location=="Lahti Drive Swale Inflow" ~ "LahtiSwale",
                              Location=="MASON_NORTH_BAY_NB-020" ~ "NB-020",
                              Location=="MASON_HOODSPORT_HS-033" ~ "HS-033",
                              TRUE ~ Location),
         Date=as.POSIXct(Field_Collection_Start_Date_Time, format="%m/%d/%Y"),
         year=year(Date),
         yday=yday(Date),
         year = case_when(year==9 ~ 2009,
                          year==10 ~ 2010,
                          TRUE ~ year),
         Analyte=Result_Parameter_Name,
         Result = case_when(Result_Value_Units=="mg/L" ~ Result_Value*1000,   #make all units ug/l
                            TRUE ~ Result_Value),
         Unit = case_when(Result_Value_Units=="mg/l" ~ "ug/l",
                          TRUE ~ "ug/l"),
         NDFlag=case_when(Result_Data_Qualifier %in% c("U") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  group_by(across(all_of(c("Location", "Analyte") ))) %>%
  filter(n() >= 3) %>%  #select only location/analyte combos where there are more than 3 samples collected (n() is similar to count())
  ungroup() %>%
  select(Project, Location, Date, Analyte, Result, Unit, NDFlag, Latitude, Longitude, year, yday)



#-----------------------------#
#  Validation Data Locations  #
#-----------------------------#

mv_locs <- rbind(comp_res, grab_res) %>%
  group_by(Location, Latitude, Longitude) %>%
  select(Location, Latitude, Longitude) %>%
  rename(site=Location,
         lat=Latitude,
         lon=Longitude) %>%
  ungroup() %>%
  distinct()

write.csv(mv_locs, "../data/model validation locations.csv", row.names=FALSE)


#-----------------------------------#
#  Add rainfall to validation data  #
#-----------------------------------#

# Download the nearest rainfall gage from daymet
daymet_p <- download_daymet_batch(file_location = "../data/model validation locations.csv", start = 2001,
                                  end = 2022, simplify = T)  #note that first collection day is 2001-03-25, and last day is 2022-05-12

# rename columns
colnames(daymet_p)[colnames(daymet_p) == "site"] <- "Location"
colnames(daymet_p)[colnames(daymet_p) == "value"] <- "daymet_precip"
colnames(daymet_p)[colnames(daymet_p) == "measurement"] <- "daymet_units"

# get just the precip data
p <- daymet_p %>%
  filter(daymet_units == "prcp..mm.day.") %>%
  select(-c(tile,altitude))

#calculate cumulative 14- and 21-day antecedant precip  
p$daymet_14day <- zoo::rollsum(p$daymet_precip, 14, fill=0, align="right")
p$daymet_21day <- zoo::rollsum(p$daymet_precip, 21, fill=0, align="right")

#join comp_res and grab_res with precip data
comp_res_p <- left_join(comp_res, p, join_by(Location, year, yday) ) %>%
  mutate(month=month(Date),
         summer=case_when(month %in% c(7,8,9) ~ 1,
                          TRUE ~ 0),
         summer=as.factor(summer)) %>%
  select(! c(latitude, longitude)) %>%
  mutate(daymet_precip_std = (daymet_precip - p_std$mean[which(p_std$dur=="daymet_precip")])/p_std$sd[which(p_std$dur=="daymet_precip")],
         daymet_14day_std = (daymet_14day - p_std$mean[which(p_std$dur=="daymet_14day")])/p_std$sd[which(p_std$dur=="daymet_14day")],
         daymet_21day_std = (daymet_21day - p_std$mean[which(p_std$dur=="daymet_21day")])/p_std$sd[which(p_std$dur=="daymet_21day")])

grab_res_p <- left_join(grab_res, p, join_by(Location, year, yday) ) %>%
  mutate(month=month(Date),
         summer=case_when(month %in% c(7,8,9) ~ 1,
                          TRUE ~ 0),
         summer=as.factor(summer)) %>%
  select(! c(latitude, longitude)) %>%
  mutate(daymet_precip_std = (daymet_precip - p_std$mean[which(p_std$dur=="daymet_precip")])/p_std$sd[which(p_std$dur=="daymet_precip")],
         daymet_14day_std = (daymet_14day - p_std$mean[which(p_std$dur=="daymet_14day")])/p_std$sd[which(p_std$dur=="daymet_14day")],
         daymet_21day_std = (daymet_21day - p_std$mean[which(p_std$dur=="daymet_21day")])/p_std$sd[which(p_std$dur=="daymet_21day")])

comp_res <- comp_res_p
grab_res <- grab_res_p


#-----------------------------------#
#  Grab Sites with 4+ vs 3 Samples  #
#-----------------------------------#

#locations sampled (with grab samples) on 4 or more occasions
grab_res_4plus <- grab_res %>%
  group_by(across(all_of(c("Location", "Analyte") ))) %>%
  filter(n() >= 4) %>%  #select only location/analyte combos where there are more than 4 samples collected (n() is similar to count())
  ungroup()

#locations sampled (with grab samples) on exactly 3 occasions
grab_res_3 <- grab_res %>%
  group_by(across(all_of(c("Location", "Analyte") ))) %>%
  filter(n() == 3) %>%  #select only location/analyte combos where there are more than 4 samples collected (n() is similar to count())
  ungroup()


#------------------------------#
#  Save Results as RData File  #
#------------------------------#

save(comp_res, grab_res, grab_res_4plus, grab_res_3, file="../data/model_validation_prepped_data.RData")

rm(daymet_p, p)  #remove large objects!




# This script uses lat/long locations to plot maps of S8.D model fitting data, and also 
#  data that were selected for model validation

# Author: Eva Dusek Jennings
# Update: Nov 18, 2024
#---------------------------------------------------------------------------------------



#install.packages("ggmap")
library(ggmap)
library(sp)
library(dplyr)
library(stringr)
library(RColorBrewer)

register_google(key="AIzaSyCxKhn9LOp1zrq4gw_-xeJHCUtE_M0Tyfw")  
# this is a private key!  Note: when not directly using this API, I will set the API security to only allow connection from our home's IP address.
# in order to use the API for this script, you will have to switch permissions to allow all IP addresses to use the API.
# https://console.cloud.google.com/google/maps-apis/credentials?project=stormwater-heatmap-353621



#run scripts that prepare data and contain functions required for this project
source("Model Validation_EIM Data.R")  #run script that ingests EIM data
source("Model Validation_Other Data.R")  #run script that ingests other data (including MLK street sweeping data)
source("Model Validation_Data Prep.R")  #run script that preps model validation data


#-----------------------------------------#
#  Map of S8.D Locations (Model Fitting)  #
#-----------------------------------------#

#load(file="../results/Bayesian_Copper.RData")  #use this to obtain Cu.coc2

my.cols <- c("red", "orange", "yellow", "green", "blue", "purple")

map <- qmap('Renton, Washington', zoom=9)
map + geom_point(data=Cu.coc2, aes(x=longitude, y=latitude, fill=agency), size=4, alpha=0.5, shape=21) +
  scale_fill_manual(values=c("red", "orange", "yellow", "green", "blue", "purple")) 



#-------------------------------------------------------------------------------#
#  Map of Validation Data Locations Using Composite/ 3+ Grab Sample Dataframes  #
#-------------------------------------------------------------------------------#

library(paletteer)
paletteer_c("viridis::inferno", n=10)
#paletteer_c("RColorBrewer::RdYlBu")
display.brewer.pal(11, "Spectral")
brewer.pal(11, "Spectral")
display.brewer.pal(11, "RdYlBu")
brewer.pal(11, "RdYlBu")
display.brewer.pal(9, "RdYlBu")
brewer.pal(9, "RdYlBu")


aaSpec <- brewer.pal(11, "Spectral")
bbSpec <- aa[c(1:7, 10:11)]


#map of composite sample locations AND grab sample locations with 3+ samplings
mv.all <- rbind(unique(comp_res[, c("Project", "Location", "Latitude", "Longitude")]), unique(grab_res[, c("Project", "Location", "Latitude", "Longitude")]) )
mv.all <- mv.all[-c(which(mv.all$Project=="Hwy DB")),]
mv.all.mod <- mv.all %>%
  filter(!(Location %in% c("520-W", "PerkinsBluff"))) %>%
  mutate(Project = case_when(Project=="LahtiDr" ~ "GardenEd/ LahtiDr",
                             Project=="GardenEd" ~ "GardenEd/ LahtiDr",
                             TRUE ~ Project) )

#select the colors for the map locations
aa <- brewer.pal(11, "RdYlBu")
bb <- aa[c(2:6, 9:11)]

map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
map + geom_point(data=mv.all.mod, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
  scale_fill_manual(values = bb) +
  labs(x="", y="")

# map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
# map + geom_point(data=mv.all.mod, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
#   scale_fill_manual(values = brewer.pal(8, "RdYlBu"))
#
# map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
# map + geom_point(data=mv.all, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
#   scale_fill_manual(values = brewer.pal(9, "Spectral"))
# 
# map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
# map + geom_point(data=mv.all, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
#   scale_fill_manual(values = bbSpec)
# 
# map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
# map + geom_point(data=mv.all, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
#   scale_fill_manual(values = bb)




#map of composite samples
map <- qmap('Everett, Washington', zoom=8)
map + geom_point(data=comp_res, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
  scale_fill_manual(values = brewer.pal(6, "Spectral"))

#map of locations with 3+ grab samples
map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
map + geom_point(data=grab_res, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
  scale_fill_manual(values = brewer.pal(4, "Spectral"))
map <- qmap('Bellingham, Washington', zoom=12)  #to distinguish between LahtiDr and GardenEd
map + geom_point(data=grab_res, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
  scale_fill_manual(values = brewer.pal(4, "Spectral"))

  




# #map of locations with 4+ grab samples
# map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
# map + geom_point(data=grab_res_4plus, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
#   scale_fill_manual(values = brewer.pal(6, "Spectral"))
# 
# #map of locations with 3 grab samples
# map <- qmap('Everett, Washington', zoom=8)  #NOTE: LahtiDr is right under GardenEd in this map!
# map + geom_point(data=grab_res_3, aes(x=Longitude, y=Latitude, fill=Project), colour="black", size=4, shape=21) +
#   scale_fill_manual(values = brewer.pal(6, "Spectral"))





#####   CODE BEYOND THIS POINT IS OLD - Not that useful anymore!   #####


#------------------------------------------------------------------------#
#  Map of Validation Data Locations Using Individual Results Dataframes  #    
#------------------------------------------------------------------------#

# mlk_gps <- unique(mlk_res[, c("Location", "Latitude", "Longitude")])
# tac_gps <- unique(tac_res[, c("Location", "Latitude", "Longitude")])

#Composite Sample locations
map <- qmap('Everett, Washington', zoom=8)
map + geom_point(data=mlk_res, aes(x=Longitude, y=Latitude), color="red3", size=4, alpha=0.5) +
  geom_text(data=mlk_res, aes(x=Longitude[1], y=Latitude[1], label="MLK sweep"), color="red3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=tac_res, aes(x=Longitude, y=Latitude), color="blue", size=4, alpha=0.5) +
  geom_text(data=tac_res, aes(x=Longitude[1], y=Latitude[1], label="Tac S8C"), color="blue", hjust=-0.5, vjust=-0.5) +
  # geom_point(data=hwy_res, aes(x=Longitude, y=Latitude), color="magenta", size=4, alpha=0.5) +
  # geom_text(data=hwy_res, aes(x=Longitude[1], y=Latitude[1], label="Hwy Data"), color="magenta", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkwa_res, aes(x=Longitude, y=Latitude), color="yellow", size=4, alpha=1) +
  geom_text(data=lkwa_res, aes(x=Longitude[1], y=Latitude[1], label="LkWA"), color="yellow", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ws14_res, aes(x=Longitude, y=Latitude), color="orange", size=4, alpha=0.5) +
  geom_text(data=ws14_res, aes(x=Longitude[1], y=Latitude[1], label="WSDOT"), color="orange", hjust=-0.5, vjust=-0.5)


#3+ Grab Sample Locations (from EIM)
map + geom_point(data=coupe_res, aes(x=Longitude, y=Latitude), color="red", size=4, alpha=1) +
  geom_text(data=coupe_res, aes(x=Longitude[1], y=Latitude[1], label="Coupeville"), color="red", hjust=-0.5, vjust=-0.5) +
  # geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  # geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5)


my_fav_colors <- c("red", "magenta3", "magenta1", "green", "blue4", "orange", "orange4")

eim_grab_4plus_Cu <- eim_grab_4plus[which(eim_grab_4plus$Analyte=="Copper"),]
eim_grab_3_Cu <- eim_grab_3[which(eim_grab_3$Analyte=="Copper"),]

map + geom_point(data=eim_grab_4plus_Cu, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_4plus_Cu, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.2, vjust=-0.2) +
  geom_point(data=eim_grab_3_Cu, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_3_Cu, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.2, vjust=-0.2)

#color=my_fav_colors[1:length(unique(eim_grab_4plus_Cu$Project))], 



eim_grab_4plus_TSS <- eim_grab_4plus[which(eim_grab_4plus$Analyte=="Total Suspended Solids"),]

map + geom_point(data=eim_grab_4plus_TSS, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_4plus_TSS, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.5, vjust=-0.5)


eim_grab_4plus_P <- eim_grab_4plus[which(eim_grab_4plus$Analyte=="Total Phosphorus"),]

map + geom_point(data=eim_grab_4plus_P, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_4plus_P, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.5, vjust=-0.5)


#sampling sites where TSS was sampled
map + 
  # geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  # geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lahti_res, aes(x=Longitude, y=Latitude), color="blue4", size=4, alpha=1) +
  geom_text(data=lahti_res, aes(x=Longitude[1], y=Latitude[1], label="Lahti"), color="blue4", hjust=-0.5, vjust=-0.5) +
  # geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  # geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5)


#sampling sites where total phosphorus was sampled
map + geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkmer_res, aes(x=Longitude, y=Latitude), color="orange4", size=4, alpha=1) +
  geom_text(data=lkmer_res, aes(x=Longitude[1], y=Latitude[1], label="LkMer"), color="orange4", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5)




#---------------------------------------------------#
#  Map of ALL Data Locations (Incl. Unusable Data)  #
#---------------------------------------------------#

# coupe_gps <- unique(coupe_res[, c("Location_ID", "Latitude", "Longitude")])
# cal_gps <- unique(cal_res[, c("Location_ID", "Latitude", "Longitude")])
# wc_gps <- unique(wc_res[, c("Location_ID", "Latitude", "Longitude")])
# lwr_gps <- unique(lwr_res[, c("Location_ID", "Latitude", "Longitude")])
# lkwa_gps <- unique(lkwa_res[, c("Location_ID", "Latitude", "Longitude")])
# lkmer_gps <- unique(lkmer_res[, c("Location_ID", "Latitude", "Longitude")])
# lahti_gps <- unique(lahti_res[, c("Location_ID", "Latitude", "Longitude")])
# ic_gps <- unique(ic_res[, c("Location_ID", "Latitude", "Longitude")])
# ge_gps <- unique(ge_res[, c("Location_ID", "Latitude", "Longitude")])
# hi_gps <- unique(hi_res[, c("Location_ID", "Latitude", "Longitude")])
# mc_gps <- unique(mc_res[, c("Location_ID", "Latitude", "Longitude")])
# lduw_gps <- unique(lduw_res[, c("Location_ID", "Latitude", "Longitude")])

#plot all sampling sights where EIM data are available
map <- qmap('Everett, Washington', zoom=8)
map + geom_point(data=coupe_res, aes(x=Longitude, y=Latitude), color="red", size=4, alpha=1) +
  geom_text(data=coupe_res, aes(x=Longitude[1], y=Latitude[1], label="Coupeville"), color="red", hjust=-0.5, vjust=-0.5) +
  geom_point(data=cal_res, aes(x=Longitude, y=Latitude), color="blue", size=4, alpha=1) +
  geom_text(data=cal_res, aes(x=Longitude[1], y=Latitude[1], label="Caldart"), color="blue", hjust=-0.5, vjust=-0.5) +
  geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lwr_res, aes(x=Longitude, y=Latitude), color="purple2", size=4, alpha=1) +
  geom_text(data=lwr_res, aes(x=Longitude[1], y=Latitude[1], label="LowWhiteR"), color="purple2", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkwa_res, aes(x=Longitude, y=Latitude), color="yellow", size=4, alpha=1) +
  geom_text(data=lkwa_res, aes(x=Longitude[1], y=Latitude[1], label="LkWA"), color="yellow", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkmer_res, aes(x=Longitude, y=Latitude), color="orange4", size=4, alpha=1) +
  geom_text(data=lkmer_res, aes(x=Longitude[1], y=Latitude[1], label="LkMer"), color="orange4", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lahti_res, aes(x=Longitude, y=Latitude), color="blue4", size=4, alpha=1) +
  geom_text(data=lahti_res, aes(x=Longitude[1], y=Latitude[1], label="Lahti"), color="blue4", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5) +
  geom_point(data=hi_res, aes(x=Longitude, y=Latitude), color="orange", size=4, alpha=1) +
  geom_text(data=hi_res, aes(x=Longitude[1], y=Latitude[1], label="HendIn"), color="orange", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lduw_res, aes(x=Longitude, y=Latitude), color="red4", size=4, alpha=1) +
  geom_text(data=lduw_res, aes(x=Longitude[1], y=Latitude[1], label="LDuw"), color="red4", hjust=-0.5, vjust=-0.5)
  

#EIM sampling sites where total copper and total zinc were sampled
map + geom_point(data=coupe_res, aes(x=Longitude, y=Latitude), color="red", size=4, alpha=1) +
  geom_text(data=coupe_res, aes(x=Longitude[1], y=Latitude[1], label="Coupeville"), color="red", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res_3, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res_3, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lduw_res, aes(x=Longitude, y=Latitude), color="red4", size=4, alpha=1) +
  geom_text(data=lduw_res, aes(x=Longitude[1], y=Latitude[1], label="LDuw"), color="red4", hjust=-0.5, vjust=-0.5)


#EIM sampling sites where TSS was sampled
map + geom_point(data=cal_res, aes(x=Longitude, y=Latitude), color="blue", size=4, alpha=1) +
  geom_text(data=cal_res, aes(x=Longitude[1], y=Latitude[1], label="Caldart"), color="blue", hjust=-0.5, vjust=-0.5) +
  geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lwr_res, aes(x=Longitude, y=Latitude), color="purple2", size=4, alpha=1) +
  geom_text(data=lwr_res, aes(x=Longitude[1], y=Latitude[1], label="LowWhiteR"), color="purple2", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkwa_res, aes(x=Longitude, y=Latitude), color="yellow", size=4, alpha=1) +
  geom_text(data=lkwa_res, aes(x=Longitude[1], y=Latitude[1], label="LkWA"), color="yellow", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lahti_res, aes(x=Longitude, y=Latitude), color="blue4", size=4, alpha=1) +
  geom_text(data=lahti_res, aes(x=Longitude[1], y=Latitude[1], label="Lahti"), color="blue4", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lduw_res, aes(x=Longitude, y=Latitude), color="red4", size=4, alpha=1) +
  geom_text(data=lduw_res, aes(x=Longitude[1], y=Latitude[1], label="LDuw"), color="red4", hjust=-0.5, vjust=-0.5)


#EIM sampling sites where total phosphorus was sampled
map + geom_point(data=cal_res, aes(x=Longitude, y=Latitude), color="blue", size=4, alpha=1) +
  geom_text(data=cal_res, aes(x=Longitude[1], y=Latitude[1], label="Caldart"), color="blue", hjust=-0.5, vjust=-0.5) +
  geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lwr_res, aes(x=Longitude, y=Latitude), color="purple2", size=4, alpha=1) +
  geom_text(data=lwr_res, aes(x=Longitude[1], y=Latitude[1], label="LowWhiteR"), color="purple2", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkmer_res, aes(x=Longitude, y=Latitude), color="orange4", size=4, alpha=1) +
  geom_text(data=lkmer_res, aes(x=Longitude[1], y=Latitude[1], label="LkMer"), color="orange4", hjust=-0.5, vjust=-0.5) +
  # geom_point(data=lahti_res, aes(x=Longitude, y=Latitude), color="blue4", size=4, alpha=1) +                               ### Eva thinks this is just Orthophosphate
  # geom_text(data=lahti_res, aes(x=Longitude[1], y=Latitude[1], label="Lahti"), color="blue4", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5) +
  geom_point(data=hi_res, aes(x=Longitude, y=Latitude), color="orange", size=4, alpha=1) +
  geom_text(data=hi_res, aes(x=Longitude[1], y=Latitude[1], label="HendIn"), color="orange", hjust=-0.5, vjust=-0.5)


#EIM sampling sites where TKN was sampled (or can be calculated from N constituents)
map + geom_point(data=lwr_res, aes(x=Longitude, y=Latitude), color="purple2", size=4, alpha=1) +
  geom_text(data=lwr_res, aes(x=Longitude[1], y=Latitude[1], label="LowWhiteR"), color="purple2", hjust=-0.5, vjust=-0.5) +
  geom_point(data=hi_res, aes(x=Longitude, y=Latitude), color="orange", size=4, alpha=1) +
  geom_text(data=hi_res, aes(x=Longitude[1], y=Latitude[1], label="HendIn"), color="orange", hjust=-0.5, vjust=-0.5)


#--------------------------------------------------------------------------------#
#  Map of EIM Validation Data Locations - 3 or 4plus locations for grab samples  #
#--------------------------------------------------------------------------------#

#EIM sampling sites where total copper and total zinc were sampled (at least 3 samples per location)
map + geom_point(data=coupe_res, aes(x=Longitude, y=Latitude), color="red", size=4, alpha=1) +
  geom_text(data=coupe_res, aes(x=Longitude[1], y=Latitude[1], label="Coupeville"), color="red", hjust=-0.5, vjust=-0.5) +
  # geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  # geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5)


my_fav_colors <- c("red", "magenta3", "magenta1", "green", "blue4", "orange", "orange4")

eim_grab_4plus_Cu <- eim_grab_4plus[which(eim_grab_4plus$Analyte=="Copper"),]
eim_grab_3_Cu <- eim_grab_3[which(eim_grab_3$Analyte=="Copper"),]

map + geom_point(data=eim_grab_4plus_Cu, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_4plus_Cu, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.2, vjust=-0.2) +
  geom_point(data=eim_grab_3_Cu, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_3_Cu, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.2, vjust=-0.2)

#color=my_fav_colors[1:length(unique(eim_grab_4plus_Cu$Project))], 



eim_grab_4plus_TSS <- eim_grab_4plus[which(eim_grab_4plus$Analyte=="Total Suspended Solids"),]

map + geom_point(data=eim_grab_4plus_TSS, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_4plus_TSS, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.5, vjust=-0.5)


eim_grab_4plus_P <- eim_grab_4plus[which(eim_grab_4plus$Analyte=="Total Phosphorus"),]

map + geom_point(data=eim_grab_4plus_P, aes(x=Longitude, y=Latitude, color=Project), size=4, alpha=1) +
  geom_text(data=eim_grab_4plus_P, aes(x=Longitude, y=Latitude, label=Project), hjust=-0.5, vjust=-0.5)


#sampling sites where TSS was sampled
map + 
  # geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  # geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lahti_res, aes(x=Longitude, y=Latitude), color="blue4", size=4, alpha=1) +
  geom_text(data=lahti_res, aes(x=Longitude[1], y=Latitude[1], label="Lahti"), color="blue4", hjust=-0.5, vjust=-0.5) +
  # geom_point(data=ic_res, aes(x=Longitude, y=Latitude), color="magenta3", size=4, alpha=1) +
  # geom_text(data=ic_res, aes(x=Longitude[1], y=Latitude[1], label="IndCr"), color="magenta3", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5) +
  geom_point(data=mc_res, aes(x=Longitude, y=Latitude), color="magenta1", size=4, alpha=1) +
  geom_text(data=mc_res, aes(x=Longitude[1], y=Latitude[1], label="MasonCo"), color="magenta1", hjust=-0.5, vjust=-0.5)


#sampling sites where total phosphorus was sampled
map + geom_point(data=wc_res, aes(x=Longitude, y=Latitude), color="green", size=4, alpha=1) +
  geom_text(data=wc_res, aes(x=Longitude[1], y=Latitude[1], label="WhiteCtr"), color="green", hjust=-0.5, vjust=-0.5) +
  geom_point(data=lkmer_res, aes(x=Longitude, y=Latitude), color="orange4", size=4, alpha=1) +
  geom_text(data=lkmer_res, aes(x=Longitude[1], y=Latitude[1], label="LkMer"), color="orange4", hjust=-0.5, vjust=-0.5) +
  geom_point(data=ge_res, aes(x=Longitude, y=Latitude), color="#d94701", size=4, alpha=1) +
  geom_text(data=ge_res, aes(x=Longitude[1], y=Latitude[1], label="GardenEd"), color="#d94701", hjust=-0.5, vjust=-0.5)



#-------------------------#
#  Polygon Experimenting  #
#-------------------------#

library(sf)

polygon_test <- st_read("../data/model validation data/Test polygons on Google Earth.kml")
plot(polygon_test[1])
plot(polygon_test[2])

output <- polygon_test %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

polygon_test$geometry

polygon_test[[3]][[1]][1]  #its the 3rd list of info in the kml file, and the first polygon 

# kml_pts <- polygon_test %>%
#   mutate(Polygon=Name,
#          Longitude=sf::st_coordinates(.)[,1],
#          Latitude=sf::st_coordinates(.)[,2]) %>%
#   select(Polygon,
#          Longitude,
#          Latitude) 
# polygon_test$geometry



map <- qmap('Anderson,California', zoom=11)
map + geom_point(data=surv_pts1, aes(x=Longitude, y=Latitude), color="red", size=2, alpha=0.5) +
  geom_text(data=surv_pts1, aes(x=Longitude, y=Latitude, label=Waypoint), color="red", hjust=0, vjust=0) +
  geom_point(data=killam_rm, aes(x=Longitude, y=Latitude), color="blue", size=2, alpha=0.5) +
  geom_text(data=killam_rm, aes(x=Longitude, y=Latitude, label=Waypoint), color="blue", hjust=0, vjust=0)


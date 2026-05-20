# Get raw spatial predictors and prepare them by standardizing as needed.
# Note that this version obtains the raw spatial predictors extracted directly from Earth Engine,
# rather than the spatial predictors in the python notebook, that were copied and pasted into a csv file.

# Additionally, v4 also processes landuse percentage predictors, with landuse based on WA commerce landuses

# avg_AADT = average annual daily traffic
# Feb 17, 2022: traffic predictor changed to sqrt_traffic
# Jan 4, 2023: add CO2_ccorr (cmv, commercial, onroad, rail, residential) & CO2_corr (cmv, onroad, rail, residential)
# Jun 7, 2023: remove CO2_tot as a predictor; it includes categories that are not in our catchment regions!  
#              CO2_cor is now CO2_transport; CO2_ccorr is now CO2_almostTotal.  Also updated figure-generating for the tech doc

# Author: Eva Dusek Jennings
# Date: Jun 16, 2023
# Rev:  Apr 28, 2025 - add bare earth and generate greenery_bareEarth
# Rev:  Jun 2, 2025  - add road density
# Rev:  Jun 6, 2025  - add CO2_cmv_rail (sqrt & log-transformed) and sqrt_paved
# Rev:  Jun 16, 2025  - add June2025_predictors.csv columns, including max, mean, sum of traffic and CO2 predictors
# Rev:  Dec 17, 2025 - add tnc_trees, to be used instead of percent_tree_cover
#---------------------------------------------

library(tidyverse)
#library(magrittr)
library("lattice")

sp <- read.csv(file="../data/spatial_predictors_raw_v3.csv") %>%            ### Make sure its the right version here!
  dplyr::rename(location=Location_N) %>%   #rename column "Location_N" to "location"
  dplyr::filter(! (location %in% c("POSOUTFALL_60",  #remove the following sites: Port of Seattle outfall (unrepresentative of other watersheds)
                            "PIEHIRES_OUT",   #                            Pierce County High Residential (data were collected in the middle of a stream)
                            "PIELORES_OUT"))) %>%  #                            Pierce County Low Residential (data collected in the middle of a stream)  
                    #NOTE: Centering and scaling happen in this script, so the mean & sd for standardization will NOT include these sites.
  dplyr::mutate(loc = case_when(  #add shorthand location names
    location == "KICLDRS8D_OUT" ~ "KIC_LDR",
    location == "KICHDRS8D_OUT" ~ "KIC_HDR",
    location == "KICCOMS8D_OUT" ~ "KIC_COM",
    location == "SEAI1S8D_OUT" ~ "SEA_IND",
    location == "SEAC1S8D_OUT" ~ "SEA_COM",
    location == "SEAR1S8D_OUT" ~ "SEA_HDR",
    location == "SNO_COM" ~ "SNO_COM",
    location == "SNO_LDR" ~ "SNO_LDR",
    location == "SNO_HDR" ~ "SNO_HDR",
    location == "TAC001S8D_OF235" ~ "TAC_COM",
    location == "TAC003S8D_OF245" ~ "TAC_IND",
    location == "TFWFD1" ~ "TAC_HDR",
    location == "PIECOMM_OUT" ~ "PIE_COM",
    location == "POT564S8D_OUT" ~ "POT_COM"
    ))
 

#create a new column for proportion of development by age as a continuous variable, where the larger
#  the number, the larger the proportion of old development.  Numbers close to 0 either have very new
#  development, or very little development.
sp$devAge <- 4*sp$dev_pre_1975 + 3*sp$dev_1975_1990 + 2*sp$dev_1990_2000 + 1*sp$dev_2000_2014

#combine some similar categories
sp$totRES <- sp$ruRES + sp$urbRES  #urban + rural residential
sp$intURB_IND <- sp$intURB + sp$IND  #intensive urban + industrial
sp$roof_intURB_IND <- sp$roof_intURB + sp$roof_IND  #roofs: intensive urban + industrial
sp$roof_totRES <- sp$roof_ruRES + sp$roof_urbRES  #roofs: intensive urban + industrial
sp$greenery <- sp$grass_low_veg + sp$shrub_med_veg + sp$tnc_trees  #low, med and high veg
sp$greenery_bareEarth <- sp$greenery + sp$bare_earth
sp$not_greenBE <- 1-sp$greenery_bareEarth
sp$not_green <- 1-sp$greenery

# sp$greenery_old <- sp$grass_low_veg + sp$shrub_med_veg + sp$percent_tree_cover/100  #low, med and high veg
# sp$greenery_bareEarth_old <- sp$greenery_old + sp$bare_earth
# sp$not_greenBE_old <- 1-sp$greenery_bareEarth_old
# sp$not_green_old <- 1-sp$greenery_old

#look at histograms of the data; determine which columns require log-transformation;
#  look for explanatory variables that have extreme observations; these will likely need
#  a transformation applied to them
ggplot(gather(subset(sp, select=-c(location, loc))), aes(value)) +
  geom_histogram(bins=10, fill = "cadetBlue") +
  facet_wrap(~key, scales = "free")

sp$loc <- as.factor(sp$loc)

#use cleveland dotcharts to determine which predictors have extreme values and thus require xforms
op <- par(mfrow=c(3,3), mar=c(3,3,3,1))
dotchart(sp$AG, main="AG", group=sp$loc)  #agriculture and silviculture
#dotchart(sqrt(sp$AG), main="sqrtAG", group=sp$loc)  #agriculture
dotchart(sp$intURB, main="intURB", group=sp$loc)  #intensive urban
dotchart(sqrt(sp$intURB), main="sqrt intURB", group=sp$loc)  #intensive urban
#dotchart(log(sp$intURB), main="logIntURB", group=sp$loc)  #intensive urban
dotchart(sp$IND, main="IND", group=sp$loc)  #industrial
dotchart(sp$urbRES, main="urbRES", group=sp$loc)  #urban residential
dotchart(sp$ruRES, main="ruRES", group=sp$loc)  #rural residential
dotchart(sp$totRES, main="totRES", group=sp$loc)  #total residential (urban + rural res)
dotchart(sqrt(sp$totRES), main="sqrt_totRES", group=sp$loc)  #total residential (urban + rural res)
dotchart(sp$intURB_IND, main="intURB + IND", group=sp$loc)
#looks like the following require strong transformations: 
#the following may require weak transformations: intURB
#the following are uninformative b/c of low data quantities: AG, ruRES, IND
#transformations applied: sqrt(intURB), sqrt(totRES); keep intURB and totRES as well
#1. predictors to keep: intURB, sqrt(intURB), urbRES, totRES, sqrt(totRES), intURB+IND

op <- par(mfrow=c(3,4), mar=c(3,3,3,1))
dotchart(sp$grass_low_veg, main="grass", group=sp$loc)  
dotchart(sp$shrub_med_veg, main="shrub", group=sp$loc) 
dotchart(sp$greenery, main="greenery", group=sp$loc)
dotchart(sp$imperv_ground, main="paved", group=sp$loc)  
dotchart(sqrt(sp$imperv_ground), main="sqrt_paved", group=sp$loc)  
dotchart(log(sp$imperv_ground), main="log_paved", group=sp$loc)  
dotchart(sp$imperv_roofs, main="roofs", group=sp$loc)  
dotchart(sp$impervious, main="impervious", group=sp$loc)
#dotchart(log(sp$no_dev*100+1), main="log_nodev", group=sp$loc)  
dotchart(sp$no_dev, main="nodev", group=sp$loc)  
dotchart(sqrt(sp$no_dev), main="sqrt nodev", group=sp$loc)  
dotchart(sp$percent_tree_cover, main="trees", group=sp$loc)  
dotchart(sp$traffic, main="traffic", group=sp$loc)  
dotchart(sp$road_density, main="roads", group=sp$loc)
dotchart(sp$pop_per_ha, main="popn", group=sp$loc) 
dotchart(sqrt(sp$pop_per_ha), main="sqrt popn", group=sp$loc) 
#dotchart(log(sp$pop_per_ha), main="log popn", group=sp$loc)  #too much
#looks like the following require strong transformations:
#the following may require weak transformations: nodev, popn
#the following are uninformative b/c of low data quantities: shrub
#transformations: sqrt(popn), sqrt(nodev); keep nodev as well...
#2. predictors to keep: grass, greenery, paved, roofs, impervious, sqrt(nodev), nodev,
#   trees, traffic, sqrt(popn)

#look at just traffic - option of transforming it to better deal with high traffic volumes 
#  not captured in our dataset
op <- par(mfrow=c(2,2), mar=c(3,3,3,1))
dotchart(sp$traffic, main="traffic", group=sp$loc)  
dotchart(sqrt(sp$traffic), main="sqrt(traffic)", group=sp$loc)  
dotchart(log(sp$traffic), main="log(traffic)", group=sp$loc)  
#while the traffic data don't really require a transformation, sqrt-transformation does improve
# the fit a bit.  log-transformation is too much.
# since actual traffic data range much higher than our dataset, Christian requests sqrt(traffic) transformation


op <- par(mfrow=c(2,3), mar=c(3,3,3,1))
dotchart(sp$NO_2, main="NO_2", group=sp$loc)
dotchart(sp$pm25, main="pm25", group=sp$loc)  
dotchart(sp$PM25_NA, main="PM25_NA", group=sp$loc)  
dotchart(sp$particulate_surface_area, main="partSA", group=sp$loc)
#dotchart(sqrt(sp$particulate_surface_area), main="sqrt partSA", group=sp$loc)  #no improvement on partSA
dotchart(sp$slope, main="slope", group=sp$loc)
dotchart(sqrt(sp$slope), main="sqrt slope", group=sp$loc)
#looks like the following require strong transformations:
#the following may require weak transformations: nodev, slope, partSA
#the following are uninformative b/c of low data quantities: shrub
#transformations: sqrt(slope); keep slope as well...
#3. predictors to keep: NO_2, pm25, PM25_NA, partSA, slope, sqrt(slope)

par(mfrow=c(3,4))
dotchart(sp$CO_emissions_residential, main="CO2_res", group=sp$loc)
dotchart(sqrt(sp$CO_emissions_residential), main="sqrt CO2_res", group=sp$loc)
# dotchart(sp$CO_emissions_total, main="CO2_tot", group=sp$loc)                     #don't use CO2_tot in our study! It includes categories missing in our catchment regions!
# dotchart(sqrt(sp$CO_emissions_total), main="sqrt CO2_tot", group=sp$loc)          #  (e.g., cement, airports, electrical production)
dotchart(sp$CO_emissions_commercial, main="CO2_com", group=sp$loc)
dotchart(sqrt(sp$CO_emissions_commercial), main="sqrt CO2_com", group=sp$loc)
dotchart(sp$CO_emissions_nonroad, main="CO2_nonroad", group=sp$loc)
dotchart(sqrt(sp$CO_emissions_nonroad), main="sqrt CO2_nonroad", group=sp$loc)
dotchart(sp$CO_emissions_onroad, main="CO2_road", group=sp$loc)
dotchart(sqrt(sp$CO_emissions_onroad), main="sqrt CO2_road", group=sp$loc)
#the following are uninformative b/c of low data quantities: CO_emissions_industrial
#transformations: sqrt(CO2_res), sqrt(CO2_road), sqrt(CO2_nonroad), sqrt(CO2_com); 
#4. predictors to keep: sqrt(CO2_res), sqrt(CO2_road), sqrt(CO2_nonroad), 
#     sqrt(CO2_com)

#try a new set of CO2 predictors: CO2_almostTotal and CO2_transport
sp$CO_almostTotal <- sp$CO_emissions_cmv + sp$CO_emissions_commercial + sp$CO_emissions_onroad + sp$CO_emissions_rail +
  sp$CO_emissions_residential
#sp$CO_corr <- sp$CO_emissions_cmv + sp$CO_emissions_onroad + sp$CO_emissions_rail + sp$CO_emissions_residential
sp$CO_transport <- sp$CO_emissions_cmv + sp$CO_emissions_onroad + sp$CO_emissions_rail  #this is general movement of goods/ people category
sp$CO_cmv_rail <- sp$CO_emissions_cmv + sp$CO_emissions_rail  #only include cmv (marine-related) and rail

par(mfrow=c(3,3))
dotchart(sp$CO_almostTotal, main="CO2_almostTotal", group=sp$loc)
dotchart(sqrt(sp$CO_almostTotal), main="sqrt CO2_almostTotal", group=sp$loc)
# dotchart(sp$CO_corr, main="CO2_corr", group=sp$loc)
# dotchart(sqrt(sp$CO_corr), main="sqrt CO2_corr", group=sp$loc)
dotchart(sp$CO_transport, main="CO2_transport", group=sp$loc)
dotchart(sqrt(sp$CO_transport), main="sqrt CO2_transport", group=sp$loc)
dotchart(sp$CO_cmv_rail, main="CO2_cmv_rail", group=sp$loc)
dotchart(sqrt(sp$CO_cmv_rail), main="sqrt CO2_cmv_rail", group=sp$loc)
dotchart(log(1 + sp$CO_cmv_rail), main="log CO2_cmv_rail", group=sp$loc)
#use sqrt_CO2_almostTotal, and sqrt_CO2_transport

# #these were attempts to figure out how to combine CO2 data wiht traffic data.  Not my favorite for explainability and justifiability
# sp$CO_cmv_rail_percent <- sp$CO_cmv_rail / sp$CO_transport
# sp$CO_MarineRail_to_Road_ratio <- sp$CO_cmv_rail / sp$CO_emissions_onroad
# dotchart(sp$CO_MarineRail_to_Road_ratio, main="ratio CMV/Rail to Onroad CO2", group=sp$loc)

#combine some sources of emissions (road, rail, marine = transport)
sp$CO_transport_max <- sp$CO_emissions_cmv_max + sp$CO_emissions_onroad_max + sp$CO_emissions_rail_max
sp$CO_transport_mean <- sp$CO_emissions_cmv_mean + sp$CO_emissions_onroad_mean + sp$CO_emissions_rail_mean
sp$CO_transport_sum <- sp$CO_emissions_cmv_sum + sp$CO_emissions_onroad_sum + sp$CO_emissions_rail_sum

par(mfrow=c(4,3))
dotchart(sp$CO_emissions_cmv_max, main="CO2_cmv_max", group=sp$loc)
dotchart(sp$CO_emissions_cmv_mean, main="CO2_cmv_mean", group=sp$loc)
dotchart(sp$CO_emissions_cmv_sum, main="CO2_cmv_sum", group=sp$loc)
dotchart(sp$CO_emissions_onroad_max, main="CO2_road_max", group=sp$loc)
dotchart(sp$CO_emissions_onroad_mean, main="CO2_road_mean", group=sp$loc)
dotchart(sp$CO_emissions_onroad_sum, main="CO2_road_sum", group=sp$loc)
dotchart(sp$CO_emissions_rail_max, main="CO2_rail_max", group=sp$loc)
dotchart(sp$CO_emissions_rail_mean, main="CO2_rail_mean", group=sp$loc)
dotchart(sp$CO_emissions_rail_sum, main="CO2_rail_sum", group=sp$loc)
dotchart(sp$CO_transport_max, main="CO2_transport_max", group=sp$loc)
dotchart(sp$CO_transport_mean, main="CO2_transport_mean", group=sp$loc)
dotchart(sp$CO_transport_sum, main="CO2_transport_sum", group=sp$loc)

par(mfrow=c(2, 3))
dotchart(sp$CO_transport_max, main="CO2_transport_max", group=sp$loc)
dotchart(sp$CO_transport_mean, main="CO2_transport_mean", group=sp$loc)
dotchart(sp$CO_transport_sum, main="CO2_transport_sum", group=sp$loc)
dotchart(sqrt(sp$CO_transport_max), main="sqrt_CO2_transport_max", group=sp$loc)
dotchart(sqrt(sp$CO_transport_mean), main="sqrt_CO2_transport_mean", group=sp$loc)
dotchart(sqrt(sp$CO_transport_sum), main="sqrt_CO2_transport_sum", group=sp$loc)


par(mfrow=c(4,3))
dotchart(sp$rail_max, main="rail_max", group=sp$loc)
dotchart(sp$rail_mean, main="rail_mean", group=sp$loc)
dotchart(sp$rail_sum, main="rail_sum", group=sp$loc)
dotchart(sp$rail_max, main="rail_max", group=sp$loc)
dotchart(sqrt(sp$rail_mean), main="sqrt_rail_mean", group=sp$loc)
dotchart(sqrt(sp$rail_sum), main="sqrt_rail_sum", group=sp$loc)
dotchart(sp$traffic_max, main="traffic_max", group=sp$loc)
dotchart(sp$traffic_mean, main="traffic_mean", group=sp$loc)
dotchart(sp$traffic_sum, main="traffic_sum", group=sp$loc)
dotchart(sqrt(sp$traffic_max), main="sqrt_traffic_max", group=sp$loc)
dotchart(sqrt(sp$traffic_mean), main="sqrt_traffic_mean", group=sp$loc)
dotchart(sqrt(sp$traffic_sum), main="sqrt_traffic_sum", group=sp$loc)



par(mfrow=c(3,2))
#dotchart(sp$no_dev, main="nodev", group=sp$loc)  
dotchart(sp$dev_pre_1975, main="dev pre 1975", group=sp$loc)  
dotchart(sp$dev_1975_1990, main="dev 1975-1990", group=sp$loc)  
dotchart(sp$dev_1990_2000, main="dev 1990-2000", group=sp$loc)  
dotchart(sp$dev_2000_2014, main="dev 2000-2014", group=sp$loc)  
dotchart(sp$devAge, main="dev age", group=sp$loc)  
# dotchart(sqrt(sp$devAge), main="sqrt dev age", group=sp$loc)
# dotchart(log(sp$devAge), main="ln dev age", group=sp$loc)
# dotchart(exp(sp$devAge), main="exp dev age", group=sp$loc)
dotchart((sp$devAge)^2, main="dev age squared", group=sp$loc)
#transformations: devAge^2; keep devAge as well...
#5. predictors to keep: dev_pre_1975, dev_1975_1990, dev_1990_2000, dev_2000_2014,
#   devAge, devAge2

par(mfrow=c(3,3))
#dotchart(sp$roof_AG, main="roof_AG", group=sp$loc)  #only one site, with very low proportion; don't use!
#dotchart(sp$roof_IND, main="roof_IND", group=sp$loc)  
dotchart(sp$roof_intURB, main="roof_intURB", group=sp$loc)  
dotchart(sqrt(sp$roof_intURB), main="sqrt roof_intURB", group=sp$loc)  
#dotchart(sp$roof_ruRES, main="roof_ruRES", group=sp$loc)  
dotchart(sp$roof_urbRES, main="roof_urbRES", group=sp$loc) 
dotchart(sqrt(sp$roof_urbRES), main="sqrt roof_urbRES", group=sp$loc) 
dotchart(sp$roof_intURB_IND, main="roof_intURB + IND", group=sp$loc)
dotchart(sqrt(sp$roof_intURB_IND), main="sqrt roof_intURB + IND", group=sp$loc)
dotchart(sp$roof_totRES, main="roof_totRES", group=sp$loc)
dotchart(sqrt(sp$roof_totRES), main="sqrt roof_totRES", group=sp$loc)
#looks like the following require strong transformations:
#the following may require weak transformations: roof_intURB, roof_urbRES, roof_intURB+IND, roof_totRES
#the following are uninformative b/c of low data quantities: roof_AG, roof_IND, roof_ruRES
#transformations: sqrt(roof_intURB), sqrt(roof_urbRES), sqrt(roof_intURB+IND); sqrt(roof_totRES);
#  keep untransformed versions as well, to see if untransformed data fit well for various COCs
#6. predictors to keep: roof_intURB, sqrt(roof_intURB), roof_urbRES, sqrt(roof_urbRES),
#   roof_intURB+IND, sqrt(roof_intURB+IND), roof_totRES, sqrt(roof_totRES)




#create new columns for transformed data
sp_t <- sp %>%
  dplyr::relocate(location, loc) %>%  #relocate "location" column to first column
  # dplyr::select(-c(loc)) %>%  #remove temporary column "loc"
  dplyr::rename(roofs=imperv_roofs, 
                paved=imperv_ground,
                trees=percent_tree_cover,
                nodev=no_dev,
                grass=grass_low_veg,
                roads=road_density,
                popn=pop_per_ha,
                no2=NO_2,
                CO2_res=CO_emissions_residential,
                CO2_com=CO_emissions_commercial,
                CO2_road=CO_emissions_onroad,
                CO2_nonroad=CO_emissions_nonroad,
                CO2_almostTotal=CO_almostTotal,
                CO2_transport=CO_transport,
                CO2_cmv_rail=CO_cmv_rail,
                pm25_na=PM25_NA,
                partSA=particulate_surface_area
                ) %>%
  dplyr::mutate(sqrt_intURB=sqrt(intURB),
                sqrt_totRES=sqrt(totRES),
                sqrt_nodev=sqrt(nodev),
                log_paved=log(paved),
                sqrt_popn=sqrt(popn),
                sqrt_traffic=sqrt(traffic),
                sqrt_traffic_max=sqrt(sp$traffic_max),
                sqrt_traffic_mean=sqrt(sp$traffic_mean), 
                sqrt_traffic_sum=sqrt(sp$traffic_sum),
                sqrt_slope=sqrt(slope),
                sqrt_CO2_res=sqrt(CO2_res),
                sqrt_CO2_com=sqrt(CO2_com),
                sqrt_CO2_road=sqrt(CO2_road),
                sqrt_CO2_nonroad=sqrt(CO2_nonroad),
                sqrt_CO2_almostTotal=sqrt(CO2_almostTotal),
                sqrt_CO2_transport=sqrt(CO2_transport),
                sqrt_CO2_cmv_rail=sqrt(CO2_cmv_rail),
                log_CO2_cmv_rail=log(1 + CO2_cmv_rail),
                sqrt_CO2_transport_max=sqrt(sp$CO_transport_max),
                sqrt_CO2_transport_mean=sqrt(sp$CO_transport_mean),
                sqrt_CO2_transport_sum=sqrt(sp$CO_transport_sum),
                devAge2=devAge^2,
                sqrt_roof_intURB=sqrt(roof_intURB),
                sqrt_roof_urbRES=sqrt(roof_urbRES),
                sqrt_roof_intURB_IND=sqrt(roof_intURB_IND),
                sqrt_roof_totRES=sqrt(roof_totRES)
  ) %>%
  dplyr::select(location,
                loc,
                ruRES,
                urbRES,
                totRES, #sqrt_totRES,
                intURB, #sqrt_intURB,
                intURB_IND,
                grass,
                greenery,
                greenery_bareEarth,
                not_greenBE,
                not_green,
                paved,
                log_paved,
                roofs,
                impervious,
                nodev, #sqrt_nodev,
                trees,
                sqrt_traffic, #traffic,  
                sqrt_traffic_max,
                sqrt_traffic_mean,
                sqrt_traffic_sum,
                roads,
                rail_max,
                rail_mean,
                rail_sum,
                sqrt_popn,
                no2,
                #pm25,
                pm25_na,
                partSA,
                sqrt_slope, #slope,
                sqrt_CO2_res,
                sqrt_CO2_com,
                sqrt_CO2_road,
                sqrt_CO2_nonroad,
                sqrt_CO2_almostTotal,
                sqrt_CO2_transport,
                sqrt_CO2_cmv_rail,
                log_CO2_cmv_rail,
                sqrt_CO2_transport_max,
                sqrt_CO2_transport_mean,
                sqrt_CO2_transport_sum,
                #dev_pre_1975,
                #dev_1975_1990,
                #dev_1990_2000,
                #dev_2000_2014,
                #devAge, 
                devAge2,
                roof_intURB, #sqrt_roof_intURB,
                roof_urbRES, #sqrt_roof_urbRES,
                roof_intURB_IND, #sqrt_roof_intURB_IND,
                roof_totRES #sqrt_roof_totRES,
                #AG
  )
                
              

sp_means <- sp_sds <- rep(NA, ncol(sp_t))
sp_means[3:ncol(sp_t)] <- colMeans(sp_t[, 3:ncol(sp_t)])
sp_sds[3:ncol(sp_t)] <- apply(sp_t[,3:ncol(sp_t)], 2, sd)

sp_std <- t((t(sp_t[, 3:ncol(sp_t)])-sp_means[3:ncol(sp_t)])/sp_sds[3:ncol(sp_t)])
sp_std <- data.frame(location=as.factor(sp_t[,c("location")]), loc=as.factor(sp_t[,c("loc")]), sp_std)
sp_final <- sp_std

write.csv(sp_final, file="../processed_data/spatial_predictors_standardized.csv", row.names=FALSE)


#save mean and sd for standardization as a csv file
sp_standardization_values <- data.frame(mean=sp_means[3:length(sp_means)],
                                        sd=sp_sds[3:length(sp_sds)])
rownames(sp_standardization_values) <- colnames(sp_t[3:ncol(sp_t)])

write.csv(sp_standardization_values, file="../processed_data/spatial_predictor_standardization_values.csv", row.names=TRUE)










#---------#
#  plots  #
#---------#


sp_test <- sp_final[,-1]
sp_2 <-
  pivot_longer(sp_test,
             cols=c(2:30))
dotplot(loc~value | name,  data=sp_2)

#dotchart(as.matrix(sp_test[, c(2:ncol(sp_test))]), groups=sp_test$loc)


#create new columns for transformed data
sp_1 <- sp %>%
  dplyr::relocate(loc) %>%  #relocate "loc" column to first column
  dplyr::rename(roofs=imperv_roofs, 
                paved=imperv_ground,
                trees=percent_tree_cover,
                nodev=no_dev,
                grass=grass_low_veg,
                popn=pop_per_ha,
                no2=NO_2,
                CO2_res=CO_emissions_residential,
                CO2_com=CO_emissions_commercial,
                CO2_road=CO_emissions_onroad,
                CO2_nonroad=CO_emissions_nonroad,
                CO2_almostTotal=CO_almostTotal,
                CO2_transport=CO_transport,
                pm25_na=PM25_NA,
                partSA=particulate_surface_area
  ) %>%
  dplyr::select(loc,
                ruRES,
                urbRES,
                totRES, #sqrt_totRES,
                intURB, #sqrt_intURB,
                intURB_IND,
                grass,
                greenery,
                trees,
                greenery_bareEarth,
                not_greenBE,
                paved,
                roofs,
                impervious,
                nodev, #sqrt_nodev,
                traffic, #traffic,
                popn,
                no2,
                #pm25,
                pm25_na,
                partSA,
                slope, #slope,
                CO2_res,
                CO2_com,
                CO2_road,
                CO2_nonroad,
                CO2_almostTotal,
                CO2_transport,
                #dev_pre_1975,
                #dev_1975_1990,
                #dev_1990_2000,
                #dev_2000_2014,
                #devAge, 
                devAge,
                roof_intURB, #sqrt_roof_intURB,
                roof_urbRES, #sqrt_roof_urbRES,
                roof_intURB_IND, #sqrt_roof_intURB_IND,
                roof_totRES #sqrt_roof_totRES,
                #AG
  )

#standardize the sp_1 (untransformed) predictors
sp_1_means <- sp_1_sds <- rep(NA, ncol(sp_1))
sp_1_means[2:ncol(sp_1)] <- colMeans(sp_1[, 2:ncol(sp_1)])
sp_1_sds[2:ncol(sp_1)] <- apply(sp_1[,2:ncol(sp_1)], 2, sd)
sp_1_std <- t((t(sp_1[, 2:ncol(sp_1)])-sp_1_means[2:ncol(sp_1)])/sp_1_sds[2:ncol(sp_1)])
sp_1_std <- data.frame(loc=as.factor(sp_1[,c("loc")]), sp_1_std)

#make the dataframe longer -- combine columns by predictor name
sp_1_std.a <- pivot_longer(sp_1_std,
                           cols=c(2:ncol(sp_1_std)))
names(sp_1_std.a) <- c("loc", "predictor", "standardized_value")




#create new columns for transformed data
sp_2 <- sp_1 %>%
  dplyr::mutate(sqrt_popn=sqrt(popn),
                sqrt_traffic=sqrt(traffic),
                sqrt_slope=sqrt(slope),
                sqrt_CO2_res=sqrt(CO2_res),
                sqrt_CO2_com=sqrt(CO2_com),
                sqrt_CO2_road=sqrt(CO2_road),
                sqrt_CO2_nonroad=sqrt(CO2_nonroad),
                sqrt_CO2_almostTotal=sqrt(CO2_almostTotal),
                sqrt_CO2_transport=sqrt(CO2_transport),
                devAge2=devAge^2,
  ) %>%
  dplyr::select(loc,
                sqrt_traffic, #traffic,
                sqrt_popn,
                sqrt_slope, #slope,
                sqrt_CO2_res,
                sqrt_CO2_com,
                sqrt_CO2_road,
                sqrt_CO2_nonroad,
                sqrt_CO2_almostTotal,
                sqrt_CO2_transport,
                devAge2
  )


#standardize the transformed sp_2 predictors
sp_2_means <- sp_2_sds <- rep(NA, ncol(sp_2))
sp_2_means[2:ncol(sp_2)] <- colMeans(sp_2[, 2:ncol(sp_2)])
sp_2_sds[2:ncol(sp_2)] <- apply(sp_2[,2:ncol(sp_2)], 2, sd)
sp_2_std <- t((t(sp_2[, 2:ncol(sp_2)])-sp_2_means[2:ncol(sp_2)])/sp_2_sds[2:ncol(sp_2)])
sp_2_std <- data.frame(loc=as.factor(sp_2[,c("loc")]), sp_2_std)

#make the dataframe longer -- combine columns by predictor name
sp_2_std.a <- pivot_longer(sp_2_std,
                           cols=c(2:ncol(sp_2)))
names(sp_2_std.a) <- c("loc", "predictor", "standardized_value")



#----------------------------#
#     PLOTS FOR TECH DOC     #
#----------------------------#

#plot of untransformed predictors
dotplot(loc~standardized_value | predictor,  data=sp_1_std.a, col="blue", layout=c(6,6))

#plot of untransformed predictors
dotplot(loc~standardized_value | predictor,  data=sp_2_std.a, col="red", layout=c(6,2))  #layout=(column, rows)







#-----------------------------------------------------------------------------------
# #old code
# 
# 
# #untransformed predictors
# op <- par(mfrow=c(6,5), mar=c(1,1,1,1), oma=c(0,0,0,0), xaxt="n", cex.main=1.5)
# dotchart(sp$urbRES, main="urbRES", group=sp$loc)  #urban residential
# dotchart(sp$ruRES, main="ruRES", group=sp$loc)  #rural residential
# dotchart(sp$totRES, main="totRES", group=sp$loc)  #total residential (urban + rural res)
# dotchart(sp$intURB, main="intURB", group=sp$loc, yaxt="n")  #intensive urban
# dotchart(sp$IND, main="IND", group=sp$loc)  #industrial
# dotchart(sp$intURB_IND, main="intURB_IND", group=sp$loc)
# dotchart(sp$grass_low_veg, main="grass", group=sp$loc)  
# dotchart(sp$greenery, main="greenery", group=sp$loc)
# dotchart(sp$percent_tree_cover, main="trees", group=sp$loc)  
# dotchart(sp$no_dev, main="nodev", group=sp$loc)  
# dotchart(sp$impervious, main="impervious", group=sp$loc)
# dotchart(sp$imperv_ground, main="paved", group=sp$loc)  
# dotchart(sp$imperv_roofs, main="roofs", group=sp$loc)  
# dotchart(sp$traffic, main="traffic", group=sp$loc)  
# dotchart(sp$pop_per_ha, main="popn", group=sp$loc) 
# dotchart(sp$NO_2, main="NO_2", group=sp$loc)
# dotchart(sp$PM25_NA, main="PM25_NA", group=sp$loc)  
# dotchart(sp$particulate_surface_area, main="partSA", group=sp$loc)
# dotchart(sp$slope, main="slope", group=sp$loc)
# dotchart(sp$CO_emissions_residential, main="CO2_res", group=sp$loc)
# dotchart(sp$CO_emissions_commercial, main="CO2_com", group=sp$loc)
# dotchart(sp$CO_emissions_nonroad, main="CO2_nonroad", group=sp$loc)
# dotchart(sp$CO_emissions_onroad, main="CO2_road", group=sp$loc)
# dotchart(sp$CO_almostTotal, main="CO2_almostTotal", group=sp$loc)
# dotchart(sp$CO_transport, main="CO2_transport", group=sp$loc)
# dotchart(sp$devAge, main="dev age", group=sp$loc)  
# dotchart(sp$roof_intURB, main="roof_intURB", group=sp$loc)  
# dotchart(sp$roof_ruRES, main="roof_ruRES", group=sp$loc)  
# dotchart(sp$roof_urbRES, main="roof_urbRES", group=sp$loc) 
# dotchart(sp$roof_intURB_IND, main="roof_intURB_IND", group=sp$loc)
# dotchart(sp$roof_totRES, main="roof_totRES", group=sp$loc)
# 
# 
# #transformed predictors
# op <- par(mfrow=c(6,5), mar=c(1,1,1,1), oma=c(0,0,0,0), xaxt="n", cex.main=1.5)
# #dotchart(sqrt(sp$totRES), main="sqrt_totRES", group=sp$loc)  #total residential (urban + rural res)
# #dotchart(sqrt(sp$intURB), main="sqrt_intURB", group=sp$loc, yaxt="n")  #intensive urban
# #dotchart(sqrt(sp$no_dev), main="sqrt_nodev", group=sp$loc)  
# dotchart(sqrt(sp$pop_per_ha), main="sqrt_popn", group=sp$loc) 
# dotchart(sqrt(sp$traffic), main="sqrt_traffic", group=sp$loc) 
# dotchart(sqrt(sp$slope), main="sqrt_slope", group=sp$loc)
# dotchart(sqrt(sp$CO_emissions_residential), main="sqrt_CO2_res", group=sp$loc)
# dotchart(sqrt(sp$CO_emissions_commercial), main="sqrt_CO2_com", group=sp$loc)
# dotchart(sqrt(sp$CO_emissions_nonroad), main="sqrt_CO2_nonroad", group=sp$loc)
# dotchart(sqrt(sp$CO_emissions_onroad), main="sqrt_CO2_road", group=sp$loc)
# dotchart(sqrt(sp$CO_almostTotal), main="sqrt CO2_almostTotal", group=sp$loc)
# dotchart(sqrt(sp$CO_transport), main="sqrt CO2_transport", group=sp$loc)
# dotchart((sp$devAge)^2, main="devAge^2", group=sp$loc)  
# #dotchart(sqrt(sp$roof_intURB), main="sqrt_roof_intURB", group=sp$loc)  
# #dotchart(sqrt(sp$roof_urbRES), main="sqrt_roof_urbRES", group=sp$loc) 
# #dotchart(sqrt(sp$roof_intURB_IND), main="sqrt_roof_intURB_IND", group=sp$loc)
# #dotchart(sqrt(sp$roof_totRES), main="sqrt_roof_totRES", group=sp$loc)
#               

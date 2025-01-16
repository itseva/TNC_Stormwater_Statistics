## Header ---------------------------
##
## Script name:get_predictors.R
##
## Abstract: This script is used to get landscape predictors from Google Earth Engine
## for use in stormwaterheatmap regressions.
##
## Author: Christian Nilsen, Geosyntec Consultants
## Email: cnilsen@geosyntec.com
##
## Date Created: 2021-01-24, Christian Nilsen
## Date Modified: 2023-11-09, EDJ - added Vulcan_rail and Vulcan_cmv to saved csv file
##
## Copyright (c) Geosyntec Consultants, 2021
##
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 3.0
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## For a copy of the GNU General Public License
##  see <https://www.gnu.org/licenses/>.
##


# Installation ------------------------------------------------------------

# # Uncomment to install the first time (only need to do this once)
#
# # Install the rgee package from GitHub
# install.packages("devtools")
# devtools::install_github("r-spatial/rgee")
# 
# #
# # rgee depends on reticulate because it has some Python dependencies (i.e. numpy and ee), run as follows to install them:
# rgee::ee_install()
# # If you are a Windows user reticulate requires miniconda/anaconda.
# # The use of rgee::ee_install() is not mandatory, you can count on with your own custom installation.
# #
# # After install rgee, you might use the function below for checking the status of rgee.
# ee_check() # Check non-R dependencies
# #if this is not working, try running the next few lines of script and coming back after libraries are updated
# 
# rgee::ee_install_upgrade()


# Libraries ---------------------------------------------------------------
library(rgee)
library(mapview)
library(purrr)
library(tidyverse)
library(data.table)
library(here)


#NOTE: this line must be run and authentication provided BEFORE continuing!
ee$Authenticate()  #the dollar sign on this function tells Python to do this.  ee is a wrapper around python commands


# initialize earth engine api:
ee$Initialize()


# Data  -------------------------------------------------------------------
watersheds <- ee$FeatureCollection("users/stormwaterheatmap/revised_s8_watersheds_v4")$select(c("Location_N"), c("Location")) # $filter(ee$Filter$neq('Location_N', 'POSOUTFALL_60')
# old watershed data with approximate(?) locations for Tacoma watershed
#watersheds <- ee$FeatureCollection("users/cnilsen/s8_watersheds")$select(c("Location_N"), c("Location")) # $filter(ee$Filter$neq('Location_N', 'POSOUTFALL_60')

## Get Predictor images from earth engine
## trees
tree_cover <- ee$Image("USGS/NLCD/NLCD2016")$select("percent_tree_cover")

## traffic
traffic <- ee$Image(0)$blend(ee$Image("users/cnilsen/traffic_raw"))$rename("traffic")

## population density
population <- ee$Image("users/stormwaterheatmap/population_per_ha")

## pm 2.5
pm25 <- (ee$Image("users/cnilsen/pm25clipped")$rename("pm25"))

## imperviousness:
tnc_landcover <- ee$Image("users/jrobertson2000/psLandCover_1m_finPS_roofs")#get land cover:
impervious <- tnc_landcover$eq(6)$Or(tnc_landcover$eq(7))$rename("impervious")# impervious (excl roofs) is coded as 6; roofs are coded as 7; this imperviousness is both combined
imp_ground <- tnc_landcover$eq(6)$rename("imperv_ground")  #roads & parking areas & sidewalks; Christian wouldn't do roads separately-- too correlated with impervious & traffic
imp_roofs <- tnc_landcover$eq(7)$rename("imperv_roofs")
grass_low_veg <- tnc_landcover$eq(1)$rename("grass_low_veg")
shrub_med_veg <- tnc_landcover$eq(2)$rename("shrub_med_veg")
#The tnc_landcover image can also be used for the following:
# values: [0, 1, 2, 3, 4,5, 6, 7]
# labels: [ 'No data',
#           'Grass/Low Vegetation',
#           'Shrub/Medium Vegetation',
#           'Trees/Forest',
#           'Bare soil',
#           'Water',
#           'Impervious – Except Roofs',
#           'Impervious - Roofs'
# ]

no2 <- ee$Image("users/stormwaterheatmap/SURFACE_NO2_010x010_2010")$rename("NO_2")

# age of development
#age_of_development <- ee$Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select("built")$rename("mean_dev_age") #mean value; this one is confusing b/c value of 2=no development
no_dev <- ee$Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select("built")$eq(2)$rename("no_dev") #proportion that has no development
age_2000_2014 <- ee$Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select("built")$eq(3)$rename("dev_2000_2014") #proportion that was built in 2000-2014
age_1990_2000 <- ee$Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select("built")$eq(4)$rename("dev_1990_2000") #proportion that was built in 1990-2000
age_1975_1990 <- ee$Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select("built")$eq(5)$rename("dev_1975_1990") #proportion that was built in 1975-1990
age_pre_1975 <- ee$Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1")$select("built")$eq(6)$rename("dev_pre_1975") #proportion that was built pre-1975

# slope
elevation <- ee$Image("USGS/NED")
slope <- ee$Terrain$slope(elevation)


# Commerce landuse data  --------------------------------------------------
# source: //https://www.commerce.wa.gov/serving-communities/growth-management/puget-sound-mapping-project/



landuse_table <- ee$FeatureCollection("users/stormwaterheatmap/psrc_landuse") 
#MASTER_CAT Master_num
##                         Undesignated          0
##                    Agricultural Area          1
##                               Tribal          2
##                         Forest Lands          3
##                      Intensive Urban          4
##          Rural Character Residential          5
##                                Water          6
##                                 PROW          7
##                                  ROW          8
##     Active Open Space and Recreation          9
##          Urban Character Residential         10
##                           Industrial         11
##                               Public         12
##Natural Preservation and Conservation         13
##                             Military         14
##                Mineral Resource Area         15



landuse <- landuse_table$reduceToImage(list('Master_num'),ee$Reducer$first())$rename("landuseCode")
# percent landuse
percent.urban_res <- landuse$eq(10)$rename("urbRES")
percent.industrial<-   landuse$eq(11)$rename("IND")
#percent.transportation <-  landuse$eq(3)$rename( "TRANS")
percent.int_urban <-  landuse$eq(4)$rename( "intURB")
#percent.ag_or_timber <-  landuse$eq(1)$Or(landuse$eq(3))$rename("AG")  #note: we don't have any forest lands in our watersheds -- this is AG only!
percent.ag <-  landuse$eq(1)$rename("AG")
percent.water <-landuse$eq(6)$rename("WATER")
percent.open_space <-  landuse$eq(13)$rename("OPEN")  #there is no "open" in our watersheds...
percent.rural_res <- landuse$eq(5)$rename("ruRES")
percent.public <- landuse$eq(12)$rename("PUBLIC")  #there is no "public" in our watersheds...


# roofs by landuse-------------------------------------------------------

#make a binary image of roofs 1 = roof, 0 = not roof
roofs <-  tnc_landcover$eq(7)

#intersect roofs and landuse values 
roofs_landuse <- landuse$multiply(roofs)$selfMask()
percent.roofs.urbRES <- roofs_landuse$eq(10)$rename("roof_urbRES")
percent.roofs.ruRES <- roofs_landuse$eq(5)$rename("roof_ruRES")
percent.roofs.IND <- roofs_landuse$eq(11)$rename("roof_IND")
#percent.roofs.TRANS <- roofs_landuse$eq(3)$rename("roof_TRANS")
percent.roofs.intURB <- roofs_landuse$eq(4)$rename("roof_intURB")
#percent.roofs.AG_TIMBER <- roofs_landuse$eq(1)$Or(roofs_landuse$eq(3))$rename("roof_AG")
percent.roofs.AG <- roofs_landuse$eq(1)$rename("roof_AG")
#percent.roofs.WATER <- roofs_landuse$eq(7)$rename("roof_WATER")
#percent.roofs.OPEN <- roofs_landuse$eq(8)$rename("roof_OPEN")

#palette for displaying land uses
landuse_pallete <- c(
  "#eb3121",
  "#6203ae",
  "#845609",
  "#ae39f3",
  "#efcb09",
#  "#a0c29b",
  "#476b9d",
  "#4c6c0a"
)


#display the results for roofs by landuse
#Map$centerObject(eeObject = roofs, zoom = 7)
#Map$addLayer(roofs_landuse,visParams = list(min = 1, max = 8, palette = landuse_pallete))

#gives an image of roofs
# "Residential": 1,
# "Industrial": 2,
# "Transportation ": 3,
# "Commercial": 4,
# "Agricultural": 5,
# "Timber and Resource Extraction": 6,
# "Water": 7,
# "open space": 8
# }


# CO Emissions ------------------------------------------------------------
#From Gurney, K.R., J. Liang, R. Patarasuk, Y. Song, J. Huang, and G. Roest. 2019. Vulcan: High-Resolution Annual Fossil Fuel 
#CO2 Emissions in USA, 2010-2015, Version 3. ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/1741

#############
##Sector Code	Description
# airport	Airport sector (taxi/takeoff to 3000’)
# cement	Cement production sector
# cmv	Commercial Marine Vessel sector
# commercial	Commercial sector
# elec_prod 	Electricity production sector
# industrial	Industrial sector
# nonroad	Nonroad sector (e.g. snowmobiles, ATVs)
# onroad	Onroad sector
# railroad	Railroad sector
# residential	Residential sector
# total	Total emissions

Vulcan_total <- ee$Image("users/stormwaterheatmap/Vulcan_total")$reduce('mean')$rename("CO_emissions_total")
Vulcan_cement <- ee$Image("users/stormwaterheatmap/Vulcan_cement")$reduce('mean')$rename("CO_emissions_cement")
Vulcan_elec_prod <- ee$Image("users/stormwaterheatmap/Vulcan_elec_prod")$reduce('mean')$rename("CO_emissions__elec_prod")
Vulcan_airport <- ee$Image("users/stormwaterheatmap/Vulcan_airport")$reduce('mean')$rename("CO_emissions_airport")
Vulcan_cmv <- ee$Image("users/stormwaterheatmap/Vulcan_cmv")$reduce('mean')$rename("CO_emissions_cmv")
Vulcan_commercial <- ee$Image("users/stormwaterheatmap/Vulcan_commercial")$reduce('mean')$rename("CO_emissions_commercial")
Vulcan_residential <- ee$Image("users/stormwaterheatmap/Vulcan_residential")$reduce('mean')$rename("CO_emissions_residential")
Vulcan_industrial <- ee$Image("users/stormwaterheatmap/Vulcan_industrial")$reduce('mean')$rename("CO_emissions_industrial")
Vulcan_nonroad <- ee$Image("users/stormwaterheatmap/Vulcan_nonroad")$reduce('mean')$rename("CO_emissions_nonroad")
Vulcan_onroad <- ee$Image("users/stormwaterheatmap/Vulcan_onroad")$reduce('mean')$rename("CO_emissions_onroad")
Vulcan_rail <- ee$Image("users/stormwaterheatmap/Vulcan_rail")$reduce('mean')$rename("CO_emissions_rail")



# Troposhphreic Air Quality  ----------------------------------------------


v4_pm25 <- ee$Image("users/stormwaterheatmap/V4NA03_PM25_NA_201001_201012-RH35-NoNegs")$rename("PM25_NA")
sa <- ee$Image("users/stormwaterheatmap/surface_area")$rename("particulate_surface_area")


# Make Map for One Predictor -----------------------------------------------
map_image <- Vulcan_rail #landuse #Vulcan_total #v4_pm25 #landuse
map_viz <- list(min = 2, max = 7, palette = list("black", "yellow","red"), opacity = 0.5)
Map$centerObject(eeObject = watersheds, zoom = 7)
Map$addLayer(map_image, visParams = map_viz) +
  Map$addLayer(watersheds)

# map_image <- pm25  #can change this to other variables to see other things (this predictor is used in map)
# map_viz <- list(min = 4.5, max = 6.8, palette = list("blue", "green", "yellow", "orange", "red"), opacity = 0.5) #0 to 100 for trees; 0 to 1 for proportions; 4.5 to 6.8 for pm25
# Map$centerObject(eeObject = watersheds, zoom = 7)
# Map$addLayer(map_image, visParams = map_viz) +
#   Map$addLayer(watersheds)


# Reduce Predictors -------------------------------------------------------
## combine predictors in one dataset (one band each) - this is an image
predictors <- ee$Image(0)$blend(
  ee$Image$cat(percent.urban_res, percent.industrial, 
               #percent.transportation,
               percent.int_urban, percent.ag, percent.water, percent.open_space, 
               percent.public, percent.rural_res, 
               impervious, imp_ground, imp_roofs, no2, grass_low_veg, shrub_med_veg, tree_cover, traffic, population, 
               pm25, slope, no_dev, age_2000_2014, age_1990_2000, age_1975_1990, age_pre_1975,
               percent.roofs.AG, percent.roofs.intURB, percent.roofs.IND, percent.roofs.urbRES, percent.roofs.ruRES,
               #percent.roofs.TRANS, 
               #percent.roofs.OPEN, percent.roofs.WATER, 
               Vulcan_total, 
               Vulcan_cmv,
               Vulcan_commercial, 
               Vulcan_residential, 
               Vulcan_nonroad, 
               Vulcan_onroad, 
               Vulcan_industrial,
               Vulcan_rail,
               v4_pm25, sa)
)


## calculate mean stats from earth engine
ee_stats <- predictors$reduceRegions(
  collection = watersheds,  #this is what we gave it for 16 watersheds; Christian has one for Puget Lowlands; will try in Sandbox to see if it works
  scale = 30,  #30m per pixel
  reducer = ee$Reducer$mean()
)

# evaluate ee object - pulls data from server to client
ee_df <- ee_stats$getInfo()

# wrangle the data
df_predictors <- ee_df$features %>%  #band names diff't than image names; constant is traffic
  map("properties") %>%
  rbindlist(fill=TRUE)
#built refers to buildup index.  Those are categorical data - maybe shouldn't be averaged?


# Results -----------------------------------------------------------------
View(df_predictors)

# pivot for charting
df_long <- df_predictors %>%
  pivot_longer(cols = -c(Location_N))

# plot value of each predictor for the 16 watersheds
ggplot(df_long) +
  geom_col(aes(x = Location_N, y = value), fill = "cadetBlue", position = "dodge") +
  facet_wrap(~name, scales = "free")

# save csv file of predictors
write.csv(df_predictors, here("..", "data", "spatial_predictors_raw.csv"), row.names=FALSE)


#---------------------------------------------
# To extract what was in other watersheds?

#whole model domain: we want to predict what is goign on in urbanized areas.  Constrain just to puget lowlands?
#probably 2000 watersheds.  EE might crash?  Or do at a larger scale?  




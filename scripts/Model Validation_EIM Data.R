# Import data obtained through Ecology's EIM search engine, and explore how many samples are available per constituent for
# outfalls that are suitable to our study

# Author: Eva Dusek Jennings
# Date: Mar 29, 2024
#-------------------------------------------------------------------------------------------------

library(here)
library(tidyverse)

parameters_EIM <- c("Zinc", "Copper", "Total Phosphorus", "Phosphorus", "Total Nitrogen, mixed forms as N", "Nitrate + Nitrite as N", "Total Suspended Solids", "Total Kjeldahl Nitrogen",
                "Total Persulfate Nitrogen", "Ammonia", "Nitrite-Nitrate")


#-------------------------------#
#  Town of Coupeville G0900060  #   Copper, Zinc
#-------------------------------#

coupeville_results <- read.csv(here("..", "data", "model validation data", "Coupeville G0900060", "EIMDiscreteResults_2024Feb13_594.csv"))
# coupeville_loc <-  read.csv(here("..", "data", "model validation data", "Coupeville G0900060", "EIMLocationDetails_2024Feb13_5.csv"))
# 
# head(coupeville_loc)
# 
# coupeville_loc1 <- coupeville_loc %>%
#   filter(Location_Setting=="Source-ManMade Industrial, agricultural, stormwater, sewer/septic, discharge/pipe, lagoon, or other source.")

coupe_res <- coupeville_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="Water",
         Result_Parameter_Name %in% parameters_EIM)

#coupe_res %>% count(Location_Name, Result_Parameter_Name)
coupe_res %>% count(Result_Parameter_Name, Location_Name)

#------------------------------------#
#  Tacoma S8C 2013-18 WAR044003_S8C  #  Copper, Zinc, Total Phosphorus, TSS, Total Nitrogen, mixed forms as N, Nitrate + Nitrite as N, (TKN = TN - Nitrite+Nitrate)
#------------------------------------#

tacoma_s8c_results <- read.csv(here("..", "data", "model validation data", "Tacoma S8C 2013_18 WAR044003_S8C", "EIMDiscreteResults_2024Feb13_21867.csv"))

unique(tacoma_s8c_results$Result_Parameter_Name)

tac_precip <- tacoma_s8c_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         # Field_Collection_Start_Date, Field_Collection_End_Date,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Reporting_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(#Location=Location_Name,
         Analyte=Result_Parameter_Name,
         Lab_Result=Result_Value,
         Lab_Unit=Result_Value_Units,
         Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="Water",
         Analyte == "Precipitation", 
         !Result_Data_Qualifier %in% "REJ") %>%
  mutate(Location=substr(Location_Name, 15, nchar(Location_Name)) )

tac_res <- tacoma_s8c_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         # Field_Collection_Start_Date, Field_Collection_End_Date,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Reporting_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(#Location=Location_Name,
         Analyte=Result_Parameter_Name,
         Lab_Result=Result_Value,
         Lab_Unit=Result_Value_Units,
         Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="Water",
         Analyte %in% parameters_EIM, 
         !Result_Data_Qualifier %in% "REJ") %>%
  mutate(Location=substr(Location_Name, 15, nchar(Location_Name)) )


#what level of precip was found in the locations (specifically for one-day precip, used for TSS)  
tac_res1 <- tac_res %>%
  select(Location, Analyte, 
         Start_Date=Field_Collection_Start_Date_Time,
         End_Date=Field_Collection_End_Date_Time,
         Lab_Result,
         Lab_Unit,
         Result_Detection_Limit) %>%
  filter(Analyte %in% c("Copper", "Zinc", "Total Phosphorus", "Total Suspended Solids"))

tac_precip1 <- tac_precip %>%
  select(Location, Analyte, 
         Start_Date=Field_Collection_Start_Date_Time,
         End_Date=Field_Collection_End_Date_Time,
         Lab_Result,
         Lab_Unit) %>%
  filter(!Location=="RG15CUW")



match(tac_precip$Field_Collection_Start_Date_Time, tac_res$Field_Collection_Start_Date_Time)
match(tac_precip$Field_Collection_End_Date_Time, tac_res$Field_Collection_End_Date_Time)

tac_precip[110,]
tac_res[664, ]
tac_res %>% count(Analyte, Location)

#what units are used for each analyte?  Do we need to convert any units to ug/L?
unique(tac_res$Lab_Unit)
unique(tac_res$Analyte[which(tac_res$Lab_Unit=="ug/L")])  #copper & zinc are in ug/L
unique(tac_res$Analyte[which(tac_res$Lab_Unit=="mg/L")])  #TSS, Nitrogen forms & phosphorus are in mg/L

#are there any results at or below the detection limit? (indicating ND result)
which(tac_res$Result_Data_Qualifier %in% c("U", "UJ"))  #U indicates that the contaminant was undetected.  UJ indicates that the contaminant was undetected but the detection limit is estimated b/c of interference in the sample

tac_res$Analyte[which(tac_res$Result_Data_Qualifier %in% c("U", "UJ"))]  #looks like some ND's from each analyte, with 21 total from the 2676 samples.  Seems not worth worrying about

#make all units ug/l
tac_res <- tac_res %>%
  mutate(Result = case_when(Lab_Unit=="mg/L" ~ Lab_Result*1000,   #make all units ug/l
                            TRUE ~ Lab_Result),
         Unit = case_when(Lab_Unit=="mg/l" ~ "ug/l",
                          TRUE ~ "ug/l")) %>%
  mutate(NDFlag = case_when(Result_Data_Qualifier %in% c("U", "UJ") ~ TRUE,
                            TRUE ~ FALSE))


###  Calculate TKN using total nitrogen and nitrite + nitrate values.  Approximation of TKN is:  TNK = Total Nitrogen - (Nitrate + Nitrite).  

#make a new DF with calculations for TKN (total nitrogen - nitrite_nitrate)
tac_tkn_calc <- tac_res %>%
  filter(Analyte %in% c("Nitrate + Nitrite as N", "Total Nitrogen, mixed forms as N")) %>%
  pivot_wider(id_cols=c(Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time, Location, NDFlag, Latitude, Longitude),
              names_from=Analyte,
              values_from=Result,
              values_fn=list) %>%
  unnest(cols=c("Nitrate + Nitrite as N",
                "Total Nitrogen, mixed forms as N")) %>%
  rename(Nitrite_Nitrate="Nitrate + Nitrite as N",
         Tot_N="Total Nitrogen, mixed forms as N") %>%
  mutate(Result=Tot_N - Nitrite_Nitrate,
         Analyte="Calculated TKN",
         Unit="ug/l") %>%
  filter(is.na(Result) == FALSE)




#--------------------------------#
#  Caldart Ave Poulsbo G0700270  #  Total Phosphorus, TSS
#--------------------------------#  ***** too few locations to be worth it *****

# caldart_results <- read.csv(here("..", "data", "model validation data", "Caldart Ave Poulsbo G0700270", "EIMDiscreteResults_2024Feb26_54.csv"))
# 
# unique(caldart_results$Result_Parameter_Name)
# 
# cal_res <- caldart_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_ID %in% c("CALD_STMWTR_S1I", "CALD_STMWTR_S2I", "CALD_STMWTR_S3I"))
# 
# cal_res %>% count(Result_Parameter_Name, Location_Name)


#-------------------------#
#  White Center G0900041  #  Phosphorus, TSS, (Total Phosphorus =? Phosphorus)
#-------------------------#

# white_ctr_results <- read.csv(here("..", "data", "model validation data", "White Center G0900041", "EIMDiscreteResults_2024Feb27_539.csv"))
# 
# unique(white_ctr_results$Result_Parameter_Name)
# white_ctr_results[which(white_ctr_results$Result_Parameter_Name=="Phosphorus"),]
# 
# wc_res <- white_ctr_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_Name %in% c("OUTFLOW/PARKING_WETLAND", "OUTFALL_TO_MALLARDLK", "INFLOW _STORMCELL1_N.SIDE", "OUTFLOW/FROM_N.TO_MALLARDLK", "OUTLFOW/BALLFIELDS_TO_LAKE"))
# 
# wc_res %>% count(Result_Parameter_Name, Location_Name)
# 

#------------------------------#
#  Lower White River GPEL0010  #  Total Phosphorus, TSS, Nitrite + Nitrate as N, Ammonia, Total Persulfate Nitrogen (TKN ?= Persulfate N - Nitrite+Nitrate)
#------------------------------#  ***** too few locations to be worth it *****

# lower_white_river_results <- read.csv(here("..", "data", "model validation data", "Lower White River GPEL0010", "EIMDiscreteResults_2024Feb26_2361.csv"))
# 
# unique(lower_white_river_results$Result_Parameter_Name)
# 
# lwr_res <- lower_white_river_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_ID %in% c("10-UNW-SW0.9", "10-UNW-SW3.3", "10-UNW-SW6.2"))
# 
# lwr_res %>% count(Result_Parameter_Name, Location_Name)


#--------------------------------#
#  Lake WA PFAS Survey SWON0003  #  TSS  ---  all of these samples are time-weighted (similar enough to flow-weighted), so we can use them all!
#--------------------------------#

lake_WA_results <- read.csv(here("..", "data", "model validation data", "Lake Washington PFAS Survey SWON0003", "EIMDiscreteResults_2024Feb26_11651.csv"))

unique(lake_WA_results$Result_Parameter_Name)
unique(lake_WA_results$Location_ID)

lkwa_res <- lake_WA_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(Location=Location_ID,
         Analyte=Result_Parameter_Name,
         Lab_Result=Result_Value,
         Lab_Unit=Result_Value_Units,
         Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="Water",
         Analyte %in% parameters_EIM,
         Location %in% c("N.MER", "520-W", "KIR-CW", "REN-JC", "KIR-63RD", "REN-CMU", "BEL-SHORE", "KEN-SWAMP"),
         !Result_Data_Qualifier %in% "REJ")

lkwa_res %>% count(Analyte, Location)  #all samples are time-weighted

#what units are used for each analyte?  Do we need to convert any units to ug/L?
unique(lkwa_res$Lab_Unit)  #all TSS results are in mg/l -- need to convert to ug/l!

#are there any results at or below the detection limit? (indicating ND result) -- NONE!
which(lkwa_res$Result_Data_Qualifier %in% c("U", "UJ"))  #U indicates that the contaminant was undetected.  UJ indicates that the contaminant was undetected but the detection limit is estimated b/c of interference in the sample

#make all units ug/l
lkwa_res <- lkwa_res %>%
  mutate(Result = case_when(Lab_Unit=="mg/L" ~ Lab_Result*1000,   #make all units ug/l
                            TRUE ~ Lab_Result),
         Unit = case_when(Lab_Unit=="mg/l" ~ "ug/l",
                          TRUE ~ "ug/l")) %>%
  mutate(NDFlag = case_when(Result_Data_Qualifier %in% c("U", "UJ") ~ TRUE,
                            TRUE ~ FALSE))

## ALL SAMPLES ARE TIME-WEIGHTED!  CAN USE LIKE FLOW-WEIGHTED SAMPLES!


#-------------------------------#
#  Lake Meridian LkMeridian_WQ  #  Total Phosphorus
#-------------------------------#

# lake_meridian_results <- read.csv(here("..", "data", "model validation data", "Lake Meridian LKMeridian_WQ", "EIMDiscreteResults_2024Feb13_11731.csv"))
# 
# unique(lake_meridian_results$Result_Parameter_Name)
# unique(lake_meridian_results$Location_ID)
# 
# lkmer_res <- lake_meridian_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_ID %in% c("MER_S1", "MER_S2", "MER_S3", "MER_S4", "MER_S5"))
# 
# lkmer_res %>% count(Result_Parameter_Name, Location_ID)
# 
# unique(lkmer_res$Sample_Collection_Method)

#------------------------#
#  Lahti Drive G1100234  #  TSS, Phosphorus
#------------------------#

lahti_drive_results <- read.csv(here("..", "data", "model validation data", "Lahti Drive G1100234", "EIMDiscreteResults_2024Feb26_111.csv"))

unique(lahti_drive_results$Result_Parameter_Name)
unique(lahti_drive_results$Location_ID)

lahti_res <- lahti_drive_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Sample_Replicate_Flag,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="Water",
         Result_Parameter_Name %in% parameters_EIM,
         Sample_Replicate_Flag != "Y",
         Location_ID %in% c("Lahti-swale-in")) %>%
  select(!Sample_Replicate_Flag)

lahti_res %>% count(Result_Parameter_Name, Location_Name)


#-------------------------#
#  Indian Creek BERA0010  #  Copper, Zinc, TSS
#-------------------------#  ***** too few locations to be worth it *****

# indian_creek_results <- read.csv(here("..", "data", "model validation data", "Indian Creek BERA0010", "EIMDiscreteResults_2024Feb13_1153.csv"))
# 
# unique(indian_creek_results$Result_Parameter_Name)
# unique(indian_creek_results$Location_ID)
# 
# ic_res <- indian_creek_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_ID %in% c("PLUM STW", "QUINCE STW"))
# 
# ic_res %>% count(Result_Parameter_Name, Location_Name)


#----------------------------------------------------#
#  Watershed Friendly Gardening Ed Program G0500170  #  TSS, Total Phosphorus
#----------------------------------------------------#

garden_ed_results <- read.csv(here("..", "data", "model validation data", "Watershed Friendly Gardening Ed Prog G0500170", "EIMDiscreteResults_2024Feb13_570.csv"))

unique(garden_ed_results$Result_Parameter_Name)
unique(garden_ed_results$Location_Name)

ge_res <- garden_ed_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="Water",
         Result_Parameter_Name %in% parameters_EIM,
         Location_Name %in% c("WOODLAKE ROAD 2 (NEAR BROOKEDGE CT)", "WOODLAKE ROAD 1 (NEAR AVALON CT)"))

ge_res %>% count(Result_Parameter_Name, Location_Name)


#-------------------------#
#  Henderson Inlet DSAR2  #  Total Phosphorus
#-------------------------#  ***** too few locations to be worth it *****

# henderson_inlet_results <- read.csv(here("..", "data", "model validation data", "Henderson Inlet DSAR2", "EIMDiscreteResults_2024Feb26_3328.csv"))
# 
# unique(henderson_inlet_results$Result_Parameter_Name)
# unique(henderson_inlet_results$Location_ID)
# 
# hi_res <- henderson_inlet_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_ID %in% c("WL2.6SW"))
# 
# hi_res %>% count(Result_Parameter_Name, Location_Name)


#-------------------------#
#  Mason County G0800631  #  Copper, TSS, Zinc
#-------------------------#

mason_county_results <- read.csv(here("..", "data", "model validation data", "Mason County G0800631", "EIMDiscreteResults_2024Feb13_3189.csv"))

unique(mason_county_results$Result_Parameter_Name)
unique(mason_county_results$Location_Name)

mc_res <- mason_county_results %>%
  select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
         Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
         Sample_Matrix, Sample_Source, Sample_Collection_Method,
         Result_Parameter_Name, Result_Value, Result_Value_Units,
         Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
         Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  filter(Sample_Matrix=="WATER",
         Result_Parameter_Name %in% parameters_EIM,
         Location_Name %in% c("MASON_BELFAIR_BF-035", "MASON_BELFAIR_BF-034", "MASON_HOODSPORT_HS-033", "MASON_NORTH_BAY_NB-001", "MASON_NORTH_BAY_NB-014",
                            "MASON_NORTH_BAY_NB-018", "MASON_NORTH_BAY_NB-019", "MASON_NORTH_BAY_NB-020"))

mc_res %>% count(Result_Parameter_Name, Location_Name)

mason_county_results[which(mason_county_results$Location_Name=="MASON_NORTH_BAY_NB-014"), ]  #this one is just for fecal coliform

#-------------------------#
#  Lower Duwamish LDWISS  #  Copper, TSS, Zinc
#-------------------------#  ***** too few locations to be worth it *****

# lower_duwamish_results <- read.csv(here("..", "data", "model validation data", "Lower Duwamish Waterway LDWISS", "EIMDiscreteResults_2024Feb26_30540.csv"))
# 
# unique(lower_duwamish_results$Result_Parameter_Name)
# unique(lower_duwamish_results$Location_ID)
# 
# lduw_res <- lower_duwamish_results %>%
#   select(Location_ID, Location_Name, Location_Setting, Field_Collection_Type,
#          Field_Collection_Start_Date_Time, Field_Collection_End_Date_Time,
#          Sample_Matrix, Sample_Source, Sample_Collection_Method,
#          Result_Parameter_Name, Result_Value, Result_Value_Units,
#          Result_Detection_Limit, Result_Data_Qualifier, Result_Suspect_or_Rejected_Flag,
#          Result_Method, Result_Method_Description, Result_Lab_Name, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   rename(Latitude=Calculated_Latitude_Decimal_Degrees_NAD83HARN,
#          Longitude=Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
#   filter(Sample_Matrix=="Water",
#          Result_Parameter_Name %in% parameters_EIM,
#          Location_Name %in% c("Outfall OF002", "Outfall OF006"))
# 
# lduw_res %>% count(Result_Parameter_Name, Location_Name)








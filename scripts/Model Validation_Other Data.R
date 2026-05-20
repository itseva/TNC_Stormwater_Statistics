


# Author: Eva Dusek Jennings
# Date: Dec 9, 2024
#-------------------------------------------------------------------------------------------------

library(here)
library(tidyverse)
library(stringr)
library(zoo)
library(ggplot2)


#-------------------#
#  Highway Dataset  #   #all samples (except maybe those with collection method="Manual") should be event mean concentration (EMC) according to release notes:
#-------------------#   #https://www.usgs.gov/software/hrdb-highway-runoff-database-software-page
                        #NOTE: some locations are duplicate data with WSDOT 2014 dataset, but the dates here are wrong.  Use WSDOT data instead

highway_dataset_results <- read.csv(here("..", "data", "model validation data", "Highway Dataset", "tblQWHighwayDataSet.csv"))
hwy_data_storm_events <- read.csv(here("..", "data", "model validation data", "Highway Dataset", "tblStormEvent.csv"))

highway_dataset_results <- left_join(x=highway_dataset_results, 
                                     y=hwy_data_storm_events[, c("StormEvent_ID", "dtmEventDate")], 
                                     join_by("StormEvent.ID"=="StormEvent_ID"))

#select the parameters that we need
unique(highway_dataset_results$Parameter.Name)
all_pars <- unique(highway_dataset_results$Parameter.Name)
parameters <- grep(pattern="Zinc, water, unfiltered|Copper, water, unfiltered|Phosphorus|Solids, suspended|TKN|Nitrite|Nitrate|Ammonia|Nitrogen", x=all_pars, ignore.case=TRUE, value=TRUE)

WA <- grep(pattern="WA ", x=unique(highway_dataset_results$T.Site.Name), value=TRUE)  #sites that are in the state of Washington

hwy_res <- highway_dataset_results %>%
  mutate(Date=as.POSIXct(dtmEventDate, format="%Y-%m-%d"),
         Year=lubridate::year(Date)) %>%
  select(Date, Year, Storm=StormEvent.ID, #Emc.Id,
         Analyte=Parameter.Name, 
         Location=T.Site.Name, Description=T.Location.Description, HwyMP=T.Highway.Mile.Post, BMP=T.Bmp,
         Lab_Result=Concentration..EMC., NDFlag=NonDetectFlag,
         T.Analysis.Method, T.Collection.Method, #CollectionMethod.ID, 
         ADT, 
         Dataset=tQWHighwayDataSet..tblQWHighwayDataSet.txt., Latitude=D.Latitude, Longitude=D.Longitude) %>%
  filter(Location %in% WA,
         Analyte %in% parameters, 
         Year >= 2001,
         Longitude < -120) %>%  #remove Spokane (longitude=-117) data from the list
  mutate(Lab_Unit = case_when(str_detect(Analyte, "micrograms per liter") ~ "ug/l",                                              #change all units to ug/l
                              str_detect(Analyte, "milligrams per liter") ~ "mg/l"), 
         Analyte = case_when(Analyte=="Copper, water, unfiltered, recoverable, micrograms per liter" ~ "Total Copper",           #change long analyte names to shorter ones
                             Analyte=="Zinc, water, unfiltered, recoverable, micrograms per liter" ~ "Total Zinc",
                             Analyte=="Solids, suspended, water, milligrams per liter" ~ "Total Suspended Solids",
                             Analyte=="Phosphorus, water, unfiltered, milligrams per liter" ~ "Phosphorus, unfiltered"
                             # Analyte=="Orthophosphate, water, unfiltered, milligrams per liter as phosphorus" ~ "Orthophosphate, unfitered, as P",
                             # Analyte=="Total nitrogen, water, unfiltered, milligrams per liter" ~ "Total Nitrogen",
                             # Analyte=="Nitrite plus nitrate, water, unfiltered, milligrams per liter as nitrogen" ~ "Nitrite plus Nitrate"
                             ))   #ONLY KEEP ANALYTES THAT CORRESPOND TO THE ANALYTES IN OUR MODEL FITTING DATASET

hwy_res %>% count(Analyte, Location)

hwy_res_locs <- hwy_res %>%
  select(Location, Description, HwyMP, BMP, Latitude, Longitude) %>%
  unique()
write.csv(hwy_res_locs, "../data/model validation data/Highway Dataset/hwy_dataset_locs.csv")

#what units are used for each analyte?  Do we need to convert any units to ug/L?
unique(hwy_res$Lab_Unit)
unique(hwy_res$Analyte[which(hwy_res$Lab_Unit=="ug/l")])  #copper & zinc are in ug/L
unique(hwy_res$Analyte[which(hwy_res$Lab_Unit=="mg/l")])  #TSS, Nitrogen forms & phosphorus are in mg/L

#make all units ug/l
hwy_res <- hwy_res %>%
  mutate(Result = case_when(Lab_Unit=="mg/l" ~ Lab_Result*1000,   #make all units ug/l
                            TRUE ~ Lab_Result),
         Unit = case_when(Lab_Unit=="mg/l" ~ "ug/l",
                          TRUE ~ "ug/l"))

unique(hwy_res$T.Collection.Method)  #automatic samplers? (it says: "Automatic")
unique(hwy_res$Year)  #which years were data collected?
unique(hwy_res$Dataset)  #datasets represented in these data


# ###  Calculate TKN using total nitrogen and nitrite + nitrate values.  Approximation of TKN is:  TNK = Total Nitrogen - (Nitrate + Nitrite). 
# ###### THIS CALCULATION DOES NOT RESULT IN ACCURATE TKN VALUES!  DON'T USE!
# 
# #verify whether there are any ND values for total nitrogen or for nitrite+nitrate
# hwy_res_test <- hwy_res %>%
#   filter(Analyte %in% c("Nitrite plus Nitrate",
#                         "Total Nitrogen"))
# hwy_res_test[which(hwy_res_test$NDFlag==TRUE),]  #no flagged values for nitrite_nitrate or tot_N
# 
# 
# #make a new DF with calculations for TKN (total nitrogen - nitrite_nitrate)
# hwy_tkn_calc <- hwy_res %>%
#   filter(Analyte %in% c("Nitrite plus Nitrate", "Total Nitrogen")) %>%
#   pivot_wider(id_cols=c(Date, Storm, Location, NDFlag, ADT, Latitude, Longitude),
#               names_from=Analyte,
#               values_from=Result,
#               values_fn=list) %>%
#   unnest(cols=c("Nitrite plus Nitrate",
#                 "Total Nitrogen")) %>%
#   rename(Nitrite_Nitrate="Nitrite plus Nitrate",
#          Tot_N="Total Nitrogen") %>%
#   mutate(Result=Tot_N - Nitrite_Nitrate,
#          Analyte="Calculated TKN",
#          Unit="ug/l") %>%
#   filter(is.na(Result) == FALSE)
# 
# 
# length(which(hwy_tkn_calc$Result>0))  #how many TKN calculations exist (where Tot_N and Nitrite_Nitrate values existed for a particular Date, Storm and Location)


#what about phosphorus? Is phosphorus, unfiltered the same as total phosphorus?  According to the Hach document, I think "Phosphorus" is total phosphorus.  It would otherwise be recorded as: Phosphate or Orthophosphate
hwy_res[which(hwy_res$Analyte=="Phosphorus, unfiltered"),]

#hwy_res[which(hwy_res$Analyte=="Orthophosphate, unfitered, as P"),]  #heavens!  These methods are the same as the Phosphorus, unfiltered methods!  Which one is Total Phosphorus?  Do I need to add them??

#remove duplicates that are found in the WSDOT dataset
hwy_res <- hwy_res %>%
  filter(! Location %in% c("WA SR 9 Northbound Mile Post 17.92 Marysville", 
                          "WA I-5 Northbound Mile Post 197.27 Everett",
                          "WA I-5 Northbound Mile Post 197.35 Everett",
                          "WA I-5 Southbound Mile Post 210.71 at Pilchuck Creek") )


#-----------------------------#
#  MLK Street Sweeping Study  #   
#-----------------------------#   

mlk_sweeping_results <- read.csv(here("..", "data", "model validation data", "MLK Street Sweeping Study", "StreetSweepWQEffectiveness2014-16_PESonly_0.5xND_4JAN2017.csv"))

names(mlk_sweeping_results)

#select the parameters that we need
unique(mlk_sweeping_results$CHEMICAL_NAME)
all_pars <- unique(mlk_sweeping_results$CHEMICAL_NAME)
parameters <- grep(pattern="Zinc|Copper|Phosphorus, Total|Kjeldahl|Solids, Total Suspended", x=all_pars, ignore.case=TRUE, value=TRUE)

mlk_res <- mlk_sweeping_results %>%
  filter(CHEMICAL_NAME %in% parameters) 

#is there any difference between the columns REPORT_xxx_xxx and xxx_xxx?
attach(mlk_res)
mlk_res[which(REPORT_RESULT_VALUE - RESULT_NUMERIC != 0), ]  #there are seven instances of -0.5 here, which are for TKN when it was ND.  Use reporting limit (1mg/l) which is in RESULT_NUMERIC
unique(LAB_QUALIFIERS)  #looks like there are no instances of "R", which indicates data should be rejected
unique(INTERPRETED_QUALIFIERS)  #no data rejects
unique(VALIDATOR_QUALIFIERS)  #no data rejects
unique(REPORTABLE_RESULT)  #all results are reportable
unique(VALIDATED_YN)  #all data have been validated
#Conclusion: use the RESULT_NUMERIC (rather than REPORT_RESULT_VALUE) column; no data rejects, and all data have been validated & are reportable
detach()

#what units are used for each analyte?  In the piped code below, we convert all units to ppm (ug/l)
unique(mlk_res$Analyte[which(mlk_res$Unit=="ug/l")])  #copper & zinc are in ug/l
unique(mlk_res$Analyte[which(mlk_res$Unit=="mg/l")])  #TSS, TKN & phosphorus are in mg/l

#clean up the data, make column names more standardized (and not all uppercase), and make all units ug/l
mlk_res <- mlk_res %>%
  select(Location=LOC_NAME, Date=SAMPLE_DATE, Condition=CONDITION, 
         Matrix=MATRIX_CODE, Method=ANALYTIC_METHOD, 
         Dilution=DILUTION_FACTOR,
         Analyte=CHEMICAL_NAME, Lab_Result=RESULT_NUMERIC, Lab_Result_Text=REPORT_RESULT_TEXT, Lab_Unit=RESULT_UNIT,
         Detection_Limit=REPORTING_DETECTION_LIMIT, Detect_Flag=DETECT_FLAG, 
         Longitude=LONGITUDE, Latitude=LATITUDE) %>%
  mutate(Result = case_when(Lab_Unit=="mg/l" ~ Lab_Result*1000,   #make all units ug/l
                                TRUE ~ Lab_Result),
         Unit = case_when(Lab_Unit=="mg/l" ~ "ug/l",
                              TRUE ~ "ug/l"))

#how many samples were collected at each location for each analyte?
mlk_res %>% count(Analyte, Location)

#does the dilution need to be accounted for, or has it already been done?  Christian says the ReadMe on the .xlsx file says it has been accounted for
plot(mlk_res[which(mlk_res$Analyte=="Copper"), "Dilution"], mlk_res[which(mlk_res$Analyte=="Copper"), "Result"])

#non-detect samples were all TKN; the Result (and Lab_Result) column gives the detection limit for the ND samples
mlk_res[which(mlk_res$Detect_Flag=="N"),]


#----------------------------#
#  WSDOT 2014 Highway Study  #   NOTE: these locations are also in the HWY DB, but the HWY DB dates are wrong.  Use these instead
#----------------------------#   

wsdot14_results <- read.csv(here("..", "data", "model validation data", "WSDOT 2014 Project", "WSDOT 2014 relevant samples.csv"))
wsdot14_gps <- read.csv(here("..", "data", "model validation data", "WSDOT 2014 Project", "WSDOT 2014 GPS locations.csv"))

names(wsdot14_results)

#select the parameters that we need
unique(wsdot14_results$Analyte)
all_pars <- unique(wsdot14_results$Analyte)
parameters <- grep(pattern="Zinc|Copper|Nitrogen, Total Kjeldahl|Total Suspended Solids|Total Phosphorus|Total Nitrogen (TKN)|NITROGEN, KJELDAHL, TOTAL", x=all_pars, ignore.case=TRUE, value=TRUE)

#add GPS locations and clean up the data
ws14_res <- 
  left_join(x=wsdot14_results, y=wsdot14_gps, join_by(Location)) %>%   #add Lat/Lon data to results data
  filter(Analyte %in% parameters) %>%
  mutate(Analyte = case_when(Analyte %in% c("Nitrogen, Total Kjeldahl", "Total Nitrogen (TKN)", "NITROGEN, KJELDAHL, TOTAL") ~ "Total Kjeldahl Nitrogen",
                             TRUE ~ Analyte)) %>%
  filter(Analyte %in% c("Total Kjeldahl Nitrogen", "Total Suspended Solids", "Total Phosphorus", "Copper", "Zinc"))  #keep only the Analytes that we want (no dissolved metals)

#remove the dissolved metals hiding amongst the total metals (they are the lower value when there are duplicates)
ws14_res <- ws14_res %>%
  group_by(Sample_Name, Sample_Date, Analyte) %>%
  slice_max(order_by=Result, n=1) %>%  #keep only the highest value of each sample/date/Analyte
  ungroup()

# ws14_res123.copper <- ws14_res123 %>%
#   filter(Analyte=="Copper")
# ws14_res.copper <- ws14_res %>%
#   filter(Analyte=="Copper")
# ggplot(data=ws14_res123.copper, aes(x=Sample_Date, y=Result, color=Analyte)) +
#   geom_point(show.legend=TRUE) +
#   facet_wrap(~Location)
# ggplot(data=ws14_res.copper, aes(x=Sample_Date, y=Result, color=Analyte)) +
#   geom_point(show.legend=TRUE) +
#   facet_wrap(~Location)

ws14_res <- ws14_res %>%
  mutate(Loc_nickname = Location,
         Location = case_when(Location=="Everett_01" ~ "I5_MP197.27",
                              Location=="Everett_04" ~ "I5_MP197.35",
                              Location=="Pilchuck_01" ~ "I5_MP210.71",
                              Location=="Pilchuck_06" ~ "I5_MP210.85",
                              Location=="SR9_01" ~ "SR9_MP17.92") )

attach(ws14_res)
unique(Lab_Qualifier)  #the following codes are present: JB, J, B, N.  No instances of "R", which indicates data should be rejected
unique(Interpreted_Qualifier)  #the following codes are present: U, J, D, N, UJ.  No instance of "R", which indicates data should be rejected
# Codes and their meaning:
#  U = analyte not detected above reported result
#  UJ = analyte not detected above reported result, reported reporting limit may be inaccurate
#  J = estimated value
#  N = 
#  D = 
#  JB = 
#  B = 
#NOTE: in the WSDOT report, Appendix E, there are no cases of N, D, JB or B.  It is unclear what these signify.
detach()

#what units are used for each analyte?  In the piped code below, we convert all units to ppm (ug/l)
unique(ws14_res$Unit)  #ug/L, mg/L, mg/l

#clean up the data, make column names more standardized, and make all units ug/l
ws14_res <- ws14_res %>%
  mutate(Result = case_when(Unit=="mg/l" ~ Result*1000,   #make all units ug/l
                            Unit=="mg/L" ~ Result*1000,
                            TRUE ~ Result),
         Unit = case_when(Unit=="mg/l" ~ "ug/l",
                          Unit=="ug/L" ~ "ug/l",
                          Unit=="mg/L" ~ "ug/l",
                          TRUE ~ "ug/l"))

#how many samples were collected at each location for each analyte?
ws14_res %>% count(Analyte, Location) %>%
  print(n=50)

# ws14_TKN <- ws14_res[which(ws14_res$Analyte=="TKN"),]
# ws14_Copper <- ws14_res[which(ws14_res$Analyte=="Copper"),]
# ws14_Zinc <- ws14_res[which(ws14_res$Analyte=="Zinc"),]
# ws14_TSS <- ws14_res[which(ws14_res$Analyte=="TSS"),]
# ws14_Phosphorus <- ws14_res[which(ws14_res$Analyte=="Phosphorus"),]
# 

ws14_sum <- ws14_res %>%
  group_by(Analyte, Location) %>%
  summarise(min_result=min(Result),
            max_result=max(Result),
            mean_result=mean(Result),
            sd_result=sd(Result),
            count=n() )
#looks like the final report doesn't quite match the stormwater samples .xlsx file.  Assume the .xlsx file is the
#  unabridged results from the lab, AND that the U's were not divided in half.  For TKN (has most of the U's), it
#  looks like the permit reporting limit is 1000 ug/L, so anything below this would be marked U (even if the lab
#  was able to detect it at a lower limit).  No ND's, just U's.  Assume all samples were detected (no true NDs)


#-----------------------------------------#
#  Ship Canal Test Facility (I-5 Bridge)  #   
#-----------------------------------------#   

i5br_results <- read.csv(here("..", "data", "model validation data", "Ship Canal Test Facility", "sctf_wq_data.csv"))

names(i5br_results)

#select the parameters that we need
unique(i5br_results$Type)
unique(i5br_results$Parameter)
all_pars <- unique(i5br_results$Parameter)
#parameters <- grep(pattern="Zinc|Copper|Nitrogen, Total Kjeldahl|Total Suspended Solids|Total Phosphorus|Total Nitrogen (TKN)|NITROGEN, KJELDAHL, TOTAL", x=all_pars, ignore.case=TRUE, value=TRUE)
parameters <- c("Copper Total", "Zinc Total", "Total Suspended Solids", "Total Phosphorus", "Total Kjeldahl Nitrogen")


i5br_res <- i5br_results %>%
  filter(Type=="Influent",
         Parameter %in% parameters) %>%
  rename(Analyte=Parameter,
         Unit=Units,
         Result=Value) %>%
  mutate(Date=as.POSIXct(Start, format="%Y-%m-%d"))

#what units are used for each analyte?  In the piped code below, we convert all units to ppm (ug/l)
unique(i5br_res$Unit)  #ug/L, mg/L, (symbol for micro)g/l

#clean up the data, make column names more standardized, and make all units ug/l
i5br_res <- i5br_res %>%
  mutate(Result = case_when(Unit=="mg/L" ~ Result*1000,   #make all units ug/l
                            TRUE ~ Result),
         Unit = case_when(Unit=="mg/L" ~ "ug/l",
                          TRUE ~ "ug/l"),
         Latitude=47.656696,
         Longitude=-122.322166)

unique(i5br_res$Flag)  #only NA or J; no ND's to worry about

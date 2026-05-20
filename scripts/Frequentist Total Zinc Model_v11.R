# This generalized script assesses linear relationships between total zinc and landscape predictors
# using mixed effects models

# Note: This version takes the updated set of predictors (from July 7, 2021) and examines each set
#       with the COC of interest, to see which set of predictors may be most correlated with
#       the COC.  Based on linear fits, a select set of predictors (best_predictors) is tested in
#       various combinations within LME models

#Four models are generated for the COC of interest (note the model numbers are 1,3,4,5 (no 2)):
#   1.) median value for all locations, irrespective of date
#   3.) COC value based only on land use (categorical) + rain, precip, summer (as applicable)
#   4.) COC value based on up to 3 landscape parameters NOT including land use + rain


# Eva Dusek Jennings
# Apr 22, 2025 -- greenery as best model; nested random effects (1|agency/location)
#                Also - remove data points where total Zinc is > 800 ug/L.  These are high values unless 
#                the stormwater is coming off of a zinc-plated metal roof!
# May 1, 2025 - substitute greenery_bareEarth for greenery (bareEarth values are mostly << 1%; max is 1.25%)
# Jun 2, 2025  -- add roads (road_density) to the analysis; NOTE: roads does NOT have a good fit to the zinc data!
# Jun 6, 2025  -- try sqrt_paved, log_CO2_cmv_rail, sqrt_CO2_cmv_rail and 3-predictor models
# Oct 10, 2025 -- After trying many things, Emily, Christian and I settled on not_greenBE as the sole predictor.
#                 Using not_greenBE makes mapping easier on the SWHM, because it is a positive relationship.
# Dec 17, 2025 -- Christian discovered that the percent trees predictor we were using has some overlap with other
#                 predictors used to calculate greenery, resulting in greenery values > 1.  Hence a new trees predictor
#                 was swapped in.  This one yielded a best fit model with not_greenBE + sqrt_CO2_transport
#----------------------------------------------

rm(list = ls(all = T))

# Load libraries ----------------------------------------------
library(effects)
library(EnvStats)  #note the objects that are masked from other packages...
#library(tidyverse)
library(ggplot2)
library(graphics)
library(stats)
library(nlme)
library(grid)
library(gridExtra)
library(lme4)  #for lmer models
library(lattice)
library(gstat)  #for spatial correlation exploration
library(sp)  #for spatial correlation exploration
library(sjPlot)
library(sjlabelled)
library(TMB)
library(glmmTMB)
library(here)
library(sp)
library(NADA)
library(ggplot2)
library(knitr)
library(texreg)
library(huxtable)
library(stringr)
library(dplyr)
#run script with functions specific to COC analysis
source(here("..", "functions", "COC analysis functions.R"))
source(here("..", "functions", "HighstatLibV10.R")) #Zuur library, incl panel.smooth2, VIF

#toggle for whether all exploratory parts of code should be run (TRUE) or just essential parts (FALSE) 
run_exploratory_code <- TRUE

#read in stormwater data with spatial predictors-------------------------------
s8 <- read.csv(here("..", "processed_data", "s8data_with_spatial_predictors.csv"))
s8$start_date <- as.Date(s8$start_date)
s8$conc <- s8$result
s8$month <- as.factor(s8$month)
s8$location_id <- as.factor(s8$location_id)
s8$loc <- as.factor(s8$loc)
s8$agency <- as.factor(s8$agency)
s8$season <- as.factor(s8$season)
s8$land_use <- as.factor(s8$land_use)

#list of landscape predictors-------------------------------
aa <- which(str_detect(names(s8), "daymet"))  #after the last predictor column is a series of "daymet" columns
bb <- aa[which(aa > 34)][1] - 1  #identify the last predictor column
predictors <- names(s8)[34:bb]  #first predictor is always column 34; get all predictors
n.preds <- length(predictors)

#select the chemical of interest
this_param <- "Zinc - Water - Total"
this_param_short <- "Zinc"
coc <- s8[which(s8$parameter==this_param),]  #create the "coc" dataframe for this chemical of interest only


#Non Detect Handing -------------------------------

#look at censored points
ggplot(coc)+geom_jitter(aes(x=agency,y=conc,color=nondetect_flag))+scale_y_log10()

#which values are ND?  min/max detection limit?  where and when were these samples recorded?
coc.nd <- coc[which(coc$nondetect_flag==TRUE),]
pct.cen <- 100*nrow(coc.nd)/nrow(coc)  #for zinc, following removal of SNO_HDR mis-labeled blanks, 0% of samples are ND's
nrow(coc.nd)
min(coc.nd$conc)
max(coc.nd$conc)
coc.nd[,c("location_id", "start_date", "conc")]  #location, date, and detection limit of ND samples
#only SNO_HDR samples are ND - see note below...

#NOTE: For zinc, all April-Sept 2009 samples at SNO_HDR are ND's at 1.0ppm.  All samples collected
#      on later dates at this location are at least 10ppm.  Furthermore, other metals tested at SNO_HDR 
#      between April-Sept 2009 were also ND, when no later samples were ND.  Remove these samples -- 
#      they are almost certainly anomalous, since SNO_HDR did not have any ND samples after 2009.
# Note from Christian - These were likely field blanks that got misclassified as samples
coc <- coc[-(which(coc$location=="SNO_HDR" & coc$nondetect_flag==TRUE & coc$year==2009)),]

#which points at SNO_COM are outliers? This location has lots of spread in results.
hist(coc[which(coc$location_id=="SNO_COM"), "result"], breaks=30)  #most values are between 0 and 150.  highest (899 & 1150) are likely outliers
#also, remove zinc values above 800 ug/L; this value is very high and may be causing our models to predict
#  unrealistically high zinc in unmonitored locations
coc <- coc[-(which(coc$result>800)),]

# #Since there are no true ND's for Zinc, no need to substitute the ND's with half the detection limit
# coc <- coc %>% 
#   mutate(conc = ifelse(nondetect_flag, conc * 0.5, conc))

# distribution exploration ----------------------------------------------
if(run_exploratory_code==TRUE) {
  #explore the data; look for underlying distribution
  par(mfrow=c(2,2))
  hist(coc$conc, breaks=50)
  qqnorm((coc$conc), main=paste("QQ-Normal plot of", this_param_short))
  qqline((coc$conc))
  hist(log(coc$conc), breaks=50)
  qqnorm(log(coc$conc), main=paste("QQ-Normal plot of log(", this_param_short, ")", sep=""))
  qqline(log(coc$conc))
  
  #consider other distribution (log-normal and gamma).  Compare QQ plots for these distributions
  par(mfrow=c(2,2))
  qqPlot(log(coc$conc), dist="norm", estimate.params=TRUE, add.line=TRUE, main=paste("QQ log-normal plot of", this_param_short))
  qqPlot(log(coc$conc), dist="norm", param.list=list(mean=-3.3, sd=2.09), add.line=TRUE, main=paste("QQ log-normal plot of", this_param_short))
  qqPlot(coc$conc, dist="gamma", param.list=list(shape=.7, scale=1), points.col="blue",
         add.line=TRUE, main=paste("QQ-gamma plot of", this_param_short))
  qqPlot(sqrt(coc$conc), dist="norm", estimate.params=TRUE, add.line=TRUE, main=paste("QQ plot of sqrt-transformed", this_param_short))
  #qqPlot(1/(coc$conc), dist="norm", estimate.params=TRUE, add.line=TRUE)
}

# best distribution is log-normal.  Ln-transform concentration data in column "result"
coc$result <- log(coc$conc)


#-------------------------------------------------------------------------------------------#
#  Explore Cleveland Dotplots and Boxplots of COC data or predictors conditional on Agency  #
#-------------------------------------------------------------------------------------------#

colors_agency <- c("#440154FF", "#453781FF", "#287C8EFF", "#29AF7FFF", "#C7E020FF")

#colors_agency <- c("red", "orange", "yellow", "green", "blue", "purple")
colors_location <- colors_agency[c(1,1,1,2,3,4,4,4,5,5,5,6,6,6)]

if(run_exploratory_code==TRUE) {
  #Cleveland Dotplot - use to detect violations of homogeneity: We are looking for whether the spread of data values
  #   differs between sampling locations (or agencies).  If so, it indicates heterogeneity, and that there may be problems with violation of
  #   homogeneity in a linear regression model applied on these data.  We're also looking for outliers.
  par(mfrow=c(1,1))
  dotchart(coc$result, groups=coc$location_id, pch=19, col=colors_agency[as.numeric(coc$agency)],
           xlab="concentration", main=paste("Cleveland Dotplot:", this_param_short))
  
  #pairs plots allow visualization of interactions between possible predictors.  Look for relationships between "result"
  #   and all of the predictor variables
  pairs(coc %>% select(result, predictors[1:ceiling(n.preds/3)]), 
        lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)
  pairs(coc %>% select(result, predictors[ (ceiling(n.preds/3)+1) : (ceiling(n.preds/3)*2) ]), 
        lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)
  pairs(coc %>% select(result, predictors[ (ceiling(n.preds/3)*2+1) : n.preds]), 
        lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)
  
  #look also at interactions between result and rainfall
  pairs(coc %>% select(result, antecedant_dry_days_std, daymet_precip_std, daymet_3day_std,
                       daymet_7day_std, daymet_14day_std, daymet_21day_std, daymet_28daySR_std), 
        lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)
  #for zinc, important predictors are likely: 
  #  totRES (-0.6), intURB_IND (0.6), grass (-0.6), greenery (-0.7), paved (0.6), impervious (0.6), 
  #  pm25_na (0.5), sqrt_CO2_tot (0.6), sqrt_CO2_com (0.5), roof_intURB_IND (0.5), roof_totRES (-0.6)
  #  daymet 28day (-0.2), daymet 21day (-0.2)
}


#-----------------------------------------------------------------------------------------------------#
#  Plot COC vs various predictors: look for potential predictors to model & sources of heterogeneity  #
#-----------------------------------------------------------------------------------------------------#

lp_plots()
pr_plots()
tlp_plots()
mp_plots()
#for zinc, 14, 21 and 28-day precip don't show evidence of heteroskedasticity.  
#   location, agency, landuse show heteroskedasticity

#best predictors for this COC; make sure to only have one version (transformed or not) of each predictor!                  
best_predictors <- c("totRES", "intURB_IND", "grass", "greenery", "not_greenBE", "paved", "impervious", "trees", "sqrt_traffic",  
                     "pm25_na", "sqrt_CO2_com", "sqrt_CO2_nonroad", "sqrt_CO2_almostTotal", "sqrt_CO2_transport",
                     "roof_intURB_IND", "roof_totRES")

pred_i <- which(predictors %in% best_predictors)
#lp_plots(pred_i)
lp_plots2(pred_i)  #this version plots up to 16 per page

#examine correlations between predictors; generate a second vector of only predictors that aren't highly correlated
#  this second vector will be used to generate FormX
pairs(coc[best_predictors], lower.panel=panel.smooth2, upper.panel=panel.cor, diag.panel=panel.hist)

elements_to_remove <- c("greenery", "grass", "paved", "impervious", 
                        "sqrt_CO2_com", "sqrt_CO2_almostTotal", "roof_intURB_IND")  #these predictors are highly correlated with others
best_predictors2 <- best_predictors[!(best_predictors %in% elements_to_remove)]

bp_coefs <- bp_signs <- rep(NA, length(best_predictors))
names(bp_coefs) <- names(bp_signs) <- best_predictors
for(i in 1:length(best_predictors)) {
  aa <- lm(as.formula(paste("result ~ ", best_predictors[i])), data=coc)
  bp_coefs[i] <- coefficients(aa)[2]
}
bp_signs <- sign(bp_coefs)


#--------------------------------------------------#
#  Any evidence of changes in COC conc over time?  #
#--------------------------------------------------#

if(run_exploratory_code==TRUE) {
  #for each agency, is there a trend in COC concentration over time?
  library(lattice)
  xyplot(result~start_date|agency, data=coc,
         xlab="time", ylab="log(conc)",
         strip=function(bg="white",...)
         strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  #   For zinc, no obvious pattern of change over time (although some agencies may have a hint of 
  #     increase over time)
}


#---------------------------------------------------#
#  How does rainfall change COC conc at each site?  #
#---------------------------------------------------#

if(run_exploratory_code==TRUE) {
  
  #daymet precip
  xyplot(result~daymet_precip_std|location_id, data=coc,
         xlab="daymet precip", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  
  #3-day daymet precip
  xyplot(result~daymet_3day_std|location_id, data=coc,
         xlab="daymet 3-day precip", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  
  #7-day daymet precip
  xyplot(result~daymet_7day_std|location_id, data=coc,
         xlab="daymet 7-day precip", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  
  #14-day daymet precip
  xyplot(result~daymet_14day_std|location_id, data=coc,
         xlab="daymet 14-day precip", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  
  #21-day daymet precip
  xyplot(result~daymet_21day_std|location_id, data=coc,
         xlab="daymet 21-day precip", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  
  #28-day daymet precip
  xyplot(result~daymet_28daySR_std|location_id, data=coc,
         xlab="daymet 28-day sqrt xform precip", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  
  #antecedant dry days
  xyplot(result~antecedant_dry_days_std|location_id, data=coc,
         xlab="antecedant dry days", ylab="log(conc)",
         strip=function(bg="white",...)
           strip.default(bg="white",...),
         panel=function(x,y) {
           panel.grid(h=-1, v=2)
           I1 <- order(x)
           llines(x[I1], y[I1], col=1)
         })
  #for total zinc, start to see a decrease in zinc conc. by location in the first daymet precip; signal gets stronger
  #   with increasing length of time for rainfall.  Slight signal with antecedant dry days
}


#--------------------------------------------#
#  Create coc2: only predictors we will use  #
#--------------------------------------------#

#see lines 126-128 for selection of predictors that are important for this COC!
coc2 <- coc %>%
  dplyr::select(result,
                location=loc, 
                latitude, longitude,
                agency,
                land_use,
                year, 
                month, season, start_date,
                all_of(best_predictors),
                dry=antecedant_dry_days_std,
                rain=daymet_14day_std,  #choose whichever precip measure makes the most sense for this coc!
                mPrecip=mPrecip  #note: if one of the other mPrecip measures looked better, use it here instead!
  ) %>%
  mutate(year=as.factor(year))  %>%
  mutate(landuse = case_when(
    land_use == "LDR" ~ "1.LDR",
    land_use == "HDR" ~ "2.HDR",
    land_use == "COM" ~ "3.COM",
    land_use == "IND" ~ "4.IND")) %>%
  mutate(landuse = as.factor(landuse)) %>%
  mutate(summer = case_when(
    season=="1" ~ "0",
    season=="2" ~ "0",
    season=="3" ~ "1",
    season=="4" ~ "0")) %>%
  mutate(summer=as.factor(summer)) %>%
  mutate(season3 = case_when(
    season=="1" ~ "winter_spring",
    season=="2" ~ "winter_spring",
    season=="3" ~ "summer",
    season=="4" ~ "fall")) %>%
  mutate(season3 = as.factor(season3))

monthlyAvgPrecip <- data.frame(month=c(1:12), 
                               precip=c(5.24, 4.09, 3.92, 2.75, 2.03, 1.55, 0.93, 1.16, 1.61, 3.24, 5.67, 6.06))
coc2$monthlyPrecip <- monthlyAvgPrecip[coc2$month,2]

rain <- "daymet 14-day, standardized"  #precip measure that was used for this COC


#------------------------------------------------#
#  Formulas for Possible Predictor Combinations  #
#------------------------------------------------#

weather <- c("rain", "summer", "dry")

FormX <- as.formula( paste("result ~ landuse + ", paste(weather, collapse=" + "), " + ", paste(best_predictors2, collapse=" + ")) )
#NOTE: it is not practical at this point to add interactions -- that will come once we find best model using just main effects
#      Also, don't include pairs of highly correlated predictors here (e.g.: nodev:devAge, paved:impervious, impervious:grass)


#-------------------------------------------------------#
#  Zuur et al Method for Mixed Effects Model Selection  #
#-------------------------------------------------------#

#------------------#
#  All Predictors  #
#------------------#

# Steps 1-2: fit LM for ALL PREDICTORS to data using gls; look for heterogeneity
#-----------

if(run_exploratory_code==TRUE) {
  M.gls1X <- gls(FormX, data=coc2, method="REML")  #use REML for comparing diff't random variances
  E.1X <- resid(object=M.gls1X, type="normalized")  
  boxplots.resids2(M.gls1X, E.1X, "X")  #check for heterogeneity in residuals vs fitted values; look for source in other plots
  
  #is there heteroscedasticity in any of these potential covariates?
  plot(coc2$result ~ coc2$location)
  plot(coc2$result ~ coc2$agency)
  plot(coc2$result ~ coc2$month)  #unlikely to be considered for use as a covariate
  plot(coc2$result ~ coc2$season)
  plot(coc2$result ~ coc2$year)
  plot(coc2$result ~ coc2$dry)
  abline(lm(coc2$result ~ coc2$dry))
  plot(coc2$result ~ coc2$rain)
  abline(lm(coc2$result ~ coc2$rain))
  plot(coc2$result ~ coc2$monthlyPrecip)
  plot(coc2$result ~ coc2$mPrecip)
  #for Zinc, dry is the most likely source of heteroskedasticity
  
  
  #Step 3:  Choose a variance structure (if there was heterogeneity)
  #-------  for selecting random structure, use REML to compare - its better at capturing random structure
  #         REML estimates the random effects by considering linear combinations of the data that remove the 
  #         fixed effects. If these fixed effects are changed, the likelihoods of the two models will not be directly comparable.
  #         Compare AIC for the various beyond-optimal models with different variance structures.
  #         Alternately, can use likelihood ratio test (anova) to compare nested models

  M.gls2X <- gls(FormX, data=coc2, method="REML", weights=varIdent(form= ~1|agency))
  M.gls3X <- gls(FormX, data=coc2, method="REML", weights=varIdent(form= ~1|location))
  
  anova(M.gls1X, M.gls2X, M.gls3X)
  #for Total Zinc: AIC & BIC are best for location
  
  #look at residuals for the three variance structure options
  par(mfrow=c(2,2), mar=c(4.5, 4.5, 4, 1), xaxt="s")
  plot(fitted(M.gls1X), resid(object=M.gls1X, type="normalized"), main="no variance structure", xlab="fitted values", ylab="normalized residuals", col="gray", pch=16)
  plot(fitted(M.gls2X), resid(object=M.gls2X, type="normalized"), main="variance covariate: agency", xlab="fitted values", ylab="normalized residuals", col="gray", pch=16)
  plot(fitted(M.gls3X), resid(object=M.gls3X, type="normalized"), main="variance covariate: location", xlab="fitted values", ylab="normalized residuals", col="gray", pch=16)
  
  #residual plots for best fit models -- look for homogeneity of residuals
  E.3X <- resid(object=M.gls3X, type="normalized")  
  boxplots.resids2(M.gls3X, E.3X, "X")
}

#variance functions for best fit model so far
vf1X <- varIdent(form= ~1|location) 


# Steps 4-6: Find the proper random effects structure; look for temporal and spatial autocorrelation
#-----------  note: look for spatial correlation AFTER setting random effects!

if(run_exploratory_code==TRUE) {
  #----Model with Variance Function 1----#
  M.vf1X <- gls(FormX, data=coc2, method="REML", weights=vf1X)
  E.vf1X <- resid(object=M.vf1X, type="normalized")  
  
  #plots of residuals: no variance structure vs. variance structure
  par(mfrow=c(2,2), mar=c(4,4,4,1))
  plot(fitted(M.gls1X), E.1X, main="no variance structure", xlab="fitted values", xaxt="s", ylab="normalized residuals", col="gray", pch=16)
  plot(fitted(M.vf1X), E.vf1X, main="variance covariate: location", xlab="fitted values", xaxt="s", ylab="normalized residuals", col="gray", pch=16)
  
  
  #Random intercept model; this is nested in the best fit model from Step 2, so can compare with a likelhiood ratio test
  M1.lme1X <- lme(data=coc2, FormX, random = ~1|agency/location, method="REML", weights=vf1X,
                  control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
  anova(M.vf1X, M1.lme1X)  #the likelihood ratio test p-value compares the two nested models, and whether they are significantly different
  #For total zinc, the model with random intercept is better than without
}

#random structure for best fit model so far
r1X <- formula(~1|agency/location)


if(run_exploratory_code==TRUE) {
  #best random effects structure & residual plots to test it
  M1.rX <- lme(data=coc2, FormX, random = r1X, method="REML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
  summary(M1.rX)
  E1.rX <- residuals(object=M1.rX, type="normalized")
  boxplots.resids2(M1.rX, E1.rX, "X")
  
  #Check for temporal correlation; look for patterns in Auto-correlation plot for residuals
  par(mfrow=c(2,2))
  plot(E1.rX~coc2$start_date, pch=16, col="cadet blue")
  acf(E1.rX, na.action=na.pass, main="Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
  #for total zinc, no indication of temporal autocorrelation
  
  #Check for spatial correlation
  mydata2 <- data.frame(E1.rX, coc2.longitude=jitter(coc2$longitude, amount=0.05), coc2.latitude=jitter(coc2$latitude, amount=0.05))  #jitter the X-Y coordinates, so that data points aren't on top of each other
  coordinates(mydata2)<-c("coc2.longitude","coc2.latitude")
  bubble(mydata2,"E1.rX",col=c("black","grey"), main="Residuals",xlab="X-coordinates", ylab="Y-coordinates")
  
  #variogram to test for spatial correlation
  Vario1 = variogram(E1.rX ~ 1, mydata2)
  plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
  Vario2 <- variogram(E1.rX ~ 1, mydata2, alpha = c(0, 45, 90,135) )
  plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?
  #for zinc, no indication of spatial autocorrelation
}

# Steps 7-8: Find the proper fixed effects structure; use ML for model comparisons
#-----------  

#-------------------------------------------------------------#
#  Exhaustive Search for Model with Best 3 Predictors + Rain  #
#-------------------------------------------------------------#

preds.1 <- best_predictors

if(run_exploratory_code==TRUE) {
  #Construct all possible models with 1, 2 and 3 landscape predictors, both including and not including rain;
  #   use AIC to determine which models provide the best fit
  
  #one predictor
  dd.1 <- data.frame(Var1=preds.1, Var2=NA, Var3=NA)
  
  #two predictors
  dd.2 <- data.frame(NULL)
  for (i in 1:length(preds.1)) {
    preds.2 <- preds.1[-c(1:i)]
    dd.2 <- rbind(dd.2, expand.grid(preds.1[i], preds.2))
  }  
  dd.2$Var3 <- NA
  
  #three predictors
  dd.3 <- data.frame(NULL)
  for (j in 1:(length(preds.1)-2)) {
    aa <- preds.1[j]
    preds.1a <- preds.1[-c(1:j)]  #remove current predictor "j", and all predictors before it
    for (i in 1:(length(preds.1a)-1)) {
      preds.2a <- preds.1a[-c(1:i)]
      bb <- expand.grid(preds.1a[i], preds.2a)
      dd.3 <- rbind(dd.3, data.frame(bb, Var3=aa))
    }
  }
  
  #combine one, two-predictor options into one dataframe  (3-predictor models always had one predictor non-signif)
  dd <- rbind(dd.1, dd.2)  #, dd.1, dd.2, dd.3)
  
  # dd.str <- paste(dd$Var1, dd$Var2, dd$Var3)  #vector with strings of the predictor sets
  
  for (i in 1:length(best_predictors)) {
    aa <- which(abs(cor(coc2[, best_predictors]) [, i] ) >= 0.85)
    aa <- names(aa)
    for (j in 1:length(aa)) {
      bb <- which(dd[, 1] == best_predictors[i] & dd[, 2] == aa[j] | 
                    dd[, 1] == best_predictors[i] & dd[, 3] == aa[j])
      if (length(bb) > 0) {
        dd <- dd[-bb, ]
      }
    }
  }
  
  #generate an exhaustive set of formulas with one, two and three predictors, including and excluding rain
  ee.1 <- list(NULL)  #list of formulas based on the sets of predictors in "dd"; including rain
  for(i in 1:nrow(dd)) {
    if(is.na(dd$Var2[i])) ee.1[[i]] <- as.formula(paste("result~ ", paste(weather, collapse=" + "), " + ", dd[i,1]))
    else if(is.na(dd$Var3[i]))  ee.1[[i]] <- as.formula(paste("result~ ", paste(weather, collapse=" + "), " + ", dd[i,1], "+", dd[i,2]))
    else  ee.1[[i]] <- as.formula(paste("result~ ", paste(weather, collapse=" + "), " + ", dd[i,1], "+", dd[i,2], "+", dd[i,3]))
  }
  # ee.2 <- list(NULL) #list of formulas based on the sets of predictors in "dd"; NOT including rain
  # for(i in 1:nrow(dd)) {
  #   if(is.na(dd$Var2[i]))  ee.2[[i]] <- as.formula(paste("result~ summer + ", dd[i,1]))
  #   else if(is.na(dd$Var3[i]))  ee.2[[i]] <- as.formula(paste("result~ summer + ", dd[i,1], "+", dd[i,2]))
  #   else ee.2[[i]] <- as.formula(paste("result~ summer + ", dd[i,1], "+", dd[i,2], "+", dd[i,3]))
  # }
  # ee.3 <- list(NULL) #list of formulas based on the sets of predictors in "dd"; NOT including rain
  # for(i in 1:nrow(dd)) {
  #   if(is.na(dd$Var2[i]))  ee.3[[i]] <- as.formula(paste("result~dry + ", dd[i,1]))
  #   else if(is.na(dd$Var3[i]))  ee.3[[i]] <- as.formula(paste("result~dry + ", dd[i,1], "+", dd[i,2]))
  #   else ee.3[[i]] <- as.formula(paste("result~dry + ", dd[i,1], "+", dd[i,2], "+", dd[i,3]))
  # }
  # ee.4 <- list(NULL) #list of formulas based on the sets of predictors in "dd"; NOT including rain
  # for(i in 1:nrow(dd)) {
  #   if(is.na(dd$Var2[i]))  ee.4[[i]] <- as.formula(paste("result~dry + rain + ", dd[i,1]))
  #   else if(is.na(dd$Var3[i]))  ee.4[[i]] <- as.formula(paste("result~dry + rain + ", dd[i,1], "+", dd[i,2]))
  #   else ee.4[[i]] <- as.formula(paste("result~dry + rain + ", dd[i,1], "+", dd[i,2], "+", dd[i,3]))
  # }
  
  ee <- ee.1  #c(ee.1, ee.2, ee.3, ee.4) 
  
  
  MyEqns <- list(formulas=ee, aics=rep(0, length(ee)), do_signs_match=rep(FALSE, length(ee)))
  
  #run the lme for each formula, saving the AIC values; takes about 7 minutes to run
  ptm <- proc.time()  #time the code!
  my.aics <- rep(0, length(ee))
  for (i in 1:length(ee)) {
    bb <- lme(data=coc2, ee[[i]], random = r1X, method="ML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
    my.aics[i] <- AIC(bb)
    MyEqns$aics[i] <- AIC(bb)
    #extract landscape predictor coefficients 
    aa <- summary(bb)$coefficients$fixed
    aa <- aa[names(aa) %in% preds.1]
    aa <- sign(aa)
    aa <- aa[sort(names(aa))]
    #extract coefficient signs for linear models of result~landscape predictors
    cc <- bp_signs[names(bp_coefs) %in% names(aa)]
    cc <- cc[sort(names(cc))]
    #compare coefficient signs from this model, to those of single-predictor linear models
    MyEqns$do_signs_match[i] <- identical(aa, cc)  #if coefficient signs match, TRUE; (default=FALSE)
    
  }
  proc.time() - ptm  #stop the clock;  9 minutes for 386 models
  
  
  xx <- which(MyEqns$do_signs_match==TRUE)
  
  ee.all <- ee
  ee <- ee[xx]
  my.aics.all <- my.aics
  my.aics <- my.aics[xx]
  
  # ee <- ee.all
  # my.aics <- my.aics.all
  
  # 
  # 
  # 
  #   
  # #run the lme for each formula, saving the AIC values; takes about 7 minutes to run
  # ptm <- proc.time()  #time the code!
  # my.aics <- rep(0, length(ee))
  # for (i in 1:length(ee)) {
  #   bb <- lme(data=coc2, ee[[i]], random = r1X, method="ML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
  #   my.aics[i] <- AIC(bb)
  # }
  # proc.time() - ptm  #stop the clock;  9 minutes for 386 models
  
  par(mfrow=c(4,4), mar=c(1,2,2,0), oma=c(1,1,1,1))
  # gg <- rep("black", length(ee))
  # gg[c(which(is.na(dd$Var3)), which(is.na(dd$Var3))+nrow(dd), which(is.na(dd$Var3))+2*nrow(dd), which(is.na(dd$Var3))+3*nrow(dd))] <- "turquoise"   #make formulas with only 2 landscape predictors red
  # gg[c(which(is.na(dd$Var2)), which(is.na(dd$Var2))+nrow(dd), which(is.na(dd$Var2))+2*nrow(dd), which(is.na(dd$Var2))+3*nrow(dd) )] <- "pink"  #make formulas with only 1 landscape predictor yellow
  # plot(my.aics, col=gg, pch=16, main="1 pred=pink, 2 preds=turquoise", xaxt="n", ylab="AIC")
  gg <- rep("black", length(ee))
  gg[which(str_detect(as.character(ee), "rain"))] <- "light blue"   #make formulas with "rain" light blue
  gg[which(str_detect(as.character(ee), "dry"))] <- "goldenrod"   #make formulas with "dry" goldenrod
  gg[which(str_detect(as.character(ee), "dry") & str_detect(as.character(ee), "rain"))] <- "green"   #make formulas with "dry" goldenrod
  plot(my.aics, col=gg, xaxt="n", yaxt="n", pch=16, main="rain=blue, dry=gold, both=green")
  #  plot(my.aics, col=ifelse(str_detect(as.character(ee), "rain"), "light blue", "black"), xaxt="n", yaxt="n", pch=16, main="rain=light blue")
  
  colAIC <- c("red", "orange", "yellow", "light green", "green3", "cadet blue", "purple1", "tan3", "salmon", "maroon1", "goldenrod", "orange", "pink", "purple")
  for (i in 1:length(best_predictors)) {
    plot(my.aics, col=ifelse(str_detect(as.character(ee), best_predictors[i]), colAIC[i], "black"), xaxt="n", yaxt="n", pch=16, main=paste(best_predictors[i], "=", colAIC[i]) )
  }
  
  plot.new()
  text(x=0.5, y=0.5, labels=paste("Landuse AIC", 
                                  round(AIC(lme(data=coc2, as.formula(paste("result~landuse+", paste(weather, collapse="+"))), random = r1X, 
                                                method="ML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))), 1)), cex=2)
  
  #arrange formulae and AICs according to order of AIC value, from smallest to largest
  my.formulas.m4 <- ee[order(my.aics)]
  my.formulas.m4[1:10]
  my.aics.m4 <- my.aics[order(my.aics)]
  my.aics.m4[1:10]  #what are the top 10 AIC's?
  
  save(my.formulas.m4, my.aics.m4, file=(here("..", "scripts", "formulas_and_aics", "total_zinc_below800_m4.RData")))  #models based on all zinc data < 800ug/L
}

if (run_exploratory_code==FALSE) {
 load(file=here("..", "scripts", "formulas_and_aics", "total_zinc_below800_m4.RData"))  #models based on all zinc data
}


# #which formula is best, that includes roof_nonRES?  (According to McIntyre et al 2019 paper, 
# #  commercial and industrial roofs are biggest shedder of zinc into stormwater)
# aa <- grep("roof_intURB_IND", my.formulas.m4)  #ordered list (by AIC value) of all formulae that include roof_nonRES
# my.aics.m4[aa]  #first entry has the lowest AIC
# my.formulas.m4[aa]  #first formula has the lowest AIC (its from an ordered list, by AIC value)




#--------------------------------------------#
#  Quantiles List for Plotting Interactions  #
#--------------------------------------------#

#list of quantiles for all landscape predictors
qList <- list(paved=quantile(coc2$paved, probs=c(0,.25,.50,.75,1)),
              impervious=quantile(coc2$impervious, probs=c(0,.25,.50,.75,1)),
              sqrt_CO2_almostTotal=quantile(coc2$sqrt_CO2_almostTotal, probs=c(0,.25,.50,.75,1)),
              sqrt_CO2_transport=quantile(coc2$sqrt_CO2_transport, probs=c(0,.25,.50,.75,1)),
              greenery=quantile(coc2$greenery, probs=c(0, .25, .50, .75, 1)),
              greenery_bareEarth=quantile(coc2$greenery_bareEarth, probs=c(0, .25, .50, .75, 1)),
              not_green=quantile(coc2$not_green, probs=c(0, .25, .50, .75, 1)),
              not_greenBE=quantile(coc2$not_greenBE, probs=c(0, .25, .50, .75, 1)),
              sqrt_traffic=quantile(coc2$sqrt_traffic, probs=c(0,.25,.50,.75,1))
              )


#--------------------#
#  Models 1, 3, & 4  #
#--------------------#
# 
# #variance functions for best fit model so far
# vf1X <- varIdent(form= ~1|agency)   #varComb(varIdent(form= ~1|agency), varConstPower(form= ~dry))
# ### SINCE NOT USING DRY IN THE MODEL, SWITCH TO VARIANCE STRUCTURE JUST BASED ON AGENCY!
# 

#------ Model 1: median value for all locations --------#

Model1 <- gls(data=coc2, result~1, method="ML")  #here, we use the actual concentration, rather than the transformed one
E1 <- residuals(object=Model1, type="normalized")


#------ Model 3: land use + rain with variance structure & random effects ---------#

Form3 <- as.formula("result ~ landuse + rain + summer")
Model3 <- lme(data=coc2, Form3, random = r1X, method="ML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E3 <- residuals(object=Model3, type="normalized")
boxplots.resids2(Model3, E3, "FormX")
AIC(Model3)

#------ Model 4: up to 2 predictors + precip/ season components ---------#
# note: I tried all sorts of models with 3 predictors, but they always
#       had at least one predictor that wasn't significant (p-value > 0.05)
#       Hence, only 1 and 2 predictor models were evaluated.

#run through the top formulas when only best predictors are on the table;
#  keep only formulas that make sense
#myForm <- my.formulas.m4[[2]]
myForm <- update(my.formulas.m4[[4]], .~. -dry)  ####2 is paved + sqrt_CO2_xport (AIC=706.7); 3 is paved + sqrt_traffic (AIC=710.3); 5 is not_greenBE + sqrt_CO2_xport (AIC=710.9)
myForm <- update(my.formulas.m4[[5]], .~. -dry +rain:not_greenBE)

#these lines of code assess fit of this particular model in terms of COC vs. individual predictors, and predictor correlation
myModel <- lme(data=coc2, myForm, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
#myModel <- gls(data=coc2, myForm, method="ML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(myModel)
AIC(myModel)
plot.single.preds(myModel)
#check.cor(myModel)
boxplots.resids2(myModel, residuals(myModel, type="normalized"), "X")


#note: no "dry" predictor; variance covariate = location
Form4a <- formula(result ~ rain + summer + not_greenBE + sqrt_CO2_transport)  #my.formulas.m4[[5]]; AIC=711.3
Model4a <- lme(data=coc2, Form4a, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E4a <- residuals(object=Model4a, type="normalized")

Form4.5 <- formula(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE)  #my.formulas.m4[[5]]; AIC=711.3
Form4.4 <- formula(result ~ rain + summer + greenery + sqrt_CO2_transport + rain:greenery)  #my.formulas.m4[[5]]; AIC=711.3
Form4.3 <- formula(result ~ rain + summer + paved + sqrt_traffic + rain:paved)  #my.formulas.m4[[5]]; AIC=711.3
Form4.2 <- formula(result ~ rain + summer + paved + sqrt_CO2_transport + rain:paved)  #my.formulas.m4[[5]]; AIC=711.3
Model4.5 <- lme(data=coc2, Form4.5, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
Model4.4 <- lme(data=coc2, Form4.4, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
Model4.3 <- lme(data=coc2, Form4.3, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
Model4.2 <- lme(data=coc2, Form4.2, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
AIC(Model4.5, Model4.4, Model4.3, Model4.2)

# #note: no "dry" predictor; variance covariate = location
# Form4b <- formula(result ~ rain + summer + sqrt_traffic + paved)  #my.formulas.m4[[9]]; AIC=710.2
# Model4b <- lme(data=coc2, Form4b, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# E4b <- residuals(object = Model4b, type="normalized")
# 
# # #note: no "dry" predictor; variance covariate = location
# Form4c <- formula(result ~ rain + summer + not_greenBE)  #my.formulas.m4[[11]]; AIC=710.60
# Model4c <- lme(data=coc2, Form4c, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# E4c <- residuals(object = Model4c, type="normalized")


# #note: no "dry" predictor; variance covariate = location
# Form4a <- formula(result ~ rain + summer + not_greenBE + sqrt_traffic)  #my.formulas.m4[[4]]; AIC=707.3
# Model4a <- lme(data=coc2, Form4a, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# E4a <- residuals(object=Model4a, type="normalized")
# 
# #note: no "dry" predictor; variance covariate = location
# Form4b <- formula(result ~ rain + summer + sqrt_traffic + paved)  #my.formulas.m4[[9]]; AIC=710.2
# Model4b <- lme(data=coc2, Form4b, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# E4b <- residuals(object = Model4b, type="normalized")
# 
# # #note: no "dry" predictor; variance covariate = location
# Form4c <- formula(result ~ rain + summer + not_greenBE)  #my.formulas.m4[[11]]; AIC=710.60
# Model4c <- lme(data=coc2, Form4c, method="ML", random=r1X, weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# E4c <- residuals(object = Model4c, type="normalized")


# Model 4a interactions
Form4 <- Form4a
Model4 <- Model4a
E4 <- residuals(object = Model4, type="normalized")


#try adding interactions and see if they are significant
M4.sub1 <- update(Model4, . ~ . + rain:sqrt_CO2_transport) #significant (p=0.0208)
anova(Model4, M4.sub1)
M4.sub2 <- update(Model4, . ~ . + rain:not_greenBE)  #significant (p=0.0061)  -- this one is more significant and gets a better AIC by 2 points
anova(Model4, M4.sub2)
# M4.sub3 <- update(M4.sub2, .~., + rain:sqrt_traffic)   #for some reason, it doesn't work to add two interation terms with rain in the model...
# anova(Model4, M4.sub2, M4.sub3)
M4.sub4 <- update(Model4, .~. + sqrt_CO2_transport:not_greenBE)  #highly insignificant (p=0.1249)
anova(Model4, M4.sub4)

AIC(Model4, M4.sub1, M4.sub2)


# #try adding interactions and see if they are significant
# M4.sub1 <- update(Model4, . ~ . + rain:sqrt_traffic) #significant (p=0.0076)
# anova(Model4, M4.sub1)
# M4.sub2 <- update(Model4, . ~ . + rain:not_greenBE)  #significant (p=0.0025)  -- this one is more significant and gets a better AIC by 2 points
# anova(Model4, M4.sub2)
# # M4.sub3 <- update(M4.sub2, .~., + rain:sqrt_traffic)   #for some reason, it doesn't work to add two interation terms with rain in the model...
# # anova(Model4, M4.sub2, M4.sub3)
# M4.sub4 <- update(Model4, .~. + sqrt_traffic:not_greenBE)  #highly insignificant (p=0.5574)
# anova(Model4, M4.sub4)
# 
# AIC(Model4, M4.sub1, M4.sub2)

#Plot formula 4 interaction
Plot.Quantile("rain", "not_greenBE", M4.sub2, yEqn=7.5)
myModel <- M4.sub2
boxplots.resids2(myModel, residuals(myModel, type="normalized"), "X")
#This relationship seems plausible, both on basis of data theoretically.  It also reduces AIC by 9-pts (to 700.17)

Form4a.int <- update(Form4a, .~. +rain:not_greenBE)
Model4a.int <- update(Model4a, .~. +rain:not_greenBE)
E4a.int <- residuals(object = Model4a.int, type = "normalized")
boxplots.resids2(Model4a.int, E4a.int, "X")


# Model 4b interactions
Form4 <- Form4b
Model4 <- Model4b

#try adding interactions and see if they are significant
M4.sub2 <- update(Model4, . ~ . + rain:paved)  #highly significant (p=0.0004)
anova(Model4, M4.sub2)

#Plot formula 4 interaction
Plot.Quantile("rain", "paved", M4.sub2, yEqn=7.5)
myModel <- M4.sub2
boxplots.resids2(myModel, residuals(myModel, type="normalized"), "X")
AIC(myModel)  #AIC = 724.4
Model4b.int <- M4.sub2
Form4b.int <- update(Form4b, .~. +rain:paved)

# Model 4c interactions
Form4 <- Form4c
Model4 <- Model4c

#try adding interactions and see if they are significant
M4.sub1 <- update(Model4, . ~ . + rain:not_greenBE) #slightly significant (p=0.0023)
anova(Model4, M4.sub1)

#Plot formula 4 interaction
Plot.Quantile("rain", "not_greenBE", M4.sub1, yEqn=7.5)
myModel <- M4.sub1
summary(myModel)  #note that sqrt_traffic p-value = 0.017;   AIC=726.0
plot.single.preds(myModel)
#check.cor(myModel)
AIC(update(myModel, .~. -sqrt_traffic))  #removing sqrt_traffic leads to a signif worse AIC (726 vs 731.9)
Model4c.int <- M4.sub1
Form4c.int <- update(Form4c, .~. +rain:not_greenBE)

# pre-2025: we tried putting all three model options onto the stormwater heatmap, and noticed that there are some
#  problems with models 4b and 4c.  Model 4b (greenery) was a favorite, until we observed that non-vegetated
#  open space (mudflats, rock formations) were shown as high zinc areas. Model 4a (paved + sqrt_CO2_almostTotal) 
#  wasn't as good as model 4c (paved + sqrt_traffic), because the CO2 data are a surrogate for traffic,
#  and are not at as good of resolution as the traffic data.  The CO2 data weren't capturing the roadways
#  as well as we would have liked, as the data grid is larger, so they look blocky/ gridded.  We initially concluded
#  that paved + sqrt(traffic) would be the best model.

# early-2025: When we tried doing the model validation with paved + sqrt(traffic), it did NOT work at all.  Predicted values
#  were crazy high, so we went back to the drawing board.  We additionally discovered that the paved predictor is
#  likely NOT linear, as the model validation data required paved to be truncated at 1.0 in order to get reasonable
#  values.  This is the attempt to use greenery as the predictor.

# Jun-2025: Christian tried applying several models to the Puget Sound heatmap, and found that some don't look so good/
#  aren't very believeable when plotted on the heatmap.  We need a traffic-based (road, traffic, CO2) predictor in there.  
#  road density wasn't correlated with zinc, and CO2_transport was really blocky because of its large grid size.  Traffic
#  works quite well with greenery/ greenery_bareEarth.  We'll try this with the validation data to see how it goes.

# Sep-2025: Following discussion with Christian, we decided to try modeling Zinc without Port of Tacoma, which seemed to
#  be throwing off the model and is quite different from the other sites.  Christian also wasn't convinced that the traffic
#  numbers for POT were correct (still need to get his gut-check on that).  Once POT was removed, the fit to traffic & not_green
#  is even better.

# Oct-2025: After testing the model validation data to the model from Sept 2025 (not_greenBE + sqrt_traffic, with POT removed),
#  we decided to go back to the not_greenBE only model.  The fit of the Sept 2025 model wasn't good to vaidation data - even worse
#  than the greenery model from before.  We will provide this information in the tech doc, including a model validation exercise
#  where we try running the model validation on land use category data to see how it compares.  Zinc is really tricky to model
#  without a spatial image layer showing which locations have galvanized roofs, fencing or ductwork.

# Dec-2025: Top 3 models worth considering were paved+sqrt_CO2_transport (AIC=706.7), paved+sqrt_traffic (AIC=710.3), and 
#  not_greenBE+sqrt_CO2_transport (AIC=710.9).  The first two options included paved, which doesn't do well across the whole SWHM
#  domain, as it may not be a linear relationship.  I like the combo of not_green and CO2 transport, which captures the fact that
#  green space doesn't have zinc-containing items, AND that cars on roadways contribute to zinc in stormwater.  Galvanized metals
#  are not included in this equation, but we don't have a great way to capture the spotty nature of those right now.


#####  Best fit Model4  ####
Form4a.int
Model4a.int   #AIC = 703.3
E4a.int <- residuals(object = Model4a.int, type = "normalized")
boxplots.resids2(Model4a.int, E4a.int, "X")

# 
# #####  Best fit Model4  ####
# Form4c.int
# Model4c.int   #AIC = 703.3
# E4c.int <- residuals(object = Model4c.int, type = "normalized")
# boxplots.resids2(Model4c.int, E4c.int, "X")


#------ Compare models and plot ---------#
AIC(Model1, Model3, Model4a, Model4a.int)

AIC(Model1, Model3, Model4a, Model4a.int)[,2] - AIC(Model4a.int)

Model4 <- Model4a.int

if(run_exploratory_code==TRUE) {
  #plot model predictions for each model (above)
  M1.preds <- predict(Model1)
  plot.preds.vs.results(M1.preds)
  
  M3.preds <- predict(Model3)
  plot.preds.vs.results(M3.preds)
  
  M4.preds <- predict(Model4)
  plot.preds.vs.results(M4.preds)
}


#----------------------------------#
#  Look at the Fits of the Models  #
#----------------------------------#

#check residuals for each model, to see if they tell us anything important
par(mfrow=c(2,2), mar=c(2,4,4,1), oma=c(0,0,0,0))
plot(coc2$location, E1, main="Null Model", ylab="Residuals", xaxt="n", col=colors_location)
axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
abline(h=0, col="gray")
plot(coc2$location, E3, main="Categorical Land Use Model", ylab="Residuals", xaxt="n", col=colors_location)
axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
abline(h=0, col="gray")
# plot(coc2$location, E4a, main="Model 4a: traffic, paved, pm25", ylab="Residuals", xaxt="n", col=colors_location)
# axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
# abline(h=0, col="gray")
plot(coc2$location, E4a.int, main="Spatial Predictor Model", ylab="Residuals", xaxt="n", col=colors_location)
axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
abline(h=0, col="gray")
# plot(coc2$location, E4b, main="Model 4b: traffic, greenery, pm25", ylab="Residuals", xaxt="n", col=colors_location)
# axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
# abline(h=0, col="gray")
# plot(coc2$location, E4c, main="Model 4c: trees, intURB_IND, pm25", ylab="Residuals", xaxt="n", col=colors_location)
# axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
# abline(h=0, col="gray")
# plot(coc2$location, E4c.int, main="Model 4c.int", ylab="Residuals", xaxt="n", col=colors_location)
# axis(side=1, at=c(2,3.8,5.2,7,10,13), labels=c("King", "Pie", "PoT", "Sea", "Sno", "Tac"))
# abline(h=0, col="gray")


# Step 9: Refit models with REML and apply graphical model validation; check for homogeneity, 
#--------    normality and independence

#------ Model 3: landuse only, with variance structure & random structure ---------#

M3.final <- lme(data=coc2, Form3, random = r1X, method="REML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E3.final <- residuals(object=M3.final, type="normalized")

#plot residuals against agency, year, month, date and other unused predictors; check if model is missing something important!
boxplots.resids2(M3.final, E3.final, "X")
#is temporal auto-correlation present?  Look for patterns in Auto-correlation plot for residuals
par(mfrow=c(2,2))
plot(E3.final~coc$start_date, pch=16, col="cadet blue")
acf(E3.final, na.action=na.pass, main="Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
#Check for spatial correlation
mydata2 <- data.frame(E3.final, coc2.longitude=jitter(coc2$longitude, amount=0.05), coc2.latitude=jitter(coc2$latitude, amount=0.05))  #jitter the X-Y coordinates, so that data points aren't on top of each other
coordinates(mydata2)<-c("coc2.longitude","coc2.latitude")
bubble(mydata2,"E3.final",col=c("black","grey"), main="Residuals",xlab="X-coordinates", ylab="Y-coordinates")
#variogram to test for spatial correlation
Vario1 = variogram(E3.final ~ 1, mydata2)
plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
Vario2 <- variogram(E3.final ~ 1, mydata2, alpha = c(0, 45, 90,135) )
plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?


#------ Model 4: no land use; up to 3 predictors + rain ---------#

M4.final <- lme(data=coc2, Form4a.int, random = r1X, method="REML", weights=vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E4.final <- residuals(object=M4.final, type="normalized")

#plot residuals against agency, year, month, date and other unused predictors; check if model is missing something important!
boxplots.resids2(M4.final, E4.final, "X")
#is temporal auto-correlation present?  Look for patterns in Auto-correlation plot for residuals
par(mfrow=c(2,2))
plot(E4.final~coc$start_date, pch=16, col="cadet blue")
acf(E4.final, na.action=na.pass, main="Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
#Check for spatial correlation
mydata2 <- data.frame(E4.final, coc2.longitude=jitter(coc2$longitude, amount=0.05), coc2.latitude=jitter(coc2$latitude, amount=0.05))  #jitter the X-Y coordinates, so that data points aren't on top of each other
coordinates(mydata2)<-c("coc2.longitude","coc2.latitude")
bubble(mydata2,"E4.final",col=c("black","grey"), main="Residuals",xlab="X-coordinates", ylab="Y-coordinates")
#variogram to test for spatial correlation
Vario1 = variogram(E4.final ~ 1, mydata2)
plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
Vario2 <- variogram(E4.final ~ 1, mydata2, alpha = c(0, 45, 90,135) )
plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?
#looks fine

AIC(M3.final, M4.final)

#-----------------#
#  Save Formulas  #
#-----------------#

#save important items with phosphorus-specific names
totZn.coc2 <- coc2
totZn.r1X <- r1X
totZn.vf1X <- vf1X
totZn.Form3 <- Form3
totZn.Form4 <- Form4a.int
totZn.rain <- rain

save(totZn.coc2, totZn.r1X, totZn.vf1X, totZn.Form3, totZn.Form4, totZn.rain, file=here("..", "results", "Frequentist_Total Zinc Models_notGreenBE.RData"))


#-------------------------------#
#  Plot Predictor Coefficients  #
#-------------------------------#

#generate plots that are shown in Rmarkdown script
totZn.null <- gls(data = coc2, result ~ 1, method = "REML") 
totZn.M3 <- lme(data = coc2, Form3, random = r1X, method = "REML", weights = vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
totZn.M4 <- lme(data = coc2, Form4a.int, random = r1X, method = "REML", weights = vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
#totZn.M4.alt <- lme(data = coc2, Form4c.int, random = r1X, method = "REML", weights = vf2X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

#P.M4.alt <- lme(data = coc2, Form4.alt, random = r1X, method = "REML", weights = vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

totZn_models <- list(
  Null_Model = totZn.null,
  Categorical_Land_Use_Model = totZn.M3,
  Spatial_Predictor_Model = totZn.M4
)

huxtablereg(totZn_models,
            single.row = TRUE, custom.model.names = names(totZn_models)) %>%
  set_bottom_border(1, -1, 0.4) %>%
  set_bold(1, -1, TRUE) 

theme_set(theme_sjplot())
plotreg(totZn_models, custom.title = "Regression Results, Total Zinc", custom.model.names = names(totZn_models))
plot_models(totZn_models, m.labels = names(totZn_models),legend.title = "Models", show.values = TRUE,show.intercept = TRUE)


#----------------------------------#
#  Plots to support Eva's summary  #
#----------------------------------#

# NONE :)


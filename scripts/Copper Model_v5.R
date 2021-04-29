# This generalized script assesses linear relationships between copper and landscape predictors
# using mixed effects models

# Note: This version takes the final set of predictors (from Jan 29, 2021) and examines each set
#       with the COC of interest, to see which set of predictors may be most correlated with
#       the COC.

#Four models are generated for the COC of interest (note the model numbers are 1,3,4,5 (no 2)):
#   1.) median value for all locations, irrespective of date
#   3.) COC value based only on land use + rain
#   4.) COC value based on up to 3 landscape parameters NOT including land use + rain
#   5.) COC value based on up to 3 parameters + land use + rain


# Eva Dusek Jennings
# Apr 27, 2021

# Load libraries ----------------------------------------------

rm(list = ls(all = T))

library(effects)
library(EnvStats)  #note the objects that are masked from other packages...
library(tidyverse)
library(ggplot2)
library(graphics)
library(stats)
library(nlme)
library(grid)
library(gridExtra)
library(lme4)  #for lmer models
source(here("functions", "HighstatLibV10.R")) #Zuur library, incl panel.smooth2, VIF
library(lattice)
library(gstat)  #for spatial correlation exploration
library(sp)  #for spatial correlation exploration
library(sjPlot)
library(sjlabelled)
#library(sjmisc)
library(TMB)
library(glmmTMB)
library(here)
library(lattice)
library(sp)
library(NADA)
library(ggplot2)
#run script with functions specific to COC analysis
source(here("functions", "COC analysis functions.R"))

#library(gstat)


#read in stormwater data with spatial predictors-------------------------------
s8 <-
  read.csv("./processed_data/s8data_with_spatial_predictors.csv")
s8$start_date <- as.Date(s8$start_date)
s8$conc <- s8$result
s8$month <- as.factor(s8$month)
s8$location_id <- as.factor(s8$location_id)
s8$agency <- as.factor(s8$agency)
s8$season <- as.factor(s8$season)
s8$land_use <- as.factor(s8$land_use)



#list of landscape predictors-------------------------------
predictors <-
  c(
    "impervious_std",
    "imperv_ground_std",
    "roofs_std",
    "grass_low_veg_std",
    "logTree_cover_std",
    "logNoDev",
    "pm25_std",
    "sqrtSlope_std",
    "logTraffic_std",
    "logPopulation_std"
  )

#select the chemical of interest
this_param <- "Copper - Water - Total"
this_param_short <- "Copper"
coc <-
  s8[which(s8$parameter == this_param), ]  #create the "coc" dataframe for this chemical of interest only

#Non Detect Handing -------------------------------
#which values are ND?  min/max detection limit?  where and when were these samples recorded?
#look at censored statistics 
censtats(obs = coc$conc,censored = coc$nondetect_flag) %>% kable()
# #
# '
# OUTPUT FOR COPPER
#          n      n.cen    pct.cen 
# 514.000000   9.000000   1.750973 
# median     mean       sd
# K-M 9.700000 15.28840 16.69165
# ROS 9.700000 15.29231 16.68790
# MLE 8.700134 17.56344 30.80055`
#   ' 
#censored boxplot for all agencies 
cenboxplot(obs=coc$conc, cen = coc$nondetect_flag, group = coc$agency,log = TRUE)

#look at censored points
ggplot(coc)+geom_jitter(aes(x=agency,y=conc,color=nondetect_flag))+scale_y_log10()

coc.nd <-
  coc[which(coc$nondetect_flag == TRUE), ]  #9 ND results, all 0.1 and 0.5
nrow(coc.nd)
min(coc.nd$conc)
max(coc.nd$conc)
coc.nd[, c("location_id", "start_date", "conc")]  #location, date, and detection limit of ND samples

#NOTE: For copper, all April-Sept 2009 samples at SNO_HDR are ND's at 0.1ppm.  Samples collected
#      at this location in Oct, Nov and Dec are 3.9, 4.5, 4.6 and 27.9.  All samples for 2010-2013
#      collected at SNO_HDR are > 2.  Furthermore, other metals tested at SNO_HDR between April-Sept
#      2009 were also ND, when no later samples were ND.  Remove these samples -- they are
#      almost certainly anomalous, since SNO_HDR did not have any ND samples after 2009.
# Note from Christian - These were likely field blanks that got misclassified as samples

xyplot(conc ~ start_date, data = coc[which(coc$location_id == "SNO_HDR"), ], pch =
         19)
#remove likely field blanks
coc <- coc[-(which(coc$location=="SNO_HDR" & coc$nondetect_flag==TRUE & coc$year==2009)),]

#Note (from Christian) instead of removing non-detects, the preferred method is to
# either substutite with half the detection limit, or use a censored distribution.
# here, I updated this code for substitution - for coc's with many non-detects, we
# should probably use a censored method. See Helsel, D. R. (2012). Statistics for Censored Environmental Data.
coc <- coc %>% mutate(conc, ifelse(nondetect_flag, conc * 0.5, conc))



# distribution exploration ----------------------------------------------


if (F) {
  #explore the data; look for underlying distribution
  par(mfrow = c(2, 2))
  hist(coc$conc, breaks = 50)
  qqnorm((coc$conc), main = paste("QQ-Normal plot of", this_param_short))
  qqline((coc$conc))
  hist(log(coc$conc), breaks = 50)
  qqnorm(log(coc$conc),
         main = paste("QQ-Normal plot of log(", this_param_short, ")", sep = ""))
  qqline(log(coc$conc))
  
  #consider other distribution (log-normal and gamma).  Compare QQ plots for these distributions
  par(mfrow = c(2, 2))
  qqPlot(
    log(coc$conc),
    dist = "norm",
    estimate.params = TRUE,
    add.line = TRUE,
    main = paste("QQ log-normal plot of", this_param_short)
  )
  qqPlot(
    log(coc$conc),
    dist = "norm",
    param.list = list(mean = -3.3, sd = 2.09),
    add.line = TRUE,
    main = paste("QQ log-normal plot of", this_param_short)
  )
  qqPlot(
    coc$conc,
    dist = "gamma",
    param.list = list(shape = .7, scale = 1),
    points.col = "blue",
    add.line = TRUE,
    main = paste("QQ-gamma plot of", this_param_short)
  )
  qqPlot(
    sqrt(coc$conc),
    dist = "norm",
    estimate.params = TRUE,
    add.line = TRUE,
    main = paste("QQ plot of sqrt-transformed", this_param_short)
  )
  #qqPlot(1/(coc$conc), dist="norm", estimate.params=TRUE, add.line=TRUE)
  
  # for both the log-normal and gamma distributions, the high tail is skewed.  lets find out where the
  # high data came from...
  coc %>%
    dplyr::filter(parameter == this_param, result > quantile(coc$conc, 0.95)) %>%
    dplyr::select(c(location_id, result, month, year)) %>%
    dplyr::arrange(location_id, result)
  # for copper, 18 of 24 come from SEAC1S8D_OUT; 5 come from TAC001s8D_OF235.  No trend in month & year
}

# best distribution is log-normal.  Ln-transform concentration data in column "result"
coc$result <- log(coc$conc)


#-------------------------------------------------------------------------------------------#
#  Explore Cleveland Dotplots and Boxplots of COC data or predictors conditional on Agency  #
#-------------------------------------------------------------------------------------------#

colors_agency <-
  c("red", "orange", "yellow", "green", "blue", "purple")

if (F) {
  #Cleveland Dotplot - use to detect violations of homogeneity: We are looking for whether the spread of data values
  #   differs between sampling locations (or agencies).  If so, it indicates heterogeneity, and that there may be problems with violation of
  #   homogeneity in a linear regression model applied on these data.  We're also looking for outliers.
  par(mfrow = c(1, 1))
  dotchart(
    coc$result,
    groups = coc$location_id,
    pch = 19,
    col = colors_agency[as.numeric(coc$agency)],
    xlab = "concentration",
    main = paste("Cleveland Dotplot:", this_param_short)
  )
  
  #pairs plots allow visualization of interactions between possible predictors.  Look for relationships between "result"
  #   and all of the predictor variables
  pairs(
    coc %>% select(result, predictors),
    lower.panel = panel.smooth2,
    upper.panel = panel.cor,
    diag.panel = panel.hist
  )
  #look also at interactions between result and rainfall
  pairs(
    coc %>% select(
      result,
      antecedant_dry_days_std,
      daymet_precip_std,
      daymet_3day_std,
      daymet_7day_std,
      daymet_14day_std,
      daymet_21day_std,
      daymet_28daySR_std
    ),
    lower.panel = panel.smooth2,
    upper.panel = panel.cor,
    diag.panel = panel.hist
  )
  #for copper, important predictors are likely:
  #  impervious (0.6), imperv_ground (0.5), trees (-0.5), noDev (-0.6), pm25 (0.6), traffic (0.6)
  #  daymet14, 21 or 28day (all -0.2)
  
  #important predictors for this COC
  importantPreds <-
    c("impervious",
      "pm25",
      "traffic",
      "nodev",
      "trees",
      "paved",
      "daymet_28")
  
  #likely most important predictors
  pairs(
    coc[, c(
      "result",
      "agency",
      "impervious_std",
      "logTree_cover_std",
      "logNoDev_std",
      "pm25_std",
      "logTraffic_std",
      "daymet_28daySR_std"
    )],
    lower.panel = panel.smooth2,
    upper.panel = panel.cor,
    diag.panel = panel.hist
  )
}

#-----------------------------------------------------------------------------------------------------#
#  Plot COC vs various predictors: look for potential predictors to model & sources of heterogeneity  #
#-----------------------------------------------------------------------------------------------------#

if (F) {
  #look for relationships between coc and some predictor variables
  p1 <- ggplot(coc, aes(agency, result)) + geom_boxplot()
  p2 <-
    ggplot(coc, aes(daymet_precip_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$daymet_precip_std))
  p3 <-
    ggplot(coc, aes(daymet_3day_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$daymet_3day_std))
  p4 <-
    ggplot(coc, aes(daymet_7day_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$daymet_7day_std))
  p5 <-
    ggplot(coc, aes(daymet_14day_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$daymet_14day_std))
  p6 <-
    ggplot(coc, aes(daymet_21day_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$daymet_21day_std))
  p7 <-
    ggplot(coc, aes(daymet_28daySR_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$daymet_28daySR_std))
  p8 <-
    ggplot(coc, aes(antecedant_dry_days_std, result)) + geom_point() + geom_smooth(method = "lm") + annotation_custom(slope_grob(coc$antecedant_dry_days_std))
  # NOTE: I have left out precip & inches_rain_per_hour b/c they are missing ~68 data points
  
  p9 <- my.ggplot(1)
  p10 <- my.ggplot(2)
  p11 <- my.ggplot(3)
  p12 <- my.ggplot(4)
  p13 <- my.ggplot(5)
  p14 <- my.ggplot(6)
  p15 <- my.ggplot(7)
  p16 <- my.ggplot(8)
  p17 <- my.ggplot(9)
  p18 <- my.ggplot(10)
  
  p19 <-
    ggplot(coc, aes(as.factor(year), result)) + geom_boxplot()  #starts in Feb, 2009, ends in April, 2013; trends could be due to months with data?
  p20 <-
    ggplot(coc, aes(month, result)) + geom_boxplot(position = position_dodge(width =
                                                                               0.9))
  a <- coc %>% count(month)
  p20 <-
    p20 + annotate(
      geom = "text",
      x = c(1:12),
      y = rep(-1.4, 12),
      label = paste("n=", a[, 2], sep = ""),
      color = "red",
      size = 3.5,
      angle = 90
    )  #note the small sample size in June-Sept
  p21 <-
    ggplot(coc, aes(as.factor(season), result)) + geom_boxplot()
  p22 <- ggplot(coc, aes(land_use, result)) + geom_boxplot()
  p23 <- ggplot(coc, aes(location_id, result)) + geom_boxplot()
  
  #Look at various transformations for monthly precip by location; which is most evenly spread out?
  par(mfrow = c(2, 2), mar = c(4, 4, 4, 2))
  plot(coc$mPrecip, coc$result)
  plot(coc$mPrecipSR, coc$result)
  plot(coc$mPrecipCR, coc$result)
  plot(coc$mPrecipLog, coc$result)
  #for copper, mPrecip is better than the rest!
  
  #look for relationships between coc's and some predictors;
  # look especially for sources of heterogeneity
  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 3)
  grid.arrange(p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, nrow = 4)
  grid.arrange(p1, p23, p19, p20, p21, p22, nrow = 2)
  #for copper, 28-day prior rainfall does not show heteroskedasticity.
  #   location, agency, landuse, month and year do show heteroskedasticity
}


#--------------------------------------------------#
#  Any evidence of changes in COC conc over time?  #
#--------------------------------------------------#

if (F) {
  #for each agency, is there a trend in COC concentration over time?
  library(lattice)
  xyplot(
    result ~ start_date | agency,
    data = coc,
    xlab = "time",
    ylab = "log(conc)",
    strip = function(bg = "white", ...)
      strip.default(bg = "white", ...),
    panel = function(x, y) {
      panel.grid(h = -1, v = 2)
      I1 <- order(x)
      llines(x[I1], y[I1], col = 1)
    }
  )
  #   For copper, no long-term trend in concentration over time, but there may be a monthly component?
  #   We also see that the log(conc) is generally higher in some agencies than others
}

#--------------------------------------------#
#  Create coc2: only predictors we will use  #
#--------------------------------------------#

#see lines 126-128 for selection of predictors that are important for this COC!
coc2 <- coc %>%
  select(
    result,
    location = location_id,
    latitude,
    longitude,
    agency,
    land_use,
    year,
    month,
    season,
    start_date,
    paved = imperv_ground_std,
    impervious = impervious_std,
    roofs = roofs_std,
    grass = grass_low_veg_std,
    trees = logTree_cover_std,
    traffic = logTraffic_std,
    pm25 = pm25_std,
    slope = sqrtSlope_std,
    nodev = logNoDev_std,
    dry = antecedant_dry_days_std,
    rain28 = daymet_28daySR,
    #unstandardized, for testing variance structure
    rain7 = daymet_7daySR,
    #unstandardized, for testing variance structure
    rain = daymet_28daySR_std,
    #choose whichever precip measure makes the most sense for this coc!
    mPrecip = mPrecip  #note: if one of the other mPrecip measures looked better, use it here instead!
  ) %>%
  mutate(year = as.factor(year)) %>%
  mutate(
    landuse = case_when(
      land_use == "LDR" ~ "1.LDR",
      land_use == "HDR" ~ "2.HDR",
      land_use == "COM" ~ "3.COM",
      land_use == "IND" ~ "4.IND"
    )
  ) %>%
  mutate(landuse = as.factor(landuse)) %>%
  mutate(
    residential = case_when(
      landuse == "1.LDR" ~ "RES",
      landuse == "2.HDR" ~ "RES",
      landuse == "3.COM" ~ "NORES",
      landuse == "4.IND" ~ "NORES"
    )
  ) %>%
  mutate(residential = as.factor(residential))

monthlyAvgPrecip <- data.frame(
  month = c(1:12),
  precip = c(
    5.24,
    4.09,
    3.92,
    2.75,
    2.03,
    1.55,
    0.93,
    1.16,
    1.61,
    3.24,
    5.67,
    6.06
  )
)
coc2$monthlyPrecip <- monthlyAvgPrecip[coc2$month, 2]

#------------------------------------------------#
#  Formulas for Possible Predictor Combinations  #
#------------------------------------------------#

FormX <-
  formula(result ~ paved + roofs + grass + trees + nodev + pm25 + traffic + slope + rain + landuse)
#NOTE: it is not practical at this point to add interactions -- that will come once we find best model using just main effects

if (F) {
  z <- coc2 %>%
    dplyr::select(
      c(
        result,
        impervious,
        paved,
        roofs,
        grass,
        trees,
        nodev,
        pm25,
        traffic,
        slope,
        rain,
        agency,
        month
      )
    )
  pairs(
    z,
    lower.panel = panel.smooth2,
    upper.panel = panel.cor,
    diag.panel = panel.hist
  )
  #For copper: impervious, nodev, pm25, traffic=abs(0.6) correlation with result; slope & rain (both 0.2)
}


#-------------------------------------------------------#
#  Zuur et al Method for Mixed Effects Model Selection  #
#-------------------------------------------------------#

#------------------#
#  All Predictors  #
#------------------#

# Steps 1-2: fit LM for ALL PREDICTORS to data using gls; look for heterogeneity
#-----------

if (F) {
  M.gls1X <-
    gls(FormX, data = coc2, method = "REML")  #use REML for comparing diff't random variances
  E.1X <- resid(object = M.gls1X, type = "normalized")
  plot.resids(M.gls1X, E.1X, "X")  #check for heterogeneity in residuals vs fitted values; look for source in other plots
  
  
  #Step 3:  Choose a variance structure (if there was heterogeneity)
  #-------  for selecting random structure, use REML to compare - its better at capturing random structure
  #         REML estimates the random effects by considering linear combinations of the data that remove the
  #         fixed effects. If these fixed effects are changed, the likelihoods of the two models will not be directly comparable.
  #         Compare AIC for the various beyond-optimal models with different variance structures.
  #         Alternately, can use likelihood ratio test (anova) to compare nested models
  
  M.gls2X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varIdent(form = ~ 1 | agency)
    )
  M.gls3X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varIdent(form = ~ 1 | month)
    )
  M.gls4X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varIdent(form = ~ 1 | season)
    )
  M.gls5X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varIdent(form = ~ 1 | agency * season)
    )
  M.gls6X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varIdent(form = ~ 1 | agency * year)
    )
  M.gls7X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 | agency), varExp(form = ~ dry))
    )
  M.gls8X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varConstPower(form = ~ dry))
    )
  M.gls9X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varExp(form = ~ rain7))
    )  #agency & previous 7-day rainfall (SR-xformed), unstandardized
  M.gls10X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varConstPower(form = ~ rain7))
    )
  M.gls11X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varExp(form = ~ rain28))
    )  #agency & previous 28-day rainfall (SR-xformed), unstandardized
  M.gls12X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varPower(form = ~ rain28))
    )  #we can try varPower since min(rain28) > 0
  M.gls13X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varConstPower(form = ~ rain28))
    )
  M.gls14X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varExp(form = ~ monthlyPrecip))
    )  #agency & monthly average precip (same for all years & locations)
  M.gls15X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(
        varIdent(form = ~ 1 | agency),
        varConstPower(form = ~ monthlyPrecip)
      )
    )
  M.gls16X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varExp(form = ~ mPrecip))
    )  #agency & monthly precip (diff't by year & location)
  M.gls17X <-
    gls(
      FormX,
      data = coc2,
      method = "REML",
      weights = varComb(varIdent(form = ~ 1 |
                                   agency), varConstPower(form = ~ (mPrecip / 10)))
    )
  
  anova(
    M.gls1X,
    M.gls2X,
    M.gls3X,
    M.gls4X,
    M.gls5X,
    M.gls6X,
    M.gls7X,
    M.gls8X,
    M.gls9X,
    M.gls10X,
    M.gls11X,
    M.gls12X,
    M.gls13X,
    M.gls14X,
    M.gls15X,
    M.gls16X
  )
  #for copper: AIC and BIC are best for agency & antecedant dry days (dry) as variance covariates (#8)
  
  #likelihood ratio tests for nested versions of the best fit models
  anova(M.gls1X, M.gls2X, M.gls8X)
  
  #residual plots for best fit models -- look for homogeneity of residuals
  E.8X <- resid(object = M.gls8X, type = "normalized")
  plot.resids(M.gls8X, E.8X, "X")
}

#variance function for best fit models so far
vf1X <-
  varComb(varIdent(form = ~ 1 | agency), varConstPower(form = ~ dry))


# Steps 4-6: Find the proper random effects structure; look for temporal and spatial autocorrelation
#-----------  note: look for spatial correlation AFTER setting random effects!

if (F) {
  #----Model with Variance Function 1----#
  M.vf1X <- gls(FormX,
                data = coc2,
                method = "REML",
                weights = vf1X)
  
  #Random intercept model; this is nested in the best fit model from Step 2, so can compare with a likelhiood ratio test
  M1.lme1X <-
    lme(
      data = coc2,
      FormX,
      random = ~ 1 | agency,
      method = "REML",
      weights = vf1X,
      control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
    )
  anova(M.vf1X, M1.lme1X)  #the likelihood ratio test p-value compares the two nested models, and whether they are significantly different
  #For copper, the model with random intercept is significantly better than without!
  
  #Random slope models; with our dataset, these only makes sense for rain or dry.  Other predictors don't have enough data points along x-axis to generate a random slope reliably.
  M1.lme2X <-
    lme(
      data = coc2,
      FormX,
      random = ~ 1 + rain | agency,
      method = "REML",
      weights = vf1X,
      control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
    )
  anova(M.vf1X, M1.lme1X, M1.lme2X)  #likelihood ratio test
  #for copper, the random slope model has is not better than the random intercept model
}

#random structure for best fit model so far
r1X <- formula( ~ 1 | agency)

if (F) {
  #best random effects structure & residual plots to test it
  M1.rX <-
    lme(
      data = coc2,
      FormX,
      random = r1X,
      method = "REML",
      weights = vf1X,
      control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
    )
  summary(M1.rX)
  E1.rX <- residuals(object = M1.rX, type = "normalized")
  plot.resids(M1.rX, E1.rX, "X")
  
  #Check for temporal correlation; look for patterns in Auto-correlation plot for residuals
  par(mfrow = c(2, 2))
  plot(E1.rX ~ coc2$start_date, pch = 16, col = "cadet blue")
  acf(E1.rX, na.action = na.pass, main = "Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
  #for copper, no indication of temporal autocorrelation
  
  #Check for spatial correlation
  mydata2 <-
    data.frame(
      E1.rX,
      coc2.longitude = jitter(coc2$longitude, amount = 0.05),
      coc2.latitude = jitter(coc2$latitude, amount = 0.05)
    )  #jitter the X-Y coordinates, so that data points aren't on top of each other
  coordinates(mydata2) <- c("coc2.longitude", "coc2.latitude")
  bubble(
    mydata2,
    "E1.rX",
    col = c("black", "grey"),
    main = "Residuals",
    xlab = "X-coordinates",
    ylab = "Y-coordinates"
  )
  
  #variogram to test for spatial correlation
  Vario1 = variogram(E1.rX ~ 1, mydata2)
  plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
  Vario2 <- variogram(E1.rX ~ 1, mydata2, alpha = c(0, 45, 90, 135))
  plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?
  #for copper, no indication of temporal autocorrelation
}


# Steps 7-8: Find the proper fixed effects structure; use ML for model comparisons
#-----------

#-------------------------------------------------------------#
#  Exhaustive Search for Model with Best 3 Predictors + Rain  #
#-------------------------------------------------------------#

if (F) {
  #Construct all possible models with 1, 2 and 3 landscape predictors, both including and not including rain;
  #   use AIC to determine which models provide the best fit
  preds.1 <-
    c(
      "impervious",
      "paved",
      "roofs",
      "trees",
      "grass",
      "nodev",
      "pm25",
      "traffic",
      "slope"
    )
  
  #one predictor
  dd.1 <- data.frame(Var1 = preds.1,
                     Var2 = NA,
                     Var3 = NA)
  
  #two predictors
  dd.2 <- data.frame(NULL)
  for (i in 1:length(preds.1)) {
    preds.2 <- preds.1[-c(1:i)]
    dd.2 <- rbind(dd.2, expand.grid(preds.1[i], preds.2))
  }
  dd.2$Var3 <- NA
  dd.2 <- dd.2[-c(1), ] #eliminate situation with impervious + paved
  
  #three predictors
  dd.3 <- data.frame(NULL)
  for (j in 1:(length(preds.1) - 2)) {
    aa <- preds.1[j]
    preds.1a <-
      preds.1[-c(1:j)]  #remove current predictor "j", and all predictors before it
    for (i in 1:(length(preds.1a) - 1)) {
      preds.2a <- preds.1a[-c(1:i)]
      bb <- expand.grid(preds.1a[i], preds.2a)
      dd.3 <- rbind(dd.3, data.frame(bb, Var3 = aa))
    }
  }
  dd.3 <-
    dd.3[-c(1:7), ]  #eliminate situations with impervious + paved
  
  #combine one, two and three-predictor options into one dataframe
  dd <- rbind(dd.1, dd.2, dd.3)  #, dd.1, dd.2, dd.3)
  
  #generate an exhaustive set of formulas with one, two and three predictors, including and excluding rain
  ee.1 <-
    list(NULL)  #list of formulas based on the sets of predictors in "dd"; including rain
  for (i in 1:nrow(dd)) {
    if (is.na(dd$Var2[i]))
      ee.1[[i]] <- as.formula(paste("result~rain + ", dd[i, 1]))
    else if (is.na(dd$Var3[i]))
      ee.1[[i]] <-
        as.formula(paste("result~rain + ", dd[i, 1], "+", dd[i, 2]))
    else
      ee.1[[i]] <-
        as.formula(paste("result~rain + ", dd[i, 1], "+", dd[i, 2], "+", dd[i, 3]))
  }
  ee.2 <-
    list(NULL) #list of formulas based on the sets of predictors in "dd"; NOT including rain
  for (i in 1:nrow(dd)) {
    if (is.na(dd$Var2[i]))
      ee.2[[i]] <- as.formula(paste("result~", dd[i, 1]))
    else if (is.na(dd$Var3[i]))
      ee.2[[i]] <- as.formula(paste("result~", dd[i, 1], "+", dd[i, 2]))
    else
      ee.2[[i]] <-
        as.formula(paste("result~", dd[i, 1], "+", dd[i, 2], "+", dd[i, 3]))
  }
  ee <- c(ee.1, ee.2)
  
  #run the lme for each formula, saving the AIC values; takes about 7 minutes to run
  ptm <- proc.time()  #time the code!
  my.aics <- rep(0, length(ee))
  for (i in 1:length(ee)) {
    bb <-
      lme(
        data = coc2,
        ee[[i]],
        random = r1X,
        method = "ML",
        weights = vf1X,
        control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
      )
    my.aics[i] <- AIC(bb)
  }
  proc.time() - ptm  #stop the clock
  
  
  par(
    mfrow = c(3, 4),
    mar = c(1, 2, 2, 0),
    oma = c(1, 1, 1, 1)
  )
  gg <- rep("black", length(ee))
  gg[c(which(is.na(dd$Var3)), which(is.na(dd$Var3)) + nrow(dd))] <-
    "turquoise"   #make formulas with only 2 landscape predictors red
  gg[c(which(is.na(dd$Var2)), which(is.na(dd$Var2)) + nrow(dd))] <-
    "pink"  #make formulas with only 1 landscape predictor yellow
  plot(
    my.aics,
    col = gg,
    pch = 16,
    main = "1 pred=pink, 2 preds=turquoise",
    xaxt = "n",
    ylab = "AIC"
  )
  gg <- rep("black", length(ee))
  gg[which(str_detect(as.character(ee), "impervious"))] <-
    "red"   #make formulas with "rain" light blue
  gg[which(str_detect(as.character(ee), "paved"))] <-
    "orange"   #make formulas with "rain" light blue
  plot(
    my.aics,
    col = gg,
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "impervious=red, paved=orange"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "rain"), "light blue", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "rain=light blue"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "slope"), "tan3", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "slope=light brown"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "trees"), "green3", "black"),
    xaxt = "n",
    pch = 16,
    main = "trees=green"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "grass"), "light green", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "grass=light green"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "nodev"), "purple1", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "nodev=light purple"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "roofs"), "cadet blue", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "roofs=blue"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "pm25"), "yellow", "black"),
    xaxt = "n",
    pch = 16,
    main = "pm25=yellow"
  )
  plot(
    my.aics,
    col = ifelse(str_detect(as.character(ee), "traffic"), "gray", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "traffic=gray"
  )
  #for copper, best fitting predictors are: rain, trees, impervious/paved
  #    2 predictors vs 3 predictors doesn't make much difference.  definitely want trees+impervious/paved + rain
  
  my.aics[head(order(my.aics))]  #what are the top 6 AIC's?
  hh <- ee[order(my.aics)]
  head(hh)
  #for copper, best AIC is for result~rain + paved + trees (any other predictors are irrelevant)
}

Form4 <-
  formula(result ~ rain + paved + trees)  #based on AIC's, best formula for model #4


#-----------------------------------------------------------------------#
#  Exhaustive Search for Model with Landuse + Best 3 Predictors + Rain  #
#-----------------------------------------------------------------------#

if (F) {
  #generate an exhaustive list of formulas based on the sets of predictors in "dd"; including landuse & rain
  ff.1 <- list(NULL)
  for (i in 1:nrow(dd)) {
    if (is.na(dd$Var2[i]))
      ff.1[[i]] <-
        as.formula(paste("result~landuse + rain + ", dd[i, 1]))
    else if (is.na(dd$Var3[i]))
      ff.1[[i]] <-
        as.formula(paste("result~landuse + rain + ", dd[i, 1], "+", dd[i, 2]))
    else
      ff.1[[i]] <-
        as.formula(paste("result~landuse + rain + ", dd[i, 1], "+", dd[i, 2], "+", dd[i, 3]))
  }
  ff.2 <- list(NULL)
  for (i in 1:nrow(dd)) {
    if (is.na(dd$Var2[i]))
      ff.2[[i]] <- as.formula(paste("result~landuse + ", dd[i, 1]))
    else if (is.na(dd$Var3[i]))
      ff.2[[i]] <-
        as.formula(paste("result~landuse + ", dd[i, 1], "+", dd[i, 2]))
    else
      ff.2[[i]] <-
        as.formula(paste("result~landuse + ", dd[i, 1], "+", dd[i, 2], "+", dd[i, 3]))
  }
  ff <- c(ff.1, ff.2)
  
  #run the lme for each formula, saving the AIC values; takes about 7 minutes to run
  ptm <- proc.time()  #time the code!
  my.aics.ff <- rep(0, length(ff))
  for (i in 1:length(ff)) {
    bb <-
      lme(
        data = coc2,
        ff[[i]],
        random = r1X,
        method = "ML",
        weights = vf1X,
        control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
      )
    my.aics.ff[i] <- AIC(bb)
  }
  proc.time() - ptm  #stop the clock
  
  #combine the formulas and AIC values for both landscape and non-landscape formulas
  jj <- c(ee, ff)
  my.aics.jj <- c(my.aics, my.aics.ff)
  
  par(
    mfrow = c(3, 4),
    mar = c(1, 2, 2, 0),
    oma = c(1, 1, 1, 1)
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "rain"), "light blue", "black"),
    xaxt = "n",
    pch = 16,
    main = "rain=light blue"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "landuse"), "pink", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "landuse=pink"
  )
  # abline(h=c(AIC(M1.LUr), AIC(M1.LU)), col="magenta", lty=2, lwd=2)
  # text(x=300, y=AIC(M1.LUr)+10, label="AIC for landuse w/random structure", col="magenta")
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "slope"), "tan3", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "slope=light brown"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "trees"), "green3", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "trees=green"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "grass"), "light green", "black"),
    xaxt = "n",
    pch = 16,
    main = "grass=light green"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "nodev"), "purple1", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "nodev=light purple"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "roofs"), "cadet blue", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "roofs=blue"
  )
  gg <- rep("black", length(jj))
  gg[which(str_detect(as.character(jj), "impervious"))] <-
    "red"   #make formulas with "rain" light blue
  gg[which(str_detect(as.character(jj), "paved"))] <-
    "orange"   #make formulas with "rain" light blue
  plot(
    my.aics.jj,
    col = gg,
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "impervious=red, paved=orange"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "pm25"), "yellow", "black"),
    xaxt = "n",
    pch = 16,
    main = "pm25=yellow"
  )
  plot(
    my.aics.jj,
    col = ifelse(str_detect(as.character(jj), "traffic"), "gray", "black"),
    xaxt = "n",
    yaxt = "n",
    pch = 16,
    main = "traffic=gray"
  )
  #for copper, adding landuse makes a big difference; landuse + rain + impervious/paved and potentially traffic
  
  my.aics.jj[head(order(my.aics.jj))]  #what are the top 6 AIC's?
  kk <- jj[order(my.aics.jj)]
  kk[1:10]
  #for copper, consistently best AICs are for result~landuse + rain + paved + traffic (can include other predictors)
}

Form5 <-
  formula(result ~ landuse + rain + paved + traffic)  #based on AIC's, best formula for model #5




#--------------------------------------------#
#  Quantiles List for Plotting Interactions  #
#--------------------------------------------#

#list of quantiles for all landscape predictors
qList <- list(
  paved = quantile(coc2$paved, probs = c(0, .25, .50, .75, 1)),
  impervious = quantile(coc2$impervious, probs = c(0, .25, .50, .75, 1)),
  roofs = quantile(coc2$roofs, probs = c(0, .25, .50, .75, 1)),
  grass = quantile(coc2$grass, probs = c(0, .25, .50, .75, 1)),
  trees = quantile(coc2$trees, probs = c(0, .25, .50, .75, 1)),
  traffic = quantile(coc2$traffic, probs = c(0, .25, .50, .75, 1)),
  pm25 = quantile(coc2$pm25, probs = c(0, .25, .50, .75, 1)),
  slope = quantile(coc2$slope, probs = c(0, .25, .50, .75, 1)),
  nodev = quantile(coc2$nodev, probs = c(0, .25, .50, .75, 1)),
  dry = quantile(coc2$dry, probs = c(0, .25, .50, .75, 1)),
  rain = quantile(coc2$rain, probs = c(0, .25, .50, .75, 1))
)



#------------------------#
#  Models 1, 3, 4 and 5  #
#------------------------#

#------ Model 1: median value for all locations --------#

Model1 <-
  gls(data = coc2, result ~ 1, method = "ML")  #here, we use the actual concentration, rather than the transformed one
E1 <- residuals(object = Model1, type = "normalized")

#------ Model 3: land use + rain with variance structure & random effects ---------#

Model3 <-
  lme(
    data = coc2,
    result ~ landuse + rain,
    random = r1X,
    method = "ML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E3 <- residuals(object = Model3, type = "normalized")

#------ Model 4: no land use; up to 3 predictors + rain, with interactions ---------#

Model4 <-
  lme(
    data = coc2,
    Form4,
    random = r1X,
    method = "ML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E4 <- residuals(object = Model4, type = "normalized")

#try adding interactions and see if they are significant
M4.sub1 <-
  update(Model4, . ~ . + rain:paved) #slightly significant (p=0.0115)
anova(Model4, M4.sub1)
M4.sub2 <-
  update(Model4, . ~ . + rain:trees)  #not significant (p=0.359)
anova(Model4, M4.sub2)
M4.sub3 <-
  update(M4.sub2, . ~ . + paved:trees)  #not significant (p=0.236)
anova(M4.sub2, M4.sub3)

#Plot interactions: rain:paved
Plot.Quantile("rain", "paved", M4.sub1, yEqn = 6)
#This interaction makes sense both graphically as well as theoretically.  Keep it in the copper: Model 4 equation!

Form4 <- formula(result ~ rain + paved + trees + rain:paved)
Model4 <-
  lme(
    data = coc2,
    Form4,
    random = r1X,
    method = "ML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E4 <- residuals(object = Model4, type = "normalized")
boxplots.resids2(Model4, E4, "X")


#------ Model 5: up to 3 predictors + landuse + rain ---------#

Model5 <-
  lme(
    data = coc2,
    Form5,
    random = r1X,
    method = "ML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E5 <- residuals(object = Model5, type = "normalized")

#try adding interactions and see if they are significant
M5.sub1 <- update(Model5, . ~ . + paved:rain)   #not significant
anova(Model5, M5.sub1)
M5.sub2 <- update(Model5, . ~ . + traffic:rain)  #not significant
anova(Model5, M5.sub2)
M5.sub3 <-
  update(Model5, . ~ . + landuse:rain)  #borderline, but not significant enough (p=0.041)
anova(Model5, M5.sub3)
M5.sub4 <-
  update(Model5, . ~ . + paved:traffic)  #significant (p=0.0058)
anova(Model5, M5.sub4)
M5.sub5 <-
  update(M5.sub4, . ~ . + landuse:paved)  #very significant (p<0.0001)
anova(M5.sub4, M5.sub5)
#M5.sub6 <- update(M5.sub5, .~. +landuse:traffic)  #singularity - don't use!


#Plot Formula 5 interactions: paved:traffic, landuse:paved
Plot.Quantile("paved", "traffic", M5.sub5, yEqn = 6.7)
#  this interaction looks like it should be quadratic rather than linear -- remove as a linear interaction.
#  note that the medium to high traffic is all clustered in the center, while the low traffic is to the left
# (with one COM exception that is to the far right, maybe POT?)


#Plot Landuse:paved
IQ <- effect(
  'landuse*paved',
  M5.sub5,
  xlevels = list(paved = qList[["paved"]], landuse = levels(coc2$landuse)),
  se = TRUE,
  confidence.level = 0.95,
  typical = mean
)
IQ <- as.data.frame(IQ)

intEqnHDR <-
  "ln(Copper, HDR) = 2.51 + 2.64*paved + 1.38*HDR + 1.31*paved*HDR"
intEqnLDR <-
  "ln(Copper, COM) = 2.51 + 2.64*paved + 0.56*COM + -2.61*paved*COM"
intEqnIND <-
  "ln(Copper, IND) = 2.51 + 2.64*paved + 0.10*IND + -2.92*paved*IND"

ggplot() +
  geom_line(
    data = IQ,
    size = 1.5,
    aes(
      x = IQ$paved,
      y = fit,
      group = IQ$landuse,
      color = IQ$landuse
    )
  ) +
  ylab("COC") + xlab("paved") +
  ggtitle(paste("Interaction between", names(IQ)[1], "and", names(IQ)[2])) +
  scale_color_manual(values = c("blue", "purple", "yellow", "orange")) +  #colors for lines (color)
  geom_point(
    data = coc2,
    aes(x = paved, y = result, fill = landuse),
    size = 2.5,
    shape = 21,
    stroke = 0
  ) +
  scale_fill_manual(values = c("blue", "purple", "yellow", "orange")) +  #colors for points (fill)
  geom_text(aes(x = 0.5, y = 12, label = intEqnHDR),
            cex = 4.5,
            color = "black") +
  geom_text(aes(x = 0.5, y = 11.5, label = intEqnLDR),
            cex = 4.5,
            color = "black") +
  geom_text(aes(x = 0.5, y = 11, label = intEqnIND),
            cex = 4.5,
            color = "black") +
  labs(color = "landuse", fill = "landuse")
#  this interaction is a little worrisome, as all residential sites are on the left, and all COM/IND sites
#  are on the right.  This one should be removed.

boxplots.resids2(Model5, E5, "X")  #keep formula & model without interactions


#------ Compare models and plot ---------#

#compare all of the models
AIC(Model1, Model3, Model4, Model5)
#obtain the delta AIC value
AIC(Model1, Model3, Model4, Model5)[, 2] - AIC(Model5)


#plot model predictions for each model (above)
if (F) {
  #change from F to T if you want to plot these
  M1.preds <- predict(Model1)
  plot.preds.vs.results(M1.preds)
  
  M3.preds <- predict(Model3)
  plot.preds.vs.results(M3.preds)
  
  M4.preds <- predict(Model4)
  plot.preds.vs.results(M4.preds)
  
  M5.preds <- predict(Model5)
  plot.preds.vs.results(M5.preds)
}

#----------------------------------#
#  Look at the Fits of the Models  #
#----------------------------------#

#check residuals for each model, to see if they tell us anything important
par(mfrow = c(2, 2), mar = c(2, 4, 4, 1))
plot(
  coc2$location,
  E1,
  main = "Model 1",
  ylab = "Residuals",
  xaxt = "n",
  col = colors_agency[c(1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)]
)
axis(
  side = 1,
  at = c(2, 5, 7, 9, 12, 15),
  labels = c("King", "Pierce", "POT", "Sea", "Sno", "Tac")
)
abline(h = 0, col = "gray")
plot(
  coc2$location,
  E3,
  main = "Model 3",
  ylab = "Residuals",
  xaxt = "n",
  col = colors_agency[c(1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)]
)
axis(
  side = 1,
  at = c(2, 5, 7, 9, 12, 15),
  labels = c("King", "Pierce", "POT", "Sea", "Sno", "Tac")
)
abline(h = 0, col = "gray")
plot(
  coc2$location,
  E4,
  main = "Model 4",
  ylab = "Residuals",
  xaxt = "n",
  col = colors_agency[c(1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)]
)
axis(
  side = 1,
  at = c(2, 5, 7, 9, 12, 15),
  labels = c("King", "Pierce", "POT", "Sea", "Sno", "Tac")
)
abline(h = 0, col = "gray")
plot(
  coc2$location,
  E5,
  main = "Model 5",
  ylab = "Residuals",
  xaxt = "n",
  col = colors_agency[c(1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)]
)
axis(
  side = 1,
  at = c(2, 5, 7, 9, 12, 15),
  labels = c("King", "Pierce", "POT", "Sea", "Sno", "Tac")
)
abline(h = 0, col = "gray")



# Step 9: Refit models with REML and apply graphical model validation; check for homogeneity,
#--------    normality and independence

#------ Model 3: land use + rain ---------#

M3.final <-
  lme(
    data = coc2,
    result ~ landuse + rain,
    random = r1X,
    method = "REML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E3.final <- residuals(object = M3.final, type = "normalized")

#plot best model residuals - check for goodness of model fit
plot.resids(M3.final, E3.final, "X")
#plot residuals against agency, year, month, date and other unused predictors; check if model is missing something important!
boxplots.resids2(M3.final, E3.final, "X")
#is temporal auto-correlation present?  Look for patterns in Auto-correlation plot for residuals
par(mfrow = c(2, 2))
plot(E3.final ~ coc$start_date, pch = 16, col = "cadet blue")
acf(E3.final, na.action = na.pass, main = "Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
#Check for spatial correlation
mydata2 <-
  data.frame(
    E3.final,
    coc2.longitude = jitter(coc2$longitude, amount = 0.05),
    coc2.latitude = jitter(coc2$latitude, amount = 0.05)
  )  #jitter the X-Y coordinates, so that data points aren't on top of each other
coordinates(mydata2) <- c("coc2.longitude", "coc2.latitude")
bubble(
  mydata2,
  "E3.final",
  col = c("black", "grey"),
  main = "Residuals",
  xlab = "X-coordinates",
  ylab = "Y-coordinates"
)
#variogram to test for spatial correlation
Vario1 = variogram(E3.final ~ 1, mydata2)
plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
Vario2 <-
  variogram(E3.final ~ 1, mydata2, alpha = c(0, 45, 90, 135))
plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?



#------ Model 4: no land use; up to 3 predictors + rain ---------#

M4.final <-
  lme(
    data = coc2,
    Form4,
    random = r1X,
    method = "REML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E4.final <- residuals(object = M4.final, type = "normalized")

#plot best model residuals - check for goodness of model fit
plot.resids(M4.final, E4.final, "X")
#plot residuals against agency, year, month, date and other unused predictors; check if model is missing something important!
boxplots.resids2(M4.final, E4.final, "X")
#is temporal auto-correlation present?  Look for patterns in Auto-correlation plot for residuals
par(mfrow = c(2, 2))
plot(E4.final ~ coc$start_date, pch = 16, col = "cadet blue")
acf(E4.final, na.action = na.pass, main = "Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
#Check for spatial correlation
mydata2 <-
  data.frame(
    E4.final,
    coc2.longitude = jitter(coc2$longitude, amount = 0.05),
    coc2.latitude = jitter(coc2$latitude, amount = 0.05)
  )  #jitter the X-Y coordinates, so that data points aren't on top of each other
coordinates(mydata2) <- c("coc2.longitude", "coc2.latitude")
bubble(
  mydata2,
  "E4.final",
  col = c("black", "grey"),
  main = "Residuals",
  xlab = "X-coordinates",
  ylab = "Y-coordinates"
)
#variogram to test for spatial correlation
Vario1 = variogram(E4.final ~ 1, mydata2)
plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
Vario2 <-
  variogram(E4.final ~ 1, mydata2, alpha = c(0, 45, 90, 135))
plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?




#------ Model 5: up to 3 predictors + landuse + rain ---------#

M5.final <-
  lme(
    data = coc2,
    Form5,
    random = r1X,
    method = "REML",
    weights = vf1X,
    control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
  )
E5.final <- residuals(object = M5.final, type = "normalized")

#plot best model residuals - check for goodness of model fit
plot.resids(M5.final, E5.final, "X")
#plot residuals against agency, year, month, date and other unused predictors; check if model is missing something important!
boxplots.resids2(M5.final, E5.final, "X")
#is temporal auto-correlation present?  Look for patterns in Auto-correlation plot for residuals
par(mfrow = c(2, 2))
plot(E5.final ~ coc$start_date, pch = 16, col = "cadet blue")
acf(E5.final, na.action = na.pass, main = "Auto-correlation plot for residuals")  #look for lines extending past blue dashed range
#Check for spatial correlation
mydata2 <-
  data.frame(
    E5.final,
    coc2.longitude = jitter(coc2$longitude, amount = 0.05),
    coc2.latitude = jitter(coc2$latitude, amount = 0.05)
  )  #jitter the X-Y coordinates, so that data points aren't on top of each other
coordinates(mydata2) <- c("coc2.longitude", "coc2.latitude")
bubble(
  mydata2,
  "E5.final",
  col = c("black", "grey"),
  main = "Residuals",
  xlab = "X-coordinates",
  ylab = "Y-coordinates"
)
#variogram to test for spatial correlation
Vario1 = variogram(E5.final ~ 1, mydata2)
plot(Vario1)  #if there is no spatial correlation, will see a horizontal bar of points at the top of the plot
Vario2 <-
  variogram(E5.final ~ 1, mydata2, alpha = c(0, 45, 90, 135))
plot(Vario2)  #do we see any different patterns in the different directions, or roughly the same pattern?


#plot parameter estimates for each model
theme_set(theme_sjplot())

plot_model(
  M3.final,
  sort.est = TRUE,
  show.values = TRUE,
  value.offset = .3,
  title = "Copper: Parameter Estimates, Model 3"
)
plot_model(
  M4.final,
  sort.est = TRUE,
  show.values = TRUE,
  value.offset = .3,
  title = "Copper: Parameter Estimates, Model 4"
)
plot_model(
  M5.final,
  sort.est = TRUE,
  show.values = TRUE,
  value.offset = .3,
  title = "Copper: Parameter Estimates, Model 5"
)

# plot_model(M4.final, type="slope")
# plot_model(M4.final, type="pred")
# plot_model(M4.final, type = "diag")

save.image(file = 'Copper Models.RData')

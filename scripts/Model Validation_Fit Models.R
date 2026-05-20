
library(nlme)
library(tidyr)
library(dplyr)
library(sjPlot)
library(sjlabelled)


load(file="../results/Frequentist_Copper Models.RData")
load(file="../results/Frequentist_Total Zinc Models_Greenery_bareEarth.RData")
load(file="../results/Frequentist_TSS Models.RData")
load(file="../results/Frequentist_Total Kjeldahl Nitrogen Models_censtat.RData")
load(file="../results/Frequentist_Total Phosphorus Models.RData")
#load(file="../results/Best Fit Bayesian Models Using Model Validation Location Names.RData.RData")


#----------------#
#  Total Copper  #
#----------------#

Cu.Form4  #lme model equation
Cu.r1X  #random effect in lme model
Cu.vf1X  #variance structure for lme model

#remove Perkins Bluff, which doesn't fit well with the other data
Cu.mv.dat3 <- Cu.mv.dat2 %>%
  filter(!location == "PerkinsBluff")

#Original copper lme model summary
Cu.lme <- lme(Cu.Form4, data=Cu.coc2, method="REML", 
              random = ~1 | agency/location, 
              weights=varIdent(form= ~1|location), 
              control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Cu.lme)

#Model Validation copper lme model summary
Cu.mv.lme <- lme(Cu.Form4, data=Cu.mv.dat2, method="REML", 
                 random = ~1 | Project/location, 
                 weights=varIdent(form= ~1|location), 
                 control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Cu.mv.lme)

#Model Validation copper lme model summary - NO PERKINS BLUFF!
Cu.mv.lme2 <- lme(Cu.Form4, data=Cu.mv.dat3, method="REML", 
                 random = ~1 | Project/location, 
                 weights=varIdent(form= ~1|location), 
                 control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Cu.mv.lme2)

Cu_models <- list(
  Spatial_Predictor_Model = Cu.lme,
  Model_Validation_Fit = Cu.mv.lme)

Cu_models <- list(
  Spatial_Predictor_Model = Cu.lme,
#  Model_Validation_Fit = Cu.mv.lme,
  MV_Fit_no_PerkinsBluff = Cu.mv.lme2)

#plot parameter estimates for each model
theme_set(theme_sjplot())
plot_models(Cu_models,m.labels = names(Cu_models),legend.title = "Models", show.values = TRUE,show.intercept = TRUE)


#--------------#
#  Total Zinc  #
#--------------#

totZn.Form4
totZn.r1X
totZn.vf1X

#remove Perkins Bluff, which doesn't fit well with the other data
Zn.mv.dat3 <- Zn.mv.dat2 %>%
  filter(!location == "PerkinsBluff")

#Total Zinc lme model summary
totZn.lme <- lme(totZn.Form4, data=totZn.coc2, method="REML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(totZn.lme)

#Model Validation copper lme model summary
Zn.mv.lme <- lme(totZn.Form4, data=Zn.mv.dat2, method="REML", 
                 random = ~1 | Project/location, 
                 weights=varIdent(form= ~1|location), 
                 control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Zn.mv.lme)

#Model Validation copper lme model summary
Zn.mv.lme2 <- lme(totZn.Form4, data=Zn.mv.dat3, method="REML",
                 random = ~1 | Project/location,
                 weights=varIdent(form= ~1|location),
                 control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Zn.mv.lme2)

Zn_models <- list(
  Spatial_Predictor_Model = totZn.lme,
#  Model_Validation_Fit = Zn.mv.lme,
  Model_Validation_Fit_no_Perkins = Zn.mv.lme2)
# 
# Zn_models <- list(
#   Spatial_Predictor_Model = totZn.lme,
#   Model_Validation_Fit = Zn.mv.lme,
#   Model_Validation_Fit_no_PerkinsBluff = Zn.mv.lme2)

#plot parameter estimates for each model
theme_set(theme_sjplot())
plot_models(Zn_models,m.labels = names(Zn_models),legend.title = "Models", show.values = TRUE,show.intercept = TRUE)



#-------#
#  TSS  #
#-------#

TSS.Form4
TSS.r1X
TSS.vf1X

#Original Model TSS lme model summary
TSS.lme <- lme(TSS.Form4, data=TSS.coc2, method="REML", random = TSS.r1X, weights=TSS.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TSS.lme)

#Model Validation TSS lme model summary
TSS.mv.lme <- lme(TSS.Form4, data=TSS.mv.dat2, method="REML", 
                 random = ~1 | Project/location, 
                 weights=varIdent(form= ~1|location), 
                 control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

TSS.mv.lme <- lme(TSS.Form4, data=TSS.mv.dat2, method="REML", 
                  random = ~1 | Project/location, 
                  weights=varIdent(form= ~1|location), 
                  control = lmeControl(opt="optim"))

summary(TSS.mv.lme)

TSS_models <- list(
  Spatial_Predictor_Model = TSS.lme,
  Model_Validation_Fit = TSS.mv.lme)

#plot parameter estimates for each model
theme_set(theme_sjplot())
plot_models(TSS_models,m.labels = names(TSS_models),legend.title = "Models", show.values = TRUE,show.intercept = TRUE)


#---------------------------#
#  Total Kjeldahl Nitrogen  #   #Bayesian model uses agency as the variance covariate!
#---------------------------#

TKN.Form4
TKN.r1X
TKN.vf1X

#Original Model Total Kjeldahl Nitrogen lme model summary
TKN.lme <- lme(TKN.Form4, data=TKN.coc2, method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TKN.lme)

#Model Validation TKN lme model summary
TKN.mv.lme <- lme(TKN.Form4, data=TKN.mv.dat2, method="REML", 
                  random = ~1 | Project/location, 
                  weights=varIdent(form= ~1|Project), 
                  control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TKN.mv.lme)

TKN_models <- list(
  Spatial_Predictor_Model = TKN.lme,
  Model_Validation_Fit = TKN.mv.lme)

#plot parameter estimates for each model
theme_set(theme_sjplot())
plot_models(TKN_models,m.labels = names(TKN_models),legend.title = "Models", show.values = TRUE,show.intercept = TRUE)




#--------------------#
#  Total Phosphorus  #
#--------------------#

P.Form4
P.r1X
P.vf1X
#  Phosphorus lme model doesn't have too strong of a landscape predictor; try a model with only rain + summer

#Original Model Phosphorus lme model summary
P.lme <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, weights=P.vf1X)
summary(P.lme)

#Model Validation Phosphorus lme model summary
P.mv.lme <- lme(P.Form4, data=P.mv.dat2, method="REML", 
                  random = ~1 | Project/location, 
                  weights=varIdent(form= ~1|location), 
                  control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(P.mv.lme)

P_models <- list(
  Spatial_Predictor_Model = P.lme,
  Model_Validation_Fit = P.mv.lme)

#plot parameter estimates for each model
theme_set(theme_sjplot())
plot_models(P_models,m.labels = names(P_models),legend.title = "Models", show.values = TRUE,show.intercept = TRUE)















# This script generates a Bayesian model for total Zinc as a function of land use (rather than landscape predictors)
#   based on the frequentist model for total zinc as a fxn of land use.

# Subsequently, model validation is carried out using the validation data's land use, for the purpose of comparing
#   validation outcomes for the zinc landscape predictor model to the land use model

# Author: Eva Dusek Jennings
# Revised: Oct 21, 2025
#-------------------------------------------------------------------------------------------------------------------

#options(mc.cores = 2)

#if having trouble with installing packages, install them from binary, like this:
#  install.packages("igraph", type="binary")

#devtools::install_github("paul-buerkner/brms")
library(brms)
library(nlme)
library(ggplot2)
library(loo)
#devtools::install_github("rmcelreath/rethinking")  #this may not work b/c dependency "cmdstanr" isn't available


#methods(class="brmsfit")  #complete list of methods available for brmsfit models


#----------------------#
#  LANDUSE Total Zinc  #
#----------------------#

load(file="../results/Frequentist_Total Zinc Models_notGreenBE.RData")
totZn.Form3   #Land use model
totZn.r1X
totZn.vf1X

#remove any zinc values over 800 ug/L (these should have been removed in the frequentist model stage also)
totZn.coc2 <- totZn.coc2 %>%
  filter(!result > log(800))
max(exp(totZn.coc2$result))

#Total Zinc lme model summary
LU_totZn.lme <- lme(totZn.Form3, data=totZn.coc2, method="REML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(LU_totZn.lme)

#compare models with simpler variance structures (no variance covariate, agency=var cov, location=var cov)
par(mfrow=c(3,1))
LU_totZn.lme0 <- lme(totZn.Form3, data=totZn.coc2, method="REML", random = totZn.r1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme0 <- resid(object = LU_totZn.lme0, type = "normalized")
plot(fitted(LU_totZn.lme0), E.lme0, main="no var struct", xlab="fitted", ylab="std residuals", col="gray", pch=16)

E.lme <- resid(object = LU_totZn.lme, type = "normalized")
plot(fitted(LU_totZn.lme), E.lme, main="var cov = location", xlab="fitted", ylab="std residuals", col="gray", pch=16)

LU_totZn.lme1 <- lme(totZn.Form3, data=totZn.coc2, method="REML", random = totZn.r1X, weights=varIdent(form= ~1|agency), control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme1 <- resid(object = LU_totZn.lme1, type = "normalized")
plot(fitted(LU_totZn.lme1), E.lme1, main="var cov = agency", xlab="fitted", ylab="std residuals", col="gray", pch=16)

AIC(LU_totZn.lme0, LU_totZn.lme, LU_totZn.lme1) #AIC best for var cov=location; NOTE: BIC best for var cov=agency
#var cov = location looks best, and also has lowest AIC
#  explanation for variance covariate = agency is that some agencies have more diversity in their types of sites, so having the variance
#  covariate set to agency allows us to compensate for this (residual error E(ijk) would be expected to be higher for some 
#  agencies and lower for others)


#Bayesian Mixed Model - check various variance structures to see if the one selected by lme is the best
#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
LU_fit1 <- brm(bf(result ~ rain + summer + landuse + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20),
            save_pars=save_pars(all=TRUE)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
LU_fit2 <- brm(bf(result ~ rain + summer + landuse + (1|agency/location),                
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20),
            save_pars=save_pars(all=TRUE)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- student t distribution
LU_fit2.t <- brm(bf(result ~ rain + summer + landuse + (1|agency/location), 
                 sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
              data=totZn.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 1),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#compare the Leave One Out (loo) criterion for these three models; LOO is a cross-validation technique to validate the model
LU_fit1 <- add_criterion(LU_fit1, criterion=c("loo"), moment_match=TRUE)
LU_fit2 <- add_criterion(LU_fit2, criterion=c("loo"), moment_match=TRUE)
LU_fit2.t <- add_criterion(LU_fit2.t, criterion=c("loo")) #, moment_match=TRUE)

loo_compare(LU_fit2, LU_fit2.t, criterion="loo")  #top one in the output gives the best model
loo_compare(LU_fit1, LU_fit2, LU_fit2.t, criterion="loo")  #top one in the output gives the best model

# PSIS diagnostic tool - look for points above 0.5, and especially above 0.7
par(mfrow=c(3,1), mar=c(2,4.5,4,2))
plot(loo(LU_fit1, cores=getOption("mc.cores", 1)))  #1 point above 0.7
plot(loo(LU_fit2, cores=getOption("mc.cores", 1)))  #several points above 0.5; 1 points above 0.7
plot(loo(LU_fit2.t, cores=getOption("mc.cores", 1)))  #1 point above 0.6
#using the student's t-distribution gives an improvement

#posterior predictive check of our two models
pp_check(LU_fit2, ndraws=100)
pp_check(LU_fit2.t, ndraws=100)
# use the student t-distribution; it deals with the high pareto K values.

#look at the relative amount of variability within each location, and relative amount of variability within each agency
par(mfrow=c(2,1))
boxplot(result ~ location, data=totZn.coc2)
boxplot(result ~ agency, data=totZn.coc2)
#There is variability at the location scale (mostly at KIC_HDR), which is driving variability at the agency scale.  I'd prefer to use location, 
#  as agency sometimes has only 1 location, sometimes 3

summary(LU_fit2.t)$fixed

#look at residuals vs fitted values for candidate model - are any markedly better than others?
par(mfrow=c(2,1), mar=c(4,4,4,2))
LU_resid.2t <- residuals(LU_fit2.t, type="ordinary")
LU_fitted.2t <- fitted(LU_fit2.t, scale="response")
plot(LU_resid.2t[,1] ~ LU_fitted.2t[,1], ylab="residuals", xlab="fitted values", main="Model 2t - student-T distribution")

LU_totZn.brm <- LU_fit2.t

#-------------------------------------------
#https://tem11010.github.io/regression_brms/

#graphical posterior predictive checking. Compare observed data to simulated data from the posterior predictive distribution. 
#  This is a density plot, where the observed y values are plotted with expected values from the posterior distribution
pp_check(LU_totZn.brm, ndraws=200)

#Look at the fit based on the grouping variable. Here are scatter-plots with the observed chemical concentrations (log scale) 
#  on the y-axis and the average model predictions (across all posterior samples) on the x-axis.
#  Red line is the 1:1 line, indicating perfect fit of model predictions to data.  Any locations where model doesn't fit?
pp_check(LU_totZn.brm, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
#-------------------------------------------

save(LU_totZn.brm, file="../results/Bayesian_LANDUSE_TotZinc.Rdata")




#---------------------------------------#
#  Re-Run Bayesian Models - Total Zinc  #
#---------------------------------------#

totZn.coc2 <- totZn.coc2 %>%
  mutate(location = case_when(location=="TAC_COM" ~ "OF235",
                              location=="TAC_IND" ~ "OF245",
                              location=="TAC_HDR" ~ "OF237B",
                              TRUE ~ location))

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- student t distribution
LU_newNames_fit2.t <- brm(bf(result ~ rain + summer + landuse + (1|agency/location), 
                 sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
              data=totZn.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 2),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

LU_newNames_totZn.brm <- LU_newNames_fit2.t

#------------------------------------------------#
#  Save Model Validation Re-Runs of BRMS models  #
#------------------------------------------------#

save(LU_newNames_totZn.brm, totZn.coc2, file="../results/Best Fit Bayesian LANDUSE Model Using Model Validation Location Names_totZinc.RData")





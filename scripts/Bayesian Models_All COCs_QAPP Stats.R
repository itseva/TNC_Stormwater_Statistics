# This script is a modified version of the "Bayesian Models_All COCs.R" script, specifically 
#  designed to run additional statistics required for the QAPP.  These include:

#  1. If the COC data includes potentially influential outliers, determine whether the model conclusions
#     are sensitive to these outliers.
#  2. Does changing the variance structure result in a quantitative change to the parameters for spatial
#     landscape predictors?
#  3. If our selected random effects structure is incorrect, does a simpler structure result in qualitative
#     differences to the model outcome?
#  4. Assess model precision by comparing the absolute distance between model estimates and the S8 data
#     used to fit the model (root mean square error -- RMSE)

# This script uses v9 lme model results (nested random effects: agency/location) to generate 
#  the Bayesian outputs.

# Author: Eva Dusek Jennings
# Revised: May 9, 2024
#---------------------------------------------------------------------------------------

#options(mc.cores = 2)

#if having trouble with installing packages, install them from binary, like this:
#  install.packages("igraph", type="binary")

#devtools::install_github("paul-buerkner/brms")
library(brms)
library(nlme)
library(ggplot2)
library(loo)
library(dplyr)
#devtools::install_github("rmcelreath/rethinking")  #this may not work b/c dependency "cmdstanr" isn't available

#install.packages("performance")
#library(performance)
#install.packages("HLMdiag")
library(HLMdiag)


#methods(class="brmsfit")  #complete list of methods available for brmsfit models

#----------------#
#  Total Copper  #
#----------------#

load(file="../results/Frequentist_Copper Models.RData")
Cu.Form4  #lme model equation
Cu.r1X  #random effect in lme model
Cu.vf1X  #variance structure for lme model
Cu.lme <- lme(Cu.Form4, data=Cu.coc2, method="REML", random = Cu.r1X, weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))


#------
# 1. Outliers
#------

par(mfrow=c(1,1))
plot(Cu.coc2$result)
#two possible high outliers, one possible low outlier

Cu.lme_inf <- hlm_influence(Cu.lme, level=1)
dotplot_diag(Cu.lme_inf$cooksd, name = "cooks.distance", cutoff = "internal")
Cu.coc2[c(226, 378, 162, 185, 173),]  #top 5 observations in terms of Cook's distance

Cu.coc3 <- Cu.coc2[-c(185, 173),]  #remove the top two, which are particularly influential
Cu.lme.rmOutliers <- lme(Cu.Form4, data=Cu.coc3, method="REML", random = Cu.r1X, weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

coefficients(Cu.lme)
coefficients(Cu.lme.rmOutliers)

fixed.effects(Cu.lme)
fixed.effects(Cu.lme.rmOutliers)

plot(Cu.lme)
plot(Cu.lme.rmOutliers)

abs(fixed.effects(Cu.lme) - fixed.effects(Cu.lme.rmOutliers) ) / fixed.effects(Cu.lme)

#while there are two possible outliers, they have a negligible effect on the landscape predictors sqrt_traffic and devAge2


#------
# End of 1
#------

#Copper lme model summary
Cu.lme <- lme(Cu.Form4, data=Cu.coc2, method="REML", random = Cu.r1X, weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Cu.lme)

par(mfrow=c(3,1))
Cu.lme0 <- lme(Cu.Form4, data=Cu.coc2, method="REML", random = Cu.r1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme0 <- resid(object = Cu.lme0, type = "normalized")
plot(fitted(Cu.lme0), E.lme0, main="no var struct", xlab="fitted", ylab="std residuals", col="gray", pch=16)

E.lme <- resid(object = Cu.lme, type = "normalized")
plot(fitted(Cu.lme), E.lme, main="var cov = location", xlab="fitted", ylab="std residuals", col="gray", pch=16)

Cu.lme1 <- lme(Cu.Form4, data=Cu.coc2, method="REML", random = Cu.r1X, weights=varIdent(form= ~1|agency), control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme1 <- resid(object = Cu.lme1, type = "normalized")
plot(fitted(Cu.lme1), E.lme1, main="var cov = agency", xlab="fitted", ylab="std residuals", col="gray", pch=16)
#var cov = agency or location are both improvements on no variance structure.
#  explanation for variance covariate = agency is that some agencies have more diversity in their types of sites, so having the variance
#  covariate set to agency allows us to compensate for this (residual error E(ijk) would be expected to be higher for some 
#  agencies and lower for others)


#Bayesian Mixed Model - check various variance structures to see if the one selected by lme is the best
#   agency/location as nested random effect; no variance structure
fit0 <- brm(formula= result ~ summer + rain + sqrt_traffic + devAge2 + (1|agency/location),
            data=Cu.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1 <- brm(bf(result ~ summer + rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=Cu.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ summer + rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=Cu.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#------
# 2. Changing Variance Structure
#------

Cu.fit0 <- summary(fit0)$fixed
Cu.fit1 <- summary(fit1)$fixed
Cu.fit2 <- summary(fit2)$fixed

abs(Cu.fit0$Estimate - Cu.fit2$Estimate[-2]) / Cu.fit2$Estimate[-2]
abs(Cu.fit1$Estimate[-2] - Cu.fit2$Estimate[-2]) / Cu.fit2$Estimate[-2]
#comparison between [no variance structure] or [agency as variance covariate] and [location as variance covariate] shows 
#  1% or less difference in values for global intercept or landscape predictors (items 1, 4, 5)

#------
# End of 2
#------

#model validation using approximate leave-one-out cross-validation
#loo package (developed by Vehtari, Gelman and Gabry (2017a, 2017b)) allows calculation of LOOIC, similar to AIC.
#  We are looking to make sure that the Pareto shape k parameter for each data point (used to test reliability and convergence
#  rate of the PSIS-based estimates) is below 0.7
fit0 <- add_criterion(fit0, criterion=c("loo"))
fit1 <- add_criterion(fit1, criterion=c("loo"))
fit2 <- add_criterion(fit2, criterion=c("loo"))
fit2 <- add_criterion(fit2, criterion=c("loo"), moment_match=TRUE)
loo_compare(fit0, fit1, fit2, criterion="loo")  #top one in the output gives the best model
loo_compare(fit0, fit1, criterion="loo")  #top one in the output gives the best model

loo(fit0)  #highest looic is the best model; lowest elpd (expected log predictive density) is the best model
loo(fit1)
loo(fit2)

# PSIS diagnostic tool - look for points (potential influential outliers) above 0.5, and especially above 0.7
plot(loo(fit0, cores=getOption("mc.cores", 1)), main="no var struct")
plot(loo(fit1, cores=getOption("mc.cores", 1)), main="varcov=agency")
plot(loo(fit2, cores=getOption("mc.cores", 1)), main="varcov=location")

pareto_k_ids(loo(fit1), threshold=0.5)  #173
pareto_k_ids(loo(fit2), threshold=0.5)  #18, 173, 350, 443



### FOR SOME ABOVE MODELS, THERE WERE SOME PROBLEMS WITH PARETO-K VALUES BEING TOO HIGH --
#      TRY A STUDENT-T DISTRIBUTION RATHER THAN A GAUSSIAN (NORMAL) DISTRIBUTION
#by using the student-t distribution instead of the gaussian, we are allowing fatter tails, and the influential
#  observation (the highest SNO-HDR value) becomes less of an outlier and its pareto_k value is not longer > 0.7
fit2.t <- brm(bf(result ~ summer + rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=Cu.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)


#------
# 2. Changing Variance Structure
#------

#agency/location as nested random effect; no variance structure
fit0.t <- brm(formula= result ~ summer + rain + sqrt_traffic + devAge2 + (1|agency/location),
            data=Cu.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1.t <- brm(bf(result ~ summer + rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=Cu.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit0.t <- add_criterion(fit0.t, criterion=c("loo"))
fit1.t <- add_criterion(fit1.t, criterion=c("loo"))
fit2.t <- add_criterion(fit2.t, criterion=c("loo"))

# PSIS diagnostic tool - look for points (potential influential outliers) above 0.5, and especially above 0.7
plot(loo(fit0.t, cores=getOption("mc.cores", 1)), main="no var struct")
plot(loo(fit1.t, cores=getOption("mc.cores", 1)), main="varcov=agency")
plot(loo(fit2.t, cores=getOption("mc.cores", 1)), main="varcov=location")

Cu.fit0.t <- summary(fit0.t)$fixed
Cu.fit1.t <- summary(fit1.t)$fixed
Cu.fit2.t <- summary(fit2.t)$fixed
round((abs(Cu.fit0.t$Estimate - Cu.fit2.t$Estimate[-2]) / Cu.fit2.t$Estimate[-2])*100, 2)
round((abs(Cu.fit1.t$Estimate[-2] - Cu.fit2.t$Estimate[-2]) / Cu.fit2.t$Estimate[-2])*100, 2)
#compare models that ALL use a student-t distribution for residual variance

abs(Cu.fit2.t$Estimate[-2] - Cu.fit2$Estimate[-2]) / Cu.fit2.t$Estimate[-2]
#comparison between the predictors assuming data are from a Gaussian vs Student-t distribution 
#  2.9% or less difference in values for global intercept or landscape predictors (items 1, 4, 5)

#------
# End of 2
#------


fit2.t <- add_criterion(fit2.t, criterion=c("loo"))
loo_compare(fit0, fit1, fit2.t, criterion="loo")  #top one in the output gives the best model
#best fit is for fit2.t, the model with sigma = (~1|location), and student-t distribution for data

plot(loo(fit2.t, cores=getOption("mc.cores", 1)), main="varcov=location")
pareto_k_ids(loo(fit2.t), threshold=0.5)  #173
pareto_k_ids(loo(fit2.t), threshold=0.7)  #no values above 0.7


#look at the relative amount of variability within each location, and relative amount of variability within each agency
par(mfrow=c(2,1))
boxplot(result ~ location, data=Cu.coc2)
boxplot(result ~ agency, data=Cu.coc2)
#I don't think I'd use the bottom plot to argue for using agency as a variance covariate (as there are other things that
#  could contribute to the spread, such as amount of sqrt_traffic for an agencies 1 to 3 sites), but the top plot indicates
#  that variability within a location is similar (except for KIC_HDR) for all locations

summary(fit1)$fixed  ###  this model has 1 divergent transition after warmup, making it possibly unreliable...
summary(fit2.t)$fixed


#look at residuals vs fitted values for each model - are any markedly better than others?
par(mfrow=c(4,1), mar=c(4,4,4,2))
resid.0 <- residuals(fit0, type="ordinary")
fitted.0 <- fitted(fit0, scale="response")
plot(resid.0[,1] ~ fitted.0[,1], ylab="residuals", xlab="fitted values", main="Model 0")

resid.1 <- residuals(fit1, type="ordinary")
fitted.1 <- fitted(fit1, scale="response")
plot(resid.1[,1] ~ fitted.1[,1], ylab="residuals", xlab="fitted values", main="Model 1")

resid.2t <- residuals(fit2.t, type="ordinary")
fitted.2t <- fitted(fit2.t, scale="response")
plot(resid.2t[,1] ~ fitted.2t[,1], ylab="residuals", xlab="fitted values", main="Model 2.t")
#these all look pretty similar...


#-------------------------------------------
#https://tem11010.github.io/regression_brms/

#graphical posterior predictive checking. Compare observed data to simulated data from the posterior predictive distribution. 
#  This is a density plot, where the observed y values are plotted with expected values from the posterior distribution
pp_check(fit1, ndraws=20)
pp_check(fit2.t, ndraws=20)
#both of these seem to be similar, just with different scales

#Look at the fit based on the grouping variable. Here are scatter-plots with the observed chemical concentrations (log scale) 
#  on the y-axis and the average model predictions (across all posterior samples) on the x-axis.
#  Red line is the 1:1 line, indicating perfect fit of model predictions to data.  Any locations where model doesn't fit?
pp_check(fit2.t, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
#-------------------------------------------

Cu.brm <- fit2.t  #best Bayesian model for copper, so far


#------
# 3. No agency as a random effect
#------

fit2.t.RE_loc <- brm(bf(result ~ summer + rain + sqrt_traffic + devAge2 + (1|location), 
                 sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
              data=Cu.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 1),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

Cu.fit2.t.RE_loc <- summary(fit2.t.RE_loc)$fixed

Cu.fit2.t.RE_loc
Cu.fit2.t

abs(Cu.fit2.t$Estimate[-2] - Cu.fit2.t.RE_loc$Estimate[-2]) / Cu.fit2.t$Estimate[-2]
#comparison between the predictors assuming random effects is LOCATION only (not nested inside agency)
#  0.1% difference in global intercept parameter, 8.2% difference in sqrt_traffic parameter, 2.8% difference in devAge2 parameter

#------
# End of 3
#------


#------
# 4. RMSE
#------

#load(file="../results/Bayesian_Copper.Rdata")

# 
# library(performance)
# performance_rmse(Cu.brm)
# 

Cu_resid2 <- (residuals(Cu.brm)[, 1])^2  #1. take the residuals & square them
Cu_RMSE <- sqrt(mean(Cu_resid2))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the result
Cu_RMSE
# 0.4900144

#RSR = RMSE/SD
Cu_RSR <- Cu_RMSE / sd(Cu.coc2$result)
Cu_RSR
# 0.6571198

#------
# End of 4
#------


#save(Cu.brm, file="../results/Bayesian_Copper.Rdata")          #think about whether we want to over-write brm best fit model



#--------------------------#
#  Total Suspended Solids  #
#--------------------------#


load(file="../results/Frequentist_TSS Models.RData")
TSS.Form4
TSS.r1X
TSS.vf1X
TSS.lme <- lme(TSS.Form4, data=TSS.coc2, method="REML", random = TSS.r1X, weights=TSS.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
#summary(TSS.lme)

#------
# 1. Outliers
#------

plot(TSS.coc2$result)
#two possible low outliers

TSS.lme_inf <- hlm_influence(TSS.lme, level=1)
dotplot_diag(TSS.lme_inf$cooksd, name = "cooks.distance", cutoff = "internal")
TSS.coc2[c(236, 361, 253, 385, 74),]  #top 5 observations in terms of Cook's distance

TSS.coc3 <- TSS.coc2[-c(253, 385, 74),]  #remove the top two, which are particularly influential
TSS.lme.rmOutliers <- lme(TSS.Form4, data=TSS.coc3, method="REML", random = TSS.r1X, weights=TSS.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

# coefficients(TSS.lme)
# coefficients(TSS.lme.rmOutliers)

fixed.effects(TSS.lme)
fixed.effects(TSS.lme.rmOutliers)

# plot(TSS.lme)
# plot(TSS.lme.rmOutliers)

abs(fixed.effects(TSS.lme) - fixed.effects(TSS.lme.rmOutliers) ) / fixed.effects(TSS.lme)

#three potential influential outliers based on Cook's distance.  Effect on landscape predictors are low:
#  0% for global intercept, 2.4% for sqrt_traffic predictor, and 3.3% for devAge2

#------
# End of 1
#------



par(mfrow=c(3,1))
TSS.lme0 <- lme(TSS.Form4, data=TSS.coc2, method="REML", random = TSS.r1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme0 <- resid(object = TSS.lme0, type = "normalized")
plot(fitted(TSS.lme0), E.lme0, main="no var struct", xlab="fitted", ylab="std residuals", col="gray", pch=16)

E.lme <- resid(object = TSS.lme, type = "normalized")
plot(fitted(TSS.lme), E.lme, main="var cov = location", xlab="fitted", ylab="std residuals", col="gray", pch=16)

TSS.lme1 <- lme(TSS.Form4, data=TSS.coc2, method="REML", random = TSS.r1X, weights=varIdent(form= ~1|agency), control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme1 <- resid(object = TSS.lme1, type = "normalized")
plot(fitted(TSS.lme1), E.lme1, main="var cov = agency", xlab="fitted", ylab="std residuals", col="gray", pch=16)

AIC(TSS.lme0, TSS.lme, TSS.lme1) #AIC also best for var cov=location
#var cov = location looks best, and also has lowest AIC
#  explanation for variance covariate = agency is that some agencies have more diversity in their types of sites, so having the variance
#  covariate set to agency allows us to compensate for this (residual error E(ijk) would be expected to be higher for some 
#  agencies and lower for others)

#Bayesian Mixed Model - check various variance structures to see if the one selected by lme is the best
#   agency/location as nested random effect; no variance structure
fit0 <- brm(bf(result ~ rain + sqrt_traffic + devAge2 + (1|agency/location)), 
            data=TSS.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1 <- brm(bf(result ~ rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=TSS.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=TSS.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#------
# 2. Changing Variance Structure
#------

TSS.fit0 <- summary(fit0)$fixed
TSS.fit1 <- summary(fit1)$fixed
TSS.fit2 <- summary(fit2)$fixed

abs(TSS.fit0$Estimate - TSS.fit2$Estimate[-2]) / TSS.fit2$Estimate[-2]
abs(TSS.fit1$Estimate[-2] - TSS.fit2$Estimate[-2]) / TSS.fit2$Estimate[-2]
#comparison between [no variance structure] or [agency as variance covariate] and [location as variance covariate] shows 
#  2.9% or less difference in values for global intercept or landscape predictors (items 1, 3, 4)

#------
# End of 2
#------

#compare the Leave One Out (loo) criterion for these three models; LOO is a cross-validation technique to validate the model
fit0 <- add_criterion(fit0, criterion=c("loo"))
fit1 <- add_criterion(fit1, criterion=c("loo"))
fit2 <- add_criterion(fit2, criterion=c("loo"))

loo_compare(fit0, fit1, fit2, criterion="loo")  #top one in the output gives the best model
#best fit is for fit2, the model with sigma = (~1|agency/location), and student-t distribution for data

#look at the relative amount of variability within each location, and relative amount of variability within each agency
par(mfrow=c(2,1))
boxplot(result ~ location, data=TSS.coc2)
boxplot(result ~ agency, data=TSS.coc2)
#There is variability at both the location AND the agency scale.  I think I'd prefer to use location, as agency sometimes has
#  only 1 location, sometimes 3

summary(fit1)$fixed
summary(fit2)$fixed

waic(fit0, fit1, fit2) #, fit2, fit4)

pareto_k_ids(loo(fit1), threshold=0.5)
pareto_k_ids(loo(fit2), threshold=0.5)

# PSIS diagnostic tool - look for points above 0.5, and especially above 0.7
par(mfrow=c(2,1))
plot(loo(fit1, cores=getOption("mc.cores", 1)))
plot(loo(fit2, cores=getOption("mc.cores", 1)))  #one point above 0.5, but just barely

#look at residuals vs fitted values for each model - are any markedly better than others?
par(mfrow=c(3,1), mar=c(4,4,4,2))
resid.0 <- residuals(fit0, type="ordinary")
fitted.0 <- fitted(fit0, scale="response")
plot(resid.0[,1] ~ fitted.0[,1], ylab="residuals", xlab="fitted values", main="Model 0")

resid.1 <- residuals(fit1, type="ordinary")
fitted.1 <- fitted(fit1, scale="response")
plot(resid.1[,1] ~ fitted.1[,1], ylab="residuals", xlab="fitted values", main="Model 1")

resid.2 <- residuals(fit2, type="ordinary")
fitted.2 <- fitted(fit2, scale="response")
plot(resid.2[,1] ~ fitted.2[,1], ylab="residuals", xlab="fitted values", main="Model 2")
#these plots all look remarkably similar - all with a bit of tapering at high TSS values

#Bayesian Mixed Model; agency/location as random effect; variance structure = varIdent(form = ~1|location)
TSS.brm <- fit2

#graphical posterior predictive checking. Compare observed data to simulated data from the posterior predictive distribution. 
#  This is a density plot, where the observed y values are plotted with expected values from the posterior distribution
pp_check(TSS.brm, ndraws=200)

#Look at the fit based on the grouping variable. Here are scatter-plots with the observed chemical concentrations (log scale) 
#  on the y-axis and the average model predictions (across all posterior samples) on the x-axis.
#  Red line is the 1:1 line, indicating perfect fit of model predictions to data.  Any locations where model doesn't fit?
pp_check(TSS.brm, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
#-------------------------------------------


#------
# 3. No agency as a random effect
#------

fit2.RE_loc <- brm(bf(result ~ rain + sqrt_traffic + devAge2 + (1|location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=TSS.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

TSS.fit2.RE_loc <- summary(fit2.RE_loc)$fixed

TSS.fit2.RE_loc
TSS.fit2


abs(TSS.fit2$Estimate[-2] - TSS.fit2.RE_loc$Estimate[-2]) / TSS.fit2$Estimate[-2]
#comparison between the predictors assuming random effects is LOCATION only (not nested inside agency)
#  0.2% difference in global intercept parameter, 10.5% difference in sqrt_traffic parameter, 1.1% difference in devAge2 parameter

#------
# End of 3
#------


#------
# 4. RMSE
#------

#load(file="../results/Bayesian_TSS.Rdata")

TSS_resid2 <- (residuals(TSS.brm)[, 1])^2  #1. take the residuals & square them
TSS_RMSE <- sqrt(mean(TSS_resid2))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the result
TSS_RMSE
# 0.9607041

#RSR = RMSE/SD
TSS_RSR <- TSS_RMSE / sd(TSS.coc2$result)
TSS_RSR
# 0.8066308

#------
# End of 4
#------


#save(TSS.brm, file="../results/Bayesian_TSS.Rdata")


#--------------------#
#  Total Phosphorus  #
#--------------------#

load(file="../results/Frequentist_Total Phosphorus Models.RData")
P.Form4
P.r1X
P.vf1X
#  Phosphorus lme model doesn't have too strong of a landscape predictor; try a model with only rain + summer
P.lme <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, weights=P.vf1X)

#------
# 1. Outliers
#------

plot(P.coc2$result)
#hmmmm.... maybe a low outlier??

# #identify the location of the lowest point/ potential outlier:
# which(P.coc2$result==min(P.coc2$result))  #its row 213...
# 
# #Total Phosphorus lme model summary -- one outlier removed (row 213)
# P.lme.outlierRemoved <- lme(P.Form4, data=P.coc2[-213,], method="REML", random = P.r1X, weights=P.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# summary(P.lme.outlierRemoved)
# 
# #Phosphorus lme model summary -- all data points
# P.lme <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, weights=P.vf1X)
# summary(P.lme)
# 
# #fixed coefficients for the lme model with all data points, vs the one with the outlier removed
# P.lme$coefficients$fixed
# P.lme.outlierRemoved$coefficients$fixed
# abs(P.lme$coefficients$fixed - P.lme.outlierRemoved$coefficients$fixed)/P.lme$coefficients$fixed  #ratio of difference btwn two models/original model
# #fixed effects for the model with all data points differs from the one with the outlier removed by 1.4% or less.
# #  the greatest difference is in rain fixed effect (4%) followed by summer (1.4%), sqrt_CO2_road (1.3%) and global intercept (0.1%)


P.lme_inf <- hlm_influence(P.lme, level=1)
dotplot_diag(P.lme_inf$cooksd, name = "cooks.distance", cutoff = "internal")
P.coc2[c(213, 399, 279, 352, 305),]  #top 5 observations in terms of Cook's distance
P.coc3 <- P.coc2[-c(305),]  #remove the top one, which looks particularly influential
P.lme.rmOutliers <- lme(P.Form4, data=P.coc3, method="REML", random = P.r1X, weights=P.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

coefficients(P.lme)
coefficients(P.lme.rmOutliers)

fixed.effects(P.lme)
fixed.effects(P.lme.rmOutliers)

plot(P.lme)
plot(P.lme.rmOutliers)

abs(fixed.effects(P.lme) - fixed.effects(P.lme.rmOutliers) ) / fixed.effects(P.lme)

#one potential influential outlier based on Cook's distance.  Effect on landscape predictor sqrt_CO2_road is low:
#  0% for global intercept, 0.9% for sqrt_CO2_road (but 8.6% for summer predictor)

#------
# End of 1
#------


par(mfrow=c(3,1))
P.lme0 <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme0 <- resid(object = P.lme0, type = "normalized")
plot(fitted(P.lme0), E.lme0, main="no var struct", xlab="fitted", ylab="std residuals", col="gray", pch=16)

E.lme <- resid(object = P.lme, type = "normalized")
plot(fitted(P.lme), E.lme, main="var cov = location", xlab="fitted", ylab="std residuals", col="gray", pch=16)

P.lme1 <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, weights=varIdent(form= ~1|agency), control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme1 <- resid(object = P.lme1, type = "normalized")
plot(fitted(P.lme1), E.lme1, main="var cov = agency", xlab="fitted", ylab="std residuals", col="gray", pch=16)

AIC(P.lme0, P.lme, P.lme1) #AIC best for var cov=location
#var cov = location looks best, and also has lowest AIC
#  explanation for variance covariate = agency is that some agencies have more diversity in their types of sites, so having the variance
#  covariate set to agency allows us to compensate for this (residual error E(ijk) would be expected to be higher for some 
#  agencies and lower for others)


#Bayesian Mixed Model - check various variance structures to see if the one selected by lme is the best

fit0 <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location)), 
#               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=P.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1 <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=P.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=P.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)


#------
# 2. Changing Variance Structure
#------

P.fit0 <- summary(fit0)$fixed
P.fit1 <- summary(fit1)$fixed
P.fit2 <- summary(fit2)$fixed

abs(P.fit0$Estimate - P.fit2$Estimate[-2]) / P.fit2$Estimate[-2]
abs(P.fit1$Estimate[-2] - P.fit2$Estimate[-2]) / P.fit2$Estimate[-2]
#comparison between [no variance structure] or [agency as variance covariate] and [location as variance covariate] shows 
#  0.2 to 0.3% difference in global intercept and 0.3 to 1.8% difference in sqrt_CO2 road predictor (items 1, 4)

#------
# End of 2
#------


#compare the Leave One Out (loo) criterion for these three models; LOO is a cross-validation technique to validate the model
fit2 <- add_criterion(fit2, criterion=c("loo"), moment_match=TRUE)
plot(loo(fit2, cores=getOption("mc.cores", 1)))  #two points above 0.7
pareto_k_ids(loo(fit2), threshold=0.5)  #187, 213, 414; 2 of these are > 0.7.  Try student-t distribution

#single predictor (sqrt_CO2_road) with student-t distribution
fit2.t <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=P.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#------
# 2. Changing Variance Structure
#------

#agency/location as nested random effect; no variance structure
fit0.t <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location)), 
              data=P.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1.t <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location), 
                 sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
              data=P.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit0.t <- add_criterion(fit0.t, criterion=c("loo"))
fit1.t <- add_criterion(fit1.t, criterion=c("loo"))
fit2.t <- add_criterion(fit2.t, criterion=c("loo"))

# PSIS diagnostic tool - look for points (potential influential outliers) above 0.5, and especially above 0.7
plot(loo(fit0.t, cores=getOption("mc.cores", 1)), main="no var struct")
plot(loo(fit1.t, cores=getOption("mc.cores", 1)), main="varcov=agency")
plot(loo(fit2.t, cores=getOption("mc.cores", 1)), main="varcov=location")

P.fit0.t <- summary(fit0.t)$fixed
P.fit1.t <- summary(fit1.t)$fixed
P.fit2.t <- summary(fit2.t)$fixed
round((abs(P.fit0.t$Estimate - P.fit2.t$Estimate[-2]) / P.fit2.t$Estimate[-2])*100, 2)
round((abs(P.fit1.t$Estimate[-2] - P.fit2.t$Estimate[-2]) / P.fit2.t$Estimate[-2])*100, 2)
#compare models that ALL use a student-t distribution for residual variance

# abs(P.fit2$Estimate[-2] - P.fit2.t$Estimate[-2]) / P.fit2.t$Estimate[-2]
#comparison between the predictors assuming data are from a Gaussian vs Student-t distribution 
#  0.4% difference in global intercept and 2.1% difference in sqrt_CO2 road predictor (items 1, 4)

#------
# End of 2
#------


fit2.t <- add_criterion(fit2.t, criterion=c("loo"))
plot(loo(fit2.t, cores=getOption("mc.cores", 1)))  #one point above 0.5 (below 0.7)
pareto_k_ids(loo(fit2.t), threshold=0.5)  #213
#t-distribution is a definite improvement on the fit

loo_compare(fit2, fit2.t, criterion="loo")


#no landscape predictor; variance structure = varIdent(form = ~1|location)
fit2.noPred <- brm(bf(result ~ rain + summer + (1|agency/location), 
                      sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                   data=P.coc2,
                   prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                   cores = getOption("mc.cores", 1),
                   control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit2.noPred <- add_criterion(fit2.noPred, criterion=c("loo"), moment_match=TRUE)
plot(loo(fit2.noPred, cores=getOption("mc.cores", 1)))  #2 points above 0.7
pareto_k_ids(loo(fit2.noPred), threshold=0.5)  #162, 187, 213, 299, 414
#try student-t distribution

#student-t distribution with no landscape parameters
fit2.noPred.t <- brm(bf(result ~ rain + summer + (1|agency/location),
                 sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
              data=P.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 1),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit2.noPred.t <- add_criterion(fit2.noPred.t, criterion=c("loo"))  #this didn't help -- one pareto k-value is now > 0.6
plot(loo(fit2.noPred.t, cores=getOption("mc.cores", 1)))  #2 points above 0.5; none above 0.7
pareto_k_ids(loo(fit2.noPred.t), threshold=0.5)  #187, 213
# student t-distribution definitely better for Phosphorus!

loo_compare(fit2.t, fit2.noPred.t, criterion="loo")  #top one in the output gives the best model
#best fit is for fit2.t models, where sigma = (~1|agency/location), and student-t distribution for data
#note that fit2.noPred.t is only slightly worse than fit2.t, indicating that the CO2_road predictor isn't strong
#  (but we knew that already)


#posterior predictive check of our two models
pp_check(fit2.t, ndraws=100)
pp_check(fit2.noPred.t, ndraws=100)
#both of these look very similar...

summary(fit2.t)$fixed
summary(fit2.noPred.t)$fixed

#look at residuals vs fitted values for candidate model - are any markedly better than others?
par(mfrow=c(2,1), mar=c(4,4,4,2))
resid.2t <- residuals(fit2.t, type="ordinary")
fitted.2t <- fitted(fit2.t, scale="response")
plot(resid.2t[,1] ~ fitted.2t[,1], ylab="residuals", xlab="fitted values", main="Model 2 - student's t distribution")

resid.2.noPred.t <- residuals(fit2.noPred.t, type="ordinary")
fitted.2.noPred.t <- fitted(fit2.noPred.t, scale="response")
plot(resid.2.noPred.t[,1] ~ fitted.2.noPred.t[,1], ylab="residuals", xlab="fitted values", main="Model 2 - no landscape predictors - student's t distr")
#these plots look remarkably similar - both with a bit of tapering at high P values

summary(fit2.t)
summary(fit2.noPred.t)

#Bayesian Mixed Model; sqrt_CO2_road + agency/location as random effect; variance structure = varIdent(form = ~1|location)
P.brm <- fit2.t
P.brm.alt <- fit2.noPred.t

#Look at the fit based on the grouping variable. Here are scatter-plots with the observed chemical concentrations (log scale) 
#  on the y-axis and the average model predictions (across all posterior samples) on the x-axis.
#  Red line is the 1:1 line, indicating perfect fit of model predictions to data.  Any locations where model doesn't fit?
pp_check(P.brm, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)

pp_check(fit2.noPred.t, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
#either way we go, the fit at TAC_IND isn't good.  High values are under-predicted, low values are over-predicted


#------
# 3. No agency as a random effect
#------

#what is the AIC for comparing best model with random effects = location only?
P.r2 <- formula( ~ 1 | location)  #non-nested random effects -- location only
P.lme.nonNestedRE <- lme(P.Form4, data=P.coc2, method="REML", random = P.r2, weights=varIdent(form= ~1|location), control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
AIC(P.lme, P.lme.nonNestedRE) #AIC best for var cov=location

fit2.t.RE_loc <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|location), 
                      sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                   data=P.coc2,
                   prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                   cores = getOption("mc.cores", 1),
                   control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

P.fit2.t.RE_loc <- summary(fit2.t.RE_loc)$fixed

P.fit2.t.RE_loc
P.fit2.t

abs(P.fit2.t$Estimate[-2] - P.fit2.t.RE_loc$Estimate[-2]) / P.fit2.t$Estimate[-2]
#comparison between the predictors assuming random effects is LOCATION only (not nested inside agency)
#  1.6% difference in global intercept parameter, 27% difference in sqrt_CO2_road parameter

#------
# End of 3
#------


#------
# 4. RMSE
#------

#load(file="../results/Bayesian_Phosphorus.Rdata")

P_resid2 <- (residuals(P.brm)[, 1])^2  #1. take the residuals & square them
P_RMSE <- sqrt(mean(P_resid2))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the result
P_RMSE
# 0.6073308

#RSR = RMSE/SD
P_RSR <- P_RMSE / sd(P.coc2$result)
P_RSR
# 0.6571198

#------
# End of 4
#------



#save P.brm (summer + rain + sqrt_CO2_road) and P.brm.alt (summer + rain)
#save(P.brm, P.brm.alt, file="../results/Bayesian_Phosphorus.Rdata")

#load(file="../results/Bayesian_Phosphorus.RData")


#--------------#
#  Total Zinc  #
#--------------#

load(file="../results/Frequentist_Total Zinc Models_notGreenBE.RData")
totZn.Form4
totZn.r1X
totZn.vf1X

#remove any zinc values over 800 ug/L (these should have been removed in the frequentist model stage also)
totZn.coc2 <- totZn.coc2 %>%
  filter(!result > log(800))
max(exp(totZn.coc2$result))

totZn.lme <- lme(totZn.Form4, data=totZn.coc2, method="REML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

#------
# 1. Outliers
#------

plot(totZn.coc2$result)

totZn.lme_inf <- hlm_influence(totZn.lme, level=1)
dotplot_diag(totZn.lme_inf$cooksd, name = "cooks.distance", cutoff = "internal")
totZn.coc2[c(244, 417, 184, 240, 138),]  #top 5 observations in terms of Cook's distance
#totZn.coc2[c(244, 417, 240, 184, 138),]  #for the old model with only not_greenBE, using percent trees

totZn.coc3 <- totZn.coc2[-c(244, 417, 184, 240, 138),]  #remove the top five, all of which are particularly influential
totZn.lme.rmOutliers <- lme(totZn.Form4, data=totZn.coc3, method="REML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

coefficients(totZn.lme)
coefficients(totZn.lme.rmOutliers)

fixed.effects(totZn.lme)
fixed.effects(totZn.lme.rmOutliers)

plot(totZn.lme)
plot(totZn.lme.rmOutliers)

abs(fixed.effects(totZn.lme) - fixed.effects(totZn.lme.rmOutliers) ) / fixed.effects(totZn.lme)

#these five outliers are not very influential.  Effect on landscape predictor is low:
#  0.2% for global intercept, 1.9% for not_greenBE; 2.0% for sqrt_CO2_transport; (but 9.5% for summer & 32% for interaction btwn rain:not_greenBE)

#------
# End of 1
#------


#compare models with simpler variance structures (no variance covariate, agency=var cov, location=var cov)
par(mfrow=c(3,1))
totZn.lme0 <- lme(totZn.Form4, data=totZn.coc2, method="REML", random = totZn.r1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme0 <- resid(object = totZn.lme0, type = "normalized")
plot(fitted(totZn.lme0), E.lme0, main="no var struct", xlab="fitted", ylab="std residuals", col="gray", pch=16)

E.lme <- resid(object = totZn.lme, type = "normalized")
plot(fitted(totZn.lme), E.lme, main="var cov = location", xlab="fitted", ylab="std residuals", col="gray", pch=16)

totZn.lme1 <- lme(totZn.Form4, data=totZn.coc2, method="REML", random = totZn.r1X, weights=varIdent(form= ~1|agency), control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
E.lme1 <- resid(object = totZn.lme1, type = "normalized")
plot(fitted(totZn.lme1), E.lme1, main="var cov = agency", xlab="fitted", ylab="std residuals", col="gray", pch=16)

AIC(totZn.lme0, totZn.lme, totZn.lme1) #AIC best for var cov=location; NOTE: BIC best for var cov=agency
#var cov = location looks best, and also has lowest AIC
#  explanation for variance covariate = agency is that some agencies have more diversity in their types of sites, so having the variance
#  covariate set to agency allows us to compensate for this (residual error E(ijk) would be expected to be higher for some 
#  agencies and lower for others)


#Bayesian Mixed Model - check various variance structures to see if the one selected by lme is the best
fit0 <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|agency/location)), 
            #               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1 <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20),
            save_pars=save_pars(all=TRUE)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|agency/location),                
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20),
            save_pars=save_pars(all=TRUE)
)

#------
# 2. Changing Variance Structure   ### NOTE THAT THE BEST FIT ZINC MODEL USES A STUDENT T-DISTRIBUTION RATHER THAN NORMAL!
#------

totZn.fit0 <- summary(fit0)$fixed
totZn.fit1 <- summary(fit1)$fixed
totZn.fit2 <- summary(fit2)$fixed

abs(totZn.fit0$Estimate - totZn.fit2$Estimate[-2]) / totZn.fit2$Estimate[-2]
abs(totZn.fit1$Estimate[-2] - totZn.fit2$Estimate[-2]) / totZn.fit2$Estimate[-2]
#comparison between [no variance structure] or [agency as variance covariate] and [location as variance covariate] shows 
#  0.07% to 0.3% difference in global intercept (item 1), 0.9 to 3.2% difference in not_greenBE (item 4), and 1.0 to 0.1% difference in sqrt_CO2_transport (item 5)

#------
# End of 2
#------


#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- student t distribution
fit2.t <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|agency/location), 
                 sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
              data=totZn.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 1),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)


#------
# 2. Changing Variance Structure
#------

#agency/location as nested random effect; no variance structure
fit0.t <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|agency/location)), 
              data=totZn.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1.t <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|agency/location), 
                 sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
              data=totZn.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit0.t <- add_criterion(fit0.t, criterion=c("loo"))
fit1.t <- add_criterion(fit1.t, criterion=c("loo"))
fit2.t <- add_criterion(fit2.t, criterion=c("loo"))

# PSIS diagnostic tool - look for points (potential influential outliers) above 0.5, and especially above 0.7
plot(loo(fit0.t, cores=getOption("mc.cores", 1)), main="no var struct")
plot(loo(fit1.t, cores=getOption("mc.cores", 1)), main="varcov=agency")
plot(loo(fit2.t, cores=getOption("mc.cores", 1)), main="varcov=location")

totZn.fit0.t <- summary(fit0.t)$fixed
totZn.fit1.t <- summary(fit1.t)$fixed
totZn.fit2.t <- summary(fit2.t)$fixed
round((abs(totZn.fit0.t$Estimate - totZn.fit2.t$Estimate[-2]) / totZn.fit2.t$Estimate[-2])*100, 2)
round((abs(totZn.fit1.t$Estimate[-2] - totZn.fit2.t$Estimate[-2]) / totZn.fit2.t$Estimate[-2])*100, 2)
#compare models that ALL use a student-t distribution for residual variance

# abs(totZn.fit2$Estimate[-2] - totZn.fit2.t$Estimate[-2]) / totZn.fit2.t$Estimate[-2]
#comparison between the predictors assuming data are from a Gaussian vs Student-t distribution 
#  0.0 to 0.4% difference in global intercept,  0.6% to 1.6% difference in not_greenBE, 4.9 to 4.8% difference in sqrt_CO2_transport (items 1, 4, 5)

#------
# End of 2
#------


#compare the Leave One Out (loo) criterion for these three models; LOO is a cross-validation technique to validate the model
fit1 <- add_criterion(fit1, criterion=c("loo"), moment_match=TRUE)
fit2 <- add_criterion(fit2, criterion=c("loo"), moment_match=TRUE)
fit1.t <- add_criterion(fit1, criterion=c("loo"), moment_match=TRUE)
fit2.t <- add_criterion(fit2.t, criterion=c("loo"))

loo_compare(fit1, fit1.t, fit2, fit2.t, criterion="loo")  #top one in the output gives the best model

# PSIS diagnostic tool - look for points above 0.5, and especially above 0.7
par(mfrow=c(3,1), mar=c(2,4,4,2))
plot(loo(fit1, cores=getOption("mc.cores", 1)))  #one point above 1.0
plot(loo(fit2, cores=getOption("mc.cores", 1)))  #3 points above 0.5; 1 point above 0.7
plot(loo(fit2.t, cores=getOption("mc.cores", 1)))  #1 point above 0.5; no points above 0.7
#using the student's t-distribution gives an improvement

#posterior predictive check of our two models
pp_check(fit2, ndraws=100)
pp_check(fit2.t, ndraws=100)
# use the student t-distribution; it deals with the 1 high pareto K value.

#look at the relative amount of variability within each location, and relative amount of variability within each agency
par(mfrow=c(2,1))
boxplot(result ~ location, data=totZn.coc2)
boxplot(result ~ agency, data=totZn.coc2)
#There is variability at both the location AND the agency scale.  I think I'd prefer to use location, as agency sometimes has
#  only 1 location, sometimes 3

summary(fit2.t)$fixed

#look at residuals vs fitted values for candidate model - are any markedly better than others?
par(mfrow=c(2,1), mar=c(4,4,4,2))
resid.2t <- residuals(fit2.t, type="ordinary")
fitted.2t <- fitted(fit2.t, scale="response")
plot(resid.2t[,1] ~ fitted.2t[,1], ylab="residuals", xlab="fitted values", main="Model 2t - student-T distribution")

totZn.brm <- fit2.t

#-------------------------------------------
#https://tem11010.github.io/regression_brms/

#graphical posterior predictive checking. Compare observed data to simulated data from the posterior predictive distribution. 
#  This is a density plot, where the observed y values are plotted with expected values from the posterior distribution
pp_check(totZn.brm, ndraws=200)

#Look at the fit based on the grouping variable. Here are scatter-plots with the observed chemical concentrations (log scale) 
#  on the y-axis and the average model predictions (across all posterior samples) on the x-axis.
#  Red line is the 1:1 line, indicating perfect fit of model predictions to data.  Any locations where model doesn't fit?
pp_check(totZn.brm, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
#-------------------------------------------


#------
# 3. No agency as a random effect
#------

fit2.t.RE_loc <- brm(bf(result ~ rain + summer + not_greenBE + sqrt_CO2_transport + rain:not_greenBE + (1|location), 
                        sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                     data=totZn.coc2,
                     prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                     cores = getOption("mc.cores", 1),
                     control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

totZn.fit2.t.RE_loc <- summary(fit2.t.RE_loc)$fixed

totZn.fit2.t.RE_loc
totZn.fit2.t

abs(totZn.fit2.t$Estimate[-2] - totZn.fit2.t.RE_loc$Estimate[-2]) / totZn.fit2.t$Estimate[-2]
#comparison between the predictors assuming random effects is LOCATION only (not nested inside agency)
#  0.3% difference in global intercept parameter, 0.3% difference in not_greenBE, 0.8% difference in sqrt_CO2_transport

#------
# End of 3
#------


#------
# 4. RMSE
#------

#load(file="../results/Bayesian_TotalZinc.Rdata")

totZn_resid2 <- (residuals(totZn.brm)[, 1])^2  #1. take the residuals & square them
totZn_RMSE <- sqrt(mean(totZn_resid2))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the result
totZn_RMSE
# 0.5160326

#RSR = RMSE/SD
totZn_RSR <- totZn_RMSE / sd(totZn.coc2$result)
totZn_RSR
# 0.5286009

#------
# End of 4
#------


#save(totZn.brm, file="../results/Bayesian_TotalZinc.Rdata")
#load(file="../results/Bayesian_TotalZinc_not_greenBE.Rdata")


#---------------------------#
#  Total Kjeldahl Nitrogen  #
#---------------------------#

load(file="../results/Frequentist_Total Kjeldahl Nitrogen Models_censtat.RData")
TKN.Form4
TKN.r1X
TKN.vf1X
TKN.lme <- lme(TKN.Form4, data=TKN.coc2, method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))


#------
# 1. Outliers
#------

#check for outliers
plot(TKN.coc2$result)   #hmmm.... no real outliers here, but lets see if removing the top point makes a difference
TKN.coc2$result[order(TKN.coc2$result)]
hist(TKN.coc2$result)

#identify the location of the highest point/ potential outlier:
which(TKN.coc2$result==max(TKN.coc2$result))  #its row 265...

#Total Kjeldahl Nitrogen lme model summary -- one outlier removed (row 265)
TKN.lme.outlierRemoved <- lme(TKN.Form4, data=TKN.coc2[-265,], method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TKN.lme.outlierRemoved)

#Total Kjeldahl Nitrogen lme model summary -- all data points
TKN.lme <- lme(TKN.Form4, data=TKN.coc2, method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TKN.lme)

#fixed coefficients for the lme model with all data points, vs the one with the outlier removed
TKN.lme$coefficients$fixed
TKN.lme.outlierRemoved$coefficients$fixed
abs(TKN.lme$coefficients$fixed - TKN.lme.outlierRemoved$coefficients$fixed)/TKN.lme$coefficients$fixed  #ratio of difference btwn two models/original model
#fixed effects for the model with all data points differs from the one with the outlier removed by 2% or less.
#  the greatest difference is in sqrt_traffic fixed effect (2%) followed by devAge2 (0.5%) and global intercept (0.05%)



plot(TKN.coc2$result)
#two possible outliers?

TKN.lme_inf <- hlm_influence(TKN.lme, level=1)
dotplot_diag(TKN.lme_inf$cooksd, name = "cooks.distance", cutoff = "internal")
TKN.coc2[c(369, 32, 194, 400, 31),]  #top 5 observations in terms of Cook's distance

min(TKN.coc2$result)
max(TKN.coc2$result)

TKN.coc3 <- TKN.coc2[-c(400, 31),]  #remove the top two, which are particularly influential
TKN.lme.rmOutliers <- lme(TKN.Form4, data=TKN.coc3, method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

coefficients(TKN.lme)
coefficients(TKN.lme.rmOutliers)

fixed.effects(TKN.lme)
fixed.effects(TKN.lme.rmOutliers)

plot(TKN.lme)
plot(TKN.lme.rmOutliers)

abs(fixed.effects(TKN.lme) - fixed.effects(TKN.lme.rmOutliers) ) / fixed.effects(TKN.lme)

#two potential influential outliers based on Cook's distance.  Effect on landscape predictors are:
#  0% for global intercept, 5.8% for sqrt_traffic predictor, and 0.4% for devAge2
# NOTE: neither of the two potentially influential observations are the min or max!  One is close to the max, other is somewhere in the middle and 
#       was generated as a replacement for censored data.  I don't think there is compelling reason to eliminate them.

#------
# End of 1
#------


#look at the relative amount of variability within each location, and relative amount of variability within each agency
par(mfrow=c(2,1))
boxplot(result ~ location, data=TKN.coc2)
boxplot(result ~ agency, data=TKN.coc2)
#There is variability at both the location AND the agency scale.  I'd prefer to use location, as agency sometimes has
#  only 1 location, sometimes 3

#Try Bayesian censored model methods
#add a column to coc2, which indicates whether there is no censoring "none", or left-censoring "left" of data points
TKN.coc2 <- TKN.coc2 %>% 
  mutate(cen1 = if_else(cen==TRUE, "left", "none"))



# #Bayesian Mixed Model using the ROS data that were also used for frequentist lme models; variance structure = varIdent(form = ~1|location)
# fit2.ros <- brm(bf(result ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
#                   sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
#                 data=TKN.coc2,
#                 prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
#                 control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
# )

#Bayesian Mixed Model using brms built-in censored data function; variance structure = varIdent(form = ~1|location)
fit2.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)
# 
# #Bayesian Mixed Model using the ROS data that were also used for frequentist lme models; variance structure = varIdent(form = ~1|agency)
# fit1.ros <- brm(bf(result ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
#                    sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
#                 data=TKN.coc2,
#                 prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
#                 control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
# )

#Bayesian Mixed Model using brms built-in censored data function; variance structure = varIdent(form = ~1|agency)
fit1.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model using brms built-in censored data function; variance structure = varIdent(form = ~1|agency)
fit0.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location)), 
                   #sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)


#------
# 2. Changing Variance Structure
#------

TKN.fit0.cen <- summary(fit0.cen)$fixed
TKN.fit1.cen <- summary(fit1.cen)$fixed   #best model is fit1.t!
TKN.fit2.cen <- summary(fit2.cen)$fixed

abs(TKN.fit0.cen$Estimate - TKN.fit1.cen$Estimate[-2]) / TKN.fit1.cen$Estimate[-2]
abs(TKN.fit2.cen$Estimate[-2] - TKN.fit1.cen$Estimate[-2]) / TKN.fit1.cen$Estimate[-2]
#comparison between [no variance structure] or [location as variance covariate] and [agency as variance covariate] shows 
#  0.01 to 0.4% difference in global intercept, 4 to 8% difference in sqrt_traffic, & 0.6 to 1.5% difference in devAge2 (items 1, 4,5)

#------
# End of 2
#------


#check the pareto k values for the brms censored model
fit2.cen <- add_criterion(fit2.cen, criterion=c("loo"))#, moment_match=TRUE)
fit2.ros <- add_criterion(fit2.ros, criterion=c("loo"))#, moment_match=TRUE)
fit1.cen <- add_criterion(fit1.cen, criterion=c("loo"))#, moment_match=TRUE)
fit1.ros <- add_criterion(fit1.ros, criterion=c("loo"))#, moment_match=TRUE)
loo_compare(fit2.cen, fit2.ros, fit1.cen, fit1.ros, criterion="loo")
fit2.cen <- add_criterion(fit2.cen, criterion=c("loo"), moment_match=TRUE)
fit1.cen <- add_criterion(fit1.cen, criterion=c("loo"), moment_match=TRUE)
loo_compare(fit2.cen, fit2.ros, fit1.cen, fit1.ros, criterion="loo")


par(mfrow=c(2,2), mar=c(4,4,4,2))
plot(loo(fit2.cen, cores=getOption("mc.cores", 1)), main="fit2.cen")  #one point above 0.7, 3 points above 1.0
plot(loo(fit2.ros, cores=getOption("mc.cores", 1)), main="fit2.ros")  #one point above 0.7, 3 points above 1.0
plot(loo(fit1.cen, cores=getOption("mc.cores", 1)), main="fit1.cen")  #one point above 0.7, 3 points above 1.0
plot(loo(fit1.ros, cores=getOption("mc.cores", 1)), main="fit1.ros")  #one point above 0.7, 3 points above 1.0
#several pareto k-values for the two .cen models are really high - try a student-t distribution model.
#  note that the ros models don't have any high pareto k-values.  I'd prefer to stick with the Bayesian censored
#  methods that are built-in, though.


#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency) -- student t distribution
fit1.t.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                 sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
              data=TKN.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 1),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- student t distribution
fit2.t.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                     sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                  data=TKN.coc2,
                  family=student,
                  prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                  cores = getOption("mc.cores", 2),
                  control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model using the ROS data that were also used for frequentist lme models; variance structure = varIdent(form = ~1|agency)
fit1.t.ros <- brm(bf(result ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                data=TKN.coc2,
                family=student,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                cores = getOption("mc.cores", 2),
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model using the ROS data that were also used for frequentist lme models; variance structure = varIdent(form = ~1|location)
fit2.t.ros <- brm(bf(result ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                data=TKN.coc2,
                family=student,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                cores = getOption("mc.cores", 2),
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#------
# 2. Changing Variance Structure
#------

#agency/location as nested random effect; no variance structure
fit0.t.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location)), 
              data=TKN.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit0.t.cen <- add_criterion(fit0.t.cen, criterion=c("loo"))
fit1.t.cen <- add_criterion(fit1.t.cen, criterion=c("loo"))
fit2.t.cen <- add_criterion(fit2.t.cen, criterion=c("loo"))

# PSIS diagnostic tool - look for points (potential influential outliers) above 0.5, and especially above 0.7
plot(loo(fit0.t.cen, cores=getOption("mc.cores", 1)), main="no var struct")
plot(loo(fit1.t.cen, cores=getOption("mc.cores", 1)), main="varcov=agency")
plot(loo(fit2.t.cen, cores=getOption("mc.cores", 1)), main="varcov=location")

TKN.fit0.t.cen <- summary(fit0.t.cen)$fixed
TKN.fit1.t.cen <- summary(fit1.t.cen)$fixed
TKN.fit2.t.cen <- summary(fit2.t.cen)$fixed
round((abs(TKN.fit0.t.cen$Estimate - TKN.fit1.t.cen$Estimate[-2]) / TKN.fit1.t.cen$Estimate[-2])*100, 2)
round((abs(TKN.fit1.t.cen$Estimate[-2] - TKN.fit2.t.cen$Estimate[-2]) / TKN.fit1.t.cen$Estimate[-2])*100, 2)
#compare models that ALL use a student-t distribution for residual variance

# abs(TKN.fit1.cen$Estimate[-2] - TKN.fit1.t.cen$Estimate[-2]) / TKN.fit1.t.cen$Estimate[-2]
#comparison between the predictors assuming data are from a Gaussian vs Student-t distribution 
#  0.5% difference in global intercept,  13.5% difference in sqrt_traffic, and 0.8% difference in devAge2 (items 1, 4, 5)

#this will be important in #3 below, since using 1/agency as a variance covariate wouldn't make sense if it wasn't a nested RE 
TKN.fit2.t.cen <- summary(fit2.t.cen)$fixed


#------
# End of 2
#------




#check the pareto k values for the brms censored model
fit2.t.cen <- add_criterion(fit2.t.cen, criterion=c("loo")) #, moment_match=TRUE)
fit2.t.ros <- add_criterion(fit2.t.ros, criterion=c("loo")) #, moment_match=TRUE)
fit1.t.cen <- add_criterion(fit1.t.cen, criterion=c("loo")) #, moment_match=TRUE)
fit1.t.ros <- add_criterion(fit1.t.ros, criterion=c("loo")) #, moment_match=TRUE)
loo_compare(fit2.t.cen, fit2.t.ros, fit1.t.cen, fit1.t.ros, criterion="loo")

par(mfrow=c(2,2), mar=c(4,4,4,2))
plot(loo(fit2.t.cen, cores=getOption("mc.cores", 1)), main="fit2.t.cen")  #one point above 0.7, 3 points above 1.0
plot(loo(fit2.t.ros, cores=getOption("mc.cores", 1)), main="fit2.t.ros")  #one point above 0.7, 3 points above 1.0
plot(loo(fit1.t.cen, cores=getOption("mc.cores", 1)), main="fit1.t.cen")  #one point above 0.7, 3 points above 1.0
plot(loo(fit1.t.ros, cores=getOption("mc.cores", 1)), main="fit1.t.ros")  #one point above 0.7, 3 points above 1.0
#student-t distribution makes a big difference!

loo_compare(fit1.cen, fit1.t.cen, fit2.cen, fit2.t.cen, criterion="loo")
loo_compare(fit1.ros, fit1.t.ros, fit2.ros, fit2.t.ros, criterion="loo")
#clearly, model with student t-distribution AND variance covariate=agency is the best choice!

loo_compare(fit1.t.cen, fit1.t.ros)

#posterior predictive check of our two models
pp_check(fit1.t.cen, ndraws=100)
pp_check(fit1.t.ros, ndraws=100)

#while both models would be fine, choose the brms cen model (fit1.t.cen); this uses censored
#  methods within the Bayesian context, making it perhaps more suitable than the ROS method
#  that was used outside of the Bayesian context.

#look at residuals vs fitted values for candidate model - are any markedly better than others?
par(mfrow=c(2,1), mar=c(4,4,4,2))
resid.1t.cen <- residuals(fit1.t.cen, type="ordinary")
fitted.1t.cen <- fitted(fit1.t.cen, scale="response")
plot(resid.1t.cen[,1] ~ fitted.1t.cen[,1], ylab="residuals", xlab="fitted values", main="Model 1 - t-distr censored model")

TKN.brm <- fit1.t.cen   #this is the model we are choosing -- using censored methods within the Bayesian context
TKN.brm.ROS <- fit1.t.ros

#Look at the fit based on the grouping variable. Here are scatter-plots with the observed chemical concentrations (log scale) 
#  on the y-axis and the average model predictions (across all posterior samples) on the x-axis.
#  Red line is the 1:1 line, indicating perfect fit of model predictions to data.  Any locations where model doesn't fit?
pp_check(TKN.brm, type = "scatter_avg_grouped", group = "location") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
#-------------------------------------------


#------
# 3. No agency as a random effect
#------

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency) -- student t distribution
fit2.t.cen.RE_loc <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|location), 
                     sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                  data=TKN.coc2,
                  family=student,
                  prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                  cores = getOption("mc.cores", 1),
                  control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

TKN.fit2.t.cen.RE_loc <- summary(fit2.t.cen.RE_loc)$fixed

TKN.fit2.t.cen.RE_loc
TKN.fit2.t.cen

abs(TKN.fit2.t.cen$Estimate[-2] - TKN.fit2.t.cen.RE_loc$Estimate[-2]) / TKN.fit2.t.cen$Estimate[-2]
#comparison between the predictors assuming random effects is LOCATION only (not nested inside agency)
#  0.1% difference in global intercept parameter, 5% difference in sqrt_traffic, 5% difference in devAge2

### NOTE: this comparison was run for the SECOND-best model, which uses location as a variance covariate.  
#         The model that uses agency as a variance covariate is not appropriate for comparison to a model
#         where agency was dropped as the random effect.

#.... but... just to be thorough (comparing two models that should not be compared...)
fit1.t.cen.RE_loc <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|location), 
                            sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                         data=TKN.coc2,
                         family=student,
                         prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                         cores = getOption("mc.cores", 1),
                         control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

TKN.fit1.t.cen.RE_loc <- summary(fit1.t.cen.RE_loc)$fixed

abs(TKN.fit1.t.cen$Estimate[-2] - TKN.fit1.t.cen.RE_loc$Estimate[-2]) / TKN.fit1.t.cen$Estimate[-2]
#comparison between the predictors assuming random effects is LOCATION only (not nested inside agency)
#  0.2% difference in global intercept parameter, 5% difference in sqrt_traffic, 5% difference in devAge2 -- 
#  so, pretty much the same as the comparison between fit2.t.cen and fit2.t.cen.RE_loc


#------
# End of 3
#------


#------
# 4. RMSE
#------

#load(file="../results/Bayesian_TotalKjeldahlNitrogen.Rdata")

TKN_resid2 <- (residuals(TKN.brm)[, 1])^2  #1. take the residuals & square them
TKN_RMSE <- sqrt(mean(TKN_resid2))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the resulting mean
TKN_RMSE
# 0.6851323

#RSR = RMSE/SD
TKN_RSR <- TKN_RMSE / sd(TKN.coc2$result)
TKN_RSR
# 0.7862108

#now remove the two ND locations from Tacoma, which were unusually low

#add a column to coc2, which indicates whether there is no censoring "none", or left-censoring "left" of data points
TKN.coc2 <- TKN.coc2 %>% 
  mutate(cen1 = if_else(cen==TRUE, "left", "none"))

#best-fit TKN model: agency/location as nested random effect; variance structure = varIdent(form = ~1|agency) -- student t distribution
fit1.t.cen.omit2 <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                     sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                  data=TKN.coc2[-which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma"),],
                  family=student,
                  prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                  cores = getOption("mc.cores", 1),
                  control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)
TKN.brm.omit <- fit1.t.cen.omit2

# TKN.coc2[which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma"),]
# which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma")

TKN_resid2.omit <- (residuals(TKN.brm.omit)[, 1])^2  #1. take the residuals & square them
TKN_RMSE.omit <- sqrt(mean(TKN_resid2.omit))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the resulting mean
TKN_RMSE.omit
# 0.6468274

#RSR = RMSE/SD
TKN_RSR.omit <- TKN_RMSE.omit / sd(TKN.coc2$result[-which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma")])
TKN_RSR.omit
# 0.7438975

# aa <- residuals(TKN.brm)[-which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma"),]
# TKN_resid2.omit <- (residuals(TKN.brm)[-which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma"), 1])^2  #1. take the residuals & square them
# TKN_RMSE.omit <- sqrt(mean(TKN_resid2.omit))  #2 & 3. take the mean of the squared residuals, then take the sqrt of the resulting mean
# TKN_RMSE.omit
# # 0.6479409
# 
# #RSR = RMSE/SD
# TKN_RSR.omit <- TKN_RMSE.omit / sd(TKN.coc2$result[-which(TKN.coc2$cen==TRUE & TKN.coc2$agency=="Tacoma")])
# TKN_RSR.omit
# # 0.745178
 

#------
# End of 4
#------

#save(TKN.brm, TKN.brm.ROS, file="../results/Bayesian_TotalKjeldahlNitrogen.Rdata")


#---------------------------------------------------#
#  Save Model Results Used in Sensitivity Analysis  #
#---------------------------------------------------#

save(Cu.fit0, Cu.fit1, Cu.fit2, Cu.fit0.t, Cu.fit1.t, Cu.fit2.t, Cu.fit2.t.RE_loc,
     TSS.fit0, TSS.fit1, TSS.fit2, TSS.fit2.RE_loc,
     P.fit0, P.fit1, P.fit2, P.fit0.t, P.fit1.t, P.fit2.t, P.fit2.t.RE_loc,
     totZn.fit0, totZn.fit1, totZn.fit2, totZn.fit0.t, totZn.fit1.t, totZn.fit2.t, totZn.fit2.t.RE_loc,
     TKN.fit0.cen, TKN.fit1.cen, TKN.fit2.cen, TKN.fit0.t.cen, TKN.fit1.t.cen, TKN.fit2.t.cen, TKN.fit1.t.cen.RE_loc, TKN.fit2.t.cen.RE_loc,
     file="../results/Bayesian_Model_Results_Sensitivity_Analysis.Rdata")

#load(file="../results/Bayesian_Model_Results_Sensitivity_Analysis.Rdata")


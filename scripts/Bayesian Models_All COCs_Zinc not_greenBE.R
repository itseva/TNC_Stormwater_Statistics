# This script opens the RData object for each COC's lme model, and translates the lme model
#  to a Bayesian framework.  An RData object is generated and saved for each Bayesian model, 
#  which can then be used for obtaining prediction intervals and plotting outputs.

# This script uses v9 lme model results (nested random effects: agency/location) to generate 
#  the Bayesian outputs.

# Author: Eva Dusek Jennings
# Revised: July 14, 2022
#          Apr 28, 2025 - change greenery to greenery_bareEarth
#          Oct 20, 2025 - Zinc model = fxn(not_greenBE)
#          Dec 19, 2025 - Zinc model -- update greenery to use tnc_trees; model is not_greenBE + sqrt_CO2_transport
#---------------------------------------------------------------------------------------

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

#----------------#
#  Total Copper  #
#----------------#

load(file="../results/Frequentist_Copper Models.RData")
Cu.Form4  #lme model equation
Cu.r1X  #random effect in lme model
Cu.vf1X  #variance structure for lme model

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

save(Cu.brm, file="../results/Bayesian_Copper.Rdata")



#--------------------------#
#  Total Suspended Solids  #
#--------------------------#


load(file="../results/Frequentist_TSS Models.RData")
TSS.Form4
TSS.r1X
TSS.vf1X

#TSS lme model summary
TSS.lme <- lme(TSS.Form4, data=TSS.coc2, method="REML", random = TSS.r1X, weights=TSS.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TSS.lme)

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
plot(resid.1[,1] ~ fitted.1[,1], ylab="residuals", xlab="fitted values", main="Model 1.t")

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

save(TSS.brm, file="../results/Bayesian_TSS.Rdata")


#--------------------#
#  Total Phosphorus  #
#--------------------#

load(file="../results/Frequentist_Total Phosphorus Models.RData")
P.Form4
P.r1X
P.vf1X
#  Phosphorus lme model doesn't have too strong of a landscape predictor; try a model with only rain + summer

#Phosphorus lme model summary
P.lme <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, weights=P.vf1X)
summary(P.lme)

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
#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=P.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

fit0 <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location)), 
#               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=P.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)





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

#save P.brm (summer + rain + sqrt_CO2_road) and P.brm.alt (summer + rain)
save(P.brm, P.brm.alt, file="../results/Bayesian_Phosphorus.Rdata")

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

#Total Zinc lme model summary
totZn.lme <- lme(totZn.Form4, data=totZn.coc2, method="REML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(totZn.lme)

# totZn.lme <- lme(totZn.Form4, data=totZn.coc2, method="ML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# 
# totZn.lme <- lme(totZn.Form4, data=totZn.coc2, method="ML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# M4.sub1 <- update(totZn.lme, . ~ . - rain:not_greenBE) #significant (p=0.0076)
# AIC(M4.sub1)
# 
# M4.2 <-  lme(result ~ rain + summer + greenery + rain:greenery, 
#              data=totZn.coc2, method="ML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# summary(M4.2)
# 
# M4.3 <-  lme(result ~ rain + summer + sqrt_traffic + paved + rain:paved, 
#              data=totZn.coc2, method="ML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# summary(M4.3)
# 
# M4.4 <-  lme(result ~ rain + summer + sqrt_CO2_almostTotal + paved + rain:paved, 
#              data=totZn.coc2, method="ML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
# summary(M4.4)
# 
# AIC(totZn.lme, M4.2, M4.3, M4.4)


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
#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency)
fit1 <- brm(bf(result ~ rain + summer + sqrt_CO2_transport + not_greenBE + rain:not_greenBE + (1|agency/location), 
               sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20),
            save_pars=save_pars(all=TRUE)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ rain + summer + sqrt_CO2_transport + not_greenBE + rain:not_greenBE + (1|agency/location),                
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=totZn.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20),
            save_pars=save_pars(all=TRUE)
)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- student t distribution
fit2.t <- brm(bf(result ~ rain + summer + sqrt_CO2_transport + not_greenBE + rain:not_greenBE + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=totZn.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#compare the Leave One Out (loo) criterion for these three models; LOO is a cross-validation technique to validate the model
fit1 <- add_criterion(fit1, criterion=c("loo"), moment_match=TRUE)
fit2 <- add_criterion(fit2, criterion=c("loo"), moment_match=TRUE)
fit2.t <- add_criterion(fit2.t, criterion=c("loo")) #, moment_match=TRUE)

#loo_compare(fit2, fit2.t, criterion="loo")  #top one in the output gives the best model
loo_compare(fit1, fit2, fit2.t, criterion="loo")  #top one in the output gives the best model

# PSIS diagnostic tool - look for points above 0.5, and especially above 0.7
par(mfrow=c(3,1), mar=c(2,4.5,4,2))
plot(loo(fit1, cores=getOption("mc.cores", 1)))  #one point at around 0.5; 1 point above 0.8
plot(loo(fit2, cores=getOption("mc.cores", 1)))  #several points above 0.5; 1 points above 0.7
plot(loo(fit2.t, cores=getOption("mc.cores", 1)))  #1 point above 0.6
#using the student's t-distribution gives an improvement

#posterior predictive check of our two models
pp_check(fit2, ndraws=100)
pp_check(fit2.t, ndraws=100)
# use the student t-distribution; it deals with the high pareto K values.

#look at the relative amount of variability within each location, and relative amount of variability within each agency
par(mfrow=c(2,1))
boxplot(result ~ location, data=totZn.coc2)
boxplot(result ~ agency, data=totZn.coc2)
#There is variability at the location scale (mostly at KIC_HDR), which is driving variability at the agency scale.  I'd prefer to use location, 
#  as agency sometimes has only 1 location, sometimes 3

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

save(totZn.brm, file="../results/Bayesian_TotalZinc_not_greenBE.Rdata")


#---------------------------#
#  Total Kjeldahl Nitrogen  #
#---------------------------#

load(file="../results/Frequentist_Total Kjeldahl Nitrogen Models_censtat.RData")
TKN.Form4
TKN.r1X
TKN.vf1X

#Total Kjeldahl Nitrogen lme model summary
TKN.lme <- lme(TKN.Form4, data=TKN.coc2, method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TKN.lme)

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



#Bayesian Mixed Model using the ROS data that were also used for frequentist lme models; variance structure = varIdent(form = ~1|location)
fit2.ros <- brm(bf(result ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                  sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model using brms built-in censored data function; variance structure = varIdent(form = ~1|location)
fit2.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model using the ROS data that were also used for frequentist lme models; variance structure = varIdent(form = ~1|agency)
fit1.ros <- brm(bf(result ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model using brms built-in censored data function; variance structure = varIdent(form = ~1|agency)
fit1.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                   sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
                data=TKN.coc2,
                prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
                control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

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


save(TKN.brm, TKN.brm.ROS, file="../results/Bayesian_TotalKjeldahlNitrogen.Rdata")






# This script re-runs the best-fit Bayesian models using the S8C naming convention for Tacoma sites,
#  where TAC_HDR = OF237B, TAC_COM = OF235, and TAC_IND = OF245.

# This script uses v9 lme model results (nested random effects: agency/location) to generate 
#  the Bayesian outputs.

# Author: Eva Dusek Jennings
# Revised: Mar 24, 2025
#          Apr 28, 2025 - add greenery_bareEarth
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

Cu.coc2 <- Cu.coc2 %>%
  mutate(location = case_when(location=="TAC_COM" ~ "OF235",
                              location=="TAC_IND" ~ "OF245",
                              location=="TAC_HDR" ~ "OF237B",
                              TRUE ~ location))

#Copper lme model summary
Cu.lme <- lme(Cu.Form4, data=Cu.coc2, method="REML", random = Cu.r1X, weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(Cu.lme)

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

Cu.brm <- fit2.t  #best Bayesian model for copper, so far

#--------------------------#
#  Total Suspended Solids  #
#--------------------------#


load(file="../results/Frequentist_TSS Models.RData")
TSS.Form4
TSS.r1X
TSS.vf1X

TSS.coc2 <- TSS.coc2 %>%
  mutate(location = case_when(location=="TAC_COM" ~ "OF235",
                              location=="TAC_IND" ~ "OF245",
                              location=="TAC_HDR" ~ "OF237B",
                              TRUE ~ location))

#TSS lme model summary
TSS.lme <- lme(TSS.Form4, data=TSS.coc2, method="REML", random = TSS.r1X, weights=TSS.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TSS.lme)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- best model from LME 
fit2 <- brm(bf(result ~ rain + sqrt_traffic + devAge2 + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=TSS.coc2,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model; agency/location as random effect; variance structure = varIdent(form = ~1|location)
TSS.brm <- fit2


#--------------------#
#  Total Phosphorus  #
#--------------------#

load(file="../results/Frequentist_Total Phosphorus Models.RData")
P.Form4
P.r1X
P.vf1X
#  Phosphorus lme model doesn't have too strong of a landscape predictor; try a model with only rain + summer

P.coc2 <- P.coc2 %>%
  mutate(location = case_when(location=="TAC_COM" ~ "OF235",
                              location=="TAC_IND" ~ "OF245",
                              location=="TAC_HDR" ~ "OF237B",
                              TRUE ~ location))

#Phosphorus lme model summary
P.lme <- lme(P.Form4, data=P.coc2, method="REML", random = P.r1X, weights=P.vf1X)
summary(P.lme)

#single predictor (sqrt_CO2_road) with student-t distribution
fit2.t <- brm(bf(result ~ rain + summer + sqrt_CO2_road + (1|agency/location), 
               sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=P.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 1),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

#Bayesian Mixed Model; sqrt_CO2_road + agency/location as random effect; variance structure = varIdent(form = ~1|location)
P.brm <- fit2.t


#--------------#
#  Total Zinc  #
#--------------#

load(file="../results/Frequentist_Total Zinc Models_notGreenBE.RData")
totZn.Form4
totZn.r1X
totZn.vf1X

totZn.coc2 <- totZn.coc2 %>%
  mutate(location = case_when(location=="TAC_COM" ~ "OF235",
                              location=="TAC_IND" ~ "OF245",
                              location=="TAC_HDR" ~ "OF237B",
                              TRUE ~ location))

#Total Zinc lme model summary
totZn.lme <- lme(totZn.Form4, data=totZn.coc2, method="REML", random = totZn.r1X, weights=totZn.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(totZn.lme)

#agency/location as nested random effect; variance structure = varIdent(form = ~1|location) -- student t distribution
fit2.t <- brm(bf(result ~ rain + summer + sqrt_CO2_transport + not_greenBE + rain:not_greenBE + (1|agency/location), 
                                  sigma ~ (1|location)),  #equivalent to varIdent(form= ~1|location)
            data=totZn.coc2,
            family=student,
            prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
            cores = getOption("mc.cores", 2),
            control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

totZn.brm <- fit2.t


#---------------------------#
#  Total Kjeldahl Nitrogen  #
#---------------------------#

load(file="../results/Frequentist_Total Kjeldahl Nitrogen Models_censtat.RData")
TKN.Form4
TKN.r1X
TKN.vf1X

TKN.coc2 <- TKN.coc2 %>%
  mutate(location = case_when(location=="TAC_COM" ~ "OF235",
                              location=="TAC_IND" ~ "OF245",
                              location=="TAC_HDR" ~ "OF237B",
                              TRUE ~ location))

#Total Kjeldahl Nitrogen lme model summary
TKN.lme <- lme(TKN.Form4, data=TKN.coc2, method="REML", random = TKN.r1X, weights=TKN.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
summary(TKN.lme)

#Try Bayesian censored model methods
#add a column to coc2, which indicates whether there is no censoring "none", or left-censoring "left" of data points
TKN.coc2 <- TKN.coc2 %>% 
  mutate(cen1 = if_else(cen==TRUE, "left", "none"))

#agency/location as nested random effect; variance structure = varIdent(form = ~1|agency) -- student t distribution
fit1.t.cen <- brm(bf(log(oconc) | cens(cen1) ~ rain + summer + sqrt_traffic + devAge2 + (1|agency/location), 
                 sigma ~ (1|agency)),  #equivalent to varIdent(form= ~1|agency)
              data=TKN.coc2,
              family=student,
              prior = c(set_prior("normal(0,10)", class="b")),   #non-informative priors on all predictors
              cores = getOption("mc.cores", 1),
              control = list(adapt_delta = 0.999, stepsize = 0.1, max_treedepth = 20)
)

TKN.brm <- fit1.t.cen   #this is the model we are choosing -- using censored methods within the Bayesian context


#---------------------------------------------#
#  Save Model Validation Runs of BRMS models  #
#---------------------------------------------#

# totZn.brm.new <- totZn.brm
# 
# load(file="../results/Best Fit Bayesian Models Using Model Validation Location Names_Zinc_notGreenBE.RData")
# 
# totZn.brm <- totZn.brm.new

save(Cu.brm, TSS.brm, P.brm, totZn.brm, TKN.brm, 
     Cu.coc2, TSS.coc2, P.coc2, totZn.coc2, TKN.coc2,
     file="../results/Best Fit Bayesian Models Using Model Validation Location Names_Zinc_notGreenBE.RData")



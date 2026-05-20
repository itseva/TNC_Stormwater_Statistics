# This script contains functions used in the Model Validation.R script

# Author: Eva Dusek Jennings
# Date: April 19, 2024
#---------------------------------------------------------------------

library(brms)


#--------------------------------------#
#  Data Prep & Manipulation Functions  #
#--------------------------------------#


#provide prediction intervals for a location with particular predictor values.  Prediction intervals predict in what range a future
#   individual observation will fall
interpPI <- function(LCI=0.1, UCI=0.9, my.brm, my.coc2, wr_preds) {
  
  # #TESTING!
  # LCI <- 0.1
  # UCI <- 0.9
  # my.brm <- Cu.brm
  # my.coc2 <- Cu.coc2
  # wr_preds <- comp_preds[, c("Location", "sqrt_traffic", "devAge2")]
  # 
  # 
  # wr_preds <- filter(wr_preds$Location %in% )
  
  #  preds <- colnames(wr_preds)
  
  #take 47 samples total (10% of all S8 Data we have) to represent feasible rain or rain/summer options
  #   42 samples from non-summer, 5 from summer; this matches the summer/ non-summer ratio of 11% summer.
  #   NOTE: for the end result, it won't matter whether summer is a predictor or not, as we are drawing the same proportion of 
  #         samples from summertime as are in the data; this should approximately happen during sampling, regardless.
  my.sample0 <- my.coc2[which(my.coc2$summer==0)[sample(length(which(my.coc2$summer==0)), 42)], c("rain", "summer")]
  my.sample1 <- my.coc2[which(my.coc2$summer==1)[sample(length(which(my.coc2$summer==1)), 5)], c("rain", "summer")]
  my.sample <- rbind(my.sample0, my.sample1)
  
  #  my.summary <- NULL  #empty object for summarized prediction intervals
  
  #generate simulated data
  newdata0 <- expand_grid(#location=NA, 
    rain=my.sample0$rain, 
    summer=0, 
    wr_preds)
  newdata1 <- expand_grid(#location=NA, 
    rain=my.sample1$rain, 
    summer=1, 
    wr_preds)
  newdata <- rbind(newdata0, newdata1)
  
  #prediction interval, which predicts the range in which a future individual observation will fall; these include three levels of 
  #   uncertainty: uncertainty in fixed coefficients, uncertainty in variance parameters for grouping factors, and residual variance
  #   re_formula=NULL indicates that all group-level effects should be considered in the prediction
  #   allow_new_levels=TRUE indicates that new group levels (agency) are fine
  my.site <- posterior_predict(my.brm, newdata = newdata, re_formula = NULL, allow_new_levels=TRUE) %>%
    t() %>%
    as_tibble() %>%
    bind_cols(newdata, .) %>%
    pivot_longer(
      cols=starts_with("V"),
      names_to="pars",
      values_to="est"
    )
  
  #summarized data for each type of site (specific values of landscape predictors)
  #  this summarizes what might be expected to occur in samples at a site with particular landscape predictor traits 
  #  over a representative set of season (summer/not summer) and rainfall value associated with that season
  my.summary <- my.site %>%
    group_by(across(all_of(names(wr_preds)))) %>%  #group by the location, and include landscape predictors for this COC
    summarise(upperCI = quantile(est, UCI),
              lowerCI = quantile(est, LCI),
              med=quantile(est, 0.5))
  rm(my.site, newdata, newdata0, newdata1)
  return(my.summary)
}


# #provide prediction intervals for a location with particular predictor values.  Prediction intervals predict in what range a future
# #   individual observation will fall
# interpPI <- function(LCI=0.1, UCI=0.9, my.brm, my.coc2, wr_preds) {
#   
#   # #TESTING!
#   # LCI <- 0.1
#   # UCI <- 0.9
#   # my.brm <- Cu.brm
#   # my.coc2 <- Cu.coc2
#   # wr_preds <- comp_preds[, c("Location", "sqrt_traffic", "devAge2")]
#   # 
#   # 
#   # wr_preds <- filter(wr_preds$Location %in% )
#   
# #  preds <- colnames(wr_preds)
#   
#   #take 47 samples total (10% of all S8 Data we have) to represent feasible rain or rain/summer options
#   #   42 samples from non-summer, 5 from summer; this matches the summer/ non-summer ratio of 11% summer.
#   #   NOTE: for the end result, it won't matter whether summer is a predictor or not, as we are drawing the same proportion of 
#   #         samples from summertime as are in the data; this should approximately happen during sampling, regardless.
#   my.sample0 <- my.coc2[which(my.coc2$summer==0)[sample(length(which(my.coc2$summer==0)), 42)], c("rain", "summer")]
#   my.sample1 <- my.coc2[which(my.coc2$summer==1)[sample(length(which(my.coc2$summer==1)), 5)], c("rain", "summer")]
#   my.sample <- rbind(my.sample0, my.sample1)
#   
# #  my.summary <- NULL  #empty object for summarized prediction intervals
#   
#   #generate simulated data
#   newdata0 <- expand_grid(#location=NA, 
#     rain=my.sample0$rain, 
#     summer=0, 
#     wr_preds)
#   newdata1 <- expand_grid(#location=NA, 
#     rain=my.sample1$rain, 
#     summer=1, 
#     wr_preds)
#   newdata <- rbind(newdata0, newdata1)
#   
#   #prediction interval, which predicts the range in which a future individual observation will fall; these include three levels of 
#   #   uncertainty: uncertainty in fixed coefficients, uncertainty in variance parameters for grouping factors, and residual variance
#   #   re_formula=NULL indicates that all group-level effects should be considered in the prediction
#   #   allow_new_levels=TRUE indicates that new group levels (agency) are fine
#   my.site <- posterior_predict(my.brm, newdata = newdata, re_formula = NULL, allow_new_levels=TRUE) %>%
#     t() %>%
#     as_tibble() %>%
#     bind_cols(newdata, .) %>%
#     pivot_longer(
#       cols=starts_with("V"),
#       names_to="pars",
#       values_to="est"
#     )
#   
#   #summarized data for each type of site (specific values of landscape predictors)
#   #  this summarizes what might be expected to occur in samples at a site with particular landscape predictor traits 
#   #  over a representative set of season (summer/not summer) and rainfall value associated with that season
#   my.summary <- my.site %>%
#     group_by(across(all_of(names(wr_preds)))) %>%  #group by the location, and include landscape predictors for this COC
#     summarise(upperCI = quantile(est, UCI),
#               lowerCI = quantile(est, LCI),
#               med=quantile(est, 0.5))
#   rm(my.site, newdata, newdata0, newdata1)
#   return(my.summary)
# }


#provide credible intervals for the mean value at a location with particular predictor values (provided in wr_preds).  Credible intervals
#   show the likely range of values associated with some statistical parameter of the data -- in this case, the population mean
interpCI <- function(LCI=0.1, UCI=0.9, my.brm, my.coc2, wr_preds) {
  
  preds <- colnames(wr_preds)
  
  #take 47 samples total (10% of all data we have) to represent feasible rain or rain/summer options
  #   42 samples from non-summer, 5 from summer; this matches the summer/ non-summer ratio of 11% summer.
  #   NOTE: for the end result, it won't matter whether summer is a predictor or not, as we are drawing the same proportion of 
  #         samples from summertime as are in the data; this should approximately happen during sampling, regardless.
  my.sample0 <- my.coc2[which(my.coc2$summer==0)[sample(length(which(my.coc2$summer==0)), 42)], c("rain", "summer")]
  my.sample1 <- my.coc2[which(my.coc2$summer==1)[sample(length(which(my.coc2$summer==1)), 5)], c("rain", "summer")]
  my.sample <- rbind(my.sample0, my.sample1)
  
  my.summary <- NULL  #empty object for summarized credibility intervals
  
  #generate simulated data
  newdata0 <- expand_grid(#location=NA, 
    rain=my.sample0$rain, 
    summer=0, 
    wr_preds)
  newdata1 <- expand_grid(#location=NA, 
    rain=my.sample1$rain, 
    summer=1, 
    wr_preds)
  newdata <- rbind(newdata0, newdata1)
  
  #conditional mean for an unknown participant, using the population-level intercept plus some variability around where the
  #   location-specific intercept should be.  This is indicated with re_formula=NULL and allow_new_levels=TRUE.  Obtain all estimates  
  #   (not just summarized data) by setting summary=FALSE
  my.site <- fitted(my.brm, newdata = newdata, re_formula = NULL, allow_new_levels=TRUE, summary=FALSE) %>%
    t() %>%
    as_tibble() %>%
    bind_cols(newdata, .) %>%
    pivot_longer(
      cols=starts_with("V"),
      names_to="pars",
      values_to="est")
  
  #summarized data for each type of site (specific values of landscape predictors)
  #  this summarizes what might be expected to occur in samples at a site with particular landscape predictor traits 
  #  over a representative set of season (summer/not summer) and rainfall value associated with that season
  my.summary.temp <- my.site %>%
    group_by(across(all_of(names(wr_preds)))) %>%  #group by the location, and include landscape predictors for this COC
    summarise(upperCI = quantile(est, UCI),
              lowerCI = quantile(est, LCI), 
              med=quantile(est, 0.5))
  rm(my.site, newdata, newdata0, newdata1)
  my.summary <- rbind(my.summary, my.summary.temp)  #combine summary for previous chunks of wr_preds with the current chunk
  return(my.summary)
}

#generate a list of upper and lower limits for 50%, 80% and 95% PI's.  Note that PI's are ln-transformed (take exp(PIs) when comparing to raw data)
make_PIs <- function(my.brm, my.coc2, wr.preds) {
#  PI_2 <- interpPI(LCI=0.49, UCI=0.51, my.brm, my.coc2, wr.preds)
  PI_50 <- interpPI(LCI=0.25, UCI=0.75, my.brm, my.coc2, wr.preds)
  PI_80 <- interpPI(LCI=0.10, UCI=0.90, my.brm, my.coc2, wr.preds)
  PI_95 <- interpPI(LCI=0.025, UCI=0.975, my.brm, my.coc2, wr.preds)
#  return(list(PI_2, PI_50, PI_80, PI_95))
  return(list(PI_50, PI_80, PI_95))
}



#----------------------#
#  Plotting Functions  #
#----------------------#

#make a function for scatter plots
scatter_cocs <- function(DF, Title, Subtitle, Caption) {
  p <- ggplot(DF, aes(1, Result)) + geom_jitter() + labs(
    title = Title,
    subtitle = Subtitle,
    caption = Caption,
    x = "Observations"
  )
  p + facet_wrap( ~ Analyte, scales = 'free')+theme(axis.title.x=element_blank(),
                                                    axis.text.x=element_blank(),
                                                    axis.ticks.x=element_blank())
}

#plot upper and lower limits with data -- all CI's and data are ln-transformed
plot_PIs <- function(my.PI, my.data) {
  ggplot() +
    geom_segment(aes(x=Location, xend=Location, y=lowerCI, yend=upperCI), data=my.PI[[3]], colour=orangePalette[3], linewidth=20) +
    geom_segment(aes(x=Location, xend=Location, y=lowerCI, yend=upperCI), data=my.PI[[2]], colour=orangePalette[2], linewidth=20) +
    geom_segment(aes(x=Location, xend=Location, y=lowerCI, yend=upperCI), data=my.PI[[1]], colour=orangePalette[1], linewidth=20) +
    geom_point(aes(x=Location, y=log(Result)), data=my.data) +  #add ln-transformed data points from this study
    ggtitle(my.data$Analyte[1]) + 
    xlab("Location") +
    ylab(paste("ln-transformed ", my.data$Analyte[1], " (", my.data$Unit[1], ")", sep="")) +
    theme(plot.title=element_text(face="bold", size=14, hjust=0.5))
}

#plot upper and lower limits with data -- all CI's and data are raw values
plot_PIs_raw <- function(my.PI, my.data) {

  # my.PI <- comp_Cu_PI %>%
  #   mutate(location = Location)
  # my.data <- comp_res_pred[which(comp_res_pred$Analyte=="Total Copper"),]
  # # my.PI.trim <- my.PI %>%
  # #   filter(my.PI[[1]]$location %in% unique(my.data$location))

  ggplot() +
    geom_segment(aes(x=location, xend=location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[3]], colour=orangePalette[3], linewidth=5) +
    geom_segment(aes(x=location, xend=location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[2]], colour=orangePalette[2], linewidth=5) +
    geom_segment(aes(x=location, xend=location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[1]], colour=orangePalette[1], linewidth=5) +
    geom_point(aes(x=location, y=rawResult), data=my.data) +  #add ln-transformed data points from this study
    ggtitle(my.data$Analyte[1]) + 
    xlab("location") +
    ylab(paste(my.data$Analyte[1], " (", my.data$Unit[1], ")", sep="")) +
    theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
          axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
}

# #plot upper and lower limits with data -- all CI's and data are raw values
# plot_PIs_raw <- function(my.PI, my.data) {
#   
#   # my.PI <- comp_TKN_PI
#   # my.data <- comp_res_pred[which(comp_res_pred$Analyte=="Total Kjeldahl Nitrogen"),]
#   # my.PI.trim <- my.PI %>%
#   #   filter(my.PI[[1]]$Location %in% unique(my.data$Location))
#   # 
#   
#   
#   ggplot() +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[3]], colour=orangePalette[3], linewidth=5) +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[2]], colour=orangePalette[2], linewidth=5) +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[1]], colour=orangePalette[1], linewidth=5) +
#     geom_point(aes(x=Location, y=Result), data=my.data) +  #add ln-transformed data points from this study
#     ggtitle(my.data$Analyte[1]) + 
#     xlab("Location") +
#     ylab(paste(my.data$Analyte[1], " (", my.data$Unit[1], ")", sep="")) +
#     theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
#           axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
# }

#plot upper and lower limits with data -- all CI's and data are raw values
plot_PIs_boxplot_raw <- function(my.PI, my.data, box.width, line.width=7) {
  ggplot() +
    geom_segment(aes(x=location, xend=location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[3]], colour=orangePalette[3], linewidth=line.width) +
    geom_segment(aes(x=location, xend=location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[2]], colour=orangePalette[2], linewidth=line.width) +
    geom_segment(aes(x=location, xend=location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[1]], colour=orangePalette[1], linewidth=line.width) +
    geom_boxplot(aes(x=location, y=rawResult), data=my.data, notch=FALSE, width=box.width, #box.linewidth=1.0, 
                 fill="white", alpha=1) +  #add boxplots; width=0.4; TKN width=0.15
    ggtitle(my.data$Analyte[1]) + 
    xlab("Location") +
    ylab(paste(my.data$Analyte[1], " (", my.data$Unit[1], ")", sep="")) +
    theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
          axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
}

# #plot upper and lower limits with data -- all CI's and data are raw values
# plot_PIs_boxplot_raw <- function(my.PI, my.data) {
#   ggplot() +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[3]], colour=orangePalette[3], linewidth=7) +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[2]], colour=orangePalette[2], linewidth=7) +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[1]], colour=orangePalette[1], linewidth=7) +
#     geom_boxplot(aes(x=Location, y=Result), data=my.data, notch=FALSE, width=0.4, alpha=1) +  #add boxplots; width=0.4; TKN width=0.15
#     ggtitle(my.data$Analyte[1]) + 
#     xlab("Location") +
#     ylab(paste(my.data$Analyte[1], " (", my.data$Unit[1], ")", sep="")) +
#     theme(plot.title=element_text(face="bold", size=14, hjust=0.5),
#           axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
# }

# #plot upper and lower limits with data -- all CI's and data are raw values
# plot_PIs_raw <- function(my.PI, my.data) {
#   ggplot() +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[3]], colour=orangePalette[3], linewidth=20) +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[2]], colour=orangePalette[2], linewidth=20) +
#     geom_segment(aes(x=Location, xend=Location, y=exp(lowerCI), yend=exp(upperCI)), data=my.PI[[1]], colour=orangePalette[1], linewidth=20) +
#     geom_point(aes(x=Location, y=Result), data=my.data) +  #add ln-transformed data points from this study
#     ggtitle(my.data$Analyte[1]) + 
#     xlab("Location") +
#     ylab(paste(my.data$Analyte[1], " (", my.data$Unit[1], ")", sep="")) +
#     theme(plot.title=element_text(face="bold", size=14, hjust=0.5))
# }


#  Observed vs Predicted
#  https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/multivariate-linear-models.html#plotting-multivariate-posteriors.
#  go down to 5.1.3.3 POSTERIOR PREDICTION PLOTS



#obs.vs.pred.mv(Cu.brm, "Copper", Cu.mv.dat2, myColors)



# observed vs predicted with points colored by location and gray 95% credible interval lines around each point
obs.vs.pred.median.mv <- function(this.brm, this.chemical, mv.dat, myColors, mySymbols) {
  
  # this.brm <- Cu.brm
  # this.chemical <- "Copper"
  # mv.dat <- Cu.mv.dat2
  # myColors <- myColors
  
  fitted(this.brm, newdata=mv.dat, allow_new_levels=TRUE, probs=c(0.25, 0.75)) %>%   #this gives 4 columns: Estimate, Est.Error, Q2.5, and Q97.5; note that fitted() ignores residual error
  as_tibble() %>%
  bind_cols(mv.dat) %>%
  group_by(location) %>%
  summarise_at(c("Estimate", "result"), median) %>%
  ggplot(aes(x = result, y = Estimate, color=location, shape=location)) +       #color points by location (see custom colors below) 
  geom_abline(linetype = 2, color = "grey50", size = .5) +
  geom_point(size = 3.5, alpha = 3/4, stroke=3) +
  labs(x = paste("Observed median ln(", this.chemical, ")", sep=""), 
       y = paste("Estimated median ln(", this.chemical, ")", sep="") ) +
  scale_color_manual(name="location", values=myColors) +
  scale_shape_manual(name="location", values=mySymbols) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text=element_text(size=rel(0.8)),
        legend.key.size=unit(0.8, "cm"))
}


# observed vs predicted with points colored by location and gray 95% credible interval lines around each point
obs.vs.pred.median.area.mv <- function(this.brm, this.chemical, mv.dat) {
  
  # this.brm <- totZn.brm
  # this.chemical <- "total Zinc"
  # mv.dat <- Zn.mv.dat2
  # myColors <- myColors
  
  fitted(this.brm, newdata=mv.dat, allow_new_levels=TRUE, probs=c(0.25, 0.75)) %>%   #this gives 4 columns: Estimate, Est.Error, Q2.5, and Q97.5; note that fitted() ignores residual error
    as_tibble() %>%
    bind_cols(mv.dat) %>%
    group_by(location, sqrt_area_m2) %>%
    summarise_at(c("Estimate", "result"), median) %>%
    ggplot(aes(x = result, y = Estimate, color=log(sqrt_area_m2))) +       #color points by area (log(sqrt(area))) 
    geom_abline(linetype = 2, color = "grey50", size = .5) +
    geom_point(size = 4.5, alpha = 3/4) +
    labs(x = paste("Observed median ln(", this.chemical, ")", sep=""), 
         y = paste("Estimated median ln(", this.chemical, ")", sep="") ) +
    scale_color_distiller(palette = "Purples", direction=-1) +   #colors should be light for low values, dark for high values
    theme_bw() +
    theme(panel.grid = element_blank())
}


# observed vs predicted with points colored by location and gray 95% credible interval lines around each point
obs.vs.pred.mv <- function(this.brm, this.chemical, mv.dat, myColors, mySymbols) {

    fitted(this.brm, newdata=mv.dat, allow_new_levels=TRUE, probs=c(0.25, 0.75)) %>%   #this gives 4 columns: Estimate, Est.Error, Q2.5, and Q97.5; note that fitted() ignores residual error
    as_tibble() %>%
    bind_cols(mv.dat) %>%
    ggplot(aes(x = result, y = Estimate, color=location, shape=location)) +       #color points by location (see custom colors below) 
    geom_abline(linetype = 2, color = "grey50", size = .5) +
    # geom_linerange(aes(ymin = Q25, ymax = Q75),               #the thin lines are the 95% intervals
    #                size = 1/4, color="gray") +
    # geom_linerange(aes(ymin = Estimate - Est.Error,              #the thicker lines are +/- the posterior SD
    #                    ymax = Estimate + Est.Error),
    #                size = 5/8, color="darkgray") +
    geom_point(size = 2.5, alpha = 3/4) +
    labs(x = paste("Observed ln(", this.chemical, ")", sep=""), 
         y = paste("Estimated ln(", this.chemical, ")", sep="") ) +
    scale_color_manual(name="location", values=myColors) +
    scale_shape_manual(name="location", values=mySymbols) +
    theme_bw() +
    theme(panel.grid = element_blank())
}


#function to plot observed vs predicted, with colors showing a selected predictor.  
#   Inputs: brm, coc2, chemical name, predictor name, color brewer palette
obsPredPlot.mv <- function(this.brm, mv.dat, this.chemical, pred, paletteCol) {
  
# #testing
# mv.dat <- mv.dat.Cu
# this.brm <- Cu.brm
# this.chemical <-  "Copper"
# pred <- "sqrt_traffic"
# paletteCol <- "Reds"
  
  fitted(this.brm, newdata=mv.dat, allow_new_levels=TRUE, probs=c(0.25, 0.75)) %>%   #this gives 4 columns: Estimate, Est.Error, Q2.5, and Q97.5; note that fitted() ignores residual error
    as_tibble() %>%
    bind_cols(mv.dat) %>%
  
    ggplot(aes(x = result, y = Estimate, color=get(pred))) +        #here, you can sub out <agency> for other predictors
    geom_abline(linetype = 2, color = "grey50", size = .5) +
    geom_point(size = 1.5, alpha = 3/4) +
    # geom_linerange(aes(ymin = Q2.5, ymax = Q97.5),               #the thin lines are the 95% intervals
    #                size = 1/4) +
    # geom_linerange(aes(ymin = Estimate - Est.Error,              #the thicker lines are +/- the posterior SD
    #                    ymax = Estimate + Est.Error),
    #                size = 1/2) +
    labs(x = paste("Observed ln(", this.chemical, ")", sep=""), 
         y = paste("Estimated ln(", this.chemical, ")", sep=""), color=pred) +
    scale_color_distiller(palette = paletteCol, direction=1) +   #colors should be light for low values, dark for high values
    theme_bw() +
    theme(panel.grid = element_blank())
}

#function to plot observed vs predicted, with colors showing a selected discrete predictor.  
#  Inputs: predictor, color brewer palette
obsPredPlot2.mv <- function(this.brm, mv.dat, this.chemical, pred, paletteCol) {

  fitted(this.brm, newdata=mv.dat, allow_new_levels=TRUE, probs=c(0.25, 0.75)) %>%   #this gives 4 columns: Estimate, Est.Error, Q2.5, and Q97.5; note that fitted() ignores residual error
    as_tibble() %>%
    bind_cols(mv.dat) %>%
    
    ggplot(aes(x = result, y = Estimate, color=get(pred))) +        #here, you can sub out <agency> for other predictors
    geom_abline(linetype = 2, color = "grey50", size = .5) +
    geom_point(size = 1.5, alpha = 3/4) +
    # geom_linerange(aes(ymin = Q2.5, ymax = Q97.5),               #the thin lines are the 95% intervals
    #                size = 1/4) +
    # geom_linerange(aes(ymin = Estimate - Est.Error,              #the thicker lines are +/- the posterior SD
    #                    ymax = Estimate + Est.Error),
    #                size = 1/2) +
    labs(x = paste("Observed ln(", this.chemical, ")", sep=""), 
         y = paste("Estimated ln(", this.chemical, ")", sep=""), color=pred) +
    scale_color_manual(values=paletteCol) +                     #HEX color values are provided based on # of discrete values for pred 
    theme_bw() +
    theme(panel.grid = element_blank())
}



# #Custom manual colour scale 
# 
# #Some test data
# dat <- data.frame(x=runif(10),y=runif(10),
#                   grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)
# 
# #Create a custom color scale
# library(RColorBrewer)
# myColors <- brewer.pal(5,"Set1")
# names(myColors) <- levels(dat$grp)
# colScale <- scale_colour_manual(name = "grp",values = myColors)
# 
# 
# 
# #One plot with all the data
# p <- ggplot(dat,aes(x,y,colour = grp)) + geom_point()
# p1 <- p + colScale
# 
# #A second plot with only four of the levels
# p2 <- p %+% droplevels(subset(dat[4:10,])) + colScale
# 






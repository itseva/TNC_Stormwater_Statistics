# This script contains additional plots for supporting the Stormwater Heatmap water quality model chapter
# and manuscript.

# Eva Dusek Jenning
# Oct 18, 2023
#-------------------------------------------------------------------------------------

#
library("ggplot2")
library("gridExtra")  #grid_arrange
library("ggpubr")  #ggarrange
library("tidyverse")
library("dplyr")

# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
# install.packages("ggpubr")
# library(ggpubr)

#read in S8 spatial predictor data
s8.sp.std <- read.csv(file="../processed_data/spatial_predictors_standardized.csv")
sp.stdVals <- read.csv(file="../processed_data/spatial_predictor_standardization_values.csv")
sp.xformVals <- read.csv(file="../processed_data/spatial_predictor_backtransform_power.csv")

#read in standardized Puget Sound basin values -- min/max and also histogram
basin.sp <- read.csv(file="../data/predictors_PugetSoundBasin_min_max.csv", header=TRUE, row.names=1)
basin.sp.hist <- read.csv(file="../data/predictors_histogram.csv")  ## make sure there are no commas in the csv file, first!!

#read in unstandardized, sqrt_transformed traffic for Puget Sound basin.  DON'T USE dev_age -- it means something diff't here!!
#  Note: also make sure the columns are "numbers" in Excel before running this, otherwise commas will mess it up!
sq_traf <- read.csv(file="../data/traffic_and_dev_histogram_Nov142023.csv", header=TRUE, colClasses=c("sqrt_traffic.Count"="numeric") )
sq_traf <- sq_traf[, 1:2] %>%
  dplyr::rename(value=Band.Value,
         count=sqrt_traffic.Count) %>%
  filter(!is.na(count)) %>%
  

#sqrt_traffic

tr <- ggplot(data=basin.sp.hist[, c(1,3)], aes(x=Band.Value, y=sqrt(sqrt_traffic.Count))) + geom_bar(stat="identity", width=0.2) +
  xlab("scaled & centered sqrt_traffic values") + ylab("sqrt( number of obs )")
tr



#extract only the spatial predictors used in water quality models
model.preds <- c("devAge2", "sqrt_traffic", "not_greenBE", "sqrt_CO2_road")
model.sp.std <- s8.sp.std[, which(names(s8.sp.std) %in% model.preds)]
basin.sp.std <- basin.sp[which(row.names(basin.sp) %in% model.preds), ]
basin.sp.hist <- basin.sp.hist[, c(1, which(names(basin.sp.hist) %in% paste(model.preds, ".Count", sep="")))]

#arrange matrices and data frames in the same order as model.preds
model.sp.std <- model.sp.std[, model.preds]
basin.sp.std <- basin.sp.std[model.preds, ]
basin.sp.hist <- basin.sp.hist[, c("Band.Value", paste(model.preds, ".Count", sep=""))]

#Plot of spatial predictors used in models, showing their range in S8 Data and their range
# in the entire Heatmap region (Puget Sound basin)
par(mfrow=c(1,1), mar=c(2, 4, 2, 2), oma=c(0, 0, 0, 0))
ylims <- c(-4, 5)
plot(1, type="n", xlim=c(0,length(model.preds)+0.5), ylim=ylims, xlab="", xaxt="n",
     ylab="scaled & centered value")
abline(h=seq(-4, 5), col="gray90")
for (i in 1:length(model.preds)) {
  model.preds[i]
  arrows(x0=i, y0=basin.sp.std[i, 1], y1=basin.sp.std[i, 2], length=0.25, angle=90, code=3, col="gray", lwd=3)
  rect(xleft=i-0.06, ybottom=max(-3, basin.sp.std[i, 1]), xright=i+0.06, ytop=min(3, basin.sp.std[i, 2]), col="maroon3")
  rect(xleft=i-0.1, ybottom=min(model.sp.std[,i]), xright=i+0.1, ytop=max(model.sp.std[,i]), col="orange")
  points(rep(i, nrow(model.sp.std)), model.sp.std[,i], pch=16, col="black")
  text(x=i, y=ylims[1]-0.65, names(model.sp.std)[i], xpd=NA, font=2, cex=1.1)
}
text(x=2-0.08, y=ylims[2]-0.65, "max sqrt_traffic = 62", srt=90, col="darkgray")

#Plot histograms of predictors for whole Puget Sound basin
#devAge2
de <- ggplot(data=basin.sp.hist[, 1:2], aes(x=Band.Value, y=sqrt(devAge2.Count))) + geom_bar(stat="identity") +
      xlab("scaled & centered devAge2 values") + ylab("sqrt( number of obs )")
#sqrt_traffic
tr <- ggplot(data=basin.sp.hist[, c(1,3)], aes(x=Band.Value, y=sqrt(sqrt_traffic.Count))) + geom_bar(stat="identity", width=0.2) +
      xlab("scaled & centered sqrt_traffic values") + ylab("sqrt( number of obs )")
#paved
pa <- ggplot(data=basin.sp.hist[, c(1,4)], aes(x=Band.Value, y=sqrt(paved.Count))) + geom_bar(stat="identity") +
      xlab("scaled & centered paved") + ylab("sqrt( number of obs )")
#sqrt_CO2_road
co <- ggplot(data=basin.sp.hist[, c(1,5)], aes(x=Band.Value, y=sqrt(sqrt_CO2_road.Count))) + geom_bar(stat="identity") +
      xlab("scaled & centered sqrt_CO2_road values") + ylab("sqrt( number of obs )")
#all four plots on one figure; 3 rows, bottom row has two plots
ggarrange(tr,
          co,
          ggarrange(de, pa, ncol=2), nrow=3)



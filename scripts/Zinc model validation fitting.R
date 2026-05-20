

pred_index=c(1:n.preds)
my.ggplot(pred_index[i])


head(Zn.mv.dat2)

Zn.mv <- mv.full_res_pred %>% 
  filter(Analyte=="Total Zinc") %>% 
  mutate(rain=daymet_14day_std,
         summer = as.factor(summer),
         rawResult = Result,
         Result = log(rawResult)) %>%
  filter(!result > 8)  #two super high outliers at Tac OF235


mv.predictors <- names(Zn.mv[21:25])

a <- my.ggplot(1)
b <- my.ggplot(2)
c <- my.ggplot(3)
d <- my.ggplot(4)
e <- my.ggplot(5)
ggarrange(a, b, c, d, e)

#obtain slope for grob function
slope_grob <- function(predictor) {
  my.lm <- lm(Result~predictor, data=Zn.mv)
  grob <- grobTree(textGrob(paste("slope p-value =", round(summary(my.lm)$coef[2,4], 4)), x=0.1,  y=0.95, hjust=0,
                            gp=gpar(col="red", fontsize=15, fontface="italic")))
  return(grob)
}


my.ggplot <- function(pred_num) {
  ggplot(Zn.mv, aes(Zn.mv[,mv.predictors[pred_num]], Result)) + geom_point() + xlab(mv.predictors[pred_num]) + 
    geom_smooth(method = "lm") + annotation_custom(slope_grob(Zn.mv[, mv.predictors[pred_num]])) + #add a smooth slope and p-value for the slope line
    theme_gray()
}


#relationships between coc and landscape predictors
lp_plots <- function(pred_index=c(1:n.preds)) {  #default input is all predictors
  
  #split up all predictors into groups of 12; each list below (lpA through lpD) will hold up to 12 grobs
  lpA <- list()
  lpB <- list()
  lpC <- list()
  lpD <- list()
  for (i in 1:length(pred_index)) {
    p <- my.ggplot(pred_index[i])
    n <- ceiling(i/12) - 1  #for indexing within lpA through lpD
    if (i<=12) { #first 12 plots go in lpA
      lpA[[i]] <- p
    } else if (i >12 & i <=24) {  #plots 13-24 go in lpB, etc.
      lpB[[i-(n*12)]] <- p
    } else if (i > 24 & i <=36) {
      lpC[[i-(n*12)]] <- p
    } else if (i > 36) {
      lpD[[i-(n*12)]] <- p
    }
  }  
  
  do.call("grid.arrange", c(lpA, nrow=3, ncol=4))
  do.call("grid.arrange", c(lpB, nrow=3, ncol=4))
  do.call("grid.arrange", c(lpC, nrow=3, ncol=4))
  do.call("grid.arrange", c(lpD, nrow=3, ncol=4))
  
  #the following code is for just one list object; if I can figure out how to split it up into
  #  several grid.arrange pages, this would work better.
  # 
  # lp <- list()
  # for (i in 1:n.preds) {
  #   n <- my.ggplot(i)
  #   lp[[i]] <- n
  # }  
  # 
  # n <- 12
  # nCol <- floor(sqrt(n))
  # do.call("grid.arrange", c(lp, ncol=nCol))
}

#relationships between coc and landscape predictors
lp_plots2 <- function(pred_index=c(1:n.preds)) {  #default input is all predictors
  #split up all predictors into groups of 16; each list below (lpA through lpD) will hold up to 16 grobs
  lpA <- list()
  lpB <- list()
  lpC <- list()
  for (i in 1:length(pred_index)) {
    p <- my.ggplot(pred_index[i])
    n <- ceiling(i/16) - 1  #for indexing within lpA through lpD
    if (i<=16) { #first 12 plots go in lpA
      lpA[[i]] <- p
    } else if (i >16 & i <=32) {  #plots 17-32 go in lpB, etc.
      lpB[[i-(n*16)]] <- p
    } else if (i > 32 & i <=48) {
      lpC[[i-(n*16)]] <- p
    }
  }  
  
  do.call("grid.arrange", c(lpA, nrow=4, ncol=4))
  do.call("grid.arrange", c(lpB, nrow=4, ncol=4))
  do.call("grid.arrange", c(lpC, nrow=4, ncol=4))
}

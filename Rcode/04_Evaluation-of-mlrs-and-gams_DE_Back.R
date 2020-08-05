################################################################################################
### Conditional mean NO2 surface modeling: Cross-validation based evaluation of MLRs and GAMs
################################################################################################



library(mgcv)

rm(list = ls())


DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]
dat <- DATA[,c(2,5:7,11:20,26:30)]


#rename some columns
names(dat)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDens")


par1 <- lm(Y ~ popDens + Forest + Lat + Alt + Agri + NatMot, data = dat)


form.gam2.3 <- Y ~ s(Lon, Lat, k=-1, bs="tp", fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

gam2.3 <- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=dat, family=gaussian(),
              weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
              select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

semipar <- gam2.3



 
# Overview over the performance of the models
df.overview <- data.frame(matrix(data = NA, nrow = 2, ncol = 9))
names(df.overview) <- c("Name", "adj. R^2", "BIC", "AIC", "LOOCV RMSE",
                        "LOOCV MAE", "LOOCV BIC", "LOOCV AIC", "LOOCV adj. R^2")
df.overview[ , 1] <- c("par1", "semipar")
df.overview[ , 2] <- round(c(summary(par1)$adj.r.squared, summary(semipar)$r.sq), digits = 4)
df.overview[ , 3] <- round(BIC(par1, semipar)[ , 2], digits = 4)
df.overview[ , 4] <- round(AIC(par1, semipar)[ , 2], digits = 4)



####
# Leave-one-out-cross-validation - prediction error, adjR2, BIC, AIC ----
####


## par1 ----

df.loocv.par1 <- data.frame(matrix(data = NA, nrow = 246, ncol = 5))
names(df.loocv.par1) <- c("AQeCode", "PredErr", "adjR2", "BIC", "AIC")
df.loocv.par1$AQeCode <- DATA$AQeCode


for(i in 1:nrow(df.loocv.par1)){
  lur.tmp	<- update(par1, data = dat[-i, ])

  pred.tmp <- predict(lur.tmp, newdata = dat[i, ])

  df.loocv.par1$`PredErr`[i] <- dat[i, "Y"] - pred.tmp
  df.loocv.par1$`adjR2`[i] <- summary(lur.tmp)$adj.r.squared
  df.loocv.par1$BIC[i] <- BIC(lur.tmp)
  df.loocv.par1$AIC[i] <- AIC(lur.tmp)
}

summary(df.loocv.par1)



## semipar ----

df.loocv.semipar  <- data.frame(matrix(data = NA, nrow = 246, ncol = 5))
names(df.loocv.semipar) <- c("AQeCode", "PredErr", "adjR2", "BIC", "AIC")
df.loocv.semipar$AQeCode <- DATA$AQeCode


for(i in 1:nrow(df.loocv.semipar)){
  lur.tmp	<- update(semipar, data = dat[-i, ])

  pred.tmp <- predict(lur.tmp, newdata = dat[i, ])

  df.loocv.semipar$`PredErr`[i] <- dat[i, "Y"] - pred.tmp
  df.loocv.semipar$`adjR2`[i] <- summary(lur.tmp)$r.sq
  df.loocv.semipar$BIC[i] <- BIC(lur.tmp)
  df.loocv.semipar$AIC[i] <- AIC(lur.tmp)
}

summary(df.loocv.semipar)


LOOCV <- list(df.loocv.par1, df.loocv.semipar)


df.overview$`LOOCV RMSE` <- round(sapply(X = LOOCV, FUN = function(X) {return(sqrt(mean(X$`PredErr`^2)))}),
                                  digits = 4)
df.overview$`LOOCV MAE`  <- round(sapply(X = LOOCV, FUN = function(X) {return(mean(abs(X$`PredErr`)))}),
                                  digits = 4)
df.overview$`LOOCV BIC`  <- round(sapply(X = LOOCV, FUN = function(X) {return(mean(X$BIC))}),
                                  digits = 4)
df.overview$`LOOCV AIC`  <- round(sapply(X = LOOCV, FUN = function(X) {return(mean(X$AIC))}),
                                  digits = 4)
df.overview$`LOOCV adj. R^2`  <- round(sapply(X = LOOCV, FUN = function(X) {return(mean(X$`adjR2`))}),
                                    digits = 4)


saveRDS(object = df.overview, file = "R/DATA/Data_built/Par1SemiparEvaluation.rds")

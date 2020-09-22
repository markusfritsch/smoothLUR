##################################################################################
### Conditional mean NO2 surface modeling: 
### MLRs and GAMs for German industrial and traffic sites ----
##################################################################################





# setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")

rm(list = ls())

#	install.packages(mgcv)
library(mgcv)



DATA <- read.csv("R/DATA/Data_built/DATA_DE_traffic_industrial.csv", header=TRUE)[, -1]
dat <- DATA[ , c(2,5:7,11:20,26:30)] 

#rename some columns
names(dat)[c(1:4, 15)] <- c("Y", "Lon", "Lat", "Alt", "popDensBBSR")


# Eliminate predictors that exhibit mainly zeros or extreme values ----
# (see Wolf et al., 2016, p.1535, and Eeftens et al., 2016, p.5)
t(apply(dat, MARGIN = 2, FUN = function(x){
  return(c(nr.values = length(unique(x)),
           max.extreme =  isTRUE(max(x) > quantile(x, 0.9) + 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           min.extreme =  isTRUE(min(x) < quantile(x, 0.1) - 3* (quantile(x, 0.9) - quantile(x, 0.1))))
  )
}
)
)
# -> eliminate Seap, Airp, Constr
names(dat)
dat2 <- dat[,-c(2:4, 9:11)]


# Standardized supervised forward stepwise procedure for predictor selection ----

# Pre-specify direction of effect -> + 1, - -1, +/- 0
df_start <-  data.frame(matrix(data = NA, nrow = ncol(dat2)-1, ncol = 4))
names(df_start) <- c("Predictor", "DirEff", "EstCoeff", "AdjR2")
df_start$Predictor <- names(dat2)[-1]
#df_start$DirEff <- c(rep(1,4),-1,0,rep(1,3))



# 1. Identify start model ----
# Run univariate regressions and choose predictor with hightest adjusted R^2
for(i in 1:nrow(df_start)){
  dat.tmp <- dat2[ , -1]
  lm.tmp <- lm(Y ~ dat.tmp[ , i], data = dat)
  df_start$EstCoeff[i] <- lm.tmp$coefficients[2]
  df_start$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}


df_start[which.max(df_start$AdjR2), ]
# the sign of the estimated effect goes in line with the expected sign
adjR2_start <- df_start[which.max(df_start$AdjR2), "AdjR2"]



# 2. and 3. Add consecutively further variable ----


# Adding a second variable ----

df_select2 <- df_start[-which.max(df_start$AdjR2), ]
col.2 <- which(names(dat2) == df_start[which.max(df_start$AdjR2), "Predictor"])
for(i in 1:nrow(df_select2)){
  dat.tmp <- dat2[ , -c(1, col.2)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] + dat.tmp[ , i])
  df_select2$EstCoeff[i] <- lm.tmp$coefficients[3]
  df_select2$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select2[which.max(df_select2$AdjR2),]
adjR2_select2 <- df_select2[which.max(df_select2$AdjR2), "AdjR2"]
adjR2_select2 - adjR2_start > 0.01


# Adding a third variable ----

df_select3 <- df_select2[-which.max(df_select2$AdjR2), ]
col.3 <- which(names(dat2) == df_select2[which.max(df_select2$AdjR2), "Predictor"])
for(i in 1:nrow(df_select3)){
  dat.tmp <- dat2[ , -c(1, col.2, col.3)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] +
                 dat[ , df_select2[which.max(df_select2$AdjR2), "Predictor"]] + dat.tmp[ , i])
  df_select3$EstCoeff[i] <- lm.tmp$coefficients[4]
  df_select3$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select3[which.max(df_select3$AdjR2), ]
adjR2_select3 <- df_select3[which.max(df_select3$AdjR2), "AdjR2"]
adjR2_select3 - adjR2_select2 > 0.01



# Adding a fourth variable ----

df_select4 <- df_select3[-which.max(df_select3$AdjR2), ]
col.4 <- which(names(dat2) == df_select3[which.max(df_select3$AdjR2), "Predictor"])

for(i in 1:nrow(df_select4)){
  dat.tmp <- dat2[ , -c(1, col.2, col.3, col.4)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] +
                 dat[ , df_select2[which.max(df_select2$AdjR2), "Predictor"]] + 
                 dat[ , df_select3[which.max(df_select3$AdjR2), "Predictor"]] +dat.tmp[ , i])
  df_select4$EstCoeff[i] <- lm.tmp$coefficients[5]
  df_select4$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select4[which.max(df_select4$AdjR2), ]
adjR2_select4 <- df_select4[which.max(df_select4$AdjR2), "AdjR2"]
adjR2_select4 - adjR2_select3 > 0.01



# Adding a fifth variable ----

df_select5 <- df_select4[-which.max(df_select4$AdjR2), ]
col.5 <- which(names(dat2) == df_select4[which.max(df_select4$AdjR2), "Predictor"])

for(i in 1:nrow(df_select5)){
  dat.tmp <- dat2[ , -c(1, col.2, col.3, col.4, col.5)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] +
                 dat[ , df_select2[which.max(df_select2$AdjR2), "Predictor"]] + 
                 dat[ , df_select3[which.max(df_select3$AdjR2), "Predictor"]] + 
                 dat[ , df_select4[which.max(df_select4$AdjR2), "Predictor"]] + dat.tmp[ , i])
  df_select5$EstCoeff[i] <- lm.tmp$coefficients[6]
  df_select5$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select5[which.max(df_select5$AdjR2), ]
adjR2_select5 <- df_select5[which.max(df_select5$AdjR2), "AdjR2"]
adjR2_select5 - adjR2_select4 > 0.01




# Adding a sixth variable ----

df_select6 <- df_select5[-which.max(df_select5$AdjR2), ]
col.6 <- which(names(dat2) == df_select5[which.max(df_select5$AdjR2), "Predictor"])

for(i in 1:nrow(df_select6)){
  dat.tmp <- dat2[ , -c(1, col.2, col.3, col.4, col.5, col.6)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] +
                 dat[ , df_select2[which.max(df_select2$AdjR2), "Predictor"]] + 
                 dat[ , df_select3[which.max(df_select3$AdjR2), "Predictor"]] + 
                 dat[ , df_select4[which.max(df_select4$AdjR2), "Predictor"]] +
                 dat[ , df_select4[which.max(df_select5$AdjR2), "Predictor"]] + dat.tmp[ , i])
  df_select5$EstCoeff[i] <- lm.tmp$coefficients[7]
  df_select5$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select6[which.max(df_select6$AdjR2), ]
adjR2_select6 <- df_select6[which.max(df_select6$AdjR2), "AdjR2"]
adjR2_select6 - adjR2_select5 > 0.01


# 4. Remove variables with p-values > 0.1 sequentially from model. ----
summary(lm(Y ~ popDensBBSR + PriRoad + LowDens + HighDens, data = dat))
# here: all p-values < 0.1



# Resulting linear parametric LUR ----
# The linear parametric land use regression model with variabel pre-selection
# through the standardised supervised forward stepwise procedure according to ESCAPE is given by:
lur.par <- lm(Y ~ popDensBBSR + PriRoad + Alt + LowDens + Lon, data = dat)
summary(lur.par)
summary(lur.par)$adj.r.squared
BIC(lur.par)
AIC(lur.par)



# Model diagnositcs ----

# Variance inflation factors
vif(lur.par)
# for comparison
1/(1-summary(lm(popDensBBSR ~ HighDens, data = dat))$r.squared)
# there are no predictor variables vif above 3


# Cook's distance
summary(cooks.distance(lur.par))
plot(cooks.distance(lur.par))
# there is no observation with Cook's distance above 1


# Moran's I
library(ape)
res.dist <- as.matrix(dist(cbind(dat$Lon, dat$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(lur.par), res.dist.inv)
# the null that the residuals do not reveal spatial autocorrelation
# cannot be rejected at every reasonable significance level





# Deriving a semiparametric land use regression model without variable pre-selection ----
# Allow for univariate and bivariat thin-plate regression splines in a generalised additive model (GAM).


# Eliminate predictors that exhibit mainly zeros or extreme values ----
# (see Wolf et al., 2016, p.1535, and Eeftens et al., 2016, p.5)
t(apply(dat, MARGIN = 2, FUN = function(x){
  return(c(nr.values = length(unique(x)),
           max.extreme =  isTRUE(max(x) > quantile(x, 0.9) + 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           min.extreme =  isTRUE(min(x) < quantile(x, 0.1) - 3* (quantile(x, 0.9) - quantile(x, 0.1))))
  )
}
)
)

# -> eliminate Seap, Airp, Constr
names(dat)
dat2 <- dat[,-c(9:11)]

names(dat2)
names(dat2)[12] <- "popDens"

# GAM with univariate spline for every predictor ----
form.gam1 <- Y ~  s(HighDens, k=-1, bs="tp") +
  #s(Lon,Lat, k=-1, bs="tp") +         #  k = -1 for automatic smoothness selection
  #s(Alt, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
  #  s(Seap, k=-1, bs="tp") + 				# insufficient amount of data for flexible estimation
  #  s(Airp, k=-1, bs="tp") +         # insufficient amount of data for flexible estimation
  #  s(Constr, k=-1, bs="tp") +       # insufficient amount of data for flexible estimation
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

gam2 <- gam(formula=form.gam1, fit=TRUE, method="P-ML", data=dat2, family=gaussian(),
            weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
            select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

summary(gam2)
summary(gam2)$r.sq
summary(lur.par)$adj.r.squared
BIC(gam2)
AIC(gam2)
#gam.check(gam2)



# Moran's I
library(ape)
res.dist <- as.matrix(dist(cbind(dat$Lon, dat$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(gam2), res.dist.inv)
# the null that the residuals do not reveal spatial autocorrelation
# cannot be rejected at any reasonable significance level


# Heteroskedasticity
par(mfrow = c(1,2))
plot(resid(gam2) ~ fitted(gam2), pch = 16, cex = 0.7)
abline(h = 0, lwd = 2, col = "blue")
plot(dat$Y ~ fitted(gam2), pch = 16, cex = 0.7)
abline(c(1,1), lwd = 2, col = "blue")
plot(gam2)
par(mfrow = c(1,1))
library(lmtest)
bptest(gam2)


# Normality?
qqnorm(resid(gam2))
qqline(resid(gam2), col = 2)
# in the upper tail a deviation from the normal distribution is visible
#########################################################################################
### Conditional mean NO2 surface modeling: MLRs and GAMs for German background sites ----
#########################################################################################





#	setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")

rm(list = ls())

#	install.packages("mgcv")
library(mgcv)
#	install.packages("ape")
library(ape)
#	install.packages("xtable")
library(xtable)
#	install.packages("car")
library(car)
#	install.packages("lmtest")
library(lmtest)
library(smoothLUR)





load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(2,4:7,9:24)]








###
### Parametric analysis
###


# Deriving a linear parametric land use regression model with predictor pre-selection ----
# through the standardised supervised forward stepwise procedure according to ESCAPE.
# (see, e.g., Wolf et al., 2017, Eeftens et al., 2016, and Beelen et al., 2013)

# 1.	Identify start model: Univariate regression analyses for all predictor;
#     choose model with highest adj R^2 and a slope of pre-specified direction
# 2.	Add consecutively all remaining predictors to start model and record effect of adj R^2;
#     maintain predictor with highest additional increase in adj R^2 if three criteria are fulfilled:
#  a.	Absolute increase in adj R^2 > 1%
#  b.	Coefficient conforms with pre-specified direction of effect
#  c.	Direction of effect for predictors already in the model did not change
# 3.	Repeat addition of predictors until there are no predictors that add more than 1% to adj R^2 of previous model.
# 4.	Finally, remove predictors with p-values larger than 0.1 sequentially from model.


# Eliminate predictors that exhibit mainly zeros or extreme values ----
# (see Wolf et al., 2016, p.1535, and Eeftens et al., 2016, p.5)
t(apply(dat[,-c(4:5,20)], MARGIN = 2, FUN = function(x){
  return(c(nr.values = length(unique(x)),
           max.extreme =  isTRUE(max(x) > quantile(x, 0.9) + 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           min.extreme =  isTRUE(min(x) < quantile(x, 0.1) - 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           no.zero = sum(x == 0))
  )
}
)
)
nrow(dat)
# -> eliminate Seap, Airp, Constr





## Estimate parametric model with structural predictors only (par0) ----

dat0.B	<- dat[dat$AQeType == "background", ]
datB		<- dat[dat$AQeType == "background", ]
dat0.TI	<- dat[(dat$AQeType != "background"), ]
datTI		<- dat[(dat$AQeType != "background"), ]
dat0.A	<- dat
datA		<- dat



#input parameters for parLUR function when including only structural effects
dirEff0	<- c(rep(1,4),-1,0,-1,rep(1,5))

m0.B		<- parLUR(data = dat0.B, y = "Y", x = paste(names(dat0.B)[-c(1:5,10:12,21)]), dirEff = dirEff0)
m0.TI		<- parLUR(data = dat0.TI, y = "Y", x = paste(names(dat0.TI)[-c(1:5,10:12,21)]), dirEff = dirEff0)
m0.A		<- parLUR(data = dat0.A, y = "Y", x = paste(names(dat0.A)[-c(1:5,10:12,21)]), dirEff = dirEff0)



xtable(summary(m0.B)$coefficients[,1:2], digits = 3)
xtable(summary(m0.TI)$coefficients[,1:2], digits = 3)
xtable(summary(m0.A)$coefficients[,1:2], digits = 3)

#	AIC(m0.B); BIC(m0.B)



# Moran's I: background
res.dist <- as.matrix(dist(cbind(datB$Lon, datB$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m0.B), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level


# Moran's I: traffic/industrial
res.dist <- as.matrix(dist(cbind(datTI$Lon, datTI$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m0.TI), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms cannot be rejected


# Moran's I: all
res.dist <- as.matrix(dist(cbind(datA$Lon, datA$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m0.A), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level






## Estimate parametric model with structural and spatial predictors (par) ----


#input parameters for parLUR function when including only structural effects
dirEff	<- c(rep(0, times = 2), -1, rep(1,4),-1,0,-1,rep(1,5))

m.B		<- parLUR(data = datB, y = "Y", x = paste(names(datB)[-c(1,5,10:12,21)]), dirEff = dirEff)
m.TI	<- parLUR(data = datTI, y = "Y", x = paste(names(datTI)[-c(1,5,10:12,21)]), dirEff = dirEff)
m.A		<- parLUR(data = datA, y = "Y", x = paste(names(datA)[-c(1,5,10:12,21)]), dirEff = dirEff)


xtable(summary(m.B)$coefficients[,1:2], digits = 3)
xtable(summary(m.TI)$coefficients[,1:2], digits = 3)
xtable(summary(m.A)$coefficients[,1:2], digits = 3)

#	AIC(m0.B); BIC(m0.B)



# Moran's I: background
res.dist <- as.matrix(dist(cbind(datB$Lon, datB$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m.B), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level


# Moran's I: traffic/industrial
res.dist <- as.matrix(dist(cbind(datTI$Lon, datTI$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m.TI), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at alpha=0.1 significance level


# Moran's I: all
res.dist <- as.matrix(dist(cbind(datA$Lon, datA$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m.A), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level










# Model diagnostics ----

m	<- m.B
dat.m	<- datB

# Variance inflation factors
vif(m)
# for comparison
1/(1-summary(m)$r.squared)
# there are no predictor predictors vif above 3


# Cook's distance
summary(cooks.distance(m))
plot(cooks.distance(m))
# there is no observation with Cook's distance above 1


# Moran's I
res.dist <- as.matrix(dist(cbind(dat.m$Lon, dat.m$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level


# Heteroskedasticity
par(mfrow = c(1,2))
plot(resid(m) ~ fitted(m), pch = 16, cex = 0.7)
abline(h = 0, lwd = 2, col = "blue", lty = 2)
plot(dat.m$Y ~ fitted(m), pch = 16, cex = 0.7)
abline(c(1,1), lwd = 2, col = "blue")
par(mfrow = c(2,2))
plot(m)
par(mfrow = c(1,1))
# plots indicate heteroskedasticity
bptest(m)		# null that predictors do not explain remaining dispersion in error terms is rejected at every reasonable significance level
ncvTest(m)		# null that variance of error terms is constant is rejected at every reasonable significance level
# tests confirm our expectations


# Normality?
qqnorm(resid(m))
qqline(resid(m), col = "blue")
# lower and upper tails exhibit deviation from normal distribution











###
### Semiparametric analysis
###


# Deriving a semiparametric land use regression model without predictor pre-selection ----
# Allow for univariate and bivariat thin-plate regression splines in a generalized additive model (GAM).


# Eliminate predictors that exhibit mainly zeros or extreme values ----
# (see Wolf et al., 2016, p.1535, and Eeftens et al., 2016, p.5)
t(apply(datB[,-5], MARGIN = 2, FUN = function(x){
  return(c(nr.values = length(unique(x)),
           max.extreme =  isTRUE(max(x) > quantile(x, 0.9) + 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           min.extreme =  isTRUE(min(x) < quantile(x, 0.1) - 3* (quantile(x, 0.9) - quantile(x, 0.1))),
           no.zero = sum(x == 0))
  )
}
)
)
nrow(datB)
# -> eliminate Seap, Airp, Constr





form.gam2.3 <- Y ~ s(Lon, Lat, k=-1, bs="tp", fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
#  s(Seap, k=-1, bs="tp") + 			# insufficient amount of data for flexible estimation
#  s(Airp, k=-1, bs="tp") +         		# insufficient amount of data for flexible estimation
#  s(Constr, k=-1, bs="tp") +       		# insufficient amount of data for flexible estimation
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(PopDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(FedAuto, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")




gam2.3B	<- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=datB, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.3B)
summary(gam2.3B)
par(mfrow = c(4,4))
plot(gam2.3B)
par(mfrow = c(1,1))
summary(gam2.3B)$r.sq
#	BIC(gam2.3B); AIC(gam2.3B)



gam2.3TI	<- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=datTI, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.3TI)
summary(gam2.3TI)
par(mfrow = c(4,4))
plot(gam2.3TI)
par(mfrow = c(1,1))
summary(gam2.3TI)$r.sq
#	BIC(gam2.3TI); AIC(gam2.3TI)



gam2.3A	<- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=datA, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.3A)
summary(gam2.3A)
par(mfrow = c(4,4))
plot(gam2.3A)
par(mfrow = c(1,1))
summary(gam2.3A)$r.sq
#	BIC(gam2.3A); AIC(gam2.3A)






# Model diagnositcs for semipar ----

	gam2.3	<- gam2.3B
	dat.m		<- datB

	gam2.3	<- gam2.3TI
	dat.m		<- datTI

	gam2.3	<- gam2.3A
	dat.m		<- dat


m			<- gam2.3
summary(m)


# Cook's distance
summary(cooks.distance(m))
plot(cooks.distance(m))
# there is no observation with Cook's distance above 1


# Moran's I
res.dist <- as.matrix(dist(cbind(dat.m$Lon, dat.m$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms cannot be rejected at any reasonable significance level


# Heteroskedasticity
par(mfrow = c(1,2))
plot(resid(m) ~ fitted(m), pch = 16, cex = 0.7)
abline(h = 0, lwd = 2, col = "blue")
plot(dat.m$Y ~ fitted(m), pch = 16, cex = 0.7)
abline(c(1,1), lwd = 2, col = "blue")
par(mfrow = c(3,5))
plot(m)
par(mfrow = c(1,1))
# plots indicate heteroskedasticity
bptest(m)


# Normality?
qqnorm(resid(m))
qqline(resid(m), col = "blue")
# upper tail exhibits a deviation from normal distribution











### Further estimation of semiparametric model


# GAM with univariate spline for every predictor ----
form.gam1 <- Y ~ s(Lon, k=-1, bs="tp") +         #  k = -1 for automatic smoothness selection
  s(Lat, k=-1, bs="tp") +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
#  s(Seap, k=-1, bs="tp") + 			# insufficient amount of data for flexible estimation
#  s(Airp, k=-1, bs="tp") +       		# insufficient amount of data for flexible estimation
#  s(Constr, k=-1, bs="tp") +     		# insufficient amount of data for flexible estimation
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(PopDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(FedAuto, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")

gam1 <- gam(formula=form.gam1, fit=TRUE, method="P-ML", data=datB, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam1)
summary(gam1)
par(mfrow = c(4,4))
plot(gam1)
par(mfrow = c(1,1))
summary(gam1)$r.sq
summary(m.B)$adj.r.squared
#	BIC(gam1); AIC(gam1)




# GAM with bivariate spline for lon and lat and univariate spline for remaining predictors ----
form.gam2.3 <- Y ~ s(Lon, Lat, k=-1, bs="tp", fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  s(Transp, k=-1, bs="tp") +
#  s(Seap, k=-1, bs="tp") + 			# insufficient amount of data for flexible estimation
#  s(Airp, k=-1, bs="tp") +         		# insufficient amount of data for flexible estimation
#  s(Constr, k=-1, bs="tp") +       		# insufficient amount of data for flexible estimation
  s(UrbGreen, k=-1, bs="tp") +
  s(Agri, k=-1, bs="tp") +
  s(Forest, k=-1, bs="tp") +
  s(PopDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(FedAuto, k=-1, bs="tp") +
  s(LocRoute, k=-1, bs="tp")



gam2.3 <- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=datB, family=gaussian(),
              weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
              select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.3)
summary(gam2.3)
par(mfrow = c(4,4))
plot(gam2.3)
par(mfrow = c(1,1))
summary(gam2.3)$r.sq
#	BIC(gam2.3); AIC(gam2.3)

semipar <- gam2.3




# Eliminating predictors with edf negligible low (smoothed out) ----
form.gam2.4 <- Y ~ s(Lon, Lat, k=-1, bs="tp",	fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
# s(Transp, k=-1, bs="tp") +      		# smoothed out in gam2.3
# s(Seap, k=-1, bs="tp") + 			# insufficient amount of data for flexible estimation
# s(Airp, k=-1, bs="tp") +        		# insufficient amount of data for flexible estimation
# s(Constr, k=-1, bs="tp") +      		# insufficient amount of data for flexible estimation
# s(UrbGreen, k=-1, bs="tp") +    		# smoothed out in gam2.3
# s(Agri, k=-1, bs="tp") +        		# smoothed out in gam2.3
  s(Forest, k=-1, bs="tp") +
  s(PopDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
#  s(SecRoad, k=-1, bs="tp") +
  s(FedAuto, k=-1, bs="tp") #+
# s(LocRoute, k=-1, bs="tp")      # smoothed out in gam2.3

gam2.4 <- gam(formula=form.gam2.4, fit=TRUE, method="P-ML", data=datB, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.4)
summary(gam2.4)
par(mfrow = c(4,4))
plot(gam2.4)
par(mfrow = c(1,1))
summary(gam2.4)$r.sq
#	BIC(gam2.4); AIC(gam2.4)







# Model diagnositcs for semipar ----

#	gam2.3	<- gam2.3B
#	gam2.3	<- gam2.3TI
#	gam2.3	<- gam2.3A


m			<- gam2.3

summary(m)


# Cook's distance
summary(cooks.distance(m))
plot(cooks.distance(m))
# there is no observation with Cook's distance above 1


# Moran's I
res.dist <- as.matrix(dist(cbind(datB$Lon, datB$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(m), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms cannot be rejected at any reasonable significance level


# Heteroskedasticity
par(mfrow = c(1,2))
plot(resid(m) ~ fitted(m), pch = 16, cex = 0.7)
abline(h = 0, lwd = 2, col = "blue")
plot(datB$Y ~ fitted(m), pch = 16, cex = 0.7)
abline(c(1,1), lwd = 2, col = "blue")
par(mfrow = c(3,5))
plot(m)
par(mfrow = c(1,1))
# plots indicate heteroskedasticity
bptest(m)


# Normality?
qqnorm(resid(m))
qqline(resid(m), col = "blue")
# upper tail exhibits a deviation from normal distribution

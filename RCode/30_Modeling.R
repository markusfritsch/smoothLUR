###########################################
### Conditional mean NO2 surface modeling
###########################################




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

datA  <- dat
datA$LonLat  <- dat$Lon*dat$Lat
datB  <- dat[dat$AQeType=="background",]
datTI <- dat[dat$AQeType!="background",]




###
### LUR modeling based on parametric polynomials (predictor pre-selection according to ESCAPE procedure)
###


parA <- parLUR(data = datA
               ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                      #				, "Seap", "Airp", "Constr"
                      ,"UrbGreen", "Agri", "Forest", "PopDens"
                      ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
               ,y = "Y"
               ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
               ,thresh = 0.95)
parA2 <- parLUR(data = datA
                ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                       #				, "Seap", "Airp", "Constr"
                       ,"UrbGreen", "Agri", "Forest", "PopDens"
                       ,"PriRoad", "SecRoad", "FedAuto", "LocRoute", "LonLat")
                ,y = "Y"
                ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1,0)
                ,thresh = 0.95)
parB <- parLUR(data = datB
               ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                      #				, "Seap", "Airp", "Constr"
                      ,"UrbGreen", "Agri", "Forest", "PopDens"
                      ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
               ,y = "Y"
               ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
               ,thresh = 0.95)
parTI <- parLUR(data = datTI
               ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                      #				, "Seap", "Airp", "Constr"
                      ,"UrbGreen", "Agri", "Forest", "PopDens"
                      ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
               ,y = "Y"
               ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
               ,thresh = 0.95)

xtable(summary(parA)$coefficients[,1:2], digits = 3); round(summary(parA)$adj.r.squared, 2)
xtable(summary(parA2)$coefficients[,1:2], digits = 3); round(summary(parA2)$adj.r.squared, 2)
xtable(summary(parB)$coefficients[,1:2], digits = 3); round(summary(parB)$adj.r.squared, 2)
xtable(summary(parTI)$coefficients[,1:2], digits = 3); round(summary(parTI)$adj.r.squared, 2)


# Moran's I: all
res.dist <- as.matrix(dist(cbind(datA$Lon, datA$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(parA), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level
Moran.I(resid(parA2), res.dist.inv)


# Moran's I: background
res.dist <- as.matrix(dist(cbind(datB$Lon, datB$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(parB), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level


# Moran's I: traffic/industrial
res.dist <- as.matrix(dist(cbind(datTI$Lon, datTI$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(parTI), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at alpha=0.1 significance level







###
### LUR modeling based on additive regression smoother (no pre-selection of predictors)
###


smoothA  <- smoothLUR(data = datA
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)

smoothB  <- smoothLUR(data = datB
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)

smoothTI <- smoothLUR(data = datTI
                      ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                             #					,"Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "PopDens"
                             , "PriRoad", "SecRoad", "FedAuto", "LocRoute")
                      ,spVar1 = "Lon"
                      ,spVar2 = "Lat"
                      ,y = "Y"
                      ,thresh = 0.95)



# Moran's I: all
res.dist <- as.matrix(dist(cbind(datA$Lon, datA$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(smoothA), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level


# Moran's I: background
res.dist <- as.matrix(dist(cbind(datB$Lon, datB$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(smoothB), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at every reasonable significance level


# Moran's I: traffic/industrial
res.dist <- as.matrix(dist(cbind(datTI$Lon, datTI$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(smoothTI), res.dist.inv)
# the null that there is no spatial autocorrelation in the error terms is rejected at alpha=0.1 significance level


# Adjusted R square
round(summary(smoothA)$r.sq,2)
round(summary(smoothB)$r.sq,2)
round(summary(smoothTI)$r.sq,2)

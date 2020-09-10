#########################################################################################
## Estimating parametric and smooth LUR models in dependence of monitoring sites' type
#########################################################################################


## we use approximated population density based on data from BKG and function gArea()


# setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")

rm(list = ls())

# install.packages("smoothLUR")
library(smoothLUR)
#	install.packages("ape")
library(ape) # for calculation of Moran's I
# null hypothesis: residuals do not exhibit spatial autocorrelation


dat <- read.csv("DATA_MonitoringSites_DE.csv", header = TRUE)[,-1]



# All monitoring sites

parA <- escapeLUR(data = dat
                  ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                            ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "popDens"
                            ,"PriRoad", "SecRoad", "NatMot", "LocRoute")
                  ,depVar = "AQeYMean"
                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95)
summary(parA)

smoothA <- smoothLUR(data = dat
                     ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                               ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                               ,"Constr", "UrbGreen", "Agri", "Forest"
                               , "popDens", "PriRoad", "SecRoad", "NatMot"
                               , "LocRoute")
                     ,spVar1 = "AQeLon"
                     ,spVar2 = "AQeLat"
                     ,depVar = "AQeYMean"
                     ,thresh = 0.95)

summary(parA)$adj.r.squared
summary(smoothA)$r.sq

BIC(parA); AIC(parA)
BIC(smoothA); AIC(smoothA)

datA <- dat
res.dist		<- as.matrix(dist(cbind(dat$AQeLon, dat$AQeLat)))
res.dist.inv	<- 1/(res.dist^2)
diag(res.dist.inv)	<- 0
Moran.I(resid(parA), res.dist.inv)
Moran.I(resid(smoothA), res.dist.inv)



# Background monitoring sites

par0 <- escapeLUR(data = dat[dat$AQeType=="background", ]
                  ,pred = c("HighDens", "LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
                           ,"UrbGreen", "Agri", "Forest", "popDens"
                           ,"PriRoad", "SecRoad", "NatMot", "LocRoute")
                 ,depVar = "AQeYMean"
                 ,dirEff = c(1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                 ,thresh = 0.95)
summary(par0)

parB <- escapeLUR(data = dat[dat$AQeType=="background", ]
                  ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                            ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
                            ,"UrbGreen", "Agri", "Forest", "popDens"
                            ,"PriRoad", "SecRoad", "NatMot", "LocRoute")
                  ,depVar = "AQeYMean"
                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                  ,thresh = 0.95)
summary(parB)

smoothB <- smoothLUR(data = dat[dat$AQeType=="background", ]
                     ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                               ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                               ,"Constr", "UrbGreen", "Agri", "Forest"
                               , "popDens", "PriRoad", "SecRoad", "NatMot"
                               , "LocRoute")
                     ,spVar1 = "AQeLon"
                     ,spVar2 = "AQeLat"
                     ,depVar = "AQeYMean"
                     ,thresh = 0.95)

summary(parB)$adj.r.squared
summary(smoothB)$r.sq

AIC(par0)
BIC(parB); AIC(parB)
BIC(smoothB); AIC(smoothB)

datB <- dat[dat$AQeType == "background", ]
res.dist		<- as.matrix(dist(cbind(datB$AQeLon, datB$AQeLat)))
res.dist.inv	<- 1/(res.dist^2)
diag(res.dist.inv)	<- 0
Moran.I(resid(par0), res.dist.inv)
Moran.I(resid(parB), res.dist.inv)
Moran.I(resid(smoothB), res.dist.inv)



# Traffic/Industrial monitoring sites

parTI <- escapeLUR(data = dat[dat$AQeType!="background", ]
                   ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                             ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
                             ,"UrbGreen", "Agri", "Forest", "popDens"
                             ,"PriRoad", "SecRoad", "NatMot", "LocRoute")
                   ,depVar = "AQeYMean"
                   ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                   ,thresh = 0.95)
summary(parTI)

smoothTI <- smoothLUR(data = dat[dat$AQeType!="background", ]
                      ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                ,"Constr", "UrbGreen", "Agri", "Forest"
                                , "popDens", "PriRoad", "SecRoad", "NatMot"
                                , "LocRoute")
                      ,spVar1 = "AQeLon"
                      ,spVar2 = "AQeLat"
                      ,depVar = "AQeYMean"
                      ,thresh = 0.95)

summary(parTI)$adj.r.squared
summary(smoothTI)$r.sq

BIC(parTI); AIC(parTI)
BIC(smoothTI); AIC(smoothTI)

datTI <- dat[dat$AQeType != "background", ]
res.dist		<- as.matrix(dist(cbind(datTI$AQeLon, datTI$AQeLat)))
res.dist.inv	<- 1/(res.dist^2)
diag(res.dist.inv)	<- 0
Moran.I(resid(parTI), res.dist.inv)
Moran.I(resid(smoothTI), res.dist.inv)

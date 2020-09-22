#########################################################################################
### Conditional mean NO2 surface modeling: MLRs and GAMs for German background sites ----
#########################################################################################





# setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")

rm(list = ls())

#	install.packages("mgcv")
library(mgcv)
#	install.packages("xtable")
library(xtable)
library(smoothLUR)




#	DATA <- read.csv("R/DATA/Data_built/DATA.csv", header=TRUE)[, -1]
#	dat <- DATA[,c(2,5:7,11:20,26:30)]
load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(2,5:8,10:24)]


#rename some columns
names(dat)[c(1:4)] <- c("Y", "Lon", "Lat", "Alt")


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
t(apply(dat[,-5], MARGIN = 2, FUN = function(x){
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

dat0.B	<- dat[dat$AQeType == "background", -c(2:4, 10:12)]
dat0.TI	<- dat[(dat$AQeType == "traffic" | dat$AQeType == "industrial"), -c(2:4, 10:12)]
dat0.A	<- dat[, -c(2:4, 10:12)]



#input parameters for escapeLUR function when including only structural effects
dirEff0	<- c(rep(1,4),-1,0,-1,rep(1,5))

m0.B		<- escapeLUR(data = dat0.B, depVar = "Y", pred = paste(names(dat0.B)[-c(1:2)]), dirEff = dirEff0)
m0.TI		<- escapeLUR(data = dat0.TI, depVar = "Y", pred = paste(names(dat0.TI)[-c(1:2)]), dirEff = dirEff0)
m0.A		<- escapeLUR(data = dat0.A, depVar = "Y", pred = paste(names(dat0.A)[-c(1:2)]), dirEff = dirEff0)



xtable(summary(m0.B)$coefficients[,1:2], digits = 4)
xtable(summary(m0.TI)$coefficients[,1:2], digits = 4)
xtable(summary(m0.A)$coefficients[,1:2], digits = 4)





## Estimate parametric model with structural and spatial predictors (par) ----

dat.B		<- dat[dat$AQeType == "background", -c(10:12)]
dat.TI	<- dat[(dat$AQeType == "traffic" | dat$AQeType == "industrial"), -c(10:12)]
dat.A		<- dat[, -c(10:12)]


#input parameters for escapeLUR function when including only structural effects
dirEff	<- c(rep(0, times = 3), rep(1,4),-1,0,-1,rep(1,5))

m.B		<- escapeLUR(data = dat.B, depVar = "Y", pred = paste(names(dat.B)[-c(1,5)]), dirEff = dirEff)
m.TI		<- escapeLUR(data = dat.TI, depVar = "Y", pred = paste(names(dat.TI)[-c(1,5)]), dirEff = dirEff)
m.A		<- escapeLUR(data = dat.A, depVar = "Y", pred = paste(names(dat.A)[-c(1,5)]), dirEff = dirEff)







### background sites: parB


# 1. Identify start model ----
# Run univariate regressions and choose predictor with hightest adjusted R^2
for(i in 1:nrow(df_start)){
  dat.tmp <- datB[ , -c(1:2)]
  lm.tmp <- lm(Y ~ dat.tmp[ , i], data = datB)
  df_start$EstCoeff[i] <- lm.tmp$coefficients[2]
  df_start$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}

df_start[which.max(df_start$AdjR2), ]
# the sign of the estimated effect goes in line with the expected sign
adjR2_start <- df_start[which.max(df_start$AdjR2), "AdjR2"]



# 2. and 3. Add consecutively further variable ----


# Adding a second variable ----

df_select2	<- df_start[-which.max(df_start$AdjR2), ]
col.2		<- which(names(datB) == df_start[which.max(df_start$AdjR2), "Predictor"])
for(i in 1:nrow(df_select2)){
  dat.tmp	<- datB[ , -c(1:2, col.2)]
  lm.tmp	<- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"], df_select2[i,1]), collapse = "+"), sep = "~" ) ), data = datB)

  df_select2$EstCoeff[i]	<- lm.tmp$coefficients[3]
  df_select2$AdjR2[i]		<- summary(lm.tmp)$`adj.r.squared`
}
df_select2[which.max(df_select2$AdjR2),]
adjR2_select2 <- df_select2[which.max(df_select2$AdjR2), "AdjR2"]
adjR2_select2 - adjR2_start > 0.01
coefficients(lm(Y ~ PopDens, data = datB))
coefficients(lm(Y ~ PopDens + Forest, data = datB))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Forest has the expected sign
# c) estimated coefficient for PopDens still positive



# Adding a third variable ----

df_select3 <- df_select2[-which.max(df_select2$AdjR2), ]
col.3 <- which(names(datB) == df_select2[which.max(df_select2$AdjR2), "Predictor"])
for(i in 1:nrow(df_select3)){
  dat.tmp <- datB[ , -c(1, col.2, col.3)]
  lm.tmp <- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"],
		df_select2[which.max(df_select2$AdjR2), "Predictor"], df_select3[i,1]), collapse = "+"), sep = "~" ) ), data = datB)
  df_select3$EstCoeff[i] <- lm.tmp$coefficients[4]
  df_select3$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select3[which.max(df_select3$AdjR2), ]
adjR2_select3 <- df_select3[which.max(df_select3$AdjR2), "AdjR2"]
adjR2_select3 - adjR2_select2 > 0.01
coefficients(lm(Y ~ PopDens + Forest, data = datB))
coefficients(lm(Y ~ PopDens + Forest + Agri, data = datB))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Agri has the expected sign
# c) sign of estimated coefficients for PopDens and Forest still positive



# Adding a fourth variable ----

df_select4 <- df_select3[-which.max(df_select3$AdjR2), ]
col.4 <- which(names(datB) == df_select3[which.max(df_select3$AdjR2), "Predictor"])

for(i in 1:nrow(df_select4)){
  dat.tmp <- datB[ , -c(1, col.2, col.3, col.4)]
  lm.tmp <- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"],
		df_select2[which.max(df_select2$AdjR2), "Predictor"], df_select3[which.max(df_select3$AdjR2), "Predictor"],
		df_select4[i,1]), collapse = "+"), sep = "~" ) ), data = datB)
  df_select4$EstCoeff[i] <- lm.tmp$coefficients[5]
  df_select4$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select4[which.max(df_select4$AdjR2), ]
adjR2_select4 <- df_select4[which.max(df_select4$AdjR2), "AdjR2"]
adjR2_select4 - adjR2_select3 > 0.01
coefficients(lm(Y ~ PopDens + Forest + Agri, data = datB))
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto, data = datB))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for FedAuto has the expected sign
# c) sign of estimated coefficients for PopDens, Forest, and Agri still the same




# Adding a fifth variable ----

df_select5 <- df_select4[-which.max(df_select4$AdjR2), ]
col.5 <- which(names(datB) == df_select4[which.max(df_select4$AdjR2), "Predictor"])

for(i in 1:nrow(df_select5)){
  dat.tmp <- datB[ , -c(1, col.2, col.3, col.4, col.5)]
  lm.tmp <- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"],
		df_select2[which.max(df_select2$AdjR2), "Predictor"], df_select3[which.max(df_select3$AdjR2), "Predictor"],
		df_select4[which.max(df_select4$AdjR2), "Predictor"], df_select5[i,1]), collapse = "+"), sep = "~" ) ), data = datB)
  df_select5$EstCoeff[i] <- lm.tmp$coefficients[6]
  df_select5$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select5[which.max(df_select5$AdjR2), ]
adjR2_select5 <- df_select5[which.max(df_select5$AdjR2), "AdjR2"]
adjR2_select5 - adjR2_select4 > 0.01
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto, data = datB))
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens, data = datB))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for HighDens has the expected sign
# c) sign of estimated coefficients for PopDens, Forest, Agri, and FedAuto still the same




# Adding a sixth variable ----

df_select6 <- df_select5[-which.max(df_select5$AdjR2), ]
col.6 <- which(names(datB) == df_select5[which.max(df_select5$AdjR2), "Predictor"])

for(i in 1:nrow(df_select6)){
  dat.tmp <- datB[ , -c(1, col.2, col.3, col.4, col.5, col.6)]
  lm.tmp <- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"],
		df_select2[which.max(df_select2$AdjR2), "Predictor"], df_select3[which.max(df_select3$AdjR2), "Predictor"],
		df_select4[which.max(df_select4$AdjR2), "Predictor"], df_select5[which.max(df_select5$AdjR2), "Predictor"],
		df_select6[i,1]), collapse = "+"), sep = "~" ) ), data = datB)
  df_select6$EstCoeff[i] <- lm.tmp$coefficients[7]
  df_select6$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select6[which.max(df_select6$AdjR2), ]
adjR2_select6 <- df_select6[which.max(df_select6$AdjR2), "AdjR2"]
adjR2_select6 - adjR2_select5 > 0.01
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens, data = datB))
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind, data = datB))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Alt has the expected sign
# c) sign of estimated coefficients for popDens, Forest, Agri, FedAuto and HighDens still the same





# Adding a seventh variable ----

df_select7 <- df_select6[-which.max(df_select6$AdjR2), ]
col.7 <- which(names(datB) == df_select6[which.max(df_select6$AdjR2), "Predictor"])

for(i in 1:nrow(df_select7)){
  dat.tmp <- datB[ , -c(1, col.2, col.3, col.4, col.5, col.6, col.7)]
  lm.tmp <- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"],
		df_select2[which.max(df_select2$AdjR2), "Predictor"], df_select3[which.max(df_select3$AdjR2), "Predictor"],
		df_select4[which.max(df_select4$AdjR2), "Predictor"], df_select5[which.max(df_select5$AdjR2), "Predictor"],
		df_select6[which.max(df_select6$AdjR2), "Predictor"], df_select7[i,1]), collapse = "+"), sep = "~" ) ), data = datB)
  df_select7$EstCoeff[i] <- lm.tmp$coefficients[8]
  df_select7$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select7[which.max(df_select7$AdjR2), ]
adjR2_select7 <- df_select7[which.max(df_select7$AdjR2), "AdjR2"]
adjR2_select7 - adjR2_select6 > 0.01
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind, data = datB))
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind + LowDens, data = datB))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Alt has the expected sign
# c) sign of estimated coefficients for popDens, Forest, Agri, FedAuto, HighDens, and Ind still the same




# Adding an eighth variable ----

df_select8 <- df_select7[-which.max(df_select7$AdjR2), ]
col.8 <- which(names(datB) == df_select7[which.max(df_select7$AdjR2), "Predictor"])

for(i in 1:nrow(df_select8)){
  dat.tmp <- datB[ , -c(1, col.2, col.3, col.4, col.5, col.6, col.7, col.8)]
  lm.tmp <- lm(as.formula(paste("Y", paste(c(df_start[which.max(df_start$AdjR2), "Predictor"],
		df_select2[which.max(df_select2$AdjR2), "Predictor"], df_select3[which.max(df_select3$AdjR2), "Predictor"],
		df_select4[which.max(df_select4$AdjR2), "Predictor"], df_select5[which.max(df_select5$AdjR2), "Predictor"],
		df_select6[which.max(df_select6$AdjR2), "Predictor"], df_select7[which.max(df_select7$AdjR2), "Predictor"],
		df_select8[i,1]), collapse = "+"), sep = "~" ) ), data = datB)
  df_select8$EstCoeff[i] <- lm.tmp$coefficients[9]
  df_select8$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select8[which.max(df_select8$AdjR2), ]
adjR2_select8 <- df_select8[which.max(df_select8$AdjR2), "AdjR2"]
adjR2_select8 - adjR2_select7 > 0.01
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind + LowDens, data = datB))
coefficients(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind + LowDens + PriRoad, data = datB))
# a) adj R^2 increase by more than 1 %: not fulfilled
# b) estimated coefficient for Alt has the expected sign
# c) sign of estimated coefficients for popDens, Forest, Agri, FedAuto, HighDens, Ind, and LowDens still the same





### --> aborting procedure after step 7 yields model par0






summary(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind + LowDens + PriRoad, data = datB))
# Remove FedAuto
summary(lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind + LowDens, data = datB))


par0 <- lm(Y ~ PopDens + Forest + Agri + FedAuto + HighDens + Ind + LowDens, data = dat)
AIC(par0); BIC(par0)


# Moran's I
library(ape)
res.dist <- as.matrix(dist(cbind(dat$Lon, dat$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(par0), res.dist.inv)
# the null that the residuals do not reveal spatial autocorrelation
# can be rejected at every reasonable significance level





## Estimate parametric model with structural and spatial predictors (par1) ----
names(dat)
dat2 <- dat[,-c(9:11)]


# Pre-specify direction of effect -> + 1, - -1, +/- 0
df_start <-  data.frame(matrix(data = NA, nrow = ncol(dat2)-1, ncol = 4))
names(df_start) <- c("Predictor", "DirEff", "EstCoeff", "AdjR2")
df_start$Predictor <- names(dat2)[-1]
df_start$DirEff <- c(0,0,-1,rep(1,4),-1,0,-1,rep(1,5))


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



# 2. and 3. Add consecutively further predictor ----


# Adding a second predictor ----

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
coefficients(lm(Y ~ popDens, data = dat))
coefficients(lm(Y ~ popDens + Forest, data = dat))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Forest has the expected sign
# c) estimated coefficient for popDens still positive


# Adding a third predictor ----

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
coefficients(lm(Y ~ popDens + Forest, data = dat))
coefficients(lm(Y ~ popDens + Forest + Lat, data = dat))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Lat has the expected sign
# c) sign of estimated coefficients for popDens and Forest still positive and negative


# Adding a fourth predictor ----

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
coefficients(lm(Y ~ popDens + Forest + Lat, data = dat))
coefficients(lm(Y ~ popDens + Forest + Lat + Alt, data = dat))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Alt has the expected sign
# c) sign of estimated coefficients for popDens, Forest and Lat still the same


# Adding a fifth predictor ----

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
coefficients(lm(Y ~ popDens + Forest + Lat + Alt, data = dat))
coefficients(lm(Y ~ popDens + Forest + Lat + Alt + Agri, data = dat))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for Agri has the expected sign
# c) sign of estimated coefficients for popDens, Forest, Lat and Alt still the same


# Adding a sixth predictor ----

df_select6 <- df_select5[-which.max(df_select5$AdjR2), ]
col.6 <- which(names(dat2) == df_select5[which.max(df_select5$AdjR2), "Predictor"])

for(i in 1:nrow(df_select6)){
  dat.tmp <- dat2[ , -c(1, col.2, col.3, col.4, col.5, col.6)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] +
                 dat[ , df_select2[which.max(df_select2$AdjR2), "Predictor"]] + 
                 dat[ , df_select3[which.max(df_select3$AdjR2), "Predictor"]] + 
                 dat[ , df_select4[which.max(df_select4$AdjR2), "Predictor"]] +
                 dat[ , df_select5[which.max(df_select5$AdjR2), "Predictor"]] + dat.tmp[ , i])
  df_select6$EstCoeff[i] <- lm.tmp$coefficients[7]
  df_select6$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select6[which.max(df_select6$AdjR2), ]
adjR2_select6 <- df_select6[which.max(df_select6$AdjR2), "AdjR2"]
adjR2_select6 - adjR2_select5 > 0.01
coefficients(lm(Y ~ popDens + Forest + Lat + Alt + Agri, data = dat))
coefficients(lm(Y ~ popDens + Forest + Lat + Alt + Agri + NatMot, data = dat))
# a) adj R^2 increase by more than 1 %
# b) estimated coefficient for NatMot has the expected sign
# c) sign of estimated coefficients for popDens, Forest, Lat, Alt and Agri still the same


# Adding a seventh predictor ----

df_select7 <- df_select6[-which.max(df_select6$AdjR2), ]
col.7 <- which(names(dat2) == df_select6[which.max(df_select6$AdjR2), "Predictor"])

for(i in 1:nrow(df_select7)){
  dat.tmp <- dat2[ , -c(1, col.2, col.3, col.4, col.5, col.6, col.7)]
  lm.tmp <- lm(dat$Y ~ dat[ , df_start[which.max(df_start$AdjR2), "Predictor"]] +
                 dat[ , df_select2[which.max(df_select2$AdjR2), "Predictor"]] + 
                 dat[ , df_select3[which.max(df_select3$AdjR2), "Predictor"]] + 
                 dat[ , df_select4[which.max(df_select4$AdjR2), "Predictor"]] +
                 dat[ , df_select5[which.max(df_select5$AdjR2), "Predictor"]] + 
                 dat[ , df_select6[which.max(df_select6$AdjR2), "Predictor"]] + dat.tmp[ , i])
  df_select7$EstCoeff[i] <- lm.tmp$coefficients[8]
  df_select7$AdjR2[i] <- summary(lm.tmp)$`adj.r.squared`
}
df_select7[which.max(df_select7$AdjR2), ]
adjR2_select7 <- df_select7[which.max(df_select7$AdjR2), "AdjR2"]
adjR2_select7 - adjR2_select6 > 0.01
# b) estimated coefficient for HighDens has the expected sign
# a) but HighDens does not lead to an increase in adjR^2 larger than 1 %.

# At this point one would stop and turn to step 3 of the supervised forward selction procedure
# 4. Remove predictors with p-values > 0.1 sequentially from model. ----
summary(lm(Y ~ popDens + Forest + Lat + Alt + Agri + NatMot, data = dat))

# here: all p-values < 0.1

# Resulting linear parametric LUR ----
# The linear parametric land use regression model with predictor pre-selection
# through the standardised supervised forward stepwise procedure according to ESCAPE is given by:
par1 <- lm(Y ~ popDens + Forest + Lat + Alt + Agri + NatMot, data = dat)
summary(par1)
summary(par1)$adj.r.squared
BIC(par1); AIC(par1)


# Model diagnostics ----

# Variance inflation factors
vif(par1)
# for comparison
1/(1-summary(par1)$r.squared)
# there are no predictor predictors vif above 3


# Cook's distance
summary(cooks.distance(par1))
plot(cooks.distance(par1))
# there is no observation with Cook's distance above 1


# Moran's I
library(ape)
res.dist <- as.matrix(dist(cbind(dat$Lon, dat$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(par1), res.dist.inv)
# the null that the residuals do not reveal spatial autocorrelation
# can be rejected at every reasonable significance level


# Heteroskedasticity
par(mfrow = c(1,2))
plot(resid(par1) ~ fitted(par1), pch = 16, cex = 0.7)
abline(h = 0, lwd = 2, col = "blue")
plot(dat$Y ~ fitted(par1), pch = 16, cex = 0.7)
abline(c(1,1), lwd = 2, col = "blue")
par(mfrow = c(2,2))
plot(par1)
par(mfrow = c(1,1))
# plots indicate heteroskedasticity
library(lmtest)
bptest(par1)
ncvTest(par1)
# tests confirm our expectations


# Normality?
qqnorm(resid(par1))
qqline(resid(par1), col = 2)
# in the lower and upper tails a deviation from the normal distribution is visible




# Deriving a semiparametric land use regression model without predictor pre-selection ----
# Allow for univariate and bivariat thin-plate regression splines in a generalized additive model (GAM).


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



# GAM with univariate spline for every predictor ----
form.gam1 <- Y ~ s(Lon, k=-1, bs="tp") +         #  k = -1 for automatic smoothness selection
  s(Lat, k=-1, bs="tp") +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
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

gam1 <- gam(formula=form.gam1, fit=TRUE, method="P-ML", data=dat, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam1)
summary(gam1)
summary(gam1)$r.sq
summary(par1)$adj.r.squared
BIC(gam1); AIC(gam1)




# GAM with bivariate spline for lon and lat and univariate spline for remaining predictors ----
form.gam2.3 <- Y ~ s(Lon, Lat, k=-1, bs="tp", fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
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



gam2.3 <- gam(formula=form.gam2.3, fit=TRUE, method="P-ML", data=dat, family=gaussian(),
              weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
              select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.3)
summary(gam2.3)
summary(gam2.3)$r.sq
BIC(gam2.3); AIC(gam2.3)

semipar <- gam2.3

# Eliminating predictors with edf negligible low (smoothed out) ----
form.gam2.4 <- Y ~ s(Lon, Lat, k=-1, bs="tp",	fx=FALSE, xt=NULL, id=NULL, sp=NULL) +
  s(Alt, k=-1, bs="tp") +
  s(HighDens, k=-1, bs="tp") +
  s(LowDens, k=-1, bs="tp") +
  s(Ind, k=-1, bs="tp") +
  # s(Transp, k=-1, bs="tp") +      # smoothed out in gam2.3
  # s(Seap, k=-1, bs="tp") + 				# insufficient amount of data for flexible estimation
  # s(Airp, k=-1, bs="tp") +        # insufficient amount of data for flexible estimation
  # s(Constr, k=-1, bs="tp") +      # insufficient amount of data for flexible estimation
  # s(UrbGreen, k=-1, bs="tp") +    # smoothed out in gam2.3
  # s(Agri, k=-1, bs="tp") +        # smoothed out in gam2.3
  s(Forest, k=-1, bs="tp") +
  s(popDens, k=-1, bs="tp") +
  s(PriRoad, k=-1, bs="tp") +
  s(SecRoad, k=-1, bs="tp") +
  s(NatMot, k=-1, bs="tp") #+
  # s(LocRoute, k=-1, bs="tp")      # smoothed out in gam2.3

gam2.4 <- gam(formula=form.gam2.4, fit=TRUE, method="P-ML", data=dat, family=gaussian(),
			weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
			select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

gam.check(gam2.4)
summary(gam2.4)
summary(gam2.4)$r.sq
BIC(gam2.4)
AIC(gam2.4)




# Model diagnositcs for semipar ----


# Cook's distance
summary(cooks.distance(semipar))
plot(cooks.distance(semipar))
# there is no observation with Cook's distance above 1


# Moran's I
library(ape)
res.dist <- as.matrix(dist(cbind(dat$Lon, dat$Lat)))
res.dist.inv <- 1/(res.dist^2)
diag(res.dist.inv) <- 0
Moran.I(resid(semipar), res.dist.inv)
# the null that the residuals do not reveal spatial autocorrelation
# cannot be rejected at any reasonable significance level


# Heteroskedasticity
par(mfrow = c(1,2))
plot(resid(semipar) ~ fitted(semipar), pch = 16, cex = 0.7)
abline(h = 0, lwd = 2, col = "blue")
plot(dat$Y ~ fitted(semipar), pch = 16, cex = 0.7)
abline(c(1,1), lwd = 2, col = "blue")
par(mfrow = c(2,5))
plot(semipar)
par(mfrow = c(1,1))
# plots indicate heteroskedasticity
library(lmtest)
bptest(semipar)
# tests confirm our expectations


# Normality?
qqnorm(resid(semipar))
qqline(resid(semipar), col = 2)
# in the upper tail a deviation from the normal distribution is visible

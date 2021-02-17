#########################################################################################
## Validation LUR models based on parametric polynomials and LUR models based on
## additive regression smoothers in dependence of monitoring sites' type
#########################################################################################


# Validation techniques:
# loocv, (stratified) 10-fold cv, and (stratified) hold-out validation


rm(list = ls())



#	install.packages("devtools")
# library(devtools)
#	install_github("https://github.com/markusfritsch/smoothLUR")			# M: works after public release of package
library(smoothLUR)





load("data/monSitesDE.rda")
dat	<- monSitesDE[, c(1,2,4:7,9:24)]



datA    <- dat
datB		<- dat[dat$AQeType == "background", ]
datTI		<- dat[dat$AQeType != "background", ]





#####################
## Background sites
#####################




###
### Leave-one-out cross-validation
###

loocv.b <- kFoldCV(data = datB
                   ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                   ,ID = "AQeCode"
                   ,spVar1 = "Lon"
                   ,spVar2 = "Lat"
                   ,y = "Y"
                   ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                   ,thresh = 0.95
                   ,seed = 1
                   ,k = 10
                   ,strat = FALSE
                   ,indRegions = "IndRegions"
                   ,loocv = TRUE
                   )

rmse_looCV_par_b	<- sqrt(mean(loocv.b$df.err$Err.par^2))
mae_looCV_par_b	  <- mean(abs(loocv.b$df.err$Err.par))
adjR2_looCV_par_b	<-  mean(sapply(loocv.b$ls.models, FUN = function(f) summary(f$mod.par)$adj.r.squared))

rmse_looCV_smooth_b	  <- sqrt(mean(loocv.b$df.err$Err.smooth^2))
mae_looCV_smooth_b  	<- mean(abs(loocv.b$df.err$Err.smooth))
adjR2_looCV_smooth_b	<- mean(sapply(loocv.b$ls.models, FUN = function(f) summary(f$mod.smooth)$r.sq))






###
### 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listTenFoldCV.b		<- vector("list", length = 100)
names(listTenFoldCV.b)	<- paste("seed",1:100,sep="")

for(l in 1:100){
  listTenFoldCV.b[[l]] <- kFoldCV(data = datB
                                  ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#							,"Seap", "Airp", "Constr"
							,"UrbGreen", "Agri", "Forest", "PopDens"
							,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                                  ,ID = "AQeCode"
                                  ,spVar1 = "Lon"
                                  ,spVar2 = "Lat"
                                  ,y = "Y"
                                  ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                  ,thresh = 0.95
                                  ,seed = l
                                  ,k = 10
                                  ,strat = FALSE
                                  ,indRegions = "IndRegions"
                                  ,loocv = FALSE
                                  )
}

rmse_TenFoldCV_par_b	<- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_TenFoldCV_par_b	<- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par)))
adjR2_TenFoldCV_par_b	<- sapply(listTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp		<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_TenFoldCV_smooth_b 	<- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_TenFoldCV_smooth_b  	<- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
adjR2_TenFoldCV_smooth_b	<- sapply(listTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_TenFoldCV_par_b); mean(rmse_TenFoldCV_smooth_b)
mean(mae_TenFoldCV_par_b); mean(mae_TenFoldCV_smooth_b)
mean(adjR2_TenFoldCV_par_b); mean(adjR2_TenFoldCV_smooth_b)





###
### Hold-out validation
###

# can easily be derived based on the results of 10-fold CV by using only Fold1 as test sample

rmse_HoldOutCV_par_b	<- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_HoldOutCV_par_b 	<- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
adjR2_HoldOutCV_par_b	<- sapply(listTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_HoldOutCV_smooth_b		<- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_HoldOutCV_smooth_b		<- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
adjR2_HoldOutCV_smooth_b	<- sapply(listTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_HoldOutCV_par_b); mean(rmse_HoldOutCV_smooth_b)
mean(mae_HoldOutCV_par_b); mean(mae_HoldOutCV_smooth_b)
mean(adjR2_HoldOutCV_par_b); mean(adjR2_HoldOutCV_smooth_b)





###
### Stratified 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listStratTenFoldCV.b		<- vector("list", length = 100)
names(listStratTenFoldCV.b)	<- paste("seed",1:100,sep="")

for(l in 1:100){
  listStratTenFoldCV.b[[l]]	<- kFoldCV(data = datB
						,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#							,"Seap", "Airp", "Constr"
							,"UrbGreen", "Agri", "Forest", "PopDens"
							,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
						,ID = "AQeCode"
						,spVar1 = "Lon"
						,spVar2 = "Lat"
						,y = "Y"
            ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
            ,thresh = 0.95
						,seed = l
						,k = 10
						,strat = TRUE
						,indRegions = "IndRegions"
						,loocv = FALSE
  					)
}

rmse_StratTenFoldCV_par_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_StratTenFoldCV_par_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par)))
adjR2_StratTenFoldCV_par_b	<- sapply(listStratTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_StratTenFoldCV_smooth_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_StratTenFoldCV_smooth_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
adjR2_StratTenFoldCV_smooth_b	<- sapply(listStratTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_StratTenFoldCV_par_b); mean(rmse_StratTenFoldCV_smooth_b)
mean(mae_StratTenFoldCV_par_b); mean(mae_StratTenFoldCV_smooth_b)
mean(adjR2_StratTenFoldCV_par_b); mean(adjR2_StratTenFoldCV_smooth_b)





###
### Stratified hold-out validation
###

# can easily be derived based on the results of stratified hold-out CV by using only Fold1 as test sample

rmse_StratHoldOutCV_par_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_par_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
adjR2_StratHoldOutCV_par_b	<- sapply(listStratTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_StratHoldOutCV_smooth_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_smooth_b	<- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
adjR2_StratHoldOutCV_smooth_b	<- sapply(listStratTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_StratHoldOutCV_par_b); mean(rmse_StratHoldOutCV_smooth_b)
mean(mae_StratHoldOutCV_par_b); mean(mae_StratHoldOutCV_smooth_b)
mean(adjR2_StratHoldOutCV_par_b); mean(adjR2_StratHoldOutCV_smooth_b)


save.image("cvResultsB_2021-01-27.RData")








#####################
## Traffic/Industrial sites
#####################



###
### Leave-one-out cross-validation
###
loocv.TI <- kFoldCV(data = datTI
                   ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
                          #					,"Seap", "Airp", "Constr"
				                	,"UrbGreen", "Agri", "Forest", "PopDens"
				                	,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
                   ,ID = "AQeCode"
                   ,spVar1 = "Lon"
                   ,spVar2 = "Lat"
                   ,y = "Y"
                   ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                   ,thresh = 0.95
                   ,seed = 1
                   ,k = 10
                   ,strat = FALSE
                   ,indRegions = "IndRegions"
                   ,loocv = TRUE
			)

rmse_looCV_par_TI		<- sqrt(mean(loocv.TI$df.err$Err.par^2))
mae_looCV_par_TI		<- mean(abs(loocv.TI$df.err$Err.par))
adjR2_looCV_par_TI	<- mean(sapply(loocv.TI$ls.models, FUN = function(f) summary(f$mod.par)$adj.r.squared))

rmse_looCV_smooth_TI	<- sqrt(mean(loocv.TI$df.err$Err.smooth^2))
mae_looCV_smooth_TI  	<- mean(abs(loocv.TI$df.err$Err.smooth))
adjR2_looCV_smooth_TI	<- mean(sapply(loocv.TI$ls.models, FUN = function(f) summary(f$mod.smooth)$r.sq))






###
### 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listTenFoldCV.TI <- vector("list", length = 100)
names(listTenFoldCV.TI) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listTenFoldCV.TI[[l]] <- kFoldCV(data = datTI
			,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
			,ID = "AQeCode"
			,spVar1 = "Lon"
			,spVar2 = "Lat"
			,y = "Y"
      ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
      ,thresh = 0.95
			,seed = l
			,k = 10
			,strat = FALSE
			,indRegions = "IndRegions"
			,loocv = FALSE
  			)
}

rmse_TenFoldCV_par_TI	<- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_TenFoldCV_par_TI	<- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par)))
adjR2_TenFoldCV_par_TI	<- sapply(listTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp		<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_TenFoldCV_smooth_TI	<- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_TenFoldCV_smooth_TI		<- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
adjR2_TenFoldCV_smooth_TI	<- sapply(listTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_TenFoldCV_par_TI); mean(rmse_TenFoldCV_smooth_TI)
mean(mae_TenFoldCV_par_TI); mean(mae_TenFoldCV_smooth_TI)
mean(adjR2_TenFoldCV_par_TI); mean(adjR2_TenFoldCV_smooth_TI)





###
### Hold-out validation
###

# can easily be derived based on the results of 10-fold CV by using only Fold1 as test sample

rmse_HoldOutCV_par_TI	<- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_HoldOutCV_par_TI	<- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
adjR2_HoldOutCV_par_TI	<- sapply(listTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_HoldOutCV_smooth_TI	<- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_HoldOutCV_smooth_TI		<- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
adjR2_HoldOutCV_smooth_TI	<- sapply(listTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_HoldOutCV_par_TI); mean(rmse_HoldOutCV_smooth_TI)
mean(mae_HoldOutCV_par_TI); mean(mae_HoldOutCV_smooth_TI)
mean(adjR2_HoldOutCV_par_TI); mean(adjR2_HoldOutCV_smooth_TI)






###
### Stratified 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listStratTenFoldCV.TI <- vector("list", length = 100)
names(listStratTenFoldCV.TI) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listStratTenFoldCV.TI[[l]] <- kFoldCV(data = datTI
                   ,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
			,ID = "AQeCode"
			,spVar1 = "Lon"
			,spVar2 = "Lat"
			,y = "Y"
      ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
      ,thresh = 0.95
			,seed = l
			,k = 10
			,strat = TRUE
			,indRegions = "IndRegions"
			,loocv = FALSE
  			)
}

rmse_StratTenFoldCV_par_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_StratTenFoldCV_par_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par)))
adjR2_StratTenFoldCV_par_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_StratTenFoldCV_smooth_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_StratTenFoldCV_smooth_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
adjR2_StratTenFoldCV_smooth_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp				<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_StratTenFoldCV_par_TI); mean(rmse_StratTenFoldCV_smooth_TI)
mean(mae_StratTenFoldCV_par_TI); mean(mae_StratTenFoldCV_smooth_TI)
mean(adjR2_StratTenFoldCV_par_TI); mean(adjR2_StratTenFoldCV_smooth_TI)






###
### Stratified hold-out validation
###

# can easily be derived based on the results of stratified hold-out CV by using only Fold1 as test sample

rmse_StratHoldOutCV_par_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_par_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
adjR2_StratHoldOutCV_par_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_StratHoldOutCV_smooth_TI		<- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_smooth_TI		<- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
adjR2_StratHoldOutCV_smooth_TI	<- sapply(listStratTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_StratHoldOutCV_par_TI); mean(rmse_StratHoldOutCV_smooth_TI)
mean(mae_StratHoldOutCV_par_TI); mean(mae_StratHoldOutCV_smooth_TI)
mean(adjR2_StratHoldOutCV_par_TI); mean(adjR2_StratHoldOutCV_smooth_TI)


save.image("cvResultsTI_2021-01-27.RData")







#####################
### All sites
#####################



###
### Leave-one-out cross-validation
###

loocv.A <- kFoldCV(data = dat
			,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
			,ID = "AQeCode"
			,spVar1 = "Lon"
			,spVar2 = "Lat"
			,y = "Y"
      ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
      ,thresh = 0.95
			,seed = 1
			,k = 10
			,strat = FALSE
			,indRegions = "IndRegions"
			,loocv = TRUE
			)

rmse_looCV_par_A	<- sqrt(mean(loocv.A$df.err$Err.par^2))
mae_looCV_par_A	<- mean(abs(loocv.A$df.err$Err.par))
adjR2_looCV_par_A	<- mean(sapply(loocv.A$ls.models, FUN = function(f) summary(f$mod.par)$adj.r.squared))

rmse_looCV_smooth_A	<- sqrt(mean(loocv.A$df.err$Err.smooth^2))
mae_looCV_smooth_A	<- mean(abs(loocv.A$df.err$Err.smooth))
adjR2_looCV_smooth_A	<- mean(sapply(loocv.A$ls.models, FUN = function(f) summary(f$mod.smooth)$r.sq))






###
### 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listTenFoldCV.A <- vector("list", length = 100)
names(listTenFoldCV.A) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listTenFoldCV.A[[l]] <- kFoldCV(data = dat
			,x = c("Lon", "Lat", "Alt", "HighDens", "LowDens", "Ind", "Transp"
#					,"Seap", "Airp", "Constr"
					,"UrbGreen", "Agri", "Forest", "PopDens"
					,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
			,ID = "AQeCode"
			,spVar1 = "Lon"
			,spVar2 = "Lat"
			,y = "Y"
      ,dirEff = c(0,0,-1,1,1,1,1,-1,0,-1,1,1,1,1,1)
      ,thresh = 0.95
			,seed = l
			,k = 10
			,strat = FALSE
			,indRegions = "IndRegions"
			,loocv = FALSE
  			)
}

rmse_TenFoldCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_TenFoldCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par)))
adjR2_TenFoldCV_par_A  <- sapply(listTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_TenFoldCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_TenFoldCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
adjR2_TenFoldCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_TenFoldCV_par_A); mean(rmse_TenFoldCV_smooth_A)
mean(mae_TenFoldCV_par_A); mean(mae_TenFoldCV_smooth_A)
mean(adjR2_TenFoldCV_par_A); mean(adjR2_TenFoldCV_smooth_A)





###
### Hold-out validation
###

# can easily be derived based on the results of 10-fold CV by using only Fold1 as test sample

rmse_HoldOutCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_HoldOutCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
adjR2_HoldOutCV_par_A  <- sapply(listTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_HoldOutCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_HoldOutCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_HoldOutCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_HoldOutCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_HoldOutCV_par_A); mean(rmse_HoldOutCV_smooth_A)
mean(mae_HoldOutCV_par_A); mean(mae_HoldOutCV_smooth_A)
mean(adjR2_HoldOutCV_par_A); mean(adjR2_HoldOutCV_smooth_A)





###
### Stratified 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listStratTenFoldCV.A <- vector("list", length = 100)
names(listStratTenFoldCV.A) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listStratTenFoldCV.A[[l]] <- kFoldCV(data = dat
                                        ,x = c("Lon", "Lat", "Alt", "HighDens"
                                                  ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                                  ,"Constr", "UrbGreen", "Agri", "Forest"
                                                  ,"PopDens", "PriRoad", "SecRoad", "FedAuto"
                                                  ,"LocRoute")
                                        ,ID = "AQeCode"
                                        ,spVar1 = "Lon"
                                        ,spVar2 = "Lat"
                                        ,y = "Y"
                                        ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                        ,thresh = 0.95
                                        ,seed = l
                                        ,k = 10
                                        ,strat = TRUE
                                        ,indRegions = "IndRegions"
                                        ,loocv = FALSE
  )
}

rmse_StratTenFoldCV_par_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_StratTenFoldCV_par_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par)))
adjR2_StratTenFoldCV_par_A	<- sapply(listStratTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_StratTenFoldCV_smooth_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_StratTenFoldCV_smooth_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
adjR2_StratTenFoldCV_smooth_A	<- sapply(listStratTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp			<- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_StratTenFoldCV_par_A); mean(rmse_StratTenFoldCV_smooth_A)
mean(mae_StratTenFoldCV_par_A); mean(mae_StratTenFoldCV_smooth_A)
mean(adjR2_StratTenFoldCV_par_A); mean(adjR2_StratTenFoldCV_smooth_A)



###
### Stratified hold-out validation
###

# can easily be derived based on the results of stratified hold-out CV by using only Fold1 as test sample

rmse_StratHoldOutCV_par_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_par_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
adjR2_StratHoldOutCV_par_A	<- sapply(listStratTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_StratHoldOutCV_smooth_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_smooth_A	<- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
adjR2_StratHoldOutCV_smooth_A	<- sapply(listStratTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_StratHoldOutCV_par_A); mean(rmse_StratHoldOutCV_smooth_A)
mean(mae_StratHoldOutCV_par_A); mean(mae_StratHoldOutCV_smooth_A)
mean(adjR2_StratHoldOutCV_par_A); mean(adjR2_StratHoldOutCV_smooth_A)


save.image("cvResultsA_2021-01-27.RData")


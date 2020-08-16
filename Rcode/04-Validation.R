#########################################################################################
## Validating parametric and smooth LUR models in dependence of monitoring sites' type
#########################################################################################


# Validation techniques:
# loocv, (stratified) 10-fold cv, and (stratified) hold-out validation

rm(list = ls())

library(smoothLUR)

dat <- read.csv("DATA_monitoringSites_DE.csv", header = TRUE)[,-1]



#####################
## Background sites
#####################



###
## leave-one-out cross-validation
###

loocv.b <- kFoldCV(data = dat[dat$AQeType=="background",]
                   ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                             ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                             ,"Constr", "UrbGreen", "Agri", "Forest"
                             , "popDens", "PriRoad", "SecRoad", "NatMot"
                             , "LocRoute")
                   ,ID = "AQeCode"
                   ,spVar1 = "AQeLon"
                   ,spVar2 = "AQeLat"
                   ,depVar = "AQeYMean"
                   ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                   ,thresh = 0.95
                   ,seed = 1
                   ,k = 10
                   ,strat = FALSE
                   ,indRegions = "indRegions"
                   ,loocv = TRUE
                   )
rmse_looCV_par_b <- sqrt(mean(loocv.b$df.err$Err.par^2))
mae_looCV_par_b <- mean(abs(loocv.b$df.err$Err.par))
aic_looCV_par_b  <- mean(sapply(loocv.b$ls.models, FUN = function(f) AIC(f$mod.par)))
adjR2_looCV_par_b  <-  mean(sapply(loocv.b$ls.models, FUN = function(f) summary(f$mod.par)$adj.r.squared))

rmse_looCV_smooth_b <- sqrt(mean(loocv.b$df.err$Err.smooth^2))
mae_looCV_smooth_b <- mean(abs(loocv.b$df.err$Err.smooth))
aic_looCV_smooth_b <- mean(sapply(loocv.b$ls.models, FUN = function(f) AIC(f$mod.smooth)))
adjR2_looCV_smooth_b  <-  mean(sapply(loocv.b$ls.models, FUN = function(f) summary(f$mod.smooth)$r.sq))






###
## 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listTenFoldCV.b <- vector("list", length = 100)
names(listTenFoldCV.b) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listTenFoldCV.b[[l]] <- kFoldCV(data = dat[dat$AQeType=="background",]
                                  ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                            ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                            ,"Constr", "UrbGreen", "Agri", "Forest"
                                            , "popDens", "PriRoad", "SecRoad", "NatMot"
                                            , "LocRoute")
                                  ,ID = "AQeCode"
                                  ,spVar1 = "AQeLon"
                                  ,spVar2 = "AQeLat"
                                  ,depVar = "AQeYMean"
                                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                  ,thresh = 0.95
                                  ,seed = l
                                  ,k = 10
                                  ,strat = FALSE
                                  ,indRegions = "indRegions"
                                  ,loocv = FALSE
                                  )
}

rmse_TenFoldCV_par_b <- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_TenFoldCV_par_b <- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par)))
aic_TenFoldCV_par_b  <- sapply(listTenFoldCV.b, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.par))
    })
  return(mean(AIC_seed_tmp))
})
adjR2_TenFoldCV_par_b  <- sapply(listTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_TenFoldCV_smooth_b <- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_TenFoldCV_smooth_b <- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
aic_TenFoldCV_smooth_b  <- sapply(listTenFoldCV.b, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.smooth))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_TenFoldCV_smooth_b  <- sapply(listTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_TenFoldCV_par_b); mean(rmse_TenFoldCV_smooth_b)
mean(mae_TenFoldCV_par_b); mean(mae_TenFoldCV_smooth_b)
mean(aic_TenFoldCV_par_b); mean(aic_TenFoldCV_smooth_b)
mean(adjR2_TenFoldCV_par_b); mean(adjR2_TenFoldCV_smooth_b)


plot(density(rmse_TenFoldCV_smooth_b), xlim = c(min(rmse_TenFoldCV_smooth_b),max(rmse_TenFoldCV_par_b)),
     col = "orange", lwd = 2)
lines(density(rmse_TenFoldCV_par_b), col = "blue", lwd = 2)




###
## Hold-out validation
###

# can easily be derived based on the results of 10-fold CV by using only Fold1 as test sample

rmse_HoldOutCV_par_b <- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_HoldOutCV_par_b <- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
aic_HoldOutCV_par_b  <- sapply(listTenFoldCV.b, FUN = function(s) AIC(s$ls.models$Fold1$mod.par))
adjR2_HoldOutCV_par_b  <- sapply(listTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_HoldOutCV_smooth_b <- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_HoldOutCV_smooth_b <- sapply(listTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_HoldOutCV_smooth_b  <- sapply(listTenFoldCV.b, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_HoldOutCV_smooth_b  <- sapply(listTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_HoldOutCV_par_b); mean(rmse_HoldOutCV_smooth_b)
mean(mae_HoldOutCV_par_b); mean(mae_HoldOutCV_smooth_b)
mean(aic_HoldOutCV_par_b); mean(aic_HoldOutCV_smooth_b)
mean(adjR2_HoldOutCV_par_b); mean(adjR2_HoldOutCV_smooth_b)





###
## Stratified 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listStratTenFoldCV.b <- vector("list", length = 100)
names(listStratTenFoldCV.b) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listStratTenFoldCV.b[[l]] <- kFoldCV(data = dat[dat$AQeType=="background",]
                                  ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                            ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                            ,"Constr", "UrbGreen", "Agri", "Forest"
                                            , "popDens", "PriRoad", "SecRoad", "NatMot"
                                            , "LocRoute")
                                  ,ID = "AQeCode"
                                  ,spVar1 = "AQeLon"
                                  ,spVar2 = "AQeLat"
                                  ,depVar = "AQeYMean"
                                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                  ,thresh = 0.95
                                  ,seed = l
                                  ,k = 10
                                  ,strat = TRUE
                                  ,indRegions = "indRegions"
                                  ,loocv = FALSE
  )
}

rmse_StratTenFoldCV_par_b <- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_StratTenFoldCV_par_b <- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par)))
aic_StratTenFoldCV_par_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.par))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_StratTenFoldCV_par_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_StratTenFoldCV_smooth_b <- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_StratTenFoldCV_smooth_b <- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
aic_StratTenFoldCV_smooth_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.smooth))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_StratTenFoldCV_smooth_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_StratTenFoldCV_par_b); mean(rmse_StratTenFoldCV_smooth_b)
mean(mae_StratTenFoldCV_par_b); mean(mae_StratTenFoldCV_smooth_b)
mean(aic_StratTenFoldCV_par_b); mean(aic_StratTenFoldCV_smooth_b)
mean(adjR2_StratTenFoldCV_par_b); mean(adjR2_StratTenFoldCV_smooth_b)



###
## Stratified hold-out validation
###

# can easily be derived based on the results of stratified hold-out CV by using only Fold1 as test sample

rmse_StratHoldOutCV_par_b <- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_par_b <- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
aic_StratHoldOutCV_par_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) AIC(s$ls.models$Fold1$mod.par))
adjR2_StratHoldOutCV_par_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_StratHoldOutCV_smooth_b <- sapply(listStratTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_smooth_b <- sapply(listStratTenFoldCV.b, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_StratHoldOutCV_smooth_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_StratHoldOutCV_smooth_b  <- sapply(listStratTenFoldCV.b, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_StratHoldOutCV_par_b); mean(rmse_StratHoldOutCV_smooth_b)
mean(mae_StratHoldOutCV_par_b); mean(mae_StratHoldOutCV_smooth_b)
mean(aic_StratHoldOutCV_par_b); mean(aic_StratHoldOutCV_smooth_b)
mean(adjR2_StratHoldOutCV_par_b); mean(adjR2_StratHoldOutCV_smooth_b)







#####################
## Traffic/Industrial sites
#####################



###
## leave-one-out cross-validation
###

loocv.TI <- kFoldCV(data = dat[dat$AQeType!="background",]
                   ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                             ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                             ,"Constr", "UrbGreen", "Agri", "Forest"
                             , "popDens", "PriRoad", "SecRoad", "NatMot"
                             , "LocRoute")
                   ,ID = "AQeCode"
                   ,spVar1 = "AQeLon"
                   ,spVar2 = "AQeLat"
                   ,depVar = "AQeYMean"
                   ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                   ,thresh = 0.95
                   ,seed = 1
                   ,k = 10
                   ,strat = FALSE
                   ,indRegions = "indRegions"
                   ,loocv = TRUE
)
rmse_looCV_par_TI <- sqrt(mean(loocv.TI$df.err$Err.par^2))
mae_looCV_par_TI <- mean(abs(loocv.TI$df.err$Err.par))
aic_looCV_par_TI  <- mean(sapply(loocv.TI$ls.models, FUN = function(f) AIC(f$mod.par)))
adjR2_looCV_par_TI  <-  mean(sapply(loocv.TI$ls.models, FUN = function(f) summary(f$mod.par)$adj.r.squared))

rmse_looCV_smooth_TI <- sqrt(mean(loocv.TI$df.err$Err.smooth^2))
mae_looCV_smooth_TI <- mean(abs(loocv.TI$df.err$Err.smooth))
aic_looCV_smooth_TI <- mean(sapply(loocv.TI$ls.models, FUN = function(f) AIC(f$mod.smooth)))
adjR2_looCV_smooth_TI  <-  mean(sapply(loocv.TI$ls.models, FUN = function(f) summary(f$mod.smooth)$r.sq))






###
## 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listTenFoldCV.TI <- vector("list", length = 100)
names(listTenFoldCV.TI) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listTenFoldCV.TI[[l]] <- kFoldCV(data = dat[dat$AQeType!="background",]
                                  ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                            ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                            ,"Constr", "UrbGreen", "Agri", "Forest"
                                            , "popDens", "PriRoad", "SecRoad", "NatMot"
                                            , "LocRoute")
                                  ,ID = "AQeCode"
                                  ,spVar1 = "AQeLon"
                                  ,spVar2 = "AQeLat"
                                  ,depVar = "AQeYMean"
                                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                  ,thresh = 0.95
                                  ,seed = l
                                  ,k = 10
                                  ,strat = FALSE
                                  ,indRegions = "indRegions"
                                  ,loocv = FALSE
  )
}

rmse_TenFoldCV_par_TI <- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_TenFoldCV_par_TI <- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par)))
aic_TenFoldCV_par_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.par))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_TenFoldCV_par_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_TenFoldCV_smooth_TI <- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_TenFoldCV_smooth_TI <- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
aic_TenFoldCV_smooth_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.smooth))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_TenFoldCV_smooth_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_TenFoldCV_par_TI); mean(rmse_TenFoldCV_smooth_TI)
mean(mae_TenFoldCV_par_TI); mean(mae_TenFoldCV_smooth_TI)
mean(aic_TenFoldCV_par_TI); mean(aic_TenFoldCV_smooth_TI)
mean(adjR2_TenFoldCV_par_TI); mean(adjR2_TenFoldCV_smooth_TI)


plot(density(rmse_TenFoldCV_smooth_TI), xlim = c(min(rmse_TenFoldCV_smooth_TI),max(rmse_TenFoldCV_par_TI)),
     col = "orange", lwd = 2)
lines(density(rmse_TenFoldCV_par_TI), col = "blue", lwd = 2)




###
## Hold-out validation
###

# can easily be derived based on the results of 10-fold CV by using only Fold1 as test sample

rmse_HoldOutCV_par_TI <- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_HoldOutCV_par_TI <- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
aic_HoldOutCV_par_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) AIC(s$ls.models$Fold1$mod.par))
adjR2_HoldOutCV_par_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_HoldOutCV_smooth_TI <- sapply(listTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_HoldOutCV_smooth_TI <- sapply(listTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_HoldOutCV_smooth_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_HoldOutCV_smooth_TI  <- sapply(listTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_HoldOutCV_par_TI); mean(rmse_HoldOutCV_smooth_TI)
mean(mae_HoldOutCV_par_TI); mean(mae_HoldOutCV_smooth_TI)
mean(aic_HoldOutCV_par_TI); mean(aic_HoldOutCV_smooth_TI)
mean(adjR2_HoldOutCV_par_TI); mean(adjR2_HoldOutCV_smooth_TI)





###
## Stratified 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listStratTenFoldCV.TI <- vector("list", length = 100)
names(listStratTenFoldCV.TI) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listStratTenFoldCV.TI[[l]] <- kFoldCV(data = dat[dat$AQeType!="background",]
                                       ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                                 ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                                 ,"Constr", "UrbGreen", "Agri", "Forest"
                                                 , "popDens", "PriRoad", "SecRoad", "NatMot"
                                                 , "LocRoute")
                                       ,ID = "AQeCode"
                                       ,spVar1 = "AQeLon"
                                       ,spVar2 = "AQeLat"
                                       ,depVar = "AQeYMean"
                                       ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                       ,thresh = 0.95
                                       ,seed = l
                                       ,k = 10
                                       ,strat = TRUE
                                       ,indRegions = "indRegions"
                                       ,loocv = FALSE
  )
}

rmse_StratTenFoldCV_par_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_StratTenFoldCV_par_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par)))
aic_StratTenFoldCV_par_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.par))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_StratTenFoldCV_par_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_StratTenFoldCV_smooth_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_StratTenFoldCV_smooth_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
aic_StratTenFoldCV_smooth_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.smooth))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_StratTenFoldCV_smooth_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_StratTenFoldCV_par_TI); mean(rmse_StratTenFoldCV_smooth_TI)
mean(mae_StratTenFoldCV_par_TI); mean(mae_StratTenFoldCV_smooth_TI)
mean(aic_StratTenFoldCV_par_TI); mean(aic_StratTenFoldCV_smooth_TI)
mean(adjR2_StratTenFoldCV_par_TI); mean(adjR2_StratTenFoldCV_smooth_TI)



###
## Stratified hold-out validation
###

# can easily be derived based on the results of stratified hold-out CV by using only Fold1 as test sample

rmse_StratHoldOutCV_par_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_par_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
aic_StratHoldOutCV_par_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) AIC(s$ls.models$Fold1$mod.par))
adjR2_StratHoldOutCV_par_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_StratHoldOutCV_smooth_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_smooth_TI <- sapply(listStratTenFoldCV.TI, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_StratHoldOutCV_smooth_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_StratHoldOutCV_smooth_TI  <- sapply(listStratTenFoldCV.TI, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_StratHoldOutCV_par_TI); mean(rmse_StratHoldOutCV_smooth_TI)
mean(mae_StratHoldOutCV_par_TI); mean(mae_StratHoldOutCV_smooth_TI)
mean(aic_StratHoldOutCV_par_TI); mean(aic_StratHoldOutCV_smooth_TI)
mean(adjR2_StratHoldOutCV_par_TI); mean(adjR2_StratHoldOutCV_smooth_TI)







#####################
## All sites
#####################



###
## leave-one-out cross-validation
###

loocv.A <- kFoldCV(data = dat
                    ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                              ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                              ,"Constr", "UrbGreen", "Agri", "Forest"
                              , "popDens", "PriRoad", "SecRoad", "NatMot"
                              , "LocRoute")
                    ,ID = "AQeCode"
                    ,spVar1 = "AQeLon"
                    ,spVar2 = "AQeLat"
                    ,depVar = "AQeYMean"
                    ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                    ,thresh = 0.95
                    ,seed = 1
                    ,k = 10
                    ,strat = FALSE
                    ,indRegions = "indRegions"
                    ,loocv = TRUE
)
rmse_looCV_par_A <- sqrt(mean(loocv.A$df.err$Err.par^2))
mae_looCV_par_A <- mean(abs(loocv.A$df.err$Err.par))
aic_looCV_par_A  <- mean(sapply(loocv.A$ls.models, FUN = function(f) AIC(f$mod.par)))
adjR2_looCV_par_A  <-  mean(sapply(loocv.A$ls.models, FUN = function(f) summary(f$mod.par)$adj.r.squared))

rmse_looCV_smooth_A <- sqrt(mean(loocv.A$df.err$Err.smooth^2))
mae_looCV_smooth_A <- mean(abs(loocv.A$df.err$Err.smooth))
aic_looCV_smooth_A <- mean(sapply(loocv.A$ls.models, FUN = function(f) AIC(f$mod.smooth)))
adjR2_looCV_smooth_A  <-  mean(sapply(loocv.A$ls.models, FUN = function(f) summary(f$mod.smooth)$r.sq))






###
## 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listTenFoldCV.A <- vector("list", length = 100)
names(listTenFoldCV.A) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listTenFoldCV.A[[l]] <- kFoldCV(data = dat
                                   ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                             ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                             ,"Constr", "UrbGreen", "Agri", "Forest"
                                             , "popDens", "PriRoad", "SecRoad", "NatMot"
                                             , "LocRoute")
                                   ,ID = "AQeCode"
                                   ,spVar1 = "AQeLon"
                                   ,spVar2 = "AQeLat"
                                   ,depVar = "AQeYMean"
                                   ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                   ,thresh = 0.95
                                   ,seed = l
                                   ,k = 10
                                   ,strat = FALSE
                                   ,indRegions = "indRegions"
                                   ,loocv = FALSE
  )
}

rmse_TenFoldCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_TenFoldCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par)))
aic_TenFoldCV_par_A  <- sapply(listTenFoldCV.A, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.par))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_TenFoldCV_par_A  <- sapply(listTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_TenFoldCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_TenFoldCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
aic_TenFoldCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.smooth))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_TenFoldCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_TenFoldCV_par_A); mean(rmse_TenFoldCV_smooth_A)
mean(mae_TenFoldCV_par_A); mean(mae_TenFoldCV_smooth_A)
mean(aic_TenFoldCV_par_A); mean(aic_TenFoldCV_smooth_A)
mean(adjR2_TenFoldCV_par_A); mean(adjR2_TenFoldCV_smooth_A)





###
## Hold-out validation
###

# can easily be derived based on the results of 10-fold CV by using only Fold1 as test sample

rmse_HoldOutCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_HoldOutCV_par_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
aic_HoldOutCV_par_A  <- sapply(listTenFoldCV.A, FUN = function(s) AIC(s$ls.models$Fold1$mod.par))
adjR2_HoldOutCV_par_A  <- sapply(listTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_HoldOutCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_HoldOutCV_smooth_A <- sapply(listTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_HoldOutCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_HoldOutCV_smooth_A  <- sapply(listTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_HoldOutCV_par_A); mean(rmse_HoldOutCV_smooth_A)
mean(mae_HoldOutCV_par_A); mean(mae_HoldOutCV_smooth_A)
mean(aic_HoldOutCV_par_A); mean(aic_HoldOutCV_smooth_A)
mean(adjR2_HoldOutCV_par_A); mean(adjR2_HoldOutCV_smooth_A)





###
## Stratified 10-fold cross-validation
###

# Apply function kFoldCV for seed values 1 to 100
listStratTenFoldCV.A <- vector("list", length = 100)
names(listStratTenFoldCV.A) <- paste("seed",1:100,sep="")

for(l in 1:100){
  listStratTenFoldCV.A[[l]] <- kFoldCV(data = dat
                                        ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                                  ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                                  ,"Constr", "UrbGreen", "Agri", "Forest"
                                                  , "popDens", "PriRoad", "SecRoad", "NatMot"
                                                  , "LocRoute")
                                        ,ID = "AQeCode"
                                        ,spVar1 = "AQeLon"
                                        ,spVar2 = "AQeLat"
                                        ,depVar = "AQeYMean"
                                        ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                                        ,thresh = 0.95
                                        ,seed = l
                                        ,k = 10
                                        ,strat = TRUE
                                        ,indRegions = "indRegions"
                                        ,loocv = FALSE
  )
}

rmse_StratTenFoldCV_par_A <- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
mae_StratTenFoldCV_par_A <- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par)))
aic_StratTenFoldCV_par_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.par))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_StratTenFoldCV_par_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.par)$adj.r.squared)
  })
  return(mean(adjR2_seed_tmp))
})

rmse_StratTenFoldCV_smooth_A <- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth^2)))
mae_StratTenFoldCV_smooth_A <- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth)))
aic_StratTenFoldCV_smooth_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) {
  AIC_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(AIC(f$mod.smooth))
  })
  return(mean(AIC_seed_tmp))
})
adjR2_StratTenFoldCV_smooth_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) {
  adjR2_seed_tmp <- sapply(s$ls.models, FUN = function(f) {
    return(summary(f$mod.smooth)$r.sq)
  })
  return(mean(adjR2_seed_tmp))
})

mean(rmse_StratTenFoldCV_par_A); mean(rmse_StratTenFoldCV_smooth_A)
mean(mae_StratTenFoldCV_par_A); mean(mae_StratTenFoldCV_smooth_A)
mean(aic_StratTenFoldCV_par_A); mean(aic_StratTenFoldCV_smooth_A)
mean(adjR2_StratTenFoldCV_par_A); mean(adjR2_StratTenFoldCV_smooth_A)



###
## Stratified hold-out validation
###

# can easily be derived based on the results of stratified hold-out CV by using only Fold1 as test sample

rmse_StratHoldOutCV_par_A <- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.par[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_par_A <- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.par[x$df.err$Fold==1])))
aic_StratHoldOutCV_par_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) AIC(s$ls.models$Fold1$mod.par))
adjR2_StratHoldOutCV_par_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.par)$adj.r.squared)

rmse_StratHoldOutCV_smooth_A <- sapply(listStratTenFoldCV.A, FUN = function(x) sqrt(mean(x$df.err$Err.smooth[x$df.err$Fold==1]^2)))
mae_StratHoldOutCV_smooth_A <- sapply(listStratTenFoldCV.A, FUN = function(x) mean(abs(x$df.err$Err.smooth[x$df.err$Fold==1])))
aic_StratHoldOutCV_smooth_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) AIC(s$ls.models$Fold1$mod.smooth))
adjR2_StratHoldOutCV_smooth_A  <- sapply(listStratTenFoldCV.A, FUN = function(s) summary(s$ls.models$Fold1$mod.smooth)$r.sq)

mean(rmse_StratHoldOutCV_par_A); mean(rmse_StratHoldOutCV_smooth_A)
mean(mae_StratHoldOutCV_par_A); mean(mae_StratHoldOutCV_smooth_A)
mean(aic_StratHoldOutCV_par_A); mean(aic_StratHoldOutCV_smooth_A)
mean(adjR2_StratHoldOutCV_par_A); mean(adjR2_StratHoldOutCV_smooth_A)

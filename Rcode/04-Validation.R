#########################################################################################
## Validating parametric and smooth LUR models in dependence of monitoring sites' type
#########################################################################################


# Validation techniques:
# loocv, 10-fold cv, and hold-out validation

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
                   ,loocv = TRUE
                   )
Sys.time()



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
                                  ,loocv = FALSE
                                  )
}

rmse_TenFoldCV_par_b <- sapply(listTenFoldCV.b, FUN = function(x) sqrt(mean(x$df.err$Err.par^2)))
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
mean(aic_TenFoldCV_par_b); mean(aic_TenFoldCV_smooth_b)
mean(adjR2_TenFoldCV_par_b); mean(adjR2_TenFoldCV_smooth_b)


plot(density(rmse_TenFoldCV_smooth_b), xlim = c(min(rmse_TenFoldCV_smooth_b),max(rmse_TenFoldCV_par_b)),
     col = "orange", lwd = 2)
lines(density(rmse_TenFoldCV_par_b), col = "blue", lwd = 2)


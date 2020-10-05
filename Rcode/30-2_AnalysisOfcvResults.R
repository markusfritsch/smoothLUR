


rm(list = ls())


setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")


#	load("cvResultsA.RData")
#	load("cvResultsB.RData")
#	load("cvResultsTI.RData")



if(sum(grepl(ls(), pattern = "_b")) > 1){
e	<- "b"
} else{
  if(sum(grepl(ls(), pattern = "_A")) > 1){
  e	<- "A"
  } else{
  e	<- "TI"
  }
}



#adjR2
get(paste("adjR2_looCV_par_", e, sep = "") )
summary(get(paste("adjR2_HoldOutCV_par_", e, sep = "")) )
summary(get(paste("adjR2_TenFoldCV_par_", e, sep = "")) )
summary(get(paste("adjR2_StratHoldOutCV_par_", e, sep = "")) )
summary(get(paste("adjR2_StratTenFoldCV_par_", e, sep = "")) )

get(paste("adjR2_looCV_smooth_", e, sep = "") )
summary(get(paste("adjR2_HoldOutCV_smooth_", e, sep = "")) )
summary(get(paste("adjR2_TenFoldCV_smooth_", e, sep = "")) )
summary(get(paste("adjR2_StratHoldOutCV_smooth_", e, sep = "")) )
summary(get(paste("adjR2_StratTenFoldCV_smooth_", e, sep = "")) )


#mae
get(paste("mae_looCV_par_", e, sep = "") )
summary(get(paste("mae_HoldOutCV_par_", e, sep = "")) )
summary(get(paste("mae_TenFoldCV_par_", e, sep = "")) )
summary(get(paste("mae_StratHoldOutCV_par_", e, sep = "")) )
summary(get(paste("mae_StratTenFoldCV_par_", e, sep = "")) )

get(paste("mae_looCV_smooth_", e, sep = "") )
summary(get(paste("mae_HoldOutCV_smooth_", e, sep = "")) )
summary(get(paste("mae_TenFoldCV_smooth_", e, sep = "")) )
summary(get(paste("mae_StratHoldOutCV_smooth_", e, sep = "")) )
summary(get(paste("mae_StratTenFoldCV_smooth_", e, sep = "")) )


#rmse
get(paste("rmse_looCV_par_", e, sep = "") )
summary(get(paste("rmse_HoldOutCV_par_", e, sep = "")) )
summary(get(paste("rmse_TenFoldCV_par_", e, sep = "")) )
summary(get(paste("rmse_StratHoldOutCV_par_", e, sep = "")) )
summary(get(paste("rmse_StratTenFoldCV_par_", e, sep = "")) )

get(paste("rmse_looCV_smooth_", e, sep = "") )
summary(get(paste("rmse_HoldOutCV_smooth_", e, sep = "")) )
summary(get(paste("rmse_TenFoldCV_smooth_", e, sep = "")) )
summary(get(paste("rmse_StratHoldOutCV_smooth_", e, sep = "")) )
summary(get(paste("rmse_StratTenFoldCV_smooth_", e, sep = "")) )


#summary statistics on errors loocv
apply(listStratTenFoldCV.A[[1]]$df.err[, 3:4], MARGIN = 2, FUN = summary)


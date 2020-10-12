


library(ggplot2)
library(smoothLUR)


rm(list = ls())


setwd("D:/Work/20_Projekte/570_Behm-and-Fritsch/smoothLUR")



	load("cvResultsA.RData")
#	load("cvResultsB.RData")
#	load("cvResultsTI.RData")

	
data(monSitesDE)
monSitesDE[monSitesDE$AQeType != "background", "AQeType"] <- "traffic/industrial"	



if(sum(grepl(ls(), pattern = "_b")) > 1){
e	<- "b"
} else{
  if(sum(grepl(ls(), pattern = "_A")) > 1){
  e	<- "A"
  } else{
  e	<- "TI"
  }
}




###
### Separate analysis for models fitted based on all monitoring sites depending on site type
###


### LOOCV

#prediction errors
loocvA <- cbind(loocv.A$df.err, monSitesDE[,c(1,7)])

#mae
loocvA <- cbind(loocv.A$df.err[,-3], "Err.par" = abs(loocv.A$df.err$Err.par), monSitesDE[,c(1,7)])
loocvA <- cbind(loocv.A$df.err[,-4], "Err.smooth" = abs(loocv.A$df.err$Err.smooth), monSitesDE[,c(1,7)])


#rmse
loocvA <- cbind(loocv.A$df.err[,-3], "Err.par" = (loocv.A$df.err$Err.par)^2, monSitesDE[,c(1,7)])
loocvA <- cbind(loocv.A$df.err[,-4], "Err.smooth" = (loocv.A$df.err$Err.smooth)^2, monSitesDE[,c(1,7)])



loocvA.bg <- loocvA[loocvA$AQeType == "background", ]
mean(loocvA.bg$Err.par)
range(loocvA.bg$Err.par)
mean(loocvA.bg$Err.smooth)
range(loocvA.bg$Err.smooth)

loocvA.ti <- loocvA[loocvA$AQeType != "background", ]
mean(loocvA.ti$Err.par)
mean(loocvA.ti$Err.smooth)


dat.val <- data.frame(Y = c(loocvA.bg$Err.par, loocvA.ti$Err.par),
                        type = c(rep("Background", 246), rep("Traffic/Industrial", 157)))
 

dat.val2 <- data.frame(Y = c(loocvA.bg$Err.smooth, loocvA.ti$Err.smooth),
                        type = c(rep("Background", 246), rep("Traffic/Industrial", 157)))





(p.hist <- ggplot(dat.val2, aes(x = Y, fill = type, color = type)) +
    theme_minimal() +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-30, 55)) +
    geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
    geom_density(alpha = 0.6, lwd = 1.2) +
    xlab("prediction error") +
    ylab("empirical density") +
    scale_color_manual(values = brewer.pal(9, "YlOrRd")[c(4,6)],
                       aesthetics = c("fill", "colour"))+
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = c(0.75, 0.9)))


pdf("../img/empDensPredLoocvATrInd.pdf", height = 6, width = 9)
p.hist
dev.off()


pdf("../img/empDensPredLoocvATrInd2.pdf", height = 6, width = 9)
p.hist
dev.off()




  
### 10-fold CV 


listKFCVA <- lapply(listTenFoldCV.A, FUN = function(x) cbind(x$df.err, monSitesDE[,c(1,7)]) )


#prediction errors
kfcvA <- lapply(listKFCVA, FUN = function(x) x$Err.par )
kfcvA2 <- lapply(listKFCVA, FUN = function(x) x$Err.smooth )

#mae
kfcvA <- lapply(listKFCVA, FUN = function(x) abs(x$Err.par) )
kfcvA2 <- lapply(listKFCVA, FUN = function(x) abs(x$Err.smooth) )

#rmse
kfcvA <- lapply(listKFCVA, FUN = function(x) (x$Err.par)^2 )
kfcvA2 <- lapply(listKFCVA, FUN = function(x) (x$Err.smooth)^2 )


kfcvA.vec <- as.vector(unlist(kfcvA))
kfcvA.vec2 <- as.vector(unlist(kfcvA2))


mon.site.vec   <- as.vector(unlist(lapply(listKFCVA, FUN = function(x) x$AQeType)))  # background monitoring sites

dat.val   <- data.frame(Y = kfcvA.vec, type = mon.site.vec)
dat.val2   <- data.frame(Y = kfcvA.vec2, type = mon.site.vec)


(p.hist <- ggplot(dat.val2, aes(x = Y, fill = type, color = type)) +
    theme_minimal() +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-30, 50)) +
    geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
    geom_density(alpha = 0.6, lwd = 1.2) +
    xlab("prediction error") +
    ylab("empirical density") +
    scale_color_manual(values = brewer.pal(9, "YlOrRd")[c(4,6)],
                       aesthetics = c("fill", "colour"))+
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = c(0.75, 0.9)))


pdf("../img/empDensPredKfcvATrInd.pdf", height = 6, width = 9)
p.hist
dev.off()


pdf("../img/empDensPredKfcvATrInd2.pdf", height = 6, width = 9)
p.hist
dev.off()








### Hold-out validation (10-fold CV fold1)


ind.vec   <- as.vector(unlist(lapply(listKFCVA, FUN = function(x) x$Fold == 1)))  # background monitoring sites

dat.val   <- data.frame(Y = kfcvA.vec[ind.vec], type = mon.site.vec[ind.vec])
dat.val2   <- data.frame(Y = kfcvA.vec2[ind.vec], type = mon.site.vec[ind.vec])



(p.hist <- ggplot(dat.val2, aes(x = Y, fill = type, color = type)) +
    theme_minimal() +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 90, by = 15), limits = c(-30, 250)) +
    geom_histogram(aes(y=..density..), fill = "white", position = "identity", alpha = 0.5, binwidth = 5, lwd = 1.4) +
    geom_density(alpha = 0.6, lwd = 1.2) +
    xlab("prediction error") +
    ylab("empirical density") +
    scale_color_manual(values = brewer.pal(9, "YlOrRd")[c(4,6)],
                       aesthetics = c("fill", "colour"))+
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = c(0.75, 0.9)))


pdf("../img/empDensPredHovATrInd.pdf", height = 6, width = 9)
p.hist
dev.off()


pdf("../img/empDensPredHovATrInd2.pdf", height = 6, width = 9)
p.hist
dev.off()




















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














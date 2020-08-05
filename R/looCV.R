###############################################################
### Function to conduct leave-one-out cross-validation (LOOCV)
###############################################################




#' Function for conducting leave-one-out cross-validation
#'
#' \code{looCV} conducts a leave-one-out cross-validation for parametric
#'    and smooth land use regression (LUR) models fitted with the functions
#'    \code{escapeLUR} and \code{smoothLUR}, respectively.
#'
#' @aliases looCV
#' @param data A data set which contains the dependent variable and the
#'    potential predictors.
#' @param pred A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param ID A character vector stating the variable name referring to the monitoring
#'    sites' ID (name has to mach the column name of `data`)
#' @param spVar1 A character vector stating the variable name referring to longitude
#'    (name has to match the column name of `data`)
#' @param spVar2 A character vector stating the variable name referring to latitude
#'    (name has to match the column name of `data`)
#' @param depVar A character string indicating the name of the dependent
#'    variable.
#' @param dirEff - only required for parametric model fitted according to ESCAPE procedure
#'    A vector that contains one entry for each potential
#'    predictor and indicates the expected direction of the effect of the
#'    potential predictor (1 for positive, -1 for negative and 0 if the
#'    expected effect sign is unclear).
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding potential
#'    predictor is excluded.
#' @return An object of class `loocvLUR` with the following elements:
#'
#' \item{df.err}{data.frame with four columns: ID (ID of monitoring
#'    site), Err.par (Errors derived from parametric LUR model),
#'    Err.smooth (Errors derived from smooth LUR model)}
#' \item{ls.models}{list with elements according to lines of data set;
#'    each list element is named according to the ID of the omitted
#'    monitoring site is itself a list containing two elements:
#'    mod.par (parametric model based on remaining sites), mod.smooth
#'    (smooth model based on remaining sites)}
#'
#' It has `...`, `...`, and `...` methods.
#'
#' @author Svenia Behm and Markus Fritsch
#' @export
#' @importFrom stats predict
#'
#' @seealso
#'
#' \code{\link{escapeLUR}} for parametric land use regression (LUR)
#'    modeling.
#' \code{\link{smoothLUR}} for smooth land use regression (LUR)
#'    modeling.
#' \code{\link{kFoldCV}} for k-fold cross-validation for
#'    escapeLUR and smoothLUR objects.
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#' ## Load data set
#' dat <- monSitesDE
#'
looCV <- function(
    data
    ,pred
    ,ID
    ,spVar1
    ,spVar2
    ,depVar
    ,dirEff
    ,thresh = 0.95
  ){

  df.err <- data.frame(ID = data[,ID],
                       Err.par = rep(NA, nrow(data)),
                       Err.smooth = rep(NA, nrow(data)))
  vec.tmp <- vector("list", length = 2)
  names(vec.tmp) <- c("mod.par", "mod.smooth")
  ls.models <- rep(list(vec.tmp), nrow(data))
  names(ls.models) <- data$ID
  for(n in 1:nrow(data)){
    mod.par.tmp <- escapeLUR(data=data[-n,], pred, depVar, dirEff, thresh = 0.95) # re-estimate model including forward stepwise predictor selection
    ls.models[[n]][[1]] <- mod.par.tmp
    df.err$Err.par[n] <- data[n, depVar] - stats::predict(mod.par.tmp, newdata = data[n,])
    mod.smooth.tmp <- smoothLUR(data = data[-n,], pred, spVar1, spVar2, depVar, thresh = 0.95)
    ls.models[[n]][[2]] <- mod.smooth.tmp
    df.err$Err.smooth[n] <- data[n, depVar] - stats::predict(mod.smooth.tmp, newdata = data[n,])
  }

  resCV <- list(df.err = df.err, ls.models = ls.models)  
  
#  attr(resCV, "class")  <- "loocvLUR" 
  
  return(resCV)

}


#source("03_FunESCAPE.R")
#source("FunSmooth.R")
#
#dat <- read.csv("DATA_monitoringSites_DE.csv", header = TRUE)
#
#
#(loocv.back <- looCV(data = dat[dat$AQeType=="background", ]
#                     ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#                               ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#                               ,"Constr", "UrbGreen", "Agri", "Forest"
#                               , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#                               , "LocRoute")
#                     ,ID = "AQeCode"
#                     ,spVar1 = "AQeLon"
#                     ,spVar2 = "AQeLat"
#                     ,depVar = "AQeYMean"
#                     ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#                     ,thresh = 0.95) )
#
#(loocv.TrInd <- loocv(data = dat[dat$AQeType!="background", ]
#                      ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#                                ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#                                ,"Constr", "UrbGreen", "Agri", "Forest"
#                                , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#                                , "LocRoute")
#                      ,ID = "AQeCode"
#                      ,spVar1 = "AQeLon"
#                      ,spVar2 = "AQeLat"
#                      ,depVar = "AQeYMean"
#                      ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#                      ,thresh = 0.95) )
#
#(loocv.All <- loocv(data = dat
#                    ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#                              ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#                              ,"Constr", "UrbGreen", "Agri", "Forest"
#                              , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#                              , "LocRoute")
#                    ,ID = "AQeCode"
#                    ,spVar1 = "AQeLon"
#                    ,spVar2 = "AQeLat"
#                    ,depVar = "AQeYMean"
#                    ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#                    ,thresh = 0.95) )
#
#rm(list = ls()[!ls()%in%c("loocv.back", "loocv.TrInd", "loocv.All")])
#save.image("loocv.RData")
#
#
#apply(loocv.back$df.err[,2:3], MARGIN = 2, function(x) sqrt(mean(x^2))) # rmse
#apply(loocv.back$df.err[,2:3], MARGIN = 2, function(x) mean(abs(x)))    # mae
#apply(loocv.back$df.err[,2:3], MARGIN = 2, function(x) mean(x))         # bias
#
#(adj.r2.par    <- mean(sapply(loocv.back$ls.models, FUN = function(x) summary(x$mod.par)$adj.r.squared)))
#(adj.r2.smooth <- mean(sapply(loocv.back$ls.models, FUN = function(x) summary(x$mod.smooth)$r.sq)))
#
#(aic.par       <- mean(sapply(loocv.back$ls.models, FUN = function(x) AIC(x$mod.par))))
#(aic.smooth    <- mean(sapply(loocv.back$ls.models, FUN = function(x) AIC(x$mod.smooth))))

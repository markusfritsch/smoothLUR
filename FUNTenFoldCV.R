#################################################
### Function to conduct 10-fold cross-validation
#################################################




#' Function for conducting 10-fold cross-validation
#'
#' \code{TenFoldCV} conducts a 10-fold cross-validation for parametric
#'    and smooth land use regression (LUR) models fitted with the functions
#'    escape and smooth, respectively.
#'
#' @aliases TenFoldCV
#' @param data A data set which contains the dependent variable and the
#'    potential predictors.
#' @param pred A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param ID A character string stating the variable name referring to the monitoring
#'    sites' ID (name has to mach the column name of `data`)
#' @param spVar1 A character string stating the variable name referring to longitude
#'    (name has to match the column name of `data`)
#' @param spVar2 A character string stating the variable name referring to latitude
#'    (name has to match the column name of `data`)
#' @param depVar A character string that indicates the name of the dependent
#'    variable.
#' @param dirEff - only required for parametric model fitted according to ESCAPE procedure
#'    A vector that contains one entry for each potential
#'    predictor and indicates the expected direction of the effect of the
#'    potential predictor (1 for positive, -1 for negative and 0 if the
#'    expected effect sign is unclear).
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding potential
#'    predictor is excluded.
#' @param seed A numeric value that defines the seed for random sampling
#' @param strat A boolean value that indicates whether stratified sampling is desired
#'    (stratified spatially w.r.t. German federal states)

# Function returns a list of two elements:
# 'df.err': data.frame with four columns:
#                      'ID': Id of monitoring site
#                      'Fold': Number of fold the monitoring site is attributed to
#                      'Err.par':  Errors derived from parametric LUR model
#                      'Err.smooth': Errors derived from smooth LUR model
# 'ls.models': list with 10 elements named according to the omitted fold
#              each list element is a list containing two elements:
#                   'mod.par': parametric model based on remaining sites
#                   'mod.smooth': smooth model based on remaining sites

source("03_FunESCAPE.R")
source("FunSmooth.R")


TenFoldCV <- function(
  data
  ,pred
  ,ID
  ,spVar1
  ,spVar2
  ,depVar
  ,dirEff
  ,thresh = 0.95
  ,seed
  ,strat = FALSE
){

  df.err <- data.frame(ID = data[,ID],
                       Fold = rep(NA, nrow(data)),
                       Err.par = rep(NA, nrow(data)),
                       Err.smooth = rep(NA, nrow(data)))
  vec.tmp <- vector("list", length = 2)
  names(vec.tmp) <- c("mod.par", "mod.smooth")
  ls.models <- rep(list(vec.tmp), 10)
  names(ls.models) <- paste("Fold", 1:10, sep = "")

  set.seed(seed)
  ind.reorder <- sample(nrow(data))

  # Define vector indicating 10 folds
  ind.folds <- cut(1:nrow(data), breaks = 10, labels = FALSE)

  pred.tmp <- rep(NA, nrow(dat))
  for(i in 1:10){
    ind.test <- ind.reorder[which(ind.folds==i)]
    df.err$Fold[ind.test] <- i

    data.test <- data[ind.test, ]
    data.train <- data[-ind.test, ]

    mod.par.tmp <- escape(data=data.train, pred, depVar, dirEff, thresh = 0.95)
    ls.models[[i]][[1]] <- mod.par.tmp
    df.err$Err.par[ind.test]    <- data[ind.test, depVar] - predict(mod.par.tmp, newdata = data.test)

    mod.smooth.tmp <- smooth(data=data.train, pred, spVar1, spVar2, depVar, thresh = 0.95)
    ls.models[[i]][[2]] <- mod.smooth.tmp
    df.err$Err.smooth[ind.test] <- data[ind.test, depVar] - predict(mod.smooth.tmp, newdata = data.test)
  }
  return(list(df.err = df.err, ls.models = ls.models))
}



(TenFoldCV.seed1 <- TenFoldCV(data = dat[dat$AQeType!="background",]
                      ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                ,"Constr", "UrbGreen", "Agri", "Forest"
                                , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
                                , "LocRoute")
                      ,ID = "AQeCode"
                      ,spVar1 = "AQeLon"
                      ,spVar2 = "AQeLat"
                      ,depVar = "AQeYMean"
                      ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
                      ,thresh = 0.95
                      ,seed = 1
                      ,strat = FALSE
) )


lapply(TenFoldCV.seed1$ls.models, FUN = function(x) x$mod.par$coefficients)

apply(TenFoldCV.seed1$df.err[,3:4], MARGIN = 2, function(x) sqrt(mean(x^2))) # rmse
apply(TenFoldCV.seed1$df.err[,3:4], MARGIN = 2, function(x) mean(abs(x)))    # mae
apply(TenFoldCV.seed1$df.err[,3:4], MARGIN = 2, function(x) mean(x))         # bias

(adj.r2.par    <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) summary(x$mod.par)$adj.r.squared)))
(adj.r2.smooth <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) summary(x$mod.smooth)$r.sq)))

(aic.par       <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) AIC(x$mod.par))))
(aic.smooth    <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) AIC(x$mod.smooth))))

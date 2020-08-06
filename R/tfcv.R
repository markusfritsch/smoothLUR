#################################################
### Function to conduct k-fold cross-validation
#################################################




#' Function for conducting k-fold cross-validation
#'
#' \code{kFoldCV} conducts a k-fold cross-validation for parametric
#'    and smooth land use regression (LUR) models fitted with the
#'    functions \code{escapeLUR} and \code{smoothLUR}, respectively.
#'
#' @aliases kFoldCV
#' @param data A data set which contains the dependent variable and
#'    the potential predictors.
#' @param pred A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param ID A character string stating the variable name referring
#'    to the monitoring sites' ID (name has to mach the column name
#'    of `data`)
#' @param spVar1 A character string stating the variable name
#'    referring to longitude (name has to match the column name of
#'    `data`)
#' @param spVar2 A character string stating the variable name
#'    referring to latitude (name has to match the column name of
#'    `data`)
#' @param depVar A character string that indicates the name of the
#'    dependent variable.
#' @param dirEff A vector that contains one entry for each potential
#'    predictor and indicates the expected direction of the effect
#'    of the potential predictor (1 for positive, -1 for negative
#'    and 0 if the expected effect sign is unclear). Argument
#'    defaults to NULL and is only required for parametric model
#'    fitting according to ESCAPE procedure.
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding
#'    potential predictor is excluded.
#' @param seed A numeric value that defines the seed for random
#'    sampling
#' @param folds An integer denoting the number of folds to use in
#'    cross-validation (defaults to 10)
#' @param strat A boolean value that indicates whether stratified
#'    sampling is desired (stratified spatially w.r.t. German federal
#'    states)
#' @return An object of class `tfcvLUR` with the following elements:
#'
#' \item{df.err}{data.frame with four columns: ID (Id of monitoring
#'    site), Fold (Number of fold the monitoring site is attributed to),
#'    Err.par (Errors derived from parametric LUR model), Err.smooth
#'    (Errors derived from smooth LUR model)}
#' \item{ls.models}{list with elements according to `folds'; each list
#'    element is named according to the omitted fold and is itself a
#'    list containing two elements: mod.par (parametric model based
#'    on remaining sites), mod.smooth (smooth model based on remaining
#'    sites)}
#'
#' It has `...`, `...`, and `...` methods.
#'
#' @author Svenia Behm and Markus Fritsch
#' @export
#' @importFrom stats predict
#' @importFrom mgcv gam
#'
#' @seealso
#'
#' \code{\link{escapeLUR}} for parametric land use regression (LUR)
#'    modeling.
#' \code{\link{smoothLUR}} for smooth land use regression (LUR)
#'    modeling.
#' \code{\link{looCV}} for leave-one-out cross-validation for
#'    `escapeLUR` and `smoothLUR` objects.
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#' ## Load data set
#' dat <- data("monSitesDE", package="smoothLUR")
#'
kFoldCV <- function(
  data
  ,pred
  ,ID
  ,spVar1
  ,spVar2
  ,depVar
  ,dirEff
  ,thresh = 0.95
  ,seed
  ,k = 10
  ,strat = FALSE
){

  df.err <- data.frame(ID = data[,ID],
                       Fold = rep(NA, nrow(data)),
                       Err.par = rep(NA, nrow(data)),
                       Err.smooth = rep(NA, nrow(data)))
  vec.tmp <- vector("list", length = 2)
  names(vec.tmp) <- c("mod.par", "mod.smooth")
  ls.models <- rep(list(vec.tmp), k)
  names(ls.models) <- paste("Fold", 1:k, sep = "")

  set.seed(seed)
  ind.reorder <- sample(nrow(data))

  # Define vector indicating K folds
  ind.folds <- cut(1:nrow(data), breaks = k, labels = FALSE)

  pred.tmp <- rep(NA, nrow(dat))
  for(i in 1:k){
    ind.test <- ind.reorder[which(ind.folds==i)]
    df.err$Fold[ind.test] <- i

    data.test <- data[ind.test, ]
    data.train <- data[-ind.test, ]

    mod.par.tmp <- escapeLUR(data=data.train, pred, depVar, dirEff, thresh = 0.95)
    ls.models[[i]][[1]] <- mod.par.tmp
    df.err$Err.par[ind.test]    <- data[ind.test, depVar] - predict(mod.par.tmp, newdata = data.test)

    mod.smooth.tmp <- smoothLUR(data=data.train, pred, spVar1, spVar2, depVar, thresh = 0.95)
    ls.models[[i]][[2]] <- mod.smooth.tmp
    df.err$Err.smooth[ind.test] <- data[ind.test, depVar] - predict(mod.smooth.tmp, newdata = data.test)
  }

  resCV <- list(df.err = df.err, ls.models = ls.models)
    
#  attr(resCV, "class")  <- "tfcvLUR"  
  
  return(resCV)
}



#source("03_FunESCAPE.R")
#source("FunSmooth.R")
#
#tenFoldCV.seed1 <- kFoldCV(data = dat[dat$AQeType!="background",]
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
#                      ,thresh = 0.95
#                      ,seed = 1
#                      ,k = 10
#                      ,strat = FALSE
#)
#
#
#lapply(TenFoldCV.seed1$ls.models, FUN = function(x) x$mod.par$coefficients)
#
#apply(TenFoldCV.seed1$df.err[,3:4], MARGIN = 2, function(x) sqrt(mean(x^2))) # rmse
#apply(TenFoldCV.seed1$df.err[,3:4], MARGIN = 2, function(x) mean(abs(x)))    # mae
#apply(TenFoldCV.seed1$df.err[,3:4], MARGIN = 2, function(x) mean(x))         # bias
#
#(adj.r2.par    <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) summary(x$mod.par)$adj.r.squared)))
#(adj.r2.smooth <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) summary(x$mod.smooth)$r.sq)))
#
#(aic.par       <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) AIC(x$mod.par))))
#(aic.smooth    <- mean(sapply(TenFoldCV.seed1$ls.models, FUN = function(x) AIC(x$mod.smooth))))

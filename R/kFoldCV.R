
#################################################
### Function to conduct k-fold cross-validation
#################################################




#' Function for conducting k-fold cross-validation
#'
#' \code{kFoldCV} conducts a k-fold cross-validation for parametric
#'    and smooth land use regression (LUR) models fitted with the
#'    functions \code{parLUR} and \code{smoothLUR}, respectively.
#'
#' @aliases kFoldCV
#' @param data A data set which contains the dependent variable and
#'    the potential predictors.
#' @param x A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param ID A character string stating the variable name referring
#'    to the monitoring sites' ID (name has to mach the column name
#'    of `data`).
#' @param spVar1 A character string stating the variable name
#'    referring to longitude (name has to match the column name of
#'    `data`).
#' @param spVar2 A character string stating the variable name
#'    referring to latitude (name has to match the column name of
#'    `data`).
#' @param y A character string that indicates the name of the
#'    dependent variable (name has to match the column name of
#'    `data`).
#' @param dirEff A vector that contains one entry for each potential
#'    predictor and indicates the expected direction of the effect
#'    of the potential predictor (1 for positive, -1 for negative
#'    and 0 if the expected effect sign is unclear). Argument
#'    defaults to NULL and is only required for parametric model
#'    fitting.
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding
#'    potential predictor is excluded.
#' @param seed A numeric value that defines the seed for random
#'    sampling.
#' @param k An integer denoting the number of folds to use in
#'    cross-validation (defaults to 10).
#' @param strat A boolean value that indicates whether stratified
#'    sampling is desired (stratified spatially w.r.t. German federal
#'    states).
#' @param loocv A boolean value that indicates whether a leave-one-out
#'    cross-validation which is a k-fold CV with `k` equal to the
#'    number of rows in `data` desired.
#' @param indRegions A character string that indicates the name of
#'    the variable referring to the geographical regions; this
#'    variable is required to perform spatial stratified sampling.
#' @return An object of class `kfcvLUR` with the following elements:
#'
#' \item{df.err}{data.frame with four columns: ID (Id of monitoring
#'    site), Fold (Number of fold the monitoring site is attributed to),
#'    Err.par (Errors derived from parametric LUR model), Err.smooth
#'    (Errors derived from smooth LUR model)}
#' \item{ls.models}{list with elements according to `k`; each list
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
#' @importFrom splitTools create_folds
#'
#' @seealso
#'
#' \code{\link{parLUR}} for parametric land use regression (LUR)
#'    modeling.
#' \code{\link{smoothLUR}} for smooth land use regression (LUR)
#'    modeling.
#'
#' @examples
#' ## Load data set
#' data(monSitesDE, package="smoothLUR")
#'
kFoldCV <- function(
  data
  ,x
  ,ID
  ,spVar1
  ,spVar2
  ,y
  ,dirEff
  ,thresh = 0.95
  ,seed
  ,k = 10
  ,strat = FALSE
  ,indRegions = "indRegions"
  ,loocv = FALSE
){

  if(loocv){
    k = nrow(data)
  }

  df.err <- data.frame(ID = data[,ID],
                       Fold = rep(NA, nrow(data)),
                       Err.par = rep(NA, nrow(data)),
                       Err.smooth = rep(NA, nrow(data)))
  vec.tmp <- vector("list", length = 2)
  names(vec.tmp) <- c("mod.par", "mod.smooth")
  ls.models <- rep(list(vec.tmp), k)
  if(loocv){
    names(ls.models) <- data[,ID]
  } else {
    names(ls.models) <- paste("Fold", 1:k, sep = "")
    if(strat){
      ls.folds <- splitTools::create_folds(data[,indRegions], k = k, type = "stratified", seed = seed)
      } else {
        set.seed(seed)
        ind.reorder <- sample(nrow(data))
        # Define vector indicating K folds
        ind.folds <- cut(1:nrow(data), breaks = k, labels = FALSE)
    }
  }

  for(i in 1:k){
    if(loocv){
      ind.test <- i
      } else {
        if(strat){
          ind.test <- seq(1:nrow(data))[-ls.folds[[i]]]
          # Checks:
          # sort(unlist(sapply(ls.folds, FUN = function(x) seq(1:nrow(data))[-x])))
          # length(unlist(sapply(ls.folds, FUN = function(x) seq(1:nrow(data))[-x])))
          # length(unique(unlist(sapply(ls.folds, FUN = function(x) seq(1:nrow(data))[-x]))))
        } else {
          ind.test <- ind.reorder[which(ind.folds==i)]
        }
      }
    df.err$Fold[ind.test] <- i

    data.test <- data[ind.test, ]
    data.train <- data[-ind.test, ]

    mod.par.tmp <- parLUR(data=data.train, x, y, dirEff, thresh = 0.95)
    ls.models[[i]][[1]] <- mod.par.tmp
    df.err$Err.par[ind.test]    <- data[ind.test, y] - predict(mod.par.tmp, newdata = data.test)

    mod.smooth.tmp <- smoothLUR(data=data.train, x, spVar1, spVar2, y, thresh = 0.95)
    ls.models[[i]][[2]] <- mod.smooth.tmp
    df.err$Err.smooth[ind.test] <- data[ind.test, y] - predict(mod.smooth.tmp, newdata = data.test)
  }

  resCV <- list(df.err = df.err, ls.models = ls.models)

  #  attr(resCV, "class")  <- "kfcvLUR"

  return(resCV)
}




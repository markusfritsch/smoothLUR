
###############################################################
### Function to conduct leave-one-out cross-validation (LOOCV)
###############################################################




#' Function for conducting leave-one-out cross-validation
#'
#' \code{looCV} conducts a leave-one-out cross-validation for parametric
#'    and smooth land use regression (LUR) models fitted with the functions
#'    \code{parLUR} and \code{smoothLUR}, respectively.
#'
#' @aliases looCV
#' @param data A data set which contains the dependent variable and the
#'    potential predictors.
#' @param x A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param ID A character vector stating the variable name referring to the
#'    monitoring sites' ID (name has to mach the column name of `data`).
#' @param spVar1 A character vector stating the variable name referring to
#'    longitude (name has to match the column name of `data`).
#' @param spVar2 A character vector stating the variable name referring to
#'    latitude (name has to match the column name of `data`).
#' @param y A character string indicating the name of the dependent
#'    variable (name has to match the column name of `data`).
#' @param dirEff A vector that contains one entry for each potential
#'    predictor and indicates the expected direction of the effect of the
#'    potential predictor (1 for positive, -1 for negative and 0 if the
#'    expected effect sign is unclear). Argument is only required for
#'    parametric model fitting.
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
#' \code{\link{parLUR}} for parametric land use regression (LUR)
#'    modeling.
#' \code{\link{smoothLUR}} for smooth land use regression (LUR)
#'    modeling.
#' \code{\link{kFoldCV}} for k-fold cross-validation for
#'    parLUR and smoothLUR objects.
#'
#' @examples
#' ## Load data set
#' data(monSitesDE, package="smoothLUR")
#'
looCV <- function(
    data
    ,x
    ,ID
    ,spVar1
    ,spVar2
    ,y
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
    mod.par.tmp <- parLUR(data=data[-n,], x, y, dirEff, thresh = 0.95) # re-estimate model including forward stepwise predictor selection
    ls.models[[n]][[1]] <- mod.par.tmp
    df.err$Err.par[n] <- data[n, y] - stats::predict(mod.par.tmp, newdata = data[n,])
    mod.smooth.tmp <- smoothLUR(data = data[-n,], x, spVar1, spVar2, y, thresh = 0.95)
    ls.models[[n]][[2]] <- mod.smooth.tmp
    df.err$Err.smooth[n] <- data[n, y] - stats::predict(mod.smooth.tmp, newdata = data[n,])
  }

  resCV <- list(df.err = df.err, ls.models = ls.models)  
  
#  attr(resCV, "class")  <- "loocvLUR" 
  
  return(resCV)

}


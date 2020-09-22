
##############################################
### Function to derive parametric LUR model
##############################################










#' Function for deriving parametric land use regression (LUR) model.
#'
#' \code{parLUR} fits a parametric land use regression (LUR) model employing
#'    parametric polynomials in a forward stepwise approach. The function
#'    allows to carry out the ESCAPE procedure outlined in
#'    \insertCite{Beelen.2013;textual}{smoothLUR},
#'    \insertCite{Eeftens.2016;textual}{smoothLUR},
#'    \insertCite{Wolf.2017;textual}{smoothLUR}, and
#'    \insertCite{Fritsch2020smooth;textual}{smoothLUR}.
#'
#' @aliases parLUR
#' @param data A data set which contains the dependent variable and the
#'    potential predictors.
#' @param x A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param y A character string indicating the name of the dependent
#'    variable (name needs to match the column names of `data`).
#' @param dirEff A vector that contains one entry for each potential
#'    predictor and indicates the expected direction of the effect of the
#'    potential predictor (1 for positive, -1 for negative and 0 if the
#'    expected effect sign is unclear).
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding potential
#'    predictor is excluded (defaults to 0.95).
#' @param thresh_pval A numeric value that indicates a threshold for
#'    removing predictors after the forward stepwise modeling procedure;
#'    the threshold is the p-value of a standard t-Test (defaults to 0.1).
#' @return An object of class `parLUR` with the following elements:
#'
#' \item{coefficients}{a vector containing the coefficient estimates}
#'
#' It has `...`, `...`, and `...` methods.
#'
#' @author Svenia Behm and Markus Fritsch
#' @export
#' @importFrom stats lm
#' @importFrom stats as.formula
#'
#' @seealso
#'
#' \code{\link{smoothLUR}} for smooth land use regression (LUR)
#'    models.
#' \code{\link{kFoldCV}} for k-fold cross-validation for
#'    parLUR and smoothLUR objects.
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#' ## Load data from package
#' data(monSitesDE, package="smoothLUR")
#' set.seed(42)
#'
#' ## Code example
#' dat <- monSitesDE[sample(1:nrow(monSitesDE), 40),]
#' m1 <- parLUR(data = dat
#'                  ,x = c("Lon", "Lat", "Alt", "HighDens"
#'                          ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
#'                          ,"UrbGreen", "Agri", "Forest", "PopDens"
#'                          ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
#'                  ,y = "Y"
#'                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#'                  ,thresh = 0.95
#'                  ,thresh_pval = 0.1)
#'
#' summary(m1)
#' summary(m1)$adj.r.squared
#' BIC(m1)
#' AIC(m1)
#'
#' \donttest{
#' ## Load data from package
#' data(monSitesDE, package="smoothLUR")
#' dat <- monSitesDE
#' m1 <- parLUR(data = dat
#'                  ,x = c("Lon", "Lat", "Alt", "HighDens"
#'                          ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
#'                          ,"UrbGreen", "Agri", "Forest", "PopDens"
#'                          ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
#'                  ,y = "Y"
#'                  ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#'                  ,thresh = 0.95
#'                  ,thresh_pval = 0.1)
#'
#' summary(m1)
#' summary(m1)$adj.r.squared
#' BIC(m1)
#' AIC(m1)
#'
#' }
#'
parLUR <- function(
			data
			,x
			,y
			,dirEff
			,thresh = 0.95
			,thresh_pval = 0.1
){

  dat <- data.frame(subset(x = data, select = c(y, x)))
  names.dat <- names(dat)

  dat <- dat[, apply(X = dat, MARGIN = 2, FUN = function(x){ return(c(sum(x == 0)/length(x) < thresh))})]
  names.dat[!(names.dat %in% names(dat))]
  predAdj <- x[x %in% names(dat)]
  dirEffAdj <- dirEff[x %in% names(dat)]

  y <- dat[, y]
  X <- subset(x = dat, select = x[x %in% names(dat)])

  adjR2 <- 0        # First, set adjusted R^2 to zero
  resPred <- NA     # Vector to be filled with predictors included in the model
  resModel <- NA    # Object referring to model
  dirEstCoeff.tmp <- vector()
  adjR2.tmp <- vector()
  lm.tmp <- list()
  pred.ind <- vector()


  for(j in 1:ncol(X)){

    if(j == 1){
      dirEstCoeff.tmp <-  rep(NA, ncol(X))  # Define empty vector for sign of estimated coefficients
      adjR2.tmp <- rep(NA, ncol(X))         # Define empty vector for adjusted R^2's of univariate regressions.

      for(i in 1:ncol(X)){
        lm.tmp <- stats::lm(y ~ X[ , i], data = dat)           # Run univariate regressions
        dirEstCoeff.tmp[i] <- sign(lm.tmp$coefficients[2])  # Store sign of estimated coefficient.
        adjR2.tmp[i] <- summary(lm.tmp)$adj.r.squared       # Store adjusted R^2.
      }

      for(k in 1:ncol(X)){
        # Check whether additional predictor leads to an increase of adjusted R^2 by more than 1%.
        # Start with predictor that yields highest adjusted R^2.

        if(sort(x = adjR2.tmp, decreasing = TRUE)[k] - adjR2 > 0.01){  # sort(AdjR2.tmp, T)[1] alternatively for max(AdjR2.tmp)
          i.tmp <- order(adjR2.tmp, decreasing = TRUE)[k]
          # Check whether sign of corresponding predictor coefficient goes in line with pre-specified direction of effect

          if(dirEffAdj[i.tmp] %in% if(dirEffAdj[i.tmp] == 0){ c( 0,  dirEstCoeff.tmp[i.tmp]) } else{dirEstCoeff.tmp[i.tmp]}){
            # Overwrite 'res.pred', 'res.model', and 'AdjR2'.
            resPred  <- colnames(X)[i.tmp]
            resModel <- stats::lm(stats::as.formula(paste("y ~",
                                             paste(resPred, collapse = "+"), # Here we can put any vector of predictor names.
                                             sep = "")),
                            data=dat)
            adjR2 <- sort(adjR2.tmp, decreasing = TRUE)[k]
            # Set 'k' to 'ncol(Dat2)-j' to jump out of for-loop.
            pred.ind <- i.tmp
            k <- ncol(X)
          }
        }
      }

      if(is.na(resPred)){  # If no predictor can be found that fulfills the two conditions defined above return message.
        resModel <- "No model can be built!"
        break
      }
    }

    if(j > 1){
      dirEstCoeff.tmp <- list()           # Define empty list for sign of estimated coefficients.
      adjR2.tmp <- rep(NA, ncol(X)-j)  # Define empty vector for adjusted R^2's of multiple regressions.

      for(i in 1:(ncol(X)-j+1)){
        cols.tmp <- names(X)[-which(names(X) %in% c("y",resPred))]
        # Define vector containing names of current predictors.
        resPred.tmp <- c(resPred, cols.tmp[i])
        # Run multiple regression.
        lm.tmp <- stats::lm(stats::as.formula(paste("y ~",
                                      paste(resPred.tmp, collapse = "+"), # Here we can put any vector of colnames.
                                      sep = "")),
                     data=dat)
        dirEstCoeff.tmp[[i]] <- sign(lm.tmp$coefficients[-1]) # Store signs of estimated coefficient.
        adjR2.tmp[i] <- summary(lm.tmp)$adj.r.squared       # Store adjusted R^2.
      }

      # Check whether additional predictor leads to an increase of adjusted R^2 by more than 1%.
      # Start with predictor that yields highest adjusted R^2.
      for(k in 1:(ncol(X)-j+1)){

        if(sort(adjR2.tmp, decreasing = TRUE)[k] - adjR2 > 0.01){
          i.tmp <- order(adjR2.tmp, decreasing = TRUE)[k]
          # Check whether signs of corresponding predictor coefficient goes in line with pre-specified directions of effects.

          if(all(apply(cbind(dirEffAdj[c(pred.ind, which(predAdj %in% cols.tmp[i.tmp]) )], dirEstCoeff.tmp[[i.tmp]] ),
#                            dirEffAdj[c(pred.ind, which(predAdj %in% cols.tmp[i.tmp]) )],
#                             0,                                                            # estimate of zero
#                             dirEstCoeff.tmp[[i.tmp]]),
                       1,
                       FUN = function(x) { x[1] == 0 | (x[1] %in% x[-1]) }))){
            # Overwrite 'res.pred', 'res.model', and 'AdjR2'.
            resPred <- c(resPred, cols.tmp[i.tmp])
            resModel <- stats::lm(stats::as.formula(paste("y ~",
                                             paste(resPred, collapse = "+"), # here we can put any vector of colnames
                                             sep = "")),
                            data=dat)
            adjR2 <- sort(adjR2.tmp, decreasing = TRUE)[k]
            # Set 'k' to 'ncol(Dat2)-j' to jump out of for-loop.
            k <- ncol(X)-j+1
          }
        }
      }

      pred.ind <- c(pred.ind, which(predAdj %in% cols.tmp[i.tmp]) )

      if(length(resPred) < j){ # If no additional predictor can be found that fulfills the two conditions defined jump out of for-loop.
        break
      }
    }
  }

  while(any(summary(resModel)$coefficients[-1,4] > thresh_pval)){ # Remove sequentially predictors attributed to p-value larger than 0.1.
    ind.tmp <- which.max(summary(resModel)$coefficients[-1,4])
    resPred <- resPred[-ind.tmp]
    resModel <- stats::lm(stats::as.formula(paste("y ~",
                                     paste(resPred, collapse = "+"), # here we can put any vector of colnames
                                     sep = "")),
                    data=dat)
  }

#  attr(resModel, "class")  <- "parLUR"

  return(resModel) # Return final model derived by supervised forward stepwise predictor selection.

}






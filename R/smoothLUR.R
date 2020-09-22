
##########################################
### Function to derive smooth LUR model
##########################################









#' Function for deriving smooth land use regression (LUR) model
#'
#' \code{smoothLUR} fits a smooth land use regression (LUR) model
#'     using the gam() function from the mgcv package. The procedure
#'     is outlined in \insertCite{Fritsch2020smooth;textual}{smoothLUR}
#'
#' @aliases smoothLUR
#' @param data A data set which contains the dependent variable and
#'    the potential predictors.
#' @param x A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param spVar1 A character vector stating the variable name referring
#'    to longitude (name has to match the column name of `data`).
#' @param spVar2 A character vector stating the variable name referring
#'    to latitude (name has to match the column name of `data`).
#' @param y A character string indicating the name of the dependent
#'    variable (name has to match the column name of `data`).
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding potential
#'    predictor is excluded.
#' @return An object of class `smoothLUR` with the following elements:
#'
#' \item{coefficients}{a vector containing the coefficient estimates}
#'
#' It has `...`, `...`, and `...` methods.
#'
#' @author Svenia Behm and Markus Fritsch
#' @export
#' @importFrom stats as.formula
#' @importFrom stats gaussian
#' @importFrom mgcv gam
#'
#' @seealso
#'
#' \code{\link{parLUR}} for parametric land use regression (LUR)
#'    models.
#' \code{\link{kFoldCV}} for k-fold cross-validation for
#'    parLUR and smoothLUR objects.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' ## Load data set
#' data(monSitesDE, package="smoothLUR")
#' set.seed(42)
#'
#' ## Code example
#' dat <- monSitesDE[sample(1:nrow(monSitesDE), 40),]
#' m1 <- smoothLUR(data = dat
#'                  ,x = c("Lon", "Lat", "Alt", "HighDens"
#'                          ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
#'                          ,"UrbGreen", "Agri", "Forest", "PopDens"
#'                          ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
#'                  ,spVar1 = "Lon"
#'                  ,spVar2 = "Lat"
#'                  ,y = "Y"
#'                  ,thresh = 0.95)
#'
#' summary(m1)
#' summary(m1)$adj.r.squared
#' BIC(m1)
#' AIC(m1)
#'
#' \donttest{
#' ## Load data set
#' data(monSitesDE, package="smoothLUR")
#' dat <- monSitesDE
#' m1 <- smoothLUR(data = dat,
#'                  ,x = c("Lon", "Lat", "Alt", "HighDens"
#'                          ,"LowDens", "Ind", "Transp", "Seap", "Airp", "Constr"
#'                          ,"UrbGreen", "Agri", "Forest", "PopDens"
#'                          ,"PriRoad", "SecRoad", "FedAuto", "LocRoute")
#'                  ,spVar1 = "Lon"
#'                  ,spVar2 = "Lat"
#'                  ,y = "Y"
#'                  ,thresh = 0.95)
#'
#' summary(m1)
#' summary(m1)$adj.r.squared
#' BIC(m1)
#' AIC(m1)
#'
#' }
#' }
smoothLUR <- function(
    data
    ,x
    ,spVar1
    ,spVar2
    ,y
    ,thresh = 0.95
  ){

  dat <- data.frame(subset(x = data, select = c(y, x)))
  names.dat <- names(dat)

  dat <- dat[, apply(X = dat, MARGIN = 2, FUN = function(x){ return(c(sum(x == 0)/length(x) < thresh))})]
  dat <- dat[, apply(X = dat, MARGIN = 2, FUN = function(x){ return(c(length(unique(x)) > 8))})]
  # 9 parameters have to be estimated by default for each univariate thin plate regression spline
  names.dat[!(names.dat %in% names(dat))]
  predAdj <- x[x %in% names(dat)]

  y <- dat[, y]
  X <- subset(x = dat, select = x[x %in% names(dat)])

  names.tmp <- names(X)[!names(X) %in% c(spVar1, spVar2)]

  form.tmp <- stats::as.formula(paste("y ~ s(",spVar1, ",", spVar2,",k = -1, bs=\"tp\") + ", # bivariate spline for longitude and latitude always considered in the model
                               paste0("s(", names.tmp, ",k=8, bs=\"tp\")", collapse = "+"), # Here we can put any vector of predictor names.
                               sep = ""))
  # to enable 10-fold CV for traffic/industrial sites the parameter k has to be reduced,
  # otherwise the error message
  # "Fehler in gam(formula = form.tmp, fit = TRUE, method = "P-ML", data = cbind(y,  :
  # Model has more coefficients than data"
  # is returned
  gam.tmp <- mgcv::gam(formula=form.tmp, fit=TRUE, method="P-ML", data=cbind(y,X), family=gaussian(),
                 weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
                 select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)

#  attr(gam.tmp, "class")  <- "smoothLUR"

  return(gam.tmp)

}




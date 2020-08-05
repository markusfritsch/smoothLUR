###########################################################
### Function to derive smooth LUR model using mgcv package
###########################################################









#' Function for deriving smooth land use regression (LUR) model
#'
#' \code{smoothLUR} fits a smooth land use regression (LUR) model
#'     using the gam() function from the mgcv package. The procedure
#'     automatically carries out the procedure outlined in ...
#'
#' @aliases smoothLUR
#' @param data A data set which contains the dependent variable and
#'    the potential predictors.
#' @param pred A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param spVar1 A character vector stating the variable name referring
#'    to longitude (name has to match the column name of `data`)
#' @param spVar2 A character vector stating the variable name referring
#'    to latitude (name has to match the column name of `data`)
#' @param depVar A character string indicating the name of the dependent
#'    variable.
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
#' @○importFrom mgcv gam
#'
#' @seealso
#'
#' \code{\link{escapeLUR}} for smooth land use regression (LUR)
#'    models.
#' \code{\link{kFoldCV}} for k-fold cross-validation for
#'    escapeLUR and smoothLUR objects.
#' \code{\link{looCV}} for leave-one-out cross-validation for
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
#' set.seed(42)
#' dat <- dat[sample(1:nrow(dat), 40),]
#'
#' ## Code example
#'
#' m1 <- smoothLUR(data = dat, pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#'                                             ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#'                                             ,"Constr", "UrbGreen", "Agri", "Forest"
#'                                             , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#'                                             , "LocRoute")
#'                        ,depVar = "AQeYMean"
#'                        ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#'                        ,thresh = 0.95)
#'
#' summary(res.model)
#' summary(res.model)$adj.r.squared
#' BIC(res.model)
#' AIC(res.model)
#'
#' \donttest{
#' ## Load data set
#' dat <- monSitesDE
#'
#' ## 
#' m1 <- smoothLUR(data = dat, pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#'                                             ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#'                                             ,"Constr", "UrbGreen", "Agri", "Forest"
#'                                             , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#'                                             , "LocRoute")
#'                        ,depVar = "AQeYMean"
#'                        ,dirEff = c(0,0,-1,1,1,1,1,1,1,1,-1,0,-1,1,1,1,1,1)
#'                        ,thresh = 0.95)
#'
#' summary(res.model)
#' summary(res.model)$adj.r.squared
#' BIC(res.model)
#' AIC(res.model) 
#'
#' }
smoothLUR <- function(
    data
    ,pred
    ,spVar1
    ,spVar2
    ,depVar
    ,thresh = 0.95
  ){

  dat <- data.frame(subset(x = data, select = c(depVar, pred)))
  names.dat <- names(dat)

  dat <- dat[, apply(X = dat, MARGIN = 2, FUN = function(x){ return(c(sum(x == 0)/length(x) < thresh))})]
  dat <- dat[, apply(X = dat, MARGIN = 2, FUN = function(x){ return(c(length(unique(x)) > 8))})]
  # 9 parameters have to be estimated by default for each univariate thin plate regression spline
  names.dat[!(names.dat %in% names(dat))]
  predAdj <- pred[pred %in% names(dat)]

  y <- dat[, depVar]
  X <- subset(x = dat, select = pred[pred %in% names(dat)])

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


#dat <- read.csv("DATA_monitoringSites_DE.csv", header = TRUE)
#
#pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#        ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#        ,"Constr", "UrbGreen", "Agri", "Forest"
#        , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#        , "LocRoute")
#depVar = "AQeYMean"
#spVar1 = "AQeLon"
#spVar2 = "AQeLat"
#thresh = 0.95
#
#
# (res.model <- smoothLUR(data = dat[dat$AQeType!="background", ]
#                      ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#                                ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#                                ,"Constr", "UrbGreen", "Agri", "Forest"
#                                , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#                                , "LocRoute")
#                      ,spVar1 = "AQeLon"
#                      ,spVar2 = "AQeLat"
#                      ,depVar = "AQeYMean"
#                      ,thresh = 0.95) )
#
#
# dat <- read.csv("DATA_monitoringSites_DE.csv", header = TRUE)
# (res.model <- smooth(data = dat[dat$AQeType!="background", ]
#                      ,pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
#                                ,"LowDens", "Ind", "Transp", "Seap", "Airp"
#                                ,"Constr", "UrbGreen", "Agri", "Forest"
#                                , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
#                                , "LocRoute")
#                      ,spVar1 = "AQeLon"
#                      ,spVar2 = "AQeLat"
#                      ,depVar = "AQeYMean"
#                      ,thresh = 0.95) )


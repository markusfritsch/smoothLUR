###########################################################
### Function to derive smooth LUR model using mgcv package
###########################################################









#' Function for deriving smooth land use regression (LUR) model
#'
#' \code{smooth} fits a smooth land use regression (LUR) model using the gam() function
#'     from the mgcv package. The procedure automatically carries out the
#'     procedure outlined in ...
#'
#' @aliases smooth
#' @param data A data set which contains the dependent variable and the
#'    potential predictors.
#' @param pred A character vector stating the variable names of the
#'    potential predictors (names have to match the column names of
#'    `data`).
#' @param depVar A character string indicating the name of the dependent
#'    variable.
#' @param thresh A numeric value that indicates the maximum share of
#'    zero values; if the share is exceeded, the corresponding potential
#'    predictor is excluded.



#install.packages(mgcv)
library(mgcv)



smooth <- function(
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
  names.dat[!(names.dat %in% names(dat))]
  predAdj <- pred[pred %in% names(dat)]

  y <- dat[, depVar]
  X <- subset(x = dat, select = pred[pred %in% names(dat)])

  names.tmp <- names(X)[!names(X) %in% c(spVar1, spVar2)]

  form.tmp <- as.formula(paste("y ~ s(",spVar1, ",", spVar2,",k = -1, bs=\"tp\") + ", # bivariate spline for longitude and latitude always considered in the model
                               paste0("s(", names.tmp, ",k=-1, bs=\"tp\")", collapse = "+"), # Here we can put any vector of predictor names.
                               sep = ""))
  gam.tmp <- gam(formula=form.tmp, fit=TRUE, method="P-ML", data=cbind(y,X), family=gaussian(),
                 weights=NULL, subset=NULL, offset=NULL, optimizer=c("outer", "newton"), scale=0,
                 select=TRUE, knots=NULL, sp=NULL, min.sp=NULL, H=NULL, gamma=1, paraPen=NULL, G=NULL)
  return(gam.tmp)
}


dat <- read.csv("DATA_monitoringSites_DE.csv", header = TRUE)


(res.model <- smooth(data = dat[dat$AQeType=="background", ], pred = c("AQeLon", "AQeLat", "AQeAlt", "HighDens"
                                          ,"LowDens", "Ind", "Transp", "Seap", "Airp"
                                          ,"Constr", "UrbGreen", "Agri", "Forest"
                                          , "BBSRpopDens", "PriRoad", "SecRoad", "NatMot"
                                          , "LocRoute")
                     ,spVar1 = "AQeLon"
                     ,spVar2 = "AQeLat"
                     ,depVar = "AQeYMean"
                     ,thresh = 0.95) )


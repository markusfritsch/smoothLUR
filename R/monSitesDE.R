#' Average NO2 pollution level data for German monitoring sites
#'
#' Data on average NO2 pollution levels for German monitoring sites
#' for 2015. The dataset contains NO2 pollution concentration levels
#' recorded at 403 monitoring sites and meta-data on the location
#' and the area surrounding the monitoring sites.
#'
#' The meta-data on the location include longitude, latitude, and
#' altitude. The surrounding area is characterized by computing
#' the fractions of various land cover classes, population density,
#' and the length of the road traffic network.
#'
#' @name monSitesDE
#'
#' @docType data
#'
#' @usage data(monSitesDE)
#'
#' @format A dataset with 403 rows and 25 variables containing:
#' \describe{
#'   \item{AQeCode}{code of monitoring site as in AirBase}
#'   \item{AQeMean}{mean NO2 concentration level}
#'   \item{Year}{year}
#'   \item{Projection}{coordinate reference system of the geocoordinates longitude (AQeLon) and latitude (AQeLat)}
#'   \item{AQeLon}{geocoordinate longitude (decimal degrees) at which the monitoring site is located}
#'   \item{AQeLat}{geocoordinate latitude (decimal degrees) at which the monitoring site is located}
#'   \item{AQeAlt}{altitude (meter above sea level) at which the monitoring site is located}
#'   \item{AQeType}{type of monitoring site - information whether it is measuring background, industrial, or traffic related air pollution}
#'   \item{AQeArea}{area of monitoring site - information whether it is measuring air pollution in urban, suburban, rural, etc. environment}
#'   \item{HighDens}{proportion of high density residential area within a buffer of radius 1km around the monitoring site}
#'   \item{LowDens}{proportion of low density residential area within a buffer of radius 1km around the monitoring site}
#'   \item{Ind}{proportion of industrial area within a buffer of radius 1km around the monitoring site}
#'   \item{Transp}{proportion of area attributed to transport within a buffer of radius 1km around the monitoring site}
#'   \item{Seap}{proportion of area attributed to seaport within a buffer of radius 1km around the monitoring site}
#'   \item{Airp}{proportion of area attributed to airport within a buffer of radius 1km around the monitoring site}
#'   \item{Constr}{proportion of area attributed to construction within a buffer of radius 1km around the monitoring site}
#'   \item{UrbGreen}{proportion of area attributed to urban green spaces within a buffer of radius 1km around the monitoring site}
#'   \item{Agri}{proportion of agricultural area within a buffer of radius 1km around the monitoring site}
#'   \item{Forest}{proportion of forestry area within a buffer of radius 1km around the monitoring site}
#'   \item{PopDens}{population density (inhabitants per km^2) at municipality key level}
#'   \item{PriRoad}{primary roads (length in meter) within buffer of radius 1km around the monitoring site}
#'   \item{SecRoad}{secondary roads (length in meter) within buffer of radius 1km around the monitoring site}
#'   \item{FedAuto}{federal autobahn (length in meter) within buffer of radius 1km around the monitoring site}
#'   \item{LocRoute}{local routes (length in meter) within buffer of radius 1km around the monitoring site}
#'   \item{IndRegions}{indicator for the German federal state the monitoring site is located in}
#' }
#'
#' @keywords datasets
#'
#' @references Fritsch and Behm (2020), Working paper
#'
#' @source \href{b21a537e763e4ad9ac8ccffe987d6f77}{European Environmental Agency: Air Quality e-Reporting}
#' @source \href{http://land.copernicus.eu/pan-european/corine-land-cover/clc-2012/view}{European Environmental Agency: CORINE land cover (CLC) 2015 raster data - version 18.5.1 (09/2016)}
#' @source \href{https://eurogeographics.org/maps-for-europe/open-data/}{EuroGeographics: Euroglobalmap (egm), v9.0}
#' @source \href{https://gdz.bkg.bund.de/index.php/default/digitale-geodaten.html}{Federal Government for Geo-Information and Geodesy, DGM200 GK3 GRID-ASCII, GeoBasis-DE / BKG 2015}
#' @source \href{https://gdz.bkg.bund.de/index.php/default/digitale-geodaten.html}{Federal Government for Geo-Information and Geodesy, VG250-EW Ebenen GK3 Shape, GeoBasis-DE /BKG 2015}
#'
#' @examples
#' \dontrun{
#'   data(monSitesDE, package = "smoothLUR")
#'   pol <- monSitesDE$AQeYMean
#'   typ <- as.factor(monSitesDE$AQeType)
#'   \donttest{plot(y = pol, x = typ)}
#' }
#'
NULL

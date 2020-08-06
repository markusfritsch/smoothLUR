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
#' @format A dataset with 403 rows and 30 variables containing:
#' \describe{
#'   \item{AQeCode}{...}
#'   \item{AQeMean}{mean NO2 concentration level}
#'   \item{Year}{year}
#'   \item{Projection}{...}
#'   \item{AQeLon}{...}
#'   \item{AQeLat}{...}
#'   \item{AQeAlt}{...}
#'   \item{AQeType}{...}
#'   \item{AQeArea}{...}
#'   \item{AQeInletHeight}{...}
#'   \item{HighDens}{...}
#'   \item{LowDens}{...}
#'   \item{Ind}{...}
#'   \item{Transp}{...}
#'   \item{Seap}{...}
#'   \item{Airp}{...}
#'   \item{Constr}{...}
#'   \item{UrbGreen}{...}
#'   \item{Agri}{...}
#'   \item{Forest}{...}
#'   \item{AGS}{...}
#'   \item{GEN}{...}
#'   \item{NUTS}{...}
#'   \item{BBSRArea}{...}
#'   \item{BBSRArea2}{...}
#'   \item{BBSRpopDens}{...}
#'   \item{PriRoad}{...}
#'   \item{SecRoad}{...}
#'   \item{NatMot}{...}
#'   \item{LocRoute}{...}
#' }
#'
#' @keywords datasets
#'
#' @references Fritsch and Behm (2020), Working paper
#'
#' @source \href{https://www.eea.europa.eu/data-and-maps/data/aqereporting-8}{European Environmental Agency: Air Quality e-Reporting}
#' @source \href{http://land.copernicus.eu/pan-european/corine-land-cover/clc-2012/view}{European Environmental Agency: CORINE land cover data}
#' @source \href{https://eurogeographics.org/maps-for-europe/open-data/}{EuroGeographics: Euroglobalmap (egm)}
#' @source \href{https://gdz.bkg.bund.de/index.php/default/digitale-geodaten.html}{Federal Government for Geo-Information and Geodesy, DGM200 GK3 GRID-ASCII, GeoBasis-DE}
#' @source \href{https://www.bbr.bund.de/BBSR/DE/Raumbeobachtung/Downloads/downloads_node.html#doc443480bodyText2}{Federal institute for research on building, urban affairs and spatial development, raumtypen: Besiedelung und Lage.}
#'
#' @examples
#' \dontrun{
#'   data(monSitesDE, package = "smoothLUR")
#'   pol <- monSitesDE$AQeYMean
#'   typ <- as.factor(monSitesDE$type)
#'   \donttest{plot(y = pol, x = typ)}
#' }
#' 
NULL
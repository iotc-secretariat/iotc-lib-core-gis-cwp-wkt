#' All fishing grounds data, including their WKT
#'
#' @format
#' \describe{
#'   \item{CODE}{The alphanumeric fishing ground code}
#'   \item{NAME_EN}{The English name for the fishing ground}
#'   \item{NAME_FR}{The French name for the fishing ground}
#'   \item{OCEAN_AREA_SURFACE_KM2 }{The area (in km2) of the ocean part of the fishing ground}
#'   \item{LAT}{The latitude of the centroid for the ocean area of the fishing ground}
#'   \item{LON}{The longitude of the centroid for the ocean area of the fishing ground}
#'   \item{WKT}{The WKT for the (multi)polygon associated to the fishing ground}
#' }
"FISHING_GROUNDS_ALL"

#' Fishing grounds data for EEZs only, including their WKT
#'
#' @format
#' \describe{
#'   \item{CODE}{The alphanumeric fishing ground code for the EEZ}
#'   \item{NAME_EN}{The English name for the EEZ fishing ground}
#'   \item{OCEAN_AREA_SURFACE_KM2 }{The area (in km2) of the ocean part of the EEZ}
#'   \item{LAT}{The latitude of the centroid for the ocean area of the EEZ}
#'   \item{LON}{The longitude of the centroid for the ocean area of the EEZ}
#'   \item{WKT}{The WKT for the (multi)polygon associated to the EEZ}
#' }
"FISHING_GROUNDS_EEZ"

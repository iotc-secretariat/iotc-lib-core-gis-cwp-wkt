#' Returns all data (CODE, NAME_EN, NAME_FR, OCEAN_AREA_SURFACE_KM2, CENTER_LAT, CENTER_LON, WKT) for the a given set of fishing ground codes
#'
#' @param fishing_ground_codes The list of fishing ground codes to consider (defaults to the codes for WIO and EIO)
#' @return a data frame containing all data fields for the unique fishing grounds with the provided \code{fishing_ground_codes}
#' @examples
#' fishing_grounds_data(c("IRFAO51", "IRFAO57"))
#' fishing_grounds_data("IRALLHS")
#' @export
fishing_grounds_data = function(fishing_ground_codes = c("IREASIO", "IRWESIO")) {
  return(
    FISHING_GROUNDS_ALL[CODE %in% fishing_ground_codes]
  )
}

#' Returns all data (CODE, NAME_EN, NAME_FR, OCEAN_AREA_SURFACE_KM2, CENTER_LAT, CENTER_LON, WKT) for fishing ground codes matching a provided pattern
#'
#' @param pattern The pattern (as expected by the LIKE operator) of fishing ground codes to consider (defaults to the codes for WIO and EIO)
#' @return a data frame containing all data for the fishing grounds with a fishing ground code matching the provided \code{pattern}
#' @examples
#' fishing_grounds_data_by_pattern("^5")
#' fishing_grounds_data("SYC")
#' @export
fishing_grounds_data_by_pattern = function(pattern) {
  return(
    FISHING_GROUNDS_ALL[CODE %like% pattern]
  )
}

#' Returns all data for fishing grounds corresponding to individual EEZs
#'
#' @return a data frame containing all data for the fishing grounds corresponding to individual EEZs
#' @export
EEZs_fishing_grounds_data = function() {
  return(
    FISHING_GROUNDS_EEZ
  )
}

#' Converts all rows of a dataframe produced by the \code{fishing_grounds_data}, \code{fishing_grounds_data_by_pattern} and \code{EEZs_fishing_grounds_data}
#' to simple fatures' instances, using the \code{WKT} column to define the spatial boundaries of the fishing ground
#'
#' @param fishing_ground_data A data frame containing all information for one or more fishing grounds
#' @param crs a coordinate reference system (defaults to EPSG:4326)
#' @param simplify If TRUE, the resulting spatial boundary will be simplified, otherwise it's returned as originally provided
#' @return a data frame containing all data for the fishing grounds corresponding to individual EEZs
#' @export
WKT_to_simple_feature = function(fishing_grounds, crs = "EPSG:4326", simplify = FALSE) {
  features = sf::st_as_sf(fishing_grounds, wkt = "WKT")

  sf::st_crs(features) = crs

  tryCatch({
      features = sf::st_make_valid(features)
    }, error = function(e) {
      warning(paste0("Unable to apply makeValid to EPSG data for fishing ground ", code, ": ", e))
    }
  )

  if(simplify) { features = sf::st_simplify(features, preserveTopology = FALSE, dTolerance = 50) }

  return(features)
}

#' Produces the spatial features for the provided fishing ground codes
#'
#' @param fishing_ground_codes A set of fishing ground codes
#' @param crs a coordinate reference system (defaults to EPSG:4326)
#' @param simplify If TRUE, the resulting spatial boundary will be simplified, otherwise it's returned as originally provided
#' @return the spatial features for the fishing grounds with the provided \code{fishing_ground_codes}
#' @export
sf_by_code = function(fishing_ground_codes, crs = "EPSG:4326", simplify = FALSE) {
  return (
    WKT_to_simple_feature(
      FISHING_GROUNDS_ALL[CODE %in% fishing_ground_codes],
      crs,
      simplify
    )
  )
}

#' Produces the spatial features for a given number of fishing grounds
#'
#' @param fishing_grounds_data A set of fishing grounds
#' @param crs a coordinate reference system (defaults to EPSG:4326)
#' @param simplify If TRUE, the resulting spatial boundary will be simplified, otherwise it's returned as originally provided
#' @return the spatial features for the provided \code{fishing_grounds_data}
#' @export
sf_for = function(fishing_grounds_data, crs = "EPSG:4326", simplify = FALSE) {
  return (
    WKT_to_simple_feature(fishing_grounds_data)
  )
}

SF_CACHE = new.env(hash = TRUE)
DT_CACHE = new.env(hash = TRUE)

#' Clears the simple-features cache
#' @export
clear_SF_cache = function() {
  cache_clear(SF_CACHE)
}

#' Clears the data cache
#' @export
clear_DT_cache = function() {
  cache_clear(DT_CACHE)
}

#' Clears the simple-features as well as the data cache
#' @export
clear_all_caches = function() {
  clear_SF_cache()
  clear_DT_cache()
}

#' Connects to an instance of the IOTCStatistics DB and returns all data (CODE, NAME_EN, NAME_FR, OCEAN_AREA_SURFACE_KM2, CENTER_LAT, CENTER_LON, WKT) for the a given set of fishing ground codes
#'
#' @param fishing_ground_codes The list of fishing ground codes to consider (defaults to the codes for WIO and EIO)
#' @param connection An active connection to the IOTCStatistics DB (defaults to the instance on 'IOTCS09')
#' @return a data frame containing all data fields for the unique fishing grounds with the provided \code{fishing_ground_codes}
#' @examples
#' fishing_grounds_data(c("IRFAO51", "IRFAO57"))
#' fishing_grounds_data("IRALLHS")
#' @export
fishing_grounds_data = function(fishing_ground_codes = c("IREASIO", "IRWESIO"), connection = DB_IOTCSTATISTICS()) {
  key = paste(fishing_ground_codes, collapse = "|")
  key = paste0(dbGetInfo(connection)$servername, "|", key)

  return(
    cache_get_or_set(DT_CACHE, key, {
      query(
        connection,
        paste0("
          SELECT
            FG.CODE,
            FG.NAME_EN AS NAME_EN,
            FG.NAME_FR AS NAME_FR,
            FG.AREA_FRACTION_IO AS OCEAN_AREA_SURFACE_KM2,
            FG.CENTER_LAT,
            FG.CENTER_LON,
           (EPSG_DATA.MakeValid()).STAsText() AS WKT
          FROM
            CL_FISHING_GROUNDS FG
          WHERE
            CODE IN (", paste(shQuote(fishing_ground_codes, type="sh"), collapse=", "), ")
        ")
      )
    })
  )
}

#' Connects to an instance of the IOTCStatistics DB and returns all data (CODE, NAME_EN, NAME_FR, OCEAN_AREA_SURFACE_KM2, CENTER_LAT, CENTER_LON, WKT) for fishing ground codes matching a provided pattern
#'
#' @param pattern The pattern (as expected by the LIKE operator) of fishing ground codes to consider (defaults to the codes for WIO and EIO)
#' @param connection An active connection to the IOTCStatistics DB (defaults to the instance on 'IOTCS09')
#' @return a data frame containing all data for the fishing grounds with a fishing ground code matching the provided \code{pattern}
#' @examples
#' fishing_grounds_data(c("IRFAO51", "IRFAO57"))
#' fishing_grounds_data("IRALLHS")
#' @export
fishing_grounds_data_by_pattern = function(pattern, connection = DB_IOTCSTATISTICS()) {
  key = paste(pattern, collapse = "|")
  key = paste0(dbGetInfo(connection)$servername, "|", key)

  return(
    cache_get_or_set(DT_CACHE, key, {
      query(
        connection,
        paste0("
          SELECT
            FG.CODE,
            FG.NAME_EN AS NAME_EN,
            FG.NAME_FR AS NAME_FR,
            FG.AREA_FRACTION_IO AS OCEAN_AREA_SURFACE_KM2,
            FG.CENTER_LAT,
            FG.CENTER_LON,
           (EPSG_DATA.MakeValid()).STAsText() AS WKT
          FROM
            CL_FISHING_GROUNDS FG
          WHERE
            CODE LIKE '", pattern, "'"
        )
      )
    })
  )
}

#' Connects to an instance of the IOTCStatistics DB and returns all data for fishing grounds corresponding to individual EEZs
#'
#' @param connection An active connection to the IOTCStatistics DB (defaults to the instance on 'IOTCS09')
#' @return a data frame containing all data for the fishing grounds corresponding to individual EEZs
#' @export
EEZs_fishing_grounds_data = function(connection = DB_IOTCSTATISTICS()) {
  key = "EEZs_fishing_grounds_data"
  key = paste0(dbGetInfo(connection)$servername, "|", key)

  return(
    cache_get_or_set(DT_CACHE, key, {
      query(
        connection,
        paste0("
          WITH CPCS AS (
            SELECT DISTINCT CODE FROM CL_COUNTRIES
            UNION ALL
            SELECT 'EUR' -- FOR REUNION EEZ CODE
            UNION ALL
            SELECT 'EUM' -- FOR MAYOTTE EEZ CODE
            UNION ALL
            SELECT 'FRT' -- FOR FRANCE(OT) EEZ CODE
            UNION ALL
            SELECT 'GBT' -- FOR UK(BIOT) EEZ CODE
          ), EEZ_FISHING_GROUNDS AS (
    		    SELECT
    		      CODE,
    		      NAME_EN,
    		      NAME_FR,
              CENTER_LAT,
              CENTER_LON,
    		      AREA_FRACTION_IO AS OCEAN_AREA_SURFACE_KM2,
    		      EPSG_DATA
    		    FROM
    		      CL_FISHING_GROUNDS
    		    WHERE
    			    CL_FISHING_GROUND_TYPE_ID = 7 AND -- IRREGULAR AREA
    		    ( CODE LIKE 'IR%EZ' OR CODE LIKE 'IR%E2' ) -- STANDARD OR REVISED EEZ SUFFIX
    		  )
          SELECT
            FG.CODE,
            FG.NAME_EN,
            FG.CENTER_LAT,
            FG.CENTER_LON,
            OCEAN_AREA_SURFACE_KM2,
           (EPSG_DATA.MakeValid()).STAsText() AS WKT
          FROM
            EEZ_FISHING_GROUNDS FG
          INNER JOIN
            CPCS CO
          ON
            FG.CODE = CONCAT('IR', CO.CODE, 'EZ') OR
            FG.CODE = CONCAT('IR', CO.CODE, 'E2')
    	    ORDER BY 1 ASC
        ")
      )
    })
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
WKT_to_simple_feature = function(fishing_ground_data, crs = "EPSG:4326", simplify = FALSE) {
  features = data.frame()

  for(code in fishing_ground_data$CODE) {
    features =
      rbind(
        features,
        cache_get_or_set(SF_CACHE, code, {
          f = subset(fishing_ground_data, CODE == code)

          feature = st_as_sf(f, wkt = "WKT")

          st_crs(feature) = crs

          tryCatch({
              feature = st_make_valid(feature)
            }, error = function(e) {
              warning(paste0("Unable to apply makeValid to EPSG data for fishing ground ", code, ": ", e))
            }
          )

          if(simplify) { feature = st_simplify(feature, preserveTopology = FALSE, dTolerance = 50) }

          feature
        })
      )
  }

  return (features)
}

#' Produces the spatial features for the provided fishing ground codes
#'
#' @param fishing_ground_codes A set of fishing ground codes
#' @param crs a coordinate reference system (defaults to EPSG:4326)
#' @param simplify If TRUE, the resulting spatial boundary will be simplified, otherwise it's returned as originally provided
#' @return the spatial features for the fishing grounds with the provided \code{fishing_ground_codes}
#' @export
sf_by_code = function(fishing_ground_codes, crs = "EPSG:4326", simplify = FALSE, connection = DB_IOTCSTATISTICS()) {
  return (
    WKT_to_simple_feature(
      fishing_grounds_data(
        fishing_ground_codes,
        connection = connection
      ),
      crs = crs,
      simplify = simplify
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

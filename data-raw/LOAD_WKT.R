library(iotc.base.common.data)
library(iotc.core.gis.cwp.IO.standalone)

all_fishing_grounds_data = function(connection = DB_IOTCSTATISTICS()) {
  return(
    query(
      connection, "
      SELECT
        FG.CODE,
        FG.NAME_EN AS NAME_EN,
        FG.NAME_FR AS NAME_FR,
        FG.AREA_FRACTION_IO AS OCEAN_AREA_SURFACE_KM2,
        FG.CENTER_LAT,
        FG.CENTER_LON,
       (EPSG_DATA.MakeValid()).STAsText() AS WKT
      FROM
        CL_FISHING_GROUNDS FG"
    )
  )
}

all_EEZ_fishing_grounds_data = function(connection = DB_IOTCSTATISTICS()) {
  return(
    query(
      connection, "
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
	    ORDER BY 1 ASC"
    )
  )
}

FISHING_GROUNDS_ALL = all_fishing_grounds_data()
FISHING_GROUNDS_EEZ = all_EEZ_fishing_grounds_data()

usethis::use_data(FISHING_GROUNDS_ALL, overwrite = TRUE)
usethis::use_data(FISHING_GROUNDS_EEZ, overwrite = TRUE)

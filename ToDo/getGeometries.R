#' Load the geometries from an arealDB database
#'
#' @param path [\code{character(1)}]\cr the path of the database.
#' @param nation [\code{character(1)}]\cr the nation for which to get the data
#' @param layer [\code{character(1)}]\cr the name of the administrative level to
#'   get.
#' @param geoID [\code{itegerish(1)}]\cr subset the data by geoID.
#' @importFrom checkmate assertCharacter
#' @importFrom sf st_read
#' @export

getGeometries <- function(path = NULL, nation = NULL, layer = NULL, geoID = NULL){

  assertCharacter(x = path, any.missing = FALSE, len = 1)
  assertCharacter(x = nation, any.missing = FALSE, len = 1)
  assertCharacter(x = layer, any.missing = FALSE, len = 1)
  assertIntegerish(x = geoID, any.missing = FALSE, null.ok = TRUE, len = 1)

  inputPath <- paste0(path, "/adb_geometries/stage3/", nation, ".gpkg")
  assertFileExists(x = inputPath, access = "rw")

  temp <- st_read(dsn = inputPath, layer = layer, quiet = TRUE, stringsAsFactors = FALSE)
  if(!is.null(geoID)){
    theID <- geoID
    temp <- temp %>%
      filter(geoID == theID)
  }

  return(temp)
}

#' Assign typical raster attributes as RAT
#'
#' @param x [\code{character(1)}]\cr path to an object to asses.
#' @importFrom checkmate assertClass
#' @importFrom raster raster writeRaster
#' @export

makeAttributes <- function(x = NULL){

  assertFileExists(x = x, extension = "tif")

  temp <- raster(x = x)

  if(length(temp@data@attributes) != 0){
    if(all(names(temp@data@attributes[[1]]) %in% c("freq", "prop"))){
      invisible(0)
    }
  }

  freq <- table(values(temp))
  cells <- temp@ncols*temp@nrows

  tempAttrib <- data.frame(ID = as.numeric(names(freq)), freq = as.numeric(freq), prop = as.numeric(freq)/cells*100, stringsAsFactors = FALSE)

  if(!length(temp@data@attributes) == 0){
    outAttrib <- left_join(temp@data@attributes[[1]], tempAttrib)
  } else {
    outAttrib <- tempAttrib
  }

  out <- ratify(temp)
  out@data@attributes <- list(as.data.frame(outAttrib))

  newName <- str_split(string = x, "[.]")[[1]]
  newName <- paste0(newName[-length(newName)], ".rds")
  saveRDS(object = out, file = newName)

}

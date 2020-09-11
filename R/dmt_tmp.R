#' Retrieves or creates a session-specific temporary directory.
#' @param verbose Logical. Should the function inform on progress? Default is FALSE.
#' @return A \emph{character} element.
#' @details The function identifies/creates the DMT temporary directory, which is required to handle raster data.
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

dmt_tmp = function(verbose=FALSE) {

  # create temporary directory (needed to store VRT files that will be accessed with terra)
  if (is.null(getOption('rast.temp'))) {

    tmpDir =file.path(tempdir(), paste0('DMT-', as.character(Sys.getpid()), '_', Sys.Date()))
    dir.create(tmpDir, showWarnings=FALSE)
    options(rast.temp=tmpDir)

    if (verbose) warning("creating a temporary directory (don't forget to remove it when done!)")

  }

  return(getOption('rast.temp'))

}

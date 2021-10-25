#' Accesses legend for a given product.
#' @param data.id Element or vector of class \emph{character}.
#' @importFrom utils read.csv
#' @return {Returns a |emph{data.frame} with the legend of the target dataset, containing:
#' \itemize{
#' \item{\emph{value} - Grid value}
#' \item{\emph{label} - class label}
#' \item{\emph{hex} - Hex color code}
#'}}
#' @details The function provides access to legend information relative to data products existing in the MAS database.
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

get_legend <- function(data.id) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. test input argument
  #----------------------------------------------------------------------------------------------------------------------------#

  if (!is.character(data.id)) {stop('"dataset" is not a character')}
  tmp = strsplit(data.id, '/')[[1]]
  if (length(tmp) != 2) {stop('"dataset" should be provided as "DATASET/SUBDATASET" (e.g. "CCI_landCover/landCover"')}

  #----------------------------------------------------------------------------------------------------------------------------#
  # 2. build target file path
  #----------------------------------------------------------------------------------------------------------------------------#

  file = paste0(getOption('dmt.data'), tmp[1], '/', tmp[1], '-', tmp[2], '.csv') # build file path

  #----------------------------------------------------------------------------------------------------------------------------#
  # 3. find and return file (if existing)
  #----------------------------------------------------------------------------------------------------------------------------#

  if (file.exists(file)) {

    return(read.csv(file))

  } else {

    warning('no legend available')
    return(NULL)

  }

}

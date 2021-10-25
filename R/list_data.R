#' Create a sandbox containing small subsets of target datasets.
#' @param data.id Element or vector of class \emph{character}.
#' @importFrom dplyr tbl filter pull slice
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom stats complete.cases
#' @return A \emph{data.frame}.
#' @details {When no argument is used, the function will return a \emph{data.frame}
#' with a summary of the existing datasets and their general characteristics. When
#' \emph{data.id} is provided, the function will search for the corresponding dataset
#' and return a \emph{data.frame} reporting on the individual files that compose each
#' of the target datasets.}
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

list_data <- function(data.id) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. test input arguments
  #----------------------------------------------------------------------------------------------------------------------------#

  # evaluate dataset argument
  if (!missing(data.id)) {
    if (!is.character(data.id)) {stop('"dataset" is not a character')}

    id.df = do.call(rbind, lapply(data.id, function(id) {

      tmp = strsplit(id, '/')[[1]]
      mds = tmp[1] # main dataset
      if (length(tmp != 2)) sds = NA else sds = tmp[2] # subdataset

      # return data.frame with relevant information
      return(data.frame(dataset=tmp[1], subdataset=tmp[2], stringsAsFactors=F))

    }))

    # test if all "data.id" elements are valid
    ind = which(complete.cases(id.df))
    if (length(ind) != length(data.id)) {
      data.id = paste0(data.id[ind], collapse=', ')
      stop(paste0(data.id, ' passed to "data.id" but invalid; need dataset/subdaset (e.g. "CCI_landCover/landCover")'))
    }

  }

  #----------------------------------------------------------------------------------------------------------------------------#
  # 2. access list of datasets
  #----------------------------------------------------------------------------------------------------------------------------#

  # access database overview table
  idb <- dbConnect(SQLite(), paste0(getOption('dmt.data'), 'mas_database.sqlite'))

  # return overview table if no dataset is requested
  if (missing(data.id)) {

    data <- as.data.frame(tbl(idb, 'overview'))

  } else {

    # find data
    data = as.data.frame(filter(tbl(idb, 'complete'), dataset %in% !!id.df$dataset & subdataset %in% !!id.df$subdataset))

  }

  dbDisconnect(idb) # close database
  if (nrow(data) == 0) {warning('"data.id" does not matched existing datasets')}
  return(data) # return overview table

}

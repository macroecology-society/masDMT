#' Returns indices to process large tables
#' @param path Path to tabular data file.
#' @param size Numeric element specifying the max. data volume (in Mb).
#' @return \emph{list} object with indices
#' @importFrom readr read_delim
#' @importFrom fpeek peek_count_lines
#' @details {This function helps process tables that are larger 
#' than memory. Given a file \emph{path}, the function infers the 
#' storage size of the input file and determines an optimal number 
#' of runs. To do, the argument \emph{size} is used, which determines 
#' the maximum amount of RAM that should be occupied in a single read. 
#' Then, for each of the n runs, the function determines the corresponding 
#' start and end rows, which are reported as a list. The output can be used 
#' in combination with, e.g., \code{\link[readr]{read_delim}}, which allows 
#' users to specify starting and ending rows when importing a file.}
#' @export
#'
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

table_indices = function(path,size) {
  
  #---------------------------------------------------------------------------#
  # test arguments
  #---------------------------------------------------------------------------#
  
  # test if path exists and it is a valid element
  if (length(path) > 1) stop('"path" only allows one element at a time')
  if (!is.character(path)) stop('"path" is not a character element')
  if (!file.exists(path)) stop('"path" is not a valid file path')
  
  # try to read input file
  samples = tryCatch(read_delim(path, n_max=1, lazy=FALSE, 
                                show_col_types=FALSE), 
                     error=function(e) return(FALSE))
  
  # report if unable to read input file
  if (is.logical(samples)) stop('"path" is not a tabular file')
  
  # test size parameter
  if (length(size) > 1) stop('"size" only allows one element at a time')
  if (!is.numeric(size)) stop('"size" is not a numeric element')
  
  #---------------------------------------------------------------------------#
  # check composition of input file
  #---------------------------------------------------------------------------#
  
  # size of file
  file_size = file.size(path)/1e+6
  
  # number of rows in original file
  rows = peek_count_lines(path)
  
  #---------------------------------------------------------------------------#
  # build indices
  #---------------------------------------------------------------------------#
  
  # nr of runs to read the full file
  nruns = ceiling(file_size/size)
  
  # number of rows to process per iteration
  nrows = rep(floor(rows/nruns), nruns)
  
  # starting indexes of eac iteration
  index = unname(sapply(1:nruns, function(r) nrows[r]*(r-1)))
  
  # update last value if needed
  nrows[nruns] = nrows[nruns] + (rows-(index[nruns]+nrows[nruns]))
  
  return(list(start=index, end=nrows))
  
}
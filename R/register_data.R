#' Build unique file identifier
#' @param input Path to the file to be added to the database.
#' @param output Name of the output file, following the output of \code{\link{build_id}}.
#' @param overwrite Logical. Should the function overwrite existing files?
#' @importFrom raster extension
#' @importFrom lubridate ymd years
#' @importFrom tools md5sum
#' @importFrom filelock lock unlock
#' @return Writes file into standardized file structure.
#' @details {This function helps users add new data file to a database as 
#' created with `masDMT`. Note that the function will work with chmod to 
#' modify file permissions. By default, the function will apply a 
#' \emph{'read-only'} restriction (code 444), preventing potentially wrongful 
#' overwrites or deletions. When \emph{overwrite} is set to TRUE, the function 
#' will change the restrictions of a file to \emph{'read-write'}, allowing 
#' users its modification. If this function is run in windows system, the 
#' file will also \href{https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/icacls}{
#' receive a system-specific icacls}.}
#' @seealso \code{\link{build_id}} \code{\link{lockpick}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

register_data = function(input, output, overwrite=FALSE) {
  
  #---------------------------------------------------------------------------#
  # test input variables ----
  #---------------------------------------------------------------------------#
  
  if (!file.exists(input)) stop('"input" missing from file system')
  if (class(output)[1] != 'mas_id') stop('"output" should be a "mas_id" object')
  if (!logical(overwrite)) stop('"overwrite" is not a logical argument')
  
  #---------------------------------------------------------------------------#
  # create file structure ----
  #---------------------------------------------------------------------------#
  
  # check if dataset folder exists (if not, create one)
  output_dir = file.path(getOption('dmt.data'), output@dataset)
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  # create directory that will contain general information (e.g. bibtex)
  info = paste0(output_dir, .Platform$file.sep, 'info')
  if (!dir.exists(info)) dir.create(info)
  
  #---------------------------------------------------------------------------#
  # move file into database ----
  #---------------------------------------------------------------------------#
  
  # combine start/end dates into single string
  dates = paste0(unique(c(output@start, output@end)), collapse='-')
  
  # build name of output file
  output_name = paste0(output@dataset, '-', output@subdataset, '_', 
                       dates, '_', output@resolution, extension(input))
  
  # compile path to new file
  file_path = file.path(output_dir, output_name)
  file_lock = lock(file_path)
  
  # stop function if output file exists but overwriting is disabled
  fe = file.exists(file_path)
  if (fe & (overwrite=FALSE)) {stop('"input" already exists, but "overwrite" is set to FALSE')
  }
  
  # create copy input to new file path
  if (fe) Sys.chmod(file_path, '700')
  unlock(file_lock)
  file.copy(input, file_path)
  
  # check if input and output files have the same content
  a = md5sum(input)
  b = md5sum(file_path)
  if (a != b) warning('"input" and "output" failed md5sum check')
  
  # enable file lock
  lockpick(file_path, 'close')

}
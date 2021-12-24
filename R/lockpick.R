#' Change file permissions
#' @param path Path to the file for which to change permissions.
#' @param mode Character element. One of 'close' or 'open'.
#' @return File permission changes.
#' @details {This function will change the target file permissions to protect 
#' it from accidental deletion and changes. When \emph{mode} is set to "close", 
#' the function will apply a chmod code "444", making the file "read-only". 
#' If set to "open", the function will apply the chmod code "666", enabling 
#' changes to the file, including deletion. This will prevent other users from modifying 
#' the file, but will allow the user to If this function is ran in windows, 
#' the function will additionally use 
#' \href{https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/icacls}{
#' icacls to modify file permissions}. Since windows cannot interpret the chmod codes.}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

lockpick = function(path, mode) {
  
  #---------------------------------------------------------------------------#
  # test arguments
  #---------------------------------------------------------------------------#
  
  if (!file.exists(path)) stop('"path" does not exist in the file system')
  if (!is.character(mode)) stop('"mode" is not a character element')
  if (!mode %in% c('open','closed')) {
    stop('"mode" is not a valid keyword')}
  
  #---------------------------------------------------------------------------#
  #
  #---------------------------------------------------------------------------#
  
  # determine which operation system
  os = .Platform$OS.type
  
  # change file permission (all systems)
  if (mode == 'close') Sys.chmod(path, '444', use_umask=FALSE)
  if (mode == 'open') Sys.chmod(path, '666', use_umask=FALSE)
  
  # change file permission (windows)
  if (os == 'windows') {
    if (mode == 'close') {
      shell(paste0('icacls ', fileSnapshot(a)$path, '/deny Everyone:(D,M)'))}
    if (mode == 'open') {
      shell(paste0('icacls ', fileSnapshot(a)$path, '/grant Everyone:(D,M)'))}
  }
  
}
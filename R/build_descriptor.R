#' Creates a metadata record for the data catalog
#' @param path Directory where to write metadata record.
#' @param data_id Dataset unique identifier.
#' @param bibtex Path to bibtex with dataset references. Optional.
#' @param preview Path to preview image.
#' @return A Rmarkdown file that can be fed into the MAS data catalog. Optional.
#' @importFrom RefManageR ReadBib
#' @importFrom imager load.image save.image
#' @details {The function creates a directory in the specified \emph{path} 
#' named as the \emph{data_id}. Within it, you will find a yaml file where 
#' you can manually edit the metadata for the new record. If a \emph{bibtex} 
#' and/or a \emph{preview} image are provided, the function will copy them 
#' into the new directory after testing their validity. Once filled, the 
#' yaml file can be compiled into a metadata record using 
#' \code{\link[masDMT]{compile_descriptor}}.}
#'
#' @export
#'
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

build_descriptor <- function(path, data_id, bibtex=NULL, preview=NULL) {
  
  # test target directory
  if (!dir.exists(path)) stop('"path" is not a valid directory')
  
  # test data_id for special characters (flag anything that is not '_')
  sc = c('*','-','^','.','@','!','~','`','(',')','+','=','{','}','[',']','/',';','<','>')
  if (sum(strsplit(data_id, '')[[1]] %in% sc) > 0) {
    stop('found unauthorized special characters in "data_id", only "_" is allowed')
  }
  
  # create new directory where dataset files will be added
  output_path = file.path(path, data_id)
  dir.create(output_path)
  
  # test dataset identifier
  if (!is.character(data_id) | (length(data_id) > 1)) {
    stop('"data_id" should be a character element')}
  
  # test bibtex record (use bibtex package to try readin)
  if (!is.null(bibtex)) {
    if (!file.exists(bibtex)) stop('"bibtex" specified, but path is invalid')
    bib = tryCatch(ReadBib(bibtex), 
                   error=function(e) stop('"bibtex" not a valid bibtex'))
    file.copy(bibtex, file.path(output_path, paste0(data_id, '.bib')))
    rm(bib)
  }
  
  # test preview image (use imagr package to try reading)
  if (!is.null(preview)) {
    if (!file.exists(preview)) stop('"bibtex" specified, but path is invalid')
    img = tryCatch(load.image(preview), 
                   error=function(e) stop('"preview" not a valid image'))
    file.copy(preview, file.path(output_path, paste0(data_id, '.png')))
    rm(img)
  }
  
  # copy metadata yaml
  yml = file.path(system.file(package='masDMT', 'extdata'), 'dataset_example.yml')
  file.copy(yml, file.path(output_path, paste0(data_id, '.yml')))
  
}

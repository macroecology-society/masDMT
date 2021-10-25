#' Merge dataset references
#' @param data_id \emph{Character} vector enumerating datasets to query.
#' @param output Path to output file in bib format.
#' @return A bibtex file combining references for requested datasets.
#' @importFrom yaml read_yaml
#' @details {This function provides access to the bibliographic 
#' references of one or more datasets in the MAS database. The 
#' function provides one bibtex file combining all relevant 
#' bibligraphic references. Note that literature references 
#' are only available at the top level. For example, the 
#' 'CCI_landCover' dataset will be accepted in the function 
#' call, but 'CCI_landCover/landCover' will not return the 
#' desired output.}
#'
#' @export
#'
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

collect_bibtex = function(data_id, output) {
  
  #---------------------------------------------------------------------------#
  # test arguments
  #---------------------------------------------------------------------------#
  
  if (!dir.exists(dirname(output))) stop('output directory does not exist')
  
  files = paste0(getOption('dmt.data'), data_id, '/', data_id, '.bib')
  fe = file.exists(files)
  if (sum(!fe) > 0) {
    warning(paste0(data_id[fe], collapse=', '), ' do not have a bibtex')
  }
  
  if (sum(!fe) == length(fe)) stop('no bibtex found for the requested datasets')
  
  #---------------------------------------------------------------------------#
  # merge/write bibtex files
  #---------------------------------------------------------------------------#
  
  bibtex_data = lapply(files, readLines)
  write(unlist(bibtex_data), file=output)
  
}



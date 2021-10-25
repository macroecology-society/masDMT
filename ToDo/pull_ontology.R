#' Pull an ontology
#'
#' @param path [\code{character(1)}]\cr the path of the ontology
#' @details This funciton expects a csv document that contains a tabular
#'   ontology, i.e., a table that contains three kinds of information, the
#'   "from" part, the "relationship" and the "to" part. Both, from and to parts
#'   are a combination of the term an its class, which results in at least 5
#'   columns in total, but could also include other columns, such as additional
#'   IDs or quality metrics for each relationship.
#' @return a tibble that contains at least the columns \code{term_from},
#'   \code{class_from}, \code{relationship}, \code{term_to} and \code{class_to}
#' @importFrom checkmate assertCharacter assertNames
#' @importFrom readr read_csv
#' @export

pull_ontology <- function(path = NULL){

  assertFileExists(x = path)

  onto <- read_csv(file = path, col_types = cols())
  assertNames(x = names(onto), must.include = c("term_from", "class_from", "relationship", "term_to", "class_to"))

  return(onto)
}

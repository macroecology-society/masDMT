#' Print a summary of an ontology
#'
#' @param ontology [\code{data.frame(1)}]\cr the ontology to summarise
#' @details This function gives a summary of an ontology by listing all unique
#'   items in the column \code{term_from} and counting how many and which
#'   relationships to all \code{class_to} are defined for each term.
#' @importFrom checkmate assertDataFrame assertNames
#' @importFrom dplyr group_by count ungroup
#' @importFrom tidyr pivot_wider
#' @export

summarise_ontology <- function(ontology = NULL, names_from = "class_to"){

  assertDataFrame(x = ontology, min.cols = 5)
  assertNames(x = names(ontology), must.include = c("term_from", "class_from", "relationship", "term_to", "class_to"))
  assertCharacter(x = names_from, len = 1, any.missing = FALSE)
  assertNames(x = names_from, subset.of = c("relationship", "class_to"))

  toClasses <- unique(ontology$class_to)

  if(names_from == "class_to"){
    temp <- ontology %>%
      group_by(term_from, class_from) %>%
      count(class_to) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(term_from, class_from), names_from = class_to, values_from = n)
  } else {
    temp <- ontology %>%
      group_by(term_from, class_from) %>%
      count(relationship) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(term_from, class_from), names_from = relationship, values_from = n)
  }


  return(temp)
}

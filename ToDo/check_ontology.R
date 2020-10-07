#' Check consistency of an ontology
#'
#' @param concepts
#' @param relations
#'


check_ontology <- function(concepts, relations){

  # things to check:
  # 1. does each class (from/to) have the correct luckinetID (from concepts)
  # 2. do all terms that are defined as part of a class (in relations) occur in that class (in concepts)
  # 3. do classes have additional terms that do not occur in the relations list
  # 4. are all terms at the same "position" in the same relation, i.e., if the commodity 'cattle and buffaloes' is parent of 'something' and synonym to 'Cattle and Buffaloes' is the later also parent of 'something'?
  # 5. that entries are unique

}

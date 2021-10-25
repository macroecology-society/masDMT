#' Assign typical raster attributes as RAT
#'
#' @param x [\code{character(1)}]\cr path to an object to asses.
#' @importFrom checkmate assertClass
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @export

View_sf <- function(x, geom){

  assertClass(x = x, classes = "sf")

  out <- x %>%
    as_tibble() %>%
    select(-geom)

  View(out)

}

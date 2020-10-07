#' Load the areal data from an arealDB database
#'
#' @param path [\code{character(1)}]\cr the path of the database.
#' @param nation [\code{character(1)}]\cr the nation for which to get the data.
#' @param pID [\code{character(1)}]\cr name of the primary ID.
#' @param geoID [\code{itegerish(1)}]\cr subset the data by geoID.
#' @param tabID [\code{itegerish(1)}]\cr subset the data by tabID.
#' @param type [\code{character(1)}]\cr the target variable to set as demand
#'   type.
#' @param commodities [\code{character(.)}]\cr the target commodities; all other
#'   commodities are grouped into the group "other".
#' @param keep_ids [\code{logical(1)}]\cr
#' @importFrom checkmate assertCharacter assertChoice assertIntegerish
#'   assertFileExists assertSubset assertLogical assertNames
#' @importFrom readr read_csv cols
#' @importFrom dplyr filter group_by left_join ungroup select summarise mutate
#'   bind_rows
#' @export

getArealData <- function(path = NULL, nation = NULL, pID = NULL, geoID = NULL,
                         tabID = NULL, type = NULL, commodities = NULL,
                         keep_ids = TRUE){

  assertCharacter(x = path, any.missing = FALSE, len = 1)
  assertSubset(x = nation, choices = countries$unit)
  assertCharacter(x = pID, len = 1, any.missing = FALSE)
  assertCharacter(x = type, any.missing = FALSE)
  assertCharacter(x = commodities, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = geoID, any.missing = FALSE, null.ok = TRUE, len = 1)
  assertIntegerish(x = tabID, any.missing = FALSE, null.ok = TRUE, len = 1)
  assertLogical(x = keep_ids, any.missing = FALSE, len = 1)

  comm <- read_csv(file = paste0(path, "/id_commodities.csv"), col_types = cols())
  assertNames(x = names(comm), must.include = pID)
  comm <- comm %>%
    filter(!duplicated(pID))

  out <- NULL
  for(i in seq_along(nation)){

    inputPath <- paste0(path, "/adb_tables/stage3/", nation[i], ".csv")
    assertFileExists(x = inputPath, access = "rw")

    input <- read_csv(file = inputPath, col_types = cols(id = "i", tabID = "i", geoID = "i", ahID = "c", year = "i", .default = "d"))
    if(!is.null(geoID)){
      geoIDs <- geoID
      input <- input %>%
        filter(geoID %in% geoIDs)
    }
    if(!is.null(tabID)){
      tabIDs <- tabID
      input <- input %>%
        filter(tabID %in% tabIDs)
    }

    temp <- input %>%
      filter(!is.na(pID) & !is.na(year)) %>%
      group_by(tabID, geoID, ahID, year) %>%
      left_join(comm, by = pID) %>%
      dplyr::select(tabID, geoID, year, commodity = target, ahID, all_of(pID), any_of(type)) %>%
      ungroup()

    if(!is.null(commodities)){
      assertSubset(x = commodities, choices = comm$target)
      temp1 <- temp %>%
        filter(commodity %in% commodities)

      temp2 <- temp %>%
        filter(!commodity %in% commodities) %>%
        group_by(ahID, year) %>%
        summarise_at(type, sum, na.rm = TRUE) %>%
        mutate(commodity = "other") %>%
        ungroup()
      tempOut <- bind_rows(temp1, temp2) %>%
        arrange(year)

    } else {
      tempOut <- temp %>%
        arrange(year)
    }

    if(!keep_ids){
      tempOut <- tempOut %>%
        select(-geoID, -tabID, -pID)
    }
  }

  out <- bind_rows(out, tempOut)

  return(out)
}

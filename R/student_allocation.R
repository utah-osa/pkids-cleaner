################################################################################
#' Pull Student Allocation Matrix
#'
#' @description
#' For use in Student Allocation
#'
#' @details
#' Load in specific entity's student allocation matrix
#'
#' @param lea The LEA code; input in quotes
#'
#' @export
pullMatrix <- function(lea) {
  lea_ids %<>% filter(district_code == "lea")
  source(paste0("C:/Users/sdutton/Desktop/SamR/testAllocSource_",lea,".R"))
  View(runMatrix)
 }


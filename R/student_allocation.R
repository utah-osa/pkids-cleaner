################################################################################
#'Pull Student Allocation Matrix
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
  source(paste0("H:/Economists/Ed/KIDS/All LEAs/Student Allocation Matrices/Allocation_Matrix_",lea,".R"))
 }

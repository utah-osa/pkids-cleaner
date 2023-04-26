################################################################################
#' Network Student Allocation
#'
#' @description
#' 'network_allocation' allows us to run all of the student allocation scripts in a loop for charter networks
#'
#' @details
#' Rather than running a dozen or so scripts manually, this code will process them in a loop and spit out a file with any errors.
#'
#' @export

#### STUDENT ALLOCATION ####

# Be sure to download the LEA Tracker and save it in the correct folder as 'tracker_names' so process the correct files
# If an error pops up saying 'master_fileCHARTER' does not exist, it means there are no entities ready to process

# Networks
network_allocation <- function(){
  source("H:/Economists/Ed/KIDS/All Charter Schools/All/Master Loop Charter Network Student Allocation.R")
}

################################################################################
#' Database District Check
#'
#' @description
#' 'single_allocation' allows us to run all of the student allocation scripts in a loop for single charter schools
#'
#' @details
#' Rather than running a dozen or so scripts manually, this code will process them in a loop and spit out a file with any errors.
#'
#' @export

#### STUDENT ALLOCATION ####
# Non-networks

single_allocation <- function() {
  source("H:/Economists/Ed/KIDS/All Charter Schools/All/Master Loop Charter Student Allocation.R")
}


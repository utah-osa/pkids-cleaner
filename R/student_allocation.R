################################################################################
#'
#' LEA ID
#'@format ## `lea_ids`
#'A data frame with 41 rows and 14 columns
#'\describe{
#'  \item{entity_name}{"Alpine School District"}
#'  \item{district_code}{"ALPS"}
#'  ...
#'  }
#'
#'
#' @export
#' # Pull Student Allocation Matrix File
.pullMatrix <- function(lea) {
  data(lea_ids)
  lea_ids %<>% filter(district_code == "lea")
  source(paste0("C:/Users/sdutton/Desktop/SamR/testAllocSource_",lea,".R"))
  View(runMatrix)
 }
#'
#' @examples
#' pullMatrix("BESD")


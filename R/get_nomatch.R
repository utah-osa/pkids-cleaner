########################################
#'First: Create table from nomatch input df that is each check that needs to be done
#'
#'@export
get_nomatch <- function(missing){
  missing %<>%
    select(TEACHER_ID, first_name, last_name, middle_name, maiden_name)

  return(missing)
}

#' Get Names
#'
#'@export
get_names <- function(names){
  return(names)
}

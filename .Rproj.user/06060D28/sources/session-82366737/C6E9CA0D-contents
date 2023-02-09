################################################################################
#' Check Variables
#'
#' @description
#' 'check_vars' returns a table displaying a summary of the old and new datasets
#'
#' @details
#' This function is used in the USBE Quality Check stage of the update process to assess the new data against data that we have received in the past. Visually inspect the returned table for notable differences.
#'
#' @param old_file The file from the previous year
#' @param new_file The file from the current year
#' @export
check_vars <- function(old_file, new_file){

  names(old_file) <- toupper(names(old_file))
  names(new_file) <- toupper(names(new_file))

  check_vars_old <- as_tibble(cbind(names(old_file),
                                    sapply(old_file, function(x) sum(is.na(x))),
                                    sapply(old_file, function(x) ifelse(is.numeric(x),mean(x), NA)),
                                    sapply(old_file, function(x) ifelse(is.numeric(x),max(x), NA)),
                                    sapply(old_file, function(x) ifelse(is.numeric(x),min(x), NA)),
                                    sapply(old_file, function(x) class(x)),
                                    sapply(old_file, function(x) list(unique(x)))))
  names(check_vars_old) <- c("Variables", "Old_NAs","Old_Mean", "Old_Max", "Old_Min","Old_Class", "Old_Values")

  check_vars_new <- as_tibble(cbind(names(new_file),
                                    sapply(new_file, function(x) sum(is.na(x))),
                                    sapply(new_file, function(x) ifelse(is.numeric(x),mean(x), NA)),
                                    sapply(new_file, function(x) ifelse(is.numeric(x),max(x), NA)),
                                    sapply(new_file, function(x) ifelse(is.numeric(x),min(x), NA)),
                                    sapply(new_file, function(x) class(x)),
                                    sapply(new_file, function(x) list(unique(x)))))
  names(check_vars_new) <- c("Variables", "New_NAs", "New_Mean", "New_Max", "New_Min", "New_Class", "New_Values")

  return(full_join(check_vars_old, check_vars_new) %>% select(Variables, Old_NAs, New_NAs, Old_Mean, New_Mean, Old_Max, New_Max, Old_Min, New_Min, Old_Class, New_Class, Old_Values, New_Values))
}

################################################################################

#' Which Variables
#'
#' @description
#' Compares inclusion of variables from past and current years
#'
#' @details
#' This function is used in the USBE Quality Check stage of the update process to assess the new data against data that we have received in the past. Note variables included in the previous year that are not present in the current year, and vice versa.
#'
#' @param old_file The file from the previous year
#' @param new_file The file from the current year
#' @export
which_vars <- function(old_file, new_file){

  new_vars <- unique(names(new_file)[!names(new_file) %in% names(old_file)])
  old_vars <- unique(names(old_file)[!names(old_file) %in% names(new_file)])

  new_vars <- if_else(length(new_vars) > 0, toString(new_vars), "None")
  old_vars <- if_else(length(old_vars) > 0, toString(old_vars), "None")

  statement <- (paste0("Variables in new file, but not in the old file: ", new_vars, "\n",
                       "Variables in old file, but not in the new file: ", old_vars))

  writeLines(statement)

}

################################################################################

#' Check Number of Rows
#'
#' @description
#' Compares number of rows from past and current years
#'
#' @details
#' This function is used in the USBE Quality Check stage of the update process to assess the new data against data that we have received in the past. Inspect the number of rows by school year and note any substantial deviations from the old years to the new year.
#'
#' @param old_file The file from the previous year
#' @param new_file The file from the current year
#'
#' @export
num_rows <- function(old_file, new_file){

  names(old_file) <- toupper(names(old_file))
  names(new_file) <- toupper(names(new_file))

  return(old_file %>% group_by(SCHOOL_YEAR) %>% summarise(rows = n()) %>%
           add_row(SCHOOL_YEAR = current_year, rows = nrow(new_file)))

}

################################################################################

#' Describe Numeric Variables
#'
#'
#' check summary statistics of numeric variables
#' @export
sum_numvars <- function(old_file, new_file, funct = mean){

  old_vars <- old_file %>% group_by(SCHOOL_YEAR) %>% summarise_if(is.numeric, funct, na.rm = TRUE) %>% arrange(SCHOOL_YEAR)
  new_vars <- new_file %>% group_by(SCHOOL_YEAR) %>% summarise_if(is.numeric, funct, na.rm = TRUE)

  return(old_vars %>% full_join(new_vars) %>% select(names(old_vars)))

}

################################################################################
#' Database Check 1 Variables
#'
#' @description
#' 'check1' allows us to run the first database check for charters and districts
#'
#' @details
#' This function is used in the charter and district database checks to confirm that the expense totals for each entity are correct.
#'
#' @param expense The file from the previous year
#' @export

#### Database Check 1 Function ####
check1 <- function(expense){
  fund <- expense %>% group_by(u_fund) %>% summarise(sum = sum(total))
  program <- expense %>% group_by(u_program) %>% summarise(sum = sum(total))
  func <-expense %>% group_by(u_function) %>% summarise(sum = sum(total))
  object <- expense %>% group_by(u_object) %>% summarise(sum = sum(total))

  sum1 <- sum(expense$total)*4

  sum2 <- sum(fund$sum) + sum(program$sum) + sum(func$sum) + sum(object$sum)

  (round(sum1) - round(sum2))
}

################################################################################
#' Database Check 2 Variables
#'
#' @description
#' 'check2' allows us to run the second database check for charters and districts
#'
#' @details
#' This function is used in the charter and district database checks to confirm that the treemap and school totals are correct. For our purposes, we want to make sure the expense totals match up when we look at them in different ways.
#'
#' @param expense The file from the previous year
#' @export

check2 <- function(expense){
  fund <- expense %>% group_by(u_fund) %>% summarise(sum = sum(total))
  program <- expense %>% group_by(u_program) %>% summarise(sum = sum(total))
  func <-expense %>% group_by(u_function) %>% summarise(sum = sum(total))
  object <- expense %>% group_by(u_object) %>% summarise(sum = sum(total))

  sum2 <- sum(fund$sum) + sum(program$sum) + sum(func$sum) + sum(object$sum)
  sum3 <- expense %>% group_by(u_location) %>% summarise(sum = sum(total))
  round(sum(sum3$sum)*4 - sum2)

}

################################################################################
#' Database Check 3a Variables
#'
#' @description
#' 'check3a' allows us to run the third database check for charters and districts
#'
#' @details
#' This function is used in the charter and district database checks to confirm that total expense is the same across expense and allocation files.
#' @param expense The expense file
#' @param allocation The allocation file
#' @export

check3a <- function(expense, allocation){
  round(sum(allocation[,1:193], na.rm = T)) - round(sum(expense$total, na.rm = T))
}

################################################################################
#' Database Check 3a Variables
#'
#' @description
#' 'check3b' allows us to run the first database check for charters and districts
#'
#' @details
#' This function is used in the charter and district database checks to confirm that teacher pay is the same across expense and allocation files.
#'
#' @param expense The expense file
#' @param allocation The allocation file
#' @export

check3b <- function(expense, allocation){
  round(sum(allocation$Teacher_Pay, na.rm = T)) - round(sum(expense$total[expense$Expense_Category=="Teacher Pay"], na.rm = T))
}

################################################################################
#' Database Charter Check Variables
#'
#' @description3
#' 'charter_final_database_checks' allows us to run all of the database checks for all charter schools.
#'
#' @details
#' In an effort to expedite database checks, we run this script to systematically run the databse checks for each charter school and spit out the results This will mirror the results we find in the online database checks, but saves us the trouble of having to reupload the files to the cloud.
#'
#' @export

## Complete charter check file
charter_final_database_checks <- function(){
  charter_ids <- read_csv("H:/Economists/Ed/KIDS/All Charter Schools/All/Charter IDs Master.csv")
  closed <- c("ALIA","AMIN","ARST","CAPC","KAIR","PION")
  charter_ids %<>% filter(!lea_acronym %in% c(closed))
  charterChecks <- NULL
  check_file <- NULL

  for(i in 1:nrow(charter_ids)){
    i_id <- charter_ids[i,] %>% mutate(lea_acronym = tolower(lea_acronym))

    try(allocation <- fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/allocation/z_",tolower(i_id$lea_acronym), "_class.txt"),
                            select = c(183:374,169)))

    try(expense <- fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/expense/z_",tolower(i_id$lea_acronym), "_expense.txt"),
                         select = c("lea_code", "fiscal_year", "total", "u_location","u_fund","u_function",
                                    "Expense_Category", "u_program", "u_object", "subcode")))

    try(charter_check <- data.frame("check1" = check1(expense),
                                    "check2" = check2(expense),
                                    "check3a" = check3a(expense, allocation),
                                    "check3b" = check3b(expense, allocation),
                                    "district" = i_id$lea_name))

    try(charterChecks %<>% rbind(charter_check))
    check_file <<- charterChecks
    print(paste0("You are ", scales::label_percent()(i/nrow(charter_ids))," complete"))
    print(paste0(charter_ids$lea_name[i]," is done. Number remaining: ",nrow(charter_ids) - i))

  }

}

################################################################################
#' Database District Check Variables
#'
#' @description
#' 'district_final_database_checks' allows us to run all of the database checks for all districts.
#'
#' @details
#' In an effort to expedite database checks, we run this script to systematically run the databse checks for each district and spit out the results This will mirror the results we find in the online database checks, but saves us the trouble of having to reupload the files to the cloud.
#'
#' @export

## Complete district check files
district_final_database_checks <- function(){
  lea_ids <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv", col_types = cols())
  lea_ids <- lea_ids[c(2,4,6),]

  dfchecks <- NULL
  district_check_file <- NULL
  for(i in 1:nrow(lea_ids)){
    i_id <- lea_ids[i,] %>% mutate(district_code = tolower(district_code))
    allocation <- fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/allocation/",i_id$district_code, "_class.txt"),
                        select = c(183:374,169))
    expense <- fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/expense/",i_id$district_code, "_expense.txt"),
                     select = c("lea_code", "fiscal_year", "total", "u_location","u_fund","u_function",
                                "Expense_Category", "u_program", "u_object", "subcode"))

    lea_check <- data.frame("check1" = check1(expense),
                            "check2" = check2(expense),
                            "check3a" = check3a(expense, allocation),
                            "check3b" = check3b(expense, allocation),
                            "district" = i_id$district_code)
    lea_check %<>% as_tibble(lea_check)

    dfchecks %<>% rbind(lea_check)
    district_check_file <<- dfchecks
    print(paste0("You are ", scales::label_percent()(i/nrow(lea_ids))," complete"))
    print(paste0(lea_ids$entity_name[i]," is done. Number remaining: ",nrow(lea_ids) - i))

  }
}

usethis::use_data_raw()

################################################################################
#' District Class File Paths
#'
#' @description
#' 'District Class File Paths' allows us to make sure files are saved in the correct location in the network drive.
#'
#' @details
#' This function pulls the file path for each district and checks it against a threshold to make sure we're saving files in the correction location. If something's wrong, you may need to rerun the code or restore previous versions of old files.
#'
#' @param version This tells us which folder in the network drive we should be pulling from.
#' @export

#### Pull file paths for DISTRICT ALLOCATION ####
district_allocation <- function(version){
  path <- paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_", version,"/allocation")
  file_names <- as_tibble(list.files(path))
  file_charters <- file_names %>% filter(str_detect(value,"^z"))
  file_districts <- file_names %>% anti_join(file_charters)

  file_districts$value <- toupper(gsub("_class.txt","",file_districts$value))

  master_fileDISTRICT <- NULL
  for (i in 1:nrow(file_districts)){
    district_i <- lea_ids %>% filter(str_detect(file_districts$value[i], lea_ids$district_code))
    master_fileDISTRICT %<>% rbind(district_i)
    rm(district_i,i)
  }

  # Which are missing?
  print(lea_ids %>% anti_join(master_fileDISTRICT) %>% select(entity_name))
  rm(file_names, file_charters, file_districts, master_fileDISTRICT)

}

################################################################################
#' District Expense File Paths
#'
#' @description
#' 'District Expense File Paths' allows us to make sure files are saved in the correct location in the network drive.
#'
#' @details
#' This function pulls the file path for each district and checks it against a threshold to make sure we're saving files in the correction location. If something's wrong, you may need to rerun the code or restore previous versions of old files.
#'
#' @param version This tells us which folder in the network drive we should be pulling from.
#' @export
#'

#### Pull file paths for DISTRICT EXPENSE ####-------------------------------------------
district_expense <- function(version){
  path <- paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_", version,"/expense")
  file_names <- as_tibble(list.files(path))
  file_charters <- file_names %>% filter(str_detect(value,"^z"))
  file_districts <- file_names %>% anti_join(file_charters)

  file_districts$value <- toupper(gsub("_expense.txt","",file_districts$value))

  master_fileDISTRICT <- NULL
  for (i in 1:nrow(file_districts)){
    district_i <- lea_ids %>% filter(str_detect(file_districts$value[i], lea_ids$district_code))
    master_fileDISTRICT %<>% rbind(district_i)
    rm(district_i,i)
  }

  # Which are missing?
  print(lea_ids %>% anti_join(master_fileDISTRICT) %>% select(entity_name))
  rm(file_names, file_charters, file_districts, master_fileDISTRICT)
}

################################################################################
#' Charter Class File Paths
#'
#' @description
#' 'Charter Class File Paths' allows us to make sure files are saved in the correct location in the network drive.
#'
#' @details
#' This function pulls the file path for each Charter and checks it against a threshold to make sure we're saving files in the correction location. If something's wrong, you may need to rerun the code or restore previous versions of old files.
#'
#' @param version This tells us which folder in the network drive we should be pulling from.
#' @export
#'
#### Pull file paths for CHARTER ALLOCATION ####-----------------------------------------
charter_allocation <- function(version){
  path <- paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_", version,"/allocation")
  file_names <- as_tibble(list.files(path))
  file_charters <- file_names %>% filter(str_detect(value,"^z"))

  file_charters$value <- toupper(gsub("_class.txt|z_","",file_charters$value))

  master_fileCHARTER <- NULL
  for (i in 1:nrow(file_charters)){
    charter_i <- charters %>% filter(str_detect(lea_acronym, file_charters$value[i]))
    master_fileCHARTER %<>% rbind(charter_i)
    rm(charter_i)
  }

  # Which are missing?
  print(charters %>% anti_join(master_fileCHARTER) %>% select(lea_name))
  rm(file_names, file_charters, master_fileCHARTER)
}

################################################################################
#' Charter Expense File Paths
#'
#' @description
#' 'Charter Expense File Paths' allows us to make sure files are saved in the correct location in the network drive.
#'
#' @details
#' This function pulls the file path for each Charter and checks it against a threshold to make sure we're saving files in the correction location. If something's wrong, you may need to rerun the code or restore previous versions of old files.
#'
#' @param version This tells us which folder in the network drive we should be pulling from.
#' @export
#'

#### Pull file paths for CHARTER EXPENSE ####-------------------------------------------
charter_expense <- function(version){
  path <- paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_", version,"/expense")
  file_names <- as_tibble(list.files(path))
  file_charters <- file_names %>% filter(str_detect(value,"^z"))
  file_districts <- file_names %>% anti_join(file_charters)

  file_charters$value <- toupper(gsub("_expense.txt|z_","",file_charters$value))

  master_fileCHARTER <- NULL
  for (i in 1:nrow(file_charters)){
    charter_i <- charters %>% filter(str_detect(lea_acronym, file_charters$value[i]))
    master_fileCHARTER %<>% rbind(charter_i)
    rm(charter_i)
  }

  # Which are missing?
  print(charters %>% anti_join(master_fileCHARTER) %>% select(lea_name))
  rm(file_names, file_charters, file_districts, master_fileCHARTER)
}

################################################################################
#' Confirm past files haven't been messed with
#'
#' @description
#' 'confirm_district_expense_time_function' helps us see if files from past yearly updates have been accidentally altered in this round of yearly updates.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we aren't altering past updates and forgetting to update the correct files. This function focuses on district expense.
#'
#' @export

#### Check Past Files ####
confirm_district_expense_time_function <- function(){

  confirm_district_expense_time <- NULL
  for(i in 1:nrow(lea_ids)){
    x <- as.Date(file.info(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_v3/expense/",lea_ids$district_code[i],"_expense.txt"))$mtime)
    x <- as.POSIXct(x)
    data <- data.frame(name = lea_ids$entity_name[i],
                       date = x)
    data %<>% mutate(correct = ifelse(data$date < threshold,"Correct","FALSE"))
    confirm_district_expense_time %<>% rbind(data)
  }
  print(confirm_district_expense_time %>% filter(correct == "FALSE") %>% select(name))
}

################################################################################
#' Confirm past files haven't been messed with
#'
#' @description
#' 'confirm_charter_expense_time_function' helps us see if files from past yearly updates have been accidentally altered in this round of yearly updates.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we aren't altering past updates and forgetting to update the correct files. This function focuses on charter expense.
#'
#' @export

#### Check Past Files ####
confirm_charter_expense_time_function <- function(){
  # Check charter expense
  confirm_charter_expense_time <- NULL
  for(i in 13:nrow(charters)){
    x <- as.Date(file.info(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_v3/expense/z_",charters$lea_acronym[i],"_expense.txt"))$mtime)
    x <- as.POSIXct(x)
    data <- data.frame(name = charters$lea_name[i],
                       date = x)
    data %<>% mutate(correct = ifelse(data$date < threshold,"Correct","FALSE"))
    confirm_charter_expense_time %<>%rbind(data)
  }
  print(confirm_charter_expense_time %>% filter(correct == "FALSE") %>% select(name))
}

################################################################################
#' Confirm past files haven't been messed with
#'
#' @description
#' 'district_allocation_time_function' helps us see if files from past yearly updates have been accidentally altered in this round of yearly updates.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we aren't altering past updates and forgetting to update the correct files. This function focuses on district allocation.
#'
#' @export

#### Check Past Files ####

district_allocation_time_function <- function(){
  district_allocation_time <- NULL
  for(i in 1:nrow(lea_ids)){
    x <- as.Date(file.info(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_v3/allocation/",lea_ids$district_code[i],"_class.txt"))$mtime)
    x <- as.POSIXct(x)
    data <- data.frame(name = lea_ids$entity_name[i],
                       date = x)
    data %<>% mutate(correct = ifelse(data$date < threshold,"Correct","FALSE"))
    district_allocation_time %<>% rbind(data)
  }
  print(district_allocation_time %>% filter(correct == "FALSE") %>% select(name))
}

################################################################################
#' Confirm past files haven't been messed with
#'
#' @description
#' 'charter_allocation_time_function' helps us see if files from past yearly updates have been accidentally altered in this round of yearly updates.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we aren't altering past updates and forgetting to update the correct files. This function focuses on charter allocation.
#'
#' @export

#### Check Past Files ####
charter_allocation_time_function <- function(){
  charter_allocation_time <- NULL
  # Check charter allocation
  for(i in 1:nrow(charters)){
    x <- as.Date(file.info(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_v3/allocation/z_",charters$lea_acronym[i],"_class.txt"))$mtime)
    x <- as.POSIXct(x)
    data <- data.frame(name = charters$lea_name[i],
                       date = x)
    data %<>% mutate(correct = ifelse(data$date < threshold,"Correct","FALSE"))
    charter_allocation_time %<>% rbind(data)
  }
  print(charter_allocation_time %>% filter(correct == "FALSE") %>% select(name))

}

################################################################################
#' Confirm column numbers
#'
#' @description
#' 'charter_expense_columns' helps us see if files have the correct number of columns.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we have the correct number of columns so database uploads work properly.
#' @param version This tells us which folder to pull from
#' @export
#'

charter_expense_columns <- function(version){
  charters <- read_csv("H:/Economists/Ed/KIDS/All Charter Schools/All/Charter IDs Master.csv") %>% mutate(lea_acronym = tolower(lea_acronym))
  closed <- c("ALIA","AMIN","ARST","CAPC","KAIR","PION")
  charters <- read_csv("H:/Economists/Ed/KIDS/All Charter Schools/All/Charter IDs Master.csv") %>%
    filter(!lea_acronym %in% c(closed) )
  master_data <- NULL
  for(i in  1:nrow(charters)){
    try(x <- ncol(fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/expense/z_",charters$lea_acronym[i],"_expense.txt"), nrow = 0)))
    if(exists("x")){
      data_tibble <- as.data.frame(name = charters$lea_name[i],
                                number = x)
    }
    try(master_data %<>% rbind(data_tibble))
    rm(i)
  }
  print(master_data %>% filter(number != 40))
}

################################################################################
#' Confirm column numbers
#'
#' @description
#' 'charter_allocation_columns' helps us see if files have the correct number of columns.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we have the correct number of columns so database uploads work properly.
#' @param version This tells us which folder to pull from
#'
#' @export
#'
charter_allocation_columns <- function(version){
  charters <- read_csv("H:/Economists/Ed/KIDS/All Charter Schools/All/Charter IDs Master.csv") %>% mutate(lea_acronym = tolower(lea_acronym))
  closed <- c("ALIA","AMIN","ARST","CAPC","KAIR","PION")
  charters <- read_csv("H:/Economists/Ed/KIDS/All Charter Schools/All/Charter IDs Master.csv") %>%
    filter(!lea_acronym %in% c(closed) )

  master_data_class <- NULL
  for(i in  1:nrow(charters)){
    try(x <- ncol(fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/allocation/z_",charters$lea_acronym[i],"_class.txt"), nrow = 0)))
    if(exists("x")){
      data_tibble <- as.data.frame(name = charters$lea_name[i],
                                number = x)
    }
    try(master_data_class %<>% rbind(data_tibble))

  }
  print(master_data_class %>% filter(number != 374))}

################################################################################
#' Confirm column numbers
#'
#' @description
#' 'district_expense_columns' helps us see if files have the correct number of columns.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we have the correct number of columns so database uploads work properly.
#' @param version This tells us which folder to pull from
#' @export


# District Expense
district_expense_columns <- function(version){
  districts <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv") %>%
    mutate(district_code = tolower(district_code))

  master_data <- NULL
  for(i in  1:nrow(districts)){
    try(x <- ncol(fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/expense/",districts$district_code[i],"_expense.txt"), nrow = 0)))
    if(exists("x")){
      data_tibble <- as.data.frame(name = districts$entity_name[i],
                                number = x)
    }
    try(master_data %<>% rbind(data_tibble))
    rm(i)
  }
  print(master_data %>% filter(number != 40))
}

################################################################################
#' Confirm column numbers
#'
#' @description
#' 'district_allocation_columns' helps us see if files have the correct number of columns.
#'
#' @details
#' Because we are constantly changing and updating files, we want to make sure we have the correct number of columns so database uploads work properly.
#' @param version This tells us which folder to pull from
#' @export

# District Allocation
district_allocation_columns <- function(version){
  districts <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv") %>%
    mutate(district_code = tolower(district_code))
  master_data_class <- NULL
  for(i in  1:nrow(districts)){
    try(x <- ncol(fread(paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_",version,"/allocation/",districts$district_code[i],"_class.txt"), nrow = 0)))
    if(exists("x")){
      data_tibble <- as.data.frame(name = districts$entity_name[i],
                                number = x)
    }
    try(master_data_class %<>% rbind(data_tibble))
    rm(i)
  }
  print(master_data_class %>% filter(number != 374))}

################################################################################
#' Database Check 1
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
#' Database Check 2
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
#' Database Check 3a
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
#' Database Check 3b
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
#' Database Charter Check
#'
#' @description3
#' 'charter_final_database_checks' allows us to run all of the database checks for all charter schools.
#'
#' @details
#' In an effort to expedite database checks, we run this script to systematically run the databse checks for each charter school and spit out the results This will mirror the results we find in the online database checks, but saves us the trouble of having to reupload the files to the cloud.
#'
#' @param version This tells us which folder we need to pull the data from
#'
#' @export

## Complete charter check file
charter_final_database_checks <- function(version){
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
#' Database District Check
#'
#' @description
#' 'district_final_database_checks' allows us to run all of the database checks for all districts.
#'
#' @details
#' In an effort to expedite database checks, we run this script to systematically run the databse checks for each district and spit out the results This will mirror the results we find in the online database checks, but saves us the trouble of having to reupload the files to the cloud.
#'
#' @param version This tells us which folder we need to pull the data from
#'
#' @export

## Complete district check files
district_final_database_checks <- function(version){
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

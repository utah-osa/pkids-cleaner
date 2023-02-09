##########################################################
#' Correct Account Codes
#'
#' @export
# correct_account_codes <- function(expense, lea){
#   if(lea == "Alpine"){
#     # edit one faulty account code
#     expense %<>% mutate(account_number = ifelse(account_number == "10-099-0005-6000-5210-0000", "10-099-0005-6000-521-0000", account_number))
#
#   } else if(lea == "Beaver"){
#     # edit faulty account codes
#     expense$account_number <- gsub("[^0-9.-]", "", expense$account_number) # remove text (descriptions attached for some)
#     expense$account_number <- str_replace(expense$account_number, "1-0000-", "0001-") # impute messed up program code from transparency program
#     expense$account_number <- str_remove_all(expense$account_number, "\\.") # remove period at end of code
#     expense %<>% mutate(account_number = str_sub(account_number, end = 20)) # remove extra codes
#
#     # add dashes to account codes
#     expense[which(str_count(expense$account_number, "-") == 0),] %<>%
#       mutate(account_number = paste0(str_sub(account_number, 1, 2), "-", str_sub(account_number, 3, -1))) %>%
#       mutate(account_number = paste0(str_sub(account_number, 1, 6), "-", str_sub(account_number, 7, -1))) %>%
#       mutate(account_number = paste0(str_sub(account_number, 1, 11), "-", str_sub(account_number, 12, -1))) %>%
#       mutate(account_number = paste0(str_sub(account_number, 1, 16), "-", str_sub(account_number, 17, -1)))
#
#     # manually edit last faulty ones
#     expense[which(expense$account_number == "6070836371000517-184"),] %<>% mutate(account_number = "60-708-3637-1000-517")
#     expense[which(expense$account_number == "607083641-1000-340"),] %<>% mutate(account_number = "60-708-3641-1000-340")
#     expense[which(expense$account_number == "10-704-95802290-610-0000"),] %<>% mutate(account_number = "10-704-9580-2290-610-0000")
#     expense[which(expense$account_number == "10-704-5420-1000610-0000"),] %<>% mutate(account_number = "10-704-5420-1000-610-0000")
#     # add revenue code
#     expense %<>% mutate(account_number = paste0(account_number, "-0000"))
#
#   } else if(lea =="Cache"){
#
#     expense %<>% mutate(account_number = ifelse(fiscal_year == 2022 & str_length(account_number) != 25,
#                                                 paste0(account_number,"-0000"), account_number))
#
#   } else if(lea == "Davis"){
#     # impute 0s for missing program codes
#     expense %<>% mutate(account_number = ifelse(is.na(program1), paste0(str_sub(account_number, 1, 7), "0000", str_sub(account_number, 8, -1)), account_number))
#
#   } else if(lea == "Duchesne"){
#     # impute 0s for missing program codes
#     expense %<>%
#       mutate(account_number = ifelse(str_detect(account_number, "^21--"),
#                                      ifelse(is.na(org3),
#                                             paste0(str_sub(account_number, 1, 3), str_sub(org2, 1, 3), str_sub(account_number, 4, -1)),
#                                             paste0(str_sub(account_number, 1, 3), str_sub(org3, 1, 3), str_sub(account_number, 4, -1))),
#                                      account_number),
#              account_number = ifelse(fiscal_year == 2022 & str_length(account_number) != 25,
#                                      paste0(str_sub(fund2, 1, 2), "-",
#                                             str_sub(org2, 1, 3), "-",
#                                             str_sub(function1, 1,4), "-",
#                                             str_sub(program2,1,3), "-",
#                                             str_sub(cat2,1,4),'-0000'), account_number))
#
#   } else if(lea == "Granite"){
#     # fix 5 account codes with formatting issues
#     expense %<>% mutate(account_number = ifelse(fiscal_year == 2017 & str_length(account_number) != 25,
#                                                 paste0("10-", str_sub(org2, 1, 3), str_sub(account_number, 9, 20), str_sub(account_number, 22, -1)),
#                                                 account_number))
#   } else if(lea == "Nebo"){
#     # fix missing accounts codes in specific batch #
#     expense %<>% mutate(account_number = ifelse(
#       batch_id == "B-0019467" & str_length(account_number) != 25,
#       paste0(str_sub(fund2, 1, 2), "-",
#              str_sub(org2, 1, 3), "-",
#              str_sub(function1, 1,4), "-",
#              str_sub(program3,1,3), "-",
#              str_sub(cat2,1,4),'-0000'), account_number))
#   } else if(lea == "North Summit"){
#     # missing additional 0s in 2022
#     expense %<>% mutate(account_number = ifelse(fiscal_year == 2022 & str_length(account_number) != 25,
#                                                 paste0(account_number,"-0000"), account_number))
#   } else if(lea == "Park"){
#     # fix account codes with missing function
#     expense %<>% mutate(account_number = ifelse(fiscal_year == 2014 & str_length(account_number) != 25,
#                                                 paste0(str_sub(account_number, 1, 12), "0000", str_sub(account_number, 13, -1)),
#                                                 account_number))
#     # reconstruct 2 missing account codes
#     expense %<>% mutate(account_number = ifelse(fiscal_year == 2014 & str_count(account_number, "-") == 2, "60-500-9999-0000-340-0000", account_number))
#
#   } else if(lea == "Provo"){
#     # fix account codes with missing function
#     expense %<>% mutate(account_number = ifelse(str_detect(account_number, "-5210-") & str_length(account_number) == 26,
#                                                 str_replace(account_number, "-5210", "-521"), account_number))
#
#   } else if(lea == "Salt Lake"){
#     # fill in missing program codes
#     expense %<>% mutate(account_number = ifelse(fiscal_year %in% c(2014, 2015, 2016) & str_length(account_number) != 25,
#                                                 paste0(str_sub(account_number, 1, 7), "0000", str_sub(account_number, 8, -1)), account_number))
#
#     expense %<>% mutate(account_number = ifelse(account_number == "----450-0000", "32-036-0000-4000-450-0000", account_number))
#     expense %<>% mutate(account_number = ifelse(account_number == "32-750-5500--450-0000", "32-750-5500-4000-450-0000", account_number))
#
#   } else if(lea == "South Summit"){
#     # reconstruct account codes
#     expense %<>% mutate(account_number = ifelse(fiscal_year %in% c(2019, 2020, 2021,2022) & str_length(account_number) == 15,
#                                                 paste0(str_sub(fund2, 1, 2), "-", str_sub(org2, 1, 3), "-", str_sub(account_number, 1, 4), "-", str_sub(function1, 1, 4), "-", str_sub(cat2, 1, 3), "-0000"),
#                                                 account_number))
#
#   } else if(lea == "Tooele"){
#     # tooele uses periods to delimit account codes in 2017-2018 - convert account numbers separated by periods into dashes
#     expense %<>% mutate(account_number = ifelse(str_detect(account_number, "."), str_replace_all(account_number, "[.]", "-"), account_number))
#
#   } else if(lea == "Washington"){
#     # correct fund range
#     expense %<>% mutate(account_number = ifelse(fiscal_year %in% c(2020, 2021, 2022) & str_length(account_number) == 28,
#                                                 paste0("60-", str_sub(account_number, 7, -1)), account_number))
#
#     # correct program range
#     expense %<>% mutate(account_number = ifelse(fiscal_year %in% c(2019, 2020, 2021, 2022) & str_length(account_number) == 30,
#                                                 paste0(str_sub(account_number, 1, 11), str_sub(account_number, 17, -1)), account_number))
#
#   } else if(lea == "Weber"){
#     # fill in account codes
#     expense %<>% mutate(account_number = ifelse(fiscal_year %in% c(2016) & str_detect(account_number, "^21-701-0005-"), "21-701-0005-0000-518-0000", account_number))
#     expense %<>% mutate(account_number = ifelse(fiscal_year %in% c(2016) & str_detect(account_number, "^21-405-0001-"), "21-405-0001-0000-610-0000", account_number))
#     expense %<>% mutate(account_number = case_when(account_number == "70-402-0005--610-0000" ~ "70-402-0005-0000-610-0000",
#                                                    account_number == "21-405--1000-610-0000" ~ "21-405-0000-1000-610-0000",
#                                                    account_number == "-035-5400-1000-860-0000" ~ "10-035-5400-1000-860-0000",
#                                                    account_number == "21- - -7101-1000-610-0000" ~ "21-105-7101-1000-610-0000",
#                                                    TRUE ~ account_number))
#
#   }
#
#   return(expense)
# }

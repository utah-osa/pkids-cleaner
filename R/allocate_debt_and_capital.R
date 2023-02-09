##########################################################
#'# DEPRECIATE_EXPENSE FUNCTION
#'
#'@export
#'
# depreciate_expense <- function(expense, depreciation, lea_ids){
#
#   ## allocate depreciated expenditures to Y99
#   depreciation %<>% mutate(CD_DEPaCAP = ifelse(is.na(CD_DEPaCAP), 0, CD_DEPaCAP)) %>% # clean up formatting
#     mutate(CD_AF_DESC = str_to_title(CD_AF_DESC)) %>%
#     filter(CD_DEPaCAP != 0)
#
#   # depreciation %<>% mutate(CD_AF_DESC = paste0("Depreciation: ", CD_AF_DESC)) # clean up category name
#
#   dep_exp <- depreciation %>% mutate(u_fund = 10, fund_desc = "General Fund", u_location = lea_ids$district_location, u_program = "0000",
#                                      u_function = 0, u_object = 0, name = "Office of the State Auditor", subcode = "Y99") %>%
#     rename(description = CD_AF_DESC, fiscal_year = FY, total = CD_DEPaCAP) %>%
#     select(fiscal_year, total, description, u_fund:u_object, name, subcode)
#
#   expense <- bind_rows(expense, dep_exp)
#
#
#   ## allocate capital expenditures to D50
#   # depreciate if in objects 700's or functions 4000's or some other programs, only if it's not compensation and if it's greater than $5,000
#   expense %<>% mutate(dep_flag = ifelse((u_object >= 700 & u_object < 800 | u_function >= 4000 & u_function < 5000 | u_program %in% c("5500", "5550", "5570")) &
#                                           !(u_object >= 100 & u_object < 300) & (total > 5000 | total < -5000), 1, 0))
#
#   # allocate debt transactions to D50
#   # exclude if in objects 800's or functions 5000's or in a couple of programs, only if it's not compensation and not interest payment on debt
#   expense %<>% mutate(dep_flag = ifelse((u_object >= 800 & u_object <= 870 | u_object == 523 | u_function >= 5000 & u_function < 6000 | u_program %in% c("5572", "5575")) &
#                                           !(u_object >= 100 & u_object < 300) & u_object != 830, 1, dep_flag))
#
#   # assign subcode for capital costs
#   expense %<>% mutate(subcode = if_else(dep_flag == 1 & !is.na(dep_flag), "D50", subcode))
#
#   return(expense)
# }

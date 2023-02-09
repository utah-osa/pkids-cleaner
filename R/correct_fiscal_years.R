##########################################################
#' CORRECT_FISCAL_YEARS FUNCTION
#'
#' @export
#
# correct_fiscal_years <- function(expense){
#   year <- Sys.Date()
#   year <- format(year, format="%Y")
#
#   ## tintic
#   # remove 2017 batch that is said to be deleted on SF but still present in data
#   expense %<>% filter(batch_id != "47476")
#
#
#   #### 2021 adjustments ####
#   # decided in 2021 to make manual adjustments on our end for issues that are easily fixable (e.g., FY issues)
#   # see script here for discovering district issues/corrections: H:\Economists\Ed\KIDS\All LEAs\Data Quality Checks\Check District Data.R
#
#   ## murray
#   # 2019-2021 FYs are based on calendar year
#
#   expense %<>%
#     # mutate(fiscal_year = ifelse(batch_id %in% c("58317", "59887"), 2019, fiscal_year)) %>% # Issue resolved
#     mutate(fiscal_year = ifelse(batch_id %in% c("64938", "64939", "B-0000876", "B-0000874"), 2020, fiscal_year)) %>%
#     mutate(fiscal_year = ifelse(batch_id %in% c("B-0004971", "B-0006262"), 2021, fiscal_year))
#
#
#   ## north sanpete
#   # 2020/2021 FY wrong
#
#   expense %<>%
#     mutate(fiscal_year = ifelse(batch_id %in% c("B-0004973", "B-0007732"), 2021, fiscal_year))
#
#
#   ## piute
#   # 2022 batch in 2021 data
#
#   curr_year <- max(expense$fiscal_year)
#
#   expense %<>%
#     mutate(fiscal_year = ifelse(batch_id %in% c("B-0012766"), 2022, fiscal_year))
#
#   if(curr_year == year) { expense %<>% filter(fiscal_year <= year)}
#
#
#   ## rich
#   # 2020/2021 FY wrong
#
#   expense %<>%
#     mutate(fiscal_year = ifelse(batch_id %in% c("B-0004780", "B-0008986"), 2021, fiscal_year))
#
#
#   ## uintah
#   # ghost batch for FY20 - occurs when someone deletes the SF object but not the file. the file is not visible from the front-end of SF
#
#   expense %<>%
#     filter(batch_id != "B-0000491")
#
#
#   ## wayne
#   # 2022 batch in 2021 data
#
#   curr_year <- max(expense$fiscal_year)
#
#   expense %<>%
#     mutate(fiscal_year = ifelse(batch_id %in% c("B-0012627"), 2022, fiscal_year))
#
#   if(curr_year == year) { expense %<>% filter(fiscal_year <= year)}
#
# }

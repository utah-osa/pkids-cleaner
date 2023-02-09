################
#' function to update data
#'
#' @export
#
# rerun <- function(start, end, LEA_ID, LEA_FOLDER){
#
#   options(warn=-1)
#
#   rerun_wout_error <- function(){
#
#     if(start == "CLASSES"){
#
#       # Rerun the class adjustments
#       source_file <- try(source(str_c("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/Final Code/Class_Adjustments_", LEA_ID, ".R")))
#       if(class(source_file)=="try-error") stop("R Script File with Error: Classes")
#       rm(list = ls(all.names = TRUE))
#
#     }
#
#     if(start %in% c("CLASSES","PAYROLL") & end %in% c("PAYROLL", "EXP_ANALYSIS", "EXP_ALLOCATION", "ALLOCATION")){
#
#       # Rerun the payroll cleaning
#       source_file <- try(source(str_c("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Payroll/Final Code/", LEA_ID, "_Payroll_Cleaning.R")))
#       if(class(source_file)=="try-error") stop("R Script File with Error: Payroll")
#       rm(list = ls(all.names = TRUE))
#
#     }
#
#     if(start %in% c("CLASSES","PAYROLL","EXP_ALLOCATION") & end %in% c("EXP_ALLOCATION", "ALLOCATION")){
#
#       # Rerun the expense allocation
#       source_file <- try(source(str_c("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Expenditures/Final Code/EXP_Allocation_", LEA_ID, ".R")))
#       if(class(source_file)=="try-error") stop("R Script File with Error: Expense Allocation")
#       rm(list = ls(all.names = TRUE))
#
#     }
#
#     if(start %in% c("CLASSES","PAYROLL","EXP_ALLOCATION","ALLOCATION") & end == "ALLOCATION"){
#
#       # Rerun the allocation
#       source_file <- try(source(str_c("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/Final Code/Allocation ", LEA_ID, ".R")))
#       if(class(source_file)=="try-error") stop("R Script File with Error: Allocation")
#       rm(list = ls(all.names = TRUE))
#
#     }
#   }
#
#   c <- try(rerun_wout_error())
#   if(class(c)=="try-error"){ stop("Error Occured")
#   } else{
#
#     print(paste0("Statewide Student file (smd.RDS) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/smd_", LEA_ID,".RDS"))$mtime))
#
#     print(paste0("Teacher Pay file (Teacher_Payroll_", LEA_ID, ".csv) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Payroll/Teacher_Payroll_", LEA_ID, ".csv"))$mtime))
#     print(paste0("Payroll with Incourse file (", LEA_ID, "_Payroll_With_Incourse.csv) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Payroll/", LEA_ID, "_Payroll_With_Incourse.csv"))$mtime))
#
#     print(paste0("Allocation Matrix (Allocation_Matrix_", LEA_ID, ".csv) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/Allocation_Matrix_", LEA_ID,".csv"))$mtime))
#
#     print(paste0("School file (SCHOOL_", LEA_ID, ".hyper) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/SCHOOL_", LEA_ID, ".hyper"))$mtime))
#     print(paste0("Agg file (Agg_", LEA_ID, ".hyper) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/Agg_", LEA_ID, ".hyper"))$mtime))
#     print(paste0("A file (A_", LEA_ID, ".hyper) last updated ", file.info(paste0("H:/Economists/Ed/KIDS/", LEA_FOLDER, "/Allocation/A_", LEA_ID, ".hyper"))$mtime))
#
#     cat("ALL CODE RAN WITHOUT ERROR")
#   }
#
# }

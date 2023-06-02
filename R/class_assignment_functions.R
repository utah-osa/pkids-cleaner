##########################################################
############# FUNCTIONS FOR CLASS ASSIGNMENT #############
##########################################################
############ Code Written by: Kaylee Hodgson #############
##########################################################

##########################################################
#'Get Mode
#'
#'@description
#''getmode' returns the mode of a given column
#'
#'@details
#'This function returns the mode value for a given column
#'
#'@param v The column for which we are obtaining a mode value
#'@param na.omit Should NA values be excluded? Default is na.omit = T
#'
#'@export
getmode <- function(v, na.omit = T) {
  if(na.omit == T){
    v <- v[!is.na(v)]
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  } else {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  }
##########################################################

##########################################################
#'Weekdays
#'
#'@description
#''weekday_cal' returns a calendar object
#'
#'@details
#'This calendar is used primarily in the class assignment process when calculating the length of a school year and class length
#'
#'
#'@export
weekday_cal <- bizdays::create.calendar(name = "cal",weekdays=c("saturday","sunday"))
##########################################################

##########################################################
#'Not in
#'
#'@description
#' 'notin' returns values that are not in the given parameters
#'
#'@details
#'This function is used throughout the PKids Update Process. It functions opposite to %in% and is equivalent to the function %nin% found in the hmisc package.
#'
#'@param x A parameter
#'@param y A parameter
#'
#'@export
`%nin%` <- function(x,y)!('%in%'(x,y))
##########################################################

##########################################################
#'Change Term
#'
#'@description
#' changeterm' defines the smallest term used for a class
#'
#'@details
#'This function returns a T/F logical statement and is used in the class assignment process
#'
#'@param variable A variable
#'@param overlap Another variable
#'@param term.type A type of term
#'
#'@export
changeterm <- function(variable,overlap,term.type){
  any(unlist(variable) %in% term.type) & any(unlist(variable) %nin% term.type) & overlap=="NONE"
}
##########################################################

##########################################################
#'New Start/End
#'
#'@description
#'new_startend' returns the new start and end dates for duplicated files
#'
#'@details
#'This function is used in defining classes in the class assignment process.
#'
#'@param start The start
#'@param end The end
#'@param n The n
#'@param nth The nth
#'
#'@export
new_startend <- function(start, end, n, nth){
  length <- nth*(end - start)/n
  start + length
}
##########################################################

##########################################################
#'Class Length Recalculation
#'
#'@description
#'clength_recalc' adjusts the length of a class for duplicated records
#'
#'@details
#'This function is used in the Statewide Merge process.
#'START_STUDENT/END_STUDENT - new start/end dates
#'CLENGTH_STUDENT - proportion of year spent in class
#'NOTINCLASS_STUDENT - proportion of year between the start/end dates not spent in class
#'
#'@param starts The column containing start dates
#'@param ends The column containing end dates
#'@param clengths The column containing class lengths
#'
#'@export
clength_recalc <- function(starts, ends, clengths){

  number_overlap <- length(clengths) # number of records

  ### if all duplicates have the same start and end dates, return the first start/end/clength ###
  if(all(starts[1]==starts) & all(ends[1]==ends)){

    return_value <- c(starts[1], ends[1], clengths[1], 0)
    names(return_value) <- c("START_STUDENT","END_STUDENT","CLENGTH_STUDENT","NOTINCLASS_STUDENT")
    return(return_value)

    ### if not, calculate the new start/end/clength ###
  } else {

    number_attempts <- 0

    # iterate through each start/end dates to look for overlap and recalculate the clength
    #   - if all overlap, so there is only one clength, break the loop
    #   - if the loop has cycled through each start/end date to check for overlap, break the loop
    while(length(clengths) > 1 & number_attempts <= number_overlap){

      # compare the first to all others' starts/ends
      overlap <- apply(cbind(starts[-1],ends[-1]), 1, function(x) c(starts[1],ends[1]) %overlaps% x)

      # if there's any overlap, combine those records (start/end/clength)
      if(any(overlap==TRUE)){

        start_overlaps <- min(starts[1], starts[-1][overlap==TRUE])
        end_overlaps <- max(ends[1], ends[-1][overlap==TRUE])
        clength_overlaps <- end_overlaps - start_overlaps
        starts <- c(start_overlaps, starts[-1][overlap==FALSE])
        ends <- c(end_overlaps, ends[-1][overlap==FALSE])
        clengths <- c(clength_overlaps, clengths[-1][overlap==FALSE])

        # if there's no overlap, reorder the starts/ends/clengths so that the next iteration will compare the next record to all others
      } else {

        starts <- c(starts[-1],starts[1])
        ends <- c(ends[-1],ends[1])
        clengths <- c(clengths[-1],clengths[1])

        number_attempts <- number_attempts + 1 # record number of attempts to evaluate overlap
      }

    } # end of while loop

    ### return the direct results from the while loop if all overlapped ###
    if(length(clengths)==1) {

      return_value <- c(starts, ends, clengths, 0)
      names(return_value) <- c("START_STUDENT","END_STUDENT","CLENGTH_STUDENT","NOTINCLASS_STUDENT")
      return(return_value)

      ### return the sum of the clengths, and the min/max start/end if some or all did not overlap ###
      # also calculate the proportion within the start/end not in class
    } else {

      return_value <- c(min(starts), max(ends), sum(clengths), (max(ends) - min(starts) - sum(clengths)))
      names(return_value) <- c("START_STUDENT","END_STUDENT","CLENGTH_STUDENT","NOTINCLASS_STUDENT")
      return(return_value)

    }

  } # end of else loop for calculating new clength if all start and end dates are not the same
}
##########################################################

#######################################################

#' Create Class Labels
#'
#'@details
#'Find the most frequent subject, course name, etc. per class id
#'
#'@param sm The statewide merge data frame for a given entity
#'
#'@export

class_labels <- function(sm){
  sm %>%
    group_by(CLASS_ID) %>%
    mutate(
      school_course_title_of_class = getmode(SCHOOL_COURSE_TITLE),
      core_short_desc_of_class = getmode(CORE_SHORT_DESC),
      subject_of_class = getmode(Subject),
      subject2_of_class = getmode(Subject2),
      school_number_of_class = getmode(SCHOOL_NUMBER),
      school_name_of_class = getmode(SCHOOL)) %>%
    ungroup()
}

#######################################################

#'Create Student Labels
#'
#'@details
#'Find the most frequent school per student
#'
#'@param sm The statewide merge data frame for a given entity
#'
#'@export

student_labels <- function(sm){
  if("SCHOOL_TYPE" %in% colnames(sm)){
    sm %>%
      group_by(STUDENT_ID, SCHOOL_YEAR) %>%
      mutate(school_number_mode_s = getmode(SCHOOL_NUMBER),
             school_name_mode_s = getmode(SCHOOL),
             school_type_mode_s = getmode(SCHOOL_TYPE)) %>%
      ungroup()
  } else {
    sm %>%
      group_by(STUDENT_ID, SCHOOL_YEAR) %>%
      mutate(school_number_mode_s = getmode(SCHOOL_NUMBER),
             school_name_mode_s = getmode(SCHOOL)) %>%
      ungroup()
  }
}

#######################################################

#' Create Testing Labels
#'
#'@details
#'Summarize testing data for visualization
#'
#'@param sm The statewide merge data frame for a given entity
#'
#'@export

test_labels <- function(sm){

  tests <- sm %>%
    group_by(CLASS_ID) %>%
    summarise(n_l = sum(!is.na(LP_YN)),
              n_s = sum(!is.na(SP_YN)),
              n_m = sum(!is.na(MP_YN)),
              # n_rw = sum(!is.na(RWP_YN)),
              # n_t = sum(!is.na(TP_YN)),
              # n_e = sum(!is.na(EP_YN)),
              nmiss_l = sum(is.na(LP_YN)),
              nmiss_s = sum(is.na(SP_YN)),
              nmiss_m = sum(is.na(MP_YN)),
              # nmiss_rw = sum(is.na(RWP_YN)),
              # nmiss_t = sum(is.na(TP_YN)),
              # nmiss_e = sum(is.na(EP_YN)),
              mean_l = (round(mean(LP_YN, na.rm=TRUE), digits=2))*100,
              mean_s = (round(mean(SP_YN, na.rm=TRUE), digits=2))*100,
              mean_m = (round(mean(MP_YN, na.rm=TRUE), digits=2))*100) %>%
    mutate(mean_l = if_else(!is.nan(mean_l), mean_l, NA),
           mean_s = if_else(!is.nan(mean_s), mean_s, NA),
           mean_m = if_else(!is.nan(mean_m), mean_m, NA)) %>%
    mutate(lab_mean_l = if_else(nmiss_l>n_l,"Insufficient Data",paste(mean_l,"%", sep=" "))) %>%
    mutate(lab_mean_s = if_else(nmiss_s>n_s,"Insufficient Data",paste(mean_s,"%", sep=" "))) %>%
    mutate(lab_mean_m = if_else(nmiss_m>n_m,"Insufficient Data",paste(mean_m,"%", sep=" ")))

  # merge into statewide merge file
  sm %<>% left_join(tests, by = c("CLASS_ID"))

}

#######################################################

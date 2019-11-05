# -------------------------------------
### DATA FILE TIME VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_time_setup <- function(data){
  time_period_check(data)
  time_identifier_check(data)
  time_identifier_mix(data)
}

# -------------------------------------
# This checks for a 4 or 6 digit number in the time_period column, and then if 6 digit if it shows consecutive years

time_period_check <- function(data) {
  time_length <- data
  time_length$digits <- str_count(time_length$time_period)
  four <- nrow(filter(time_length,digits == 4))
  six <- nrow(filter(time_length,digits == 6))
  all <- nrow(time_length)
  time_periods <- unique(data$time_period)
  try(cat(if((four + six == all)==FALSE) 
    stop(writeLines(c("FAIL - time period must be a four or six digit number e.g. 2016 or 201617.",
                      "Here are the time period values in your file:","",time_periods,""))),
    message("PASS - time period is always a four or six digit number.")))
        consecutive_mini_function <- function(data) {
          six_digit_years <- filter(time_length,digits==6)
          currentyearend <- as.numeric(substr(six_digit_years$time_period,3,4))
          nextyearend <- as.numeric(substr(six_digit_years$time_period,5,6))
          check_yearends <- any(((currentyearend+1)==nextyearend)==FALSE)
          try(cat(if(check_yearends==TRUE) stop("FAIL - when the time period is 6 digits, the years must be consecutive."),
                  message('PASS - Your 6 digit time period/s show consecutive years.')))
        }
        if(six==0){message("IGNORE - There are no 6 digit time periods in the data file.")
        }else{
          consecutive_mini_function(data)
        }
}

# -------------------------------------
# checking the time identifier values are valid

time_identifier_check <- function(data) {
  acceptable_time_identifiers <- c("Spring term","Autumn term","Autumn and spring term","Up until 31st March",
                                   "January","February","March","April","May","June","July","August","September","October","November","Decemeber",
                                   "Calendar year","Calendar year Q1","Calendar year Q2","Calendar year Q3","Calendar year Q4",
                                   "Calendar year Q1-2","Calendar year Q1-3","Calendar year Q1-4","Calendar year Q2-3","Calendar year Q2-4","Calendar year Q3-4",
                                   "Financial year","Financial year Q1","Financial year Q2","Financial year Q3","Financial year Q4",
                                   "Financial year Q1-2","Financial year Q1-3","Financial year Q1-4","Financial year Q2-3","Financial year Q2-4","Financial year Q3-4",
                                   "Academic year","Academic year Q1","Academic year Q2","Academic year Q3","Academic year Q4",
                                   "Academic year Q1-2","Academic year Q1-3","Academic year Q1-4","Academic year Q2-3","Academic year Q2-4","Academic year Q3-4",
                                   "Tax year","Tax year Q1","Tax year Q2","Tax year Q3","Tax year Q4",
                                   "Tax year Q1-2","Tax year Q1-3","Tax year Q1-4","Tax year Q2-3","Tax year Q2-4","Tax year Q3-4")
  time_identifiers <- unique(data$time_identifier)
  #identifier_test <- intersect(time_identifiers,acceptable_time_identifiers)
  #try(cat(if(FALSE == identical(identifier_test,time_identifiers)) 
  #  stop(writeLines(c("FAIL - There is at least one invalid time_identifier present.",
   #                   "Here are the time_identifiers in your file:","",time_identifiers,""))),
  #message('PASS - Your time identifier/s are valid')))
  message('This will show if any of your time identifier/s are invalid:')
  for(i in time_identifiers){
    if((i %in% acceptable_time_identifiers)==FALSE)
      warning("
             FAIL - ", i, " is not a valid time identifier.")
  }
}

# -------------------------------------
# print the unique time_identifiers for conceptual checking

# need to create a number of vectors for time_identifiers that can be crossed, and flag if they have time_identifiers from multiple groups

time_identifier_mix <- function(data) {
 cat(unique(dataset$time_identifier),sep = ", ") 
}
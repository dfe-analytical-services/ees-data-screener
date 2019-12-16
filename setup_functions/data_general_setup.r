# -------------------------------------
### DATA FILE GENERAL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_general_setup <- function(data){
  data_comp_col(data)
  comma_check(data)
  data_spaces_check(data)
}

data_general_results_function <- function(){
  assign("data_general_results",c(data_comp_col_result),envir = .GlobalEnv)
}

# -------------------------------------
# check that the compulsory columns exist

data_comp_col <- function(data) {
  comp_col <- c("geographic_level","time_period","time_identifier")
  data_comp_col_preresult <- c()
  for(i in comp_col){
    if(i %in% names(data)){
      data_comp_col_preresult[i] <- TRUE
    }else{
      message("FAIL - The ",i," variable is missing from the data file.")
      data_comp_col_preresult[i] <- FALSE
      }
  }
  if(FALSE %in% data_comp_col_preresult){
    assign("data_comp_col_result",FALSE,envir = .GlobalEnv)
  }else{
    message("PASS - time_period, time_identifier, and geographic_level are present.")
    assign("data_comp_col_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# flag for commas across each column

comma_check <- function(data) {
  message("This will show if there are commas present in your data file:")
  for (i in names(data)) {
    if(any(grepl(",",data[[i]]))) warning("FAIL - Comma/s present in ",i,".")
  }
}

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces_check <- function(data) {
  variable_names <- names(dataset)
  message("This will show if there are any spaces in your variable names:")
  for (i in variable_names) {
    if(any(grepl('\\s',i))) warning("
FAIL - There are spaces in ", i,".")
  }
}
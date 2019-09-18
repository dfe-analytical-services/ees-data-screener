# -------------------------------------
### DATA FILE GENERAL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_general_setup <- function(data){
  data_comp_col(data)
  comma_check(data)
  data_spaces_check(data)
}

# -------------------------------------
# check that the compulsory columns exist

data_comp_col <- function(data) {
  comp_col <- c("geographic_level","time_period","time_identifier")
  for(i in comp_col){
    if(i %in% names(data)){message("PASS - ",i," is present in the data file.")}
    else{message("FAIL - The ",i," variable is missing from the data file.")}
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
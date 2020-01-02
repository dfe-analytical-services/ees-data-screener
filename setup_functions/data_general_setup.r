# -------------------------------------
### DATA FILE GENERAL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_general_setup <- function(data){
  data_comp_col(data)
  comma(data)
  data_spaces(data)
}

data_general_results_function <- function(){
  assign("data_general_results",c(data_comp_col_result,
                                  comma_result,
                                  data_spaces_result)
         ,envir = .GlobalEnv)
}

# -------------------------------------
# check that the compulsory columns exist

data_comp_col <- function(data) {
  
  data_comp_col_preresult <- c()
  
  for(i in c("geographic_level","time_period","time_identifier")){
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

comma <- function(data) {
  
  comma_preresult <- c()
  
  for (i in names(data)) {
    if(any(grepl(",",data[[i]]))){
      message("FAIL - Comma/s present in ",i,".")
      comma_preresult[i] <- FALSE
    }else{
      comma_preresult[i] <- TRUE
    }
  }
  if(FALSE %in% comma_preresult){
    assign("comma_result",FALSE,envir = .GlobalEnv)
  }else{
    message("PASS - There are no commas present in the data file.")
    assign("comma_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces <- function(data) {
  
  data_spaces_preresult <- c()
  
  for (i in names(dataset)) {
    if(any(grepl('\\s',i))){
      message("FAIL - There are spaces in ",i,".")
      data_spaces_preresult[i] <- FALSE
    }else{
      data_spaces_preresult[i] <- TRUE
    }
  }
  if(FALSE %in% data_spaces_preresult){
    assign("data_spaces_result",FALSE,envir = .GlobalEnv)
  }else{
    message("PASS - There are no 'Total' values in the observational units.")
    assign("data_spaces_result",TRUE,envir = .GlobalEnv)
  }
}
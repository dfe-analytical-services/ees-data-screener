# -------------------------------------
### DATA FILE INDICATOR VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_indicator_setup <- function(data) {
 suppression_symbols(data)
 no_data_symbols(data)
 null(data)
}

data_indicator_results_function <- function() {
  assign("data_indicator_results", c(
    suppression_symbols_result,
    no_data_symbols_result,
    null_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# check that the compulsory columns exist

suppression_symbols <- function(data) {
  suppression_symbols_preresult <- c()
  
  for (i in present_indicators) {
    if (any(grepl("x", data[[i]]))) {
      suppression_symbols_preresult[i] <- TRUE
    } else {
      message("FAIL - You have 'x' values in your indicators, please update these to the GSS recommended 'c' for suppressed data.")
      suppression_symbols_preresult[i] <- FALSE
    }
  }
  if (FALSE %in% suppression_symbols_preresult) {
    assign("suppression_symbols_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - You have used the GSS recommended symbol for suppressed figures.")
    assign("suppression_symbols_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# check that the compulsory columns exist

no_data_symbols <- function(data) {
  # if any of the .. . NA "N/A" "NA" "n/a" are in the file flag an advisory to check the gss symbols
}

# -------------------------------------
# check for any NULL/Null/null values

null <- function(data) {

  # need to check if this file is being tracked by git or not
    if("NULL" %in% unlist(data)) {
      message("FAIL - There should be no null values in your file.")
      assign("null_result", FALSE, envir = .GlobalEnv)
    } else {
      if("Null" %in% unlist(data)) {
        message("FAIL - There should be no null values in your file.")
        assign("null_result", FALSE, envir = .GlobalEnv)
      } else {
        if("null" %in% unlist(data)) {
          message("FAIL - There should be no null values in your file.")
          assign("null_result", FALSE, envir = .GlobalEnv)
        } else {
          if(any(is.null(unlist(data)))) {
            message("FAIL - There should be no null values in your file.")
            assign("null_result", FALSE, envir = .GlobalEnv)
          } else {
      message("PASS - There are no null values in your file.")
      assign("null_result", TRUE, envir = .GlobalEnv)
        }
      }
      }
    }
}

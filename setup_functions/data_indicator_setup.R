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
      message("ADVISORY - You have 'x' values in your indicators, please update these to the GSS recommended 'c' for suppressed data.")
      suppression_symbols_preresult[i] <- TRUE
    } else {
      suppression_symbols_preresult[i] <- FALSE
    }
  }
  if (TRUE %in% suppression_symbols_preresult) {
    assign("suppression_symbols_result", "Advisory", envir = .GlobalEnv)
  } else {
    message("PASS - You have not used the legacy symbol for suppressed figures.")
    assign("suppression_symbols_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# check that the compulsory columns exist

no_data_symbols <- function(data) {
  old_no_data_symbols <- c("N/A", "NA", "n/a", ".", "..")
  suppressWarnings(if (all(old_no_data_symbols[!old_no_data_symbols %in% unlist(data)] == old_no_data_symbols)) {
    message("PASS - You have not used any legacy symbols for missing data in your file.")
    assign("no_data_symbols_result", TRUE, envir = .GlobalEnv)
  } else {
    message("ADVISORY - Please check the GSS guidance document for advice on the symbols to use for no data.")
    assign("no_data_symbols_result", "Advisory", envir = .GlobalEnv)
  })
}

# -------------------------------------
# check for any NULL/Null/null values

null <- function(data) {
  if ("NA" %in% unlist(data)) {
    message("FAIL - There should be no NA or NULL values in your file.")
    assign("null_result", FALSE, envir = .GlobalEnv)
  } else {
    if ("NULL" %in% unlist(data)) {
      message("FAIL - There should be no NULL values in your file.")
      assign("null_result", FALSE, envir = .GlobalEnv)
    } else {
      if ("Null" %in% unlist(data)) {
        message("FAIL - There should be no Null values in your file.")
        assign("null_result", FALSE, envir = .GlobalEnv)
      } else {
        if ("null" %in% unlist(data)) {
          message("FAIL - There should be no null values in your file.")
          assign("null_result", FALSE, envir = .GlobalEnv)
        } else {
          if (any(is.null(unlist(select(dataset, present_filters_indicators))))) {
            message("FAIL - There should be no null values in your filters or indicators.")
            assign("null_result", FALSE, envir = .GlobalEnv)
          } else {
            message("PASS - There are no null or NA values in your file.")
            assign("null_result", TRUE, envir = .GlobalEnv)
          }
        }
      }
    }
}
}


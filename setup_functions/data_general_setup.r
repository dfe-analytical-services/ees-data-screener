# -------------------------------------
### DATA FILE GENERAL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_general_setup <- function(data) {
  data_comp_col(data)
  data_spaces(data)
  duplicate_rows(data)
}

data_general_results_function <- function() {
  assign("data_general_results", c(
    data_comp_col_result,
    data_spaces_result,
    duplicate_rows_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# check that the compulsory columns exist

data_comp_col <- function(data) {
  data_comp_col_preresult <- c()

  for (i in c("geographic_level", "time_period", "time_identifier")) {
    if (i %in% names(data)) {
      data_comp_col_preresult[i] <- TRUE
    } else {
      message("FAIL - The ", i, " variable is missing from the data file.")
      data_comp_col_preresult[i] <- FALSE
    }
  }
  if (FALSE %in% data_comp_col_preresult) {
    assign("data_comp_col_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - time_period, time_identifier, and geographic_level are present.")
    assign("data_comp_col_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces <- function(data) {
  data_spaces_preresult <- c()

  for (i in names(dataset)) {
    if (any(grepl("\\s", i))) {
      message("FAIL - There are spaces in ", i, ".")
      data_spaces_preresult[i] <- FALSE
    } else {
      data_spaces_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% data_spaces_preresult) {
    assign("data_spaces_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - There are no spaces in the variable names in the data file.")
    assign("data_spaces_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# Checking datafile for duplicate rows across ob. units and filters

duplicate_rows <- function(data) {
  dupes <- suppressMessages(data %>% select(-present_indicators) %>% get_dupes())

  if (nrow(dupes) > 0) {
    message("FAIL - There are ", nrow(dupes), " duplicate rows in your data file.")
    assign("duplicate_rows_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - there are no duplicate rows in your data file.")
    assign("duplicate_rows_result", TRUE, envir = .GlobalEnv)
  }
}

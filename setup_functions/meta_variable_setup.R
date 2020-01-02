# -------------------------------------
### META FILE VARIABLE VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_variable_setup <- function(data, meta) {
  col_name_completed(meta)
  meta_duplicate(meta)
  col_name_spaces(meta)
  column_crosscheck(data, meta)
}

meta_variable_results_function <- function() {
  assign("meta_variable_results", c(
    col_name_completed_result,
    meta_duplicate_result,
    col_name_spaces_result,
    column_crosscheck_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# is col_name completed for every row

col_name_completed <- function(meta) {
  if (any(is.na(meta$col_name))) {
    cat("FAIL - There is a col name missing in ", sum(is.na(meta$col_name)), "row/s.")
    assign("col_name_completed_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - col_name is completed for all rows.")
    assign("col_name_completed_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# checking for duplicates in col_name

meta_duplicate <- function(meta) {
  if (any(meta$col_name %in% meta$col_name[duplicated(meta$col_name)])) {
    message("FAIL - At least one of the variable names is duplicated.")
    assign("meta_duplicate_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - All col_name values are unique.")
    assign("meta_duplicate_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# check that no value in col_name has any spaces

col_name_spaces <- function(meta) {
  if (any(grepl("\\s", meta$col_name))) {
    message("FAIL - There are spaces in the col name values.")
    assign("col_name_spaces_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - There are no spaces in the col name values.")
    assign("col_name_spaces_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# For each col_name in the metadata check these each appear in the data file
column_crosscheck <- function(data, meta) {
  column_crosscheck_preresult <- c()
  for (i in c(meta$col_name)) {
    if ((i %in% names(data)) == FALSE) {
      message("FAIL - ", i, " is not a variable in the data file.")
      column_crosscheck_preresult[i] <- FALSE
    } else {
      column_crosscheck_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% column_crosscheck_preresult) {
    assign("column_crosscheck_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - All col_name values appear in the data file.")
    assign("column_crosscheck_result", TRUE, envir = .GlobalEnv)
  }
}

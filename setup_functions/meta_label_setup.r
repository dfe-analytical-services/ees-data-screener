# -------------------------------------
### META FILE LABEL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_label_setup <- function(meta) {
  col_type(meta)
  label(meta)
  duplicate_label(meta)
}

meta_label_results_function <- function() {
  assign("meta_label_results", c(
    col_type_result,
    label_result,
    duplicate_label_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type <- function(meta) {
  if ((nrow(mfilters) + nrow(mindicators) == nrow(meta)) == FALSE) {
    assign("col_type_result", FALSE, envir = .GlobalEnv)
    message("FAIL - col_type must be either 'Filter' or 'Indicator', and cannot be blank.")
    message("Here are the col type values in your file:", "", unique(meta$col_type), "")
  } else {
    message("PASS - col_type is always 'Filter' or 'Indicator'.")
    assign("col_type_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# is label completed for every row

label <- function(meta) {
  if (any(is.na(meta$label))) {
    message("FAIL - There is a label missing in ", sum(is.na(meta$label)), " row/s.")
    assign("label_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - label is completed for all rows.")
    assign("label_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# checking for duplicate labels

duplicate_label <- function(meta) {
  if (any(meta$label %in% meta$label[duplicated(meta$label)])) {
    message("FAIL - At least one of the variable labels is duplicated.")
    assign("duplicate_label_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - All labels are unique.")
    assign("duplicate_label_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
### META FILE INDICATOR VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_indicator_setup <- function(data, meta) {
  indicator_group(meta)
  indicator_unit(meta)
  indicator_unit_validation(meta)
  indicator_dp(meta)
  indicator_dp_numeric(meta)
}

meta_indicator_results_function <- function() {
  assign("meta_indicator_results", c(
    indicator_group_result,
    indicator_unit_validation_result,
    indicator_unit_result,
    indicator_dp_result,
    indicator_dp_numeric_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# indicator grouping - should be blank for all filters

indicator_group <- function(meta) {
  if (any(!is.na(mfilters$indicator_grouping))) {
    message("FAIL - Filters cannot have an indicator grouping.")
    assign("indicator_group_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - No filters have an indicator grouping.")
    assign("indicator_group_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# Validation for the indicator units

indicator_unit_validation <- function(meta) {
  if (valid_indicatorunits == number_present_indicatorunits) {
    message("PASS - The indicator units are valid.")
    assign("indicator_unit_validation_result", TRUE, envir = .GlobalEnv)
  } else {
    message("FAIL - You have the following invalid indicator units in your metadata: ", paste(invalid_indicatorunits, sep = " "))
    message("If '<U+00A3>' is showing above, then you need to specify UTF-8 encoding when saving your file.")
    assign("indicator_unit_validation_result", FALSE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# indicator unit should be blank for all filters

indicator_unit <- function(meta) {
  if (any(!is.na(mfilters$indicator_unit))) {
    message("FAIL - Filters cannot have an indicator unit.")
    assign("indicator_unit_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - No filters have an indicator unit.")
    assign("indicator_unit_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# indicator dp should be blank for all filters

indicator_dp <- function(meta) {
  if (any(!is.na(mfilters$indicator_dp))) {
    message("FAIL - Filters cannot have an indicator_dp value.")
    assign("indicator_dp_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - No filters have an indicator_dp value.")
    assign("indicator_dp_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# indicator dp should be numeric

indicator_dp_numeric <- function(meta) {
  
  if (all(is.na(meta$indicator_dp))) {
    message("PASS - indicator_dp only contains blanks.")
    assign("indicator_dp_numeric_result", TRUE, envir = .GlobalEnv)
  } else {
    if (is.double(meta$indicator_dp)) {
      message("PASS - indicator_dp only contains numeric values or blanks.")
      assign("indicator_dp_numeric_result", TRUE, envir = .GlobalEnv)
    } else {
      message("FAIL - indicator_dp must only contain numeric values or blanks.")
      assign("indicator_dp_numeric_result", FALSE, envir = .GlobalEnv)
    }
  }
}
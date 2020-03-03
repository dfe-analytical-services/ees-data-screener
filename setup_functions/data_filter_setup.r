# -------------------------------------
### DATA FILE FILTER VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file
data_filter_setup <- function(data, meta) {
  filter_level(data, meta)
  total(data, meta)
  observational_total(data)
}

data_filter_results_function <- function() {
  assign("data_filter_results", c(
    filter_level_result,
    total_result,
    observational_total_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one

filter_level <- function(data, meta) {
  if (ncol(dfilters) == 0) {
    message("IGNORE - There are no filters in your data to test.")
    assign("filter_level_result", NA, envir = .GlobalEnv)
  } else {
    filter_level_preresult <- c()
    for (i in names(dfilters)) {
      if ((length(unique(data[[i]]))) < 2) {
        message("FAIL - There are fewer than two levels in ", i, ", remove this from the metadata file.")
        filter_level_preresult[i] <- FALSE
      } else {
        filter_level_preresult[i] <- TRUE
      }
    }
    if (FALSE %in% filter_level_preresult) {
      assign("filter_level_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - No filters have fewer than two levels.")
      assign("filter_level_result", TRUE, envir = .GlobalEnv)
    }
  }
}

# -------------------------------------
# Check for Total in all filters

total <- function(data, meta) {
  if (!"Filter" %in% meta$col_type) {
    message("IGNORE - There are no filters in your data to test.")
    assign("total_result", NA, envir = .GlobalEnv)
  } else {
    total_preresult <- c()
    for (i in names(dfilters)) {
      if (!"Total" %in% dfilters[[i]]) {
        message("RECOMMENDATION - Add a total aggregation in ", i, ".")
        total_preresult[i] <- FALSE
      } else {
        total_preresult[i] <- TRUE
      }
    }
    if (FALSE %in% total_preresult) {
      assign("total_result", "Advisory", envir = .GlobalEnv)
    } else {
      message("PASS - Every filter has a total level.")
      assign("total_result", TRUE, envir = .GlobalEnv)
    }
  }
}

# -------------------------------------
# Check if Total has been used errorneously in any observational units

observational_total <- function(data) {
  observational_total_preresult <- c()
  for (i in intersect(acceptable_observational_units, names(dataset))) {
    if ("Total" %in% data[[i]] || "total" %in% data[[i]]) {
      message("FAIL - A 'total' value is present in ", i, ", this should be replaced with a blank.")
      observational_total_preresult[i] <- FALSE
    } else {
      observational_total_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% observational_total_preresult) {
    assign("observational_total_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - There are no 'Total' values in the observational units.")
    assign("observational_total_result", TRUE, envir = .GlobalEnv)
  }
}

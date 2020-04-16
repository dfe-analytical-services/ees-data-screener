# -------------------------------------
### META FILE FILTER VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_filter_setup <- function(data, meta) {
  filter_hint(meta)
  filter_group(meta)
  filter_group_match(data, meta)
  filter_group_level(data, meta)
  filter_group_not_filter(meta)
  filter_group_duplicate(meta)
}

meta_filter_results_function <- function() {
  assign("meta_filter_results", c(
    filter_hint_result,
    filter_group_result,
    filter_group_match_result,
    filter_group_level_result,
    filter_group_not_filter,
    filter_group_duplicate 
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# filter_hint should be blank for indicators

filter_hint <- function(meta) {
  if (any(!is.na(mindicators$filter_hint))) {
    message("FAIL - Indicators cannot have an filter hint.")
    assign("filter_hint_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - No indicators have a filter hint.")
    assign("filter_hint_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# filter_grouping column is blank for all indicators

filter_group <- function(meta) {
  if (any(!is.na(mindicators$filter_grouping_column))) {
    message("FAIL - Indicators cannot have a filter group assigned to them.")
    assign("filter_group_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - No indicators have a filter group.")
    assign("filter_group_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# filter groups should be in the vector for column names for the data file

filter_group_match <- function(data, meta) {
  filter_group_match_preresult <- c()
  if (length(present_filtergroups) == 0) {
    message("IGNORE - There are no filter groups present to test.")
    assign("filter_group_match_result", NA, envir = .GlobalEnv)
  } else {
    for (i in present_filtergroups) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " is not a variable in the data file.")
        filter_group_match_preresult[i] <- FALSE
      } else {
        filter_group_match_preresult[i] <- TRUE
      }
    }
    if (FALSE %in% filter_group_match_preresult) {
      assign("filter_group_match_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - All specified filter_group values are present in the data file.")
      assign("filter_group_match_result", TRUE, envir = .GlobalEnv)
    }
  }
}

# -------------------------------------
# Checking that filter groups have fewer levels than their filters

filter_group_level <- function(data, meta) {
  fgs_focus <- select(filtered_filtergroups, c(col_name, filter_grouping_column))
  fgs_list <- c(unlist(fgs_focus, use.names = FALSE))

  filter_group_level_preresult <- c()
  if (length(present_filtergroups) == 0) {
    message("IGNORE - There are no filter groups present to test.")
    assign("filter_group_level_result", NA, envir = .GlobalEnv)
  } else {
    for (i in seq(1, by = 1, len = (length(fgs_list) / 2))) {
      x <- i + (length(fgs_list) / 2)
      y <- fgs_list[[i]]
      z <- fgs_list[[x]]
      if ((length(unique(dataset[[y]]))) < (length(unique(dataset[[z]])))) {
        message("FAIL - ", fgs_list[[i]], " has less levels than its filter group - ", fgs_list[[x]], ".")
        message("You should check that you've entered the filter and filter group in the right columns in the metadata.")
        filter_group_level_preresult[i] <- FALSE
      } else {
        filter_group_level_preresult[i] <- TRUE
      }
    }
    if (FALSE %in% filter_group_level_preresult) {
      assign("filter_group_level_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - All filter groups have fewer levels than their corresponding filter.")
      assign("filter_group_level_result", TRUE, envir = .GlobalEnv)
    }
  }
}

# -------------------------------------
# Checking that filter groups are not filters

filter_group_not_filter <- function(meta) {
  
  if (all(is.na(meta$filter_grouping_column))) {
    message("IGNORE - There are no filter groups present to test.")
    assign("filter_group_not_filter_result", NA, envir = .GlobalEnv)
  } else {
  
  filter_group_not_filter_preresult <- c()
  
    for (i in meta$filter_grouping_column) {
      if ((i %in% meta$col_name) == FALSE) {
        message("FAIL - ", i, " should not be in the col_type column if it is a filter grouping column.")
        filter_group_not_filter_preresult[i] <- FALSE
      } else {
        filter_group_not_filter_preresult[i] <- TRUE
      }
    }
  
  if (FALSE %in% filter_group_not_filter_preresult) {
    assign("filter_group_not_filter_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - Your filter groups are not in the col_name column.")
    assign("filter_group_not_filter_result", TRUE, envir = .GlobalEnv)
  }
  }
}

# -------------------------------------
# Checking that filter groups are not duplicated

filter_group_duplicate <- function(meta) {
  if (all(is.na(meta$filter_grouping_column))) {
    message("IGNORE - There are no filter groups present to test.")
    assign("filter_group_duplicate_result", NA, envir = .GlobalEnv)
  } else {
    if (any(meta$filter_grouping_column %in% meta$filter_grouping_column[duplicated(meta$filter_grouping_column)])) {
      message("FAIL - At least one of the filter groups is duplicated.")
      assign("filter_group_duplicate_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - All filter groups are unique.")
      assign("filter_group_duplicate_result", TRUE, envir = .GlobalEnv)
    }
  }
}

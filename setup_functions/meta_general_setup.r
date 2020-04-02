# -------------------------------------
### ,ETA FILE GENERAL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_general_setup <- function(data, meta) {
  meta_comp_col(meta)
  meta_crosscheck(data, meta)
  comp_col_meta(meta)
  row(data, meta)
}

meta_general_results_function <- function() {
  assign("meta_general_results", c(
    meta_comp_col_result,
    meta_crosscheck_result,
    comp_col_meta_result,
    row_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# Check all compulsory columns exist

meta_comp_col <- function(meta) {
  meta_comp_col_preresult <- c()

  for (i in meta_cols) {
    if (!i %in% names(meta)) {
      message("FAIL - The ", i, " variable is missing.")
      meta_comp_col_preresult[i] <- FALSE
    } else {
      meta_comp_col_preresult[i] <- TRUE
    }
  }

  if (length(setdiff(names(meta), meta_cols)) != 0) {
    message("FAIL - You have the following invalid columns in your metadata file: ", paste(setdiff(names(meta), meta_cols)), sep = " ", ".")
    meta_comp_col_preresult[8] <- FALSE
  }

  if (FALSE %in% meta_comp_col_preresult) {
    assign("meta_comp_col_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - The required metadata columns are present.")
    assign("meta_comp_col_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# List those in the data file that aren't in the metadata (or observational units, or variables with only one level)

meta_crosscheck <- function(data, meta) {
  meta_variables <- setdiff(c(meta$col_name, meta$filter_grouping_column), acceptable_observational_units)
  data_variables <- setdiff(names(data), acceptable_observational_units)
  possible_variables <- setdiff(data_variables, meta_variables)
  missing_and_meta_variables <- meta_variables[!is.na(meta_variables)]

  for (i in possible_variables) {
    if ((length(unique(data[[i]])) > 1) && (i %in% names(data))) {
      missing_and_meta_variables <- c(missing_and_meta_variables, i)
    }
  }

  meta_crosscheck_preresult <- c()
  for (i in unique(missing_and_meta_variables)) {
    if ((i %in% meta_variables) == FALSE) {
      message("ADVISORY - ", i, " was found in the data, and is not in the metadata or an observational unit.")
      meta_crosscheck_preresult[i] <- FALSE
    } else {
      meta_crosscheck_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% meta_crosscheck_preresult) {
    assign("meta_crosscheck_result", "Advisory", envir = .GlobalEnv)
    message("Columns relating to School, Provider, or Planning area data may be flagged above.")
    message("It is correct to not include these in the metadata file if they are blank for all other levels.")
    message("Contact explore.statistics@education.gov.uk if you are unsure.")
  } else {
    message("PASS - All variables in the data file are present in the metadata.")
    assign("meta_crosscheck_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# check if any observational units are in the metadata

comp_col_meta <- function(meta) {
  comp_col_meta_preresult <- c()
  for (i in acceptable_observational_units) {
    if (i %in% meta$col_name) {
      message("FAIL - ", i, " should not be in the metadata. ")
      comp_col_meta_preresult[i] <- FALSE
    } else {
      comp_col_meta_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% comp_col_meta_preresult) {
    assign("comp_col_meta_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - All of the metadata columns are present.")
    assign("comp_col_meta_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# rows in meta < cols in data file

row <- function(data, meta) {
  if (ncol(data) < nrow(meta)) {
    message("FAIL - Your metadata file has more rows than your data file has columns, this means that something is wrong.")
    message("There are either too many rows in the metadata, or too few columns in the data file.")
    message("TRY - Check your .csv files in a text editor as this might help you find the problem.")
    assign("row_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - You have fewer rows in your metadata than you have columns in your data file.")
    assign("row_result", TRUE, envir = .GlobalEnv)
  }
}

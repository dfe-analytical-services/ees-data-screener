# -------------------------------------
### SETTING UP THE FUNCTIONS
# -------------------------------------
# running the setup files, creating the functions to use
source("setup_functions/data_filter_setup.r")
source("setup_functions/data_general_setup.r")
source("setup_functions/data_geography_setup.r")
source("setup_functions/data_time_setup.r")
source("setup_functions/data_indicator_setup.r")
source("setup_functions/meta_general_setup.r")
source("setup_functions/meta_label_setup.r")
source("setup_functions/meta_variable_setup.r")
source("setup_functions/meta_indicator_setup.r")
source("setup_functions/meta_filter_setup.r")

# -------------------------------------
# combining the functions to run against data
screening_tests <- function(data, meta) {
  data_general_setup(data)
  data_geography_setup(data)
  data_time_setup(data)
  data_filter_setup(data, meta)
  data_indicator_setup(data)
  meta_general_setup(data, meta)
  meta_label_setup(meta)
  meta_variable_setup(data, meta)
  meta_filter_setup(data, meta)
  meta_indicator_setup(data, meta)
}

# -------------------------------------
# Variables based on the data being read to be used across functions

mfilters <- filter(metadata, col_type == "Filter")
dfilters <- select(dataset, mfilters$col_name)

mindicators <- filter(metadata, col_type == "Indicator")

present_indicators <- intersect(mindicators$col_name,names(dataset))
present_filters <- intersect(mfilters$col_name,names(dataset))
present_filters_indicators <- c(present_indicators, present_filters)

present_indicatorunits <- mindicators$indicator_unit[!mindicators$indicator_unit == ""]
number_present_indicatorunits <- length(unique(present_indicatorunits))

valid_indicatorunits <- length(intersect(acceptable_indicatorunits, present_indicatorunits))
invalid_indicatorunits <- setdiff(unique(present_indicatorunits), acceptable_indicatorunits)

filtered_filtergroups <- mfilters %>% filter(!is.na(filter_grouping_column))
present_filtergroups <- c(filtered_filtergroups$filter_grouping_column)

# -------------------------------------
# function for the rmd file to show the right icon

pass_fail_image <- function(input) {
  if (is.na(input)) {
    knitr::include_graphics("images/not-applicable.png")
  } else {
    if (input == "Advisory") {
      knitr::include_graphics("images/advisory.png")
    } else {
      if (input == "FALSE") {
        knitr::include_graphics("images/cancel.png")
      } else {
        knitr::include_graphics("images/checked.png")
      }
    }
  }
}

# -------------------------------------
# running the tests and capturing the results

screening_tests(dataset, metadata)

data_filter_results_function()
data_general_results_function()
data_geography_results_function()
data_time_results_function()
data_indicator_results_function()
meta_general_results_function()
meta_variable_results_function()
meta_label_results_function()
meta_filter_results_function()
meta_indicator_results_function()

data_results <- c(data_filter_results, data_general_results, data_geography_results, data_time_results, data_indicator_results)
meta_results <- c(meta_filter_results, meta_indicator_results, meta_general_results, meta_variable_results, meta_label_results)
assign("all_results", c(data_results, meta_results), envir = .GlobalEnv)

pass <- length(which(all_results == TRUE))
fail <- length(which(all_results == FALSE))
total_percent <- paste(round((pass / (pass + fail)) * 100, 1), "%", sep = "")

assign("advisory", length(which(all_results == "Advisory")), envir = .GlobalEnv)

data_pass <- length(which(data_results == TRUE))
data_fail <- length(which(data_results == FALSE))
data_percent <- paste(round((data_pass / (data_pass + data_fail)) * 100, 1), "%", sep = "")
assign("data_advisory", length(which(data_results == "Advisory")), envir = .GlobalEnv)

meta_pass <- length(which(meta_results == TRUE))
meta_fail <- length(which(meta_results == FALSE))
meta_percent <- paste(round((meta_pass / (meta_pass + meta_fail)) * 100, 1), "%", sep = "")
assign("meta_advisory", length(which(meta_results == "Advisory")), envir = .GlobalEnv)

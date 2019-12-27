# -------------------------------------
### SETTING UP THE FUNCTIONS
# -------------------------------------
# Combining each setup script

# running the files
source('setup_functions/data_filters_setup.r')
source('setup_functions/data_general_setup.r')
source('setup_functions/data_geography_setup.r')
source('setup_functions/data_time_setup.r')
source('setup_functions/meta_filter_indicator_setup.r')
source('setup_functions/meta_general_setup.r')
source('setup_functions/meta_variable_label_setup.r')

# adding the functions to run against data
screening_tests <- function(data,meta) {
  data_filters_setup(data,meta)
  data_general_setup(data)
  data_geography_setup(data)
  data_time_setup(data)
  meta_filter_indicator_setup(data,meta)
  meta_general_setup(data,meta)
  meta_variable_label_setup(meta)
}

# combining the outcomes of the checks
# data_results <- c(data_filters_results,data_general_results,data_geography_results,data_time_results)
# meta_results <- c(meta_filter_indicator_results,meta_general_results,meta_variable_label_results)
# all_results <- c(data_results,meta_results)
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
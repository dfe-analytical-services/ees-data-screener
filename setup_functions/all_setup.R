# -------------------------------------
### WRAPPING UP THE FUNCTIONS ACROSS EACH SETUP FILE
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
  meta_variable_label_setup
}

# function to check the pandoc version and install the latest if not 2.7.3 or later
cam_pandoc_install <- function(){
  if(rmarkdown::pandoc_version()>='2.7.3'){print("You already have version 2.7.3 or later of Pandoc installed, you're good to use the screener.")}
  else{if(!require(installr)){install.packages("installr");require(installr)}
  install.pandoc()}
}

# function to install/load the packages needed to run the screener
cam_envrionment_setup <- function(){
  if(!require(govdown)){install.packages("govdown");require(govdown)}
  if(!require(tidyr)){install.packages("tidyr");require(tidyr)}
  if(!require(readr)){install.packages("readr");require(readr)}
  if(!require(stringr)){install.packages("stringr");require(stringr)}
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
}
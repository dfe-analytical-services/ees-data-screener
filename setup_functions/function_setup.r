# -------------------------------------
### SETTING UP THE FUNCTIONS
# -------------------------------------
# running the setup files, creating the functions to use
source('setup_functions/data_filter_setup.r')
source('setup_functions/data_general_setup.r')
source('setup_functions/data_geography_setup.r')
source('setup_functions/data_time_setup.r')
source('setup_functions/meta_filter_indicator_setup.r')
source('setup_functions/meta_general_setup.r')
source('setup_functions/meta_variable_label_setup.r')

# -------------------------------------
# combining the functions to run against data
screening_tests <- function(data,meta,meta_utf) {
  data_filter_setup(data,meta)
  data_general_setup(data)
  data_geography_setup(data)
  data_time_setup(data)
  meta_filter_indicator_setup(data,meta,meta_utf)
  meta_general_setup(data,meta)
  meta_variable_label_setup(meta)
}

# -------------------------------------
# Function-dependent variables

# indicator unit test
indicators_u <- filter(metadata_utf16,col_type == "Indicator")
real_indicators <- indicators_u$indicator_unit[!indicators_u$indicator_unit == ""]
all <- length(unique(real_indicators))
acceptable <- length(intersect(acceptable_indicator_units,real_indicators))
invalid_indicator_units <- setdiff(unique(real_indicators),acceptable_indicator_units)

# -------------------------------------
# function for the rmd file to show the right icon
pass_fail_image <- function(input){
  if(is.na(input)){knitr::include_graphics("images/not-applicable.png")
    }else{if(input=="FALSE"){knitr::include_graphics("images/cancel.png")
    }else{knitr::include_graphics("images/checked.png")}
  }
}

# running the tests and capturing the results
screening_tests(dataset,metadata,metadata_utf16)

data_filter_results_function()
data_general_results_function()
data_geography_results_function()
data_time_results_function()
meta_filter_indicator_results_function()
meta_general_results_function()
meta_variable_label_results_function()

data_results <- c(data_filter_results,data_general_results,data_geography_results,data_time_results)
meta_results <- c(meta_filter_indicator_results,meta_general_results,meta_variable_label_results)
all_results <- c(data_results,meta_results)

pass <- length(which(all_results==TRUE))
fail <- length(which(all_results==FALSE))
total_percent <- paste(round((pass/(pass+fail))*100,1),"%",sep="")

data_pass <- length(which(data_results==TRUE))
data_fail <- length(which(data_results==FALSE))
data_percent <- paste(round((data_pass/(data_pass+data_fail))*100,1),"%",sep="")

meta_pass <- length(which(meta_results==TRUE))
meta_fail <- length(which(meta_results==FALSE))
meta_percent <- paste(round((meta_pass/(meta_pass+meta_fail))*100,1),"%",sep="")


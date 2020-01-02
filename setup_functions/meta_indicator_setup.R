# -------------------------------------
### META FILE INDICATOR VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_indicator_setup <- function(data,meta,meta_utf){
  indicator_group(meta)
  indicator_unit_validation(meta_utf)
  indicator_unit(meta)
}

meta_indicator_results_function <- function(){
  assign("meta_indicator_results",c(indicator_group_result,
                                    indicator_unit_validation_result,
                                    indicator_unit_result),
         envir = .GlobalEnv)
}

# -------------------------------------
# indicator grouping - should be blank for all filters

indicator_group <- function(meta) {
  
  if(any(!is.na(mfilters$indicator_grouping))){
    message('FAIL - Filters cannot have an indicator grouping.')
    assign("indicator_group_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - No filters have an indicator grouping.')
    assign("indicator_group_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# Validation for the indicator units

indicator_unit_validation <- function(meta) {
  
  if(valid_indicator_units == number_present_indicatorunits){
    message("PASS - The indicator units are valid.")
    assign("indicator_unit_validation_result",TRUE,envir = .GlobalEnv)
  } else {
    message("FAIL - You have the following invalid indicator units in your metadata: ",paste(invalid_indicator_units,sep=" "))
    assign("indicator_unit_validation_result",FALSE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# indicator unit should be blank for all filters

indicator_unit <- function(meta) {
  
  if(any(!is.na(mfilters$indicator_unit))){
    message('FAIL - Filters cannot have an indicator unit.')
    assign("indicator_unit_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - No filters have an indicator unit.')
    assign("indicator_unit_result",TRUE,envir = .GlobalEnv)
  }
}

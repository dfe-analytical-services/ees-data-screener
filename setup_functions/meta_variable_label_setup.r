# -------------------------------------
### META FILE VARIABLE AND LABEL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_variable_label_setup <- function(meta){
  col_name_completed(meta)
  meta_duplicate(meta)
  col_name_spaces(meta)
  comp_col_meta(meta)
  col_type(meta)
  label(meta)
  duplicate_label(meta)
}

meta_variable_label_results_function <- function(){
  assign("meta_variable_label_results",c(col_name_completed_result,
                                         meta_duplicate_result,
                                         col_name_spaces_result,
                                         comp_col_meta_result,
                                         col_type_result,
                                         label_result,
                                         duplicate_label_result)
         ,envir = .GlobalEnv)
}

# -------------------------------------
# is col_name completed for every row

col_name_completed <- function(meta) {
  
  if(any(is.na(meta$col_name))){
    cat('FAIL - There is a col name missing in ', sum(is.na(meta$col_name)), 'row/s.')
    assign("col_name_completed_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - col_name is completed for all rows.')
    assign("col_name_completed_result",TRUE,envir = .GlobalEnv)
    }
}

# -------------------------------------
# checking for duplicates in col_name

meta_duplicate <- function(meta) {
  
  if(any(meta$col_name %in% meta$col_name[duplicated(meta$col_name)])){
    message('FAIL - At least one of the variable names is duplicated.')
    assign("meta_duplicate_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - All col_name values are unique.')
    assign("meta_duplicate_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# check that no value in col_name has any spaces

col_name_spaces <- function(meta) {
  
  if(any(grepl('\\s',meta$col_name))){
    message("FAIL - There are spaces in the col name values.")
    assign("col_name_spaces_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - There are no spaces in the col name values.')
    assign("col_name_spaces_result",TRUE,envir = .GlobalEnv)
  
  }
}

# -------------------------------------
# check if any observational units are in the metadata

comp_col_meta <- function(meta) {

  comp_col_meta_preresult <- c()
  for (i in acceptable_observational_units){
    if(i %in% meta$col_name){
      message("FAIL - ", i, " should not be in the metadata. ")
      comp_col_meta_preresult[i] <- FALSE
    }else{
      comp_col_meta_preresult[i] <- TRUE
    }
  }
  if(FALSE %in% comp_col_meta_preresult){
    assign("comp_col_meta_result",FALSE,envir = .GlobalEnv)
  }else{
    message("PASS - All of the metadata columns are present.")
    assign("comp_col_meta_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type <- function(meta) {

    if((nrow(mfilters) + nrow(mindicators) == nrow(meta))==FALSE){
      assign("col_type_result",FALSE,envir = .GlobalEnv)
      message("FAIL - col_type must be either 'Filter' or 'Indicator', and cannot be blank.")
      message("Here are the col type values in your file:","",unique(meta$col_type),"")
    }else{
      message("PASS - col_type is always 'Filter' or 'Indicator'.")
      assign("col_type_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# is label completed for every row

label <- function(meta) {
  
  if(any(is.na(meta$label))){
    message('FAIL - There is a label missing in ', sum(is.na(meta$label)), ' row/s.')
    assign("label_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - label is completed for all rows.')
    assign("label_result",TRUE,envir = .GlobalEnv)
    }
}

# -------------------------------------
# checking for duplicate labels

duplicate_label <- function(meta) {
  
  if(any(meta$label %in% meta$label[duplicated(meta$label)])){
    message('FAIL - At least one of the variable names is duplicated.')
    assign("duplicate_label_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - All labels are unique.')
    assign("duplicate_label_result",TRUE,envir = .GlobalEnv)
  }
}
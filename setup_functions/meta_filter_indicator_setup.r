# -------------------------------------
### META FILE FILTER AND INDICATOR VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_filter_indicator_setup <- function(data,meta,meta_utf){
  indicator_group(meta)
  indicator_unit_validation(meta_utf)
  indicator_unit(meta)
  filter_hint(meta)
  filter_group(meta)
  row(data,meta)
  filter_group_match(data,meta)
  filter_group_level(data,meta)
}

meta_filter_indicator_results_function <- function(){
  assign("meta_filter_indicator_results",c(indicator_group_result,
                                           indicator_unit_validation_result,
                                           indicator_unit_result,
                                           filter_hint_result,
                                           filter_group_result,
                                           row_result,
                                           filter_group_match_result,
                                           filter_group_level_result),
         envir = .GlobalEnv)
}

# -------------------------------------
# indicator grouping - should be blank for all filters

indicator_group <- function(meta) {
  filters <- filter(meta,col_type == "Filter")
  if(any(!is.na(filters$indicator_grouping))){
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
  if(acceptable == all){
    message("PASS - The indicator units are valid.")
    assign("indicator_unit_validation_result",TRUE,envir = .GlobalEnv)
  } else {
    message("FAIL - You have the following invalid indicator units in your metadata:",invalid_indicator_units)
    assign("indicator_unit_validation_result",FALSE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# indicator unit should be blank for all filters

indicator_unit <- function(meta) {
  filters <- filter(meta,col_type =='Filter')
  if(any(!is.na(filters$indicator_unit))){
    message('FAIL - Filters cannot have an indicator unit.')
    assign("indicator_unit_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - No filters have an indicator unit.')
    assign("indicator_unit_result",TRUE,envir = .GlobalEnv)
    }
}

# -------------------------------------
# filter_hint should be blank for indicators

filter_hint <- function(meta) {
  indicators <- filter(meta,col_type =='Indicator')
  if(any(!is.na(indicators$filter_hint))){
    message('FAIL - Indicators cannot have an filter hint.')
    assign("filter_hint_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - No indicators have a filter hint.')
    assign("filter_hint_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# filter_grouping column is blank for all indicators

filter_group <- function(meta) {
  indicators <- filter(meta,col_type =='Indicator')
  if(any(!is.na(indicators$filter_grouping_column))){
    message('FAIL - Indicators cannot have a filter group assigned to them.')
    assign("filter_group_result",FALSE,envir = .GlobalEnv)
  }else{
    message('PASS - No indicators have a filter group.')
    assign("filter_group_result",FALSE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# rows in meta < cols in data file

row <- function(data,meta) {
  if(ncol(data)<nrow(meta)){
    message("FAIL - Your metadata file has more rows than your data file has columns, this means that something is wrong.")
    message("There are either too many rows in the metadata, or too few columns in the data file.")
    message("TRY - Check your .csv files in a text editor as this might help you find the problem.")
    assign("row_result",FALSE,envir = .GlobalEnv)
  }else{
    message("PASS - You have fewer rows in your metadata than you have columns in your data file.")
    assign("row_result",TRUE,envir = .GlobalEnv)
  }
}

# -------------------------------------
# filter groups should be in the vector for column names for the data file

filter_group_match <- function(data,meta) {
  filters <- filter(meta,col_type == "Filter")
  filtered_g <- filters %>% drop_na(filter_grouping_column)
  filter_groups <- c(filtered_g$filter_grouping_column)
  filter_group_match_preresult <- c()
    if(length(filter_groups)==0){
      message("IGNORE - There are no filter groups present to test.")
      assign("filter_group_match_result",NA,envir = .GlobalEnv)
    }else{
    for(i in filter_groups){
      if((i %in% names(data))==FALSE){
        message("FAIL - ",i," is not a variable in the data file.")
        filter_group_match_preresult[i] <- FALSE
      }else{
        filter_group_match_preresult[i] <- TRUE
      }
    }
    if(FALSE %in% filter_group_match_preresult){
      assign("filter_group_match_result",FALSE,envir = .GlobalEnv)
    }else{
      message("PASS - All specified filter_group values are present in the data file.")
      assign("filter_group_match_result",TRUE,envir = .GlobalEnv)
    }
  }
}

# -------------------------------------
# Checking that filter groups have fewer levels than their filters

filter_group_level <- function(data,meta){
  filters <- filter(meta,col_type == "Filter")
  filtered_g <- filters %>% drop_na(filter_grouping_column)
  filter_groups <- c(filtered_g$filter_grouping_column)
  filter_group_level_preresult <- c()
  if(length(filter_groups)==0){
    message("IGNORE - There are no filter groups present to test.")
    assign("filter_group_level_result",NA,envir = .GlobalEnv)
    }else{
    filter_groups <- drop_na(metadata,filter_grouping_column)
    fgs_focus <- select(filter_groups,c(col_name,filter_grouping_column))
    fgs_list <- c(unlist(fgs_focus,use.names=FALSE))
    
    for(i in seq(1,by=1,len=(length(fgs_list)/2))){
      x <- i+(length(fgs_list)/2)
      y <- fgs_list[[i]]
      z <- fgs_list[[x]]
      if((length(unique(dataset[[y]])))<(length(unique(dataset[[z]])))){
        message("FAIL - ",fgs_list[[i]]," has less levels than its filter group - ",fgs_list[[x]],".")
        message("You should check that you've entered the filter and filter group in the right columns in the metadata.")
        filter_group_level_preresult[i] <- FALSE
      }else{
        filter_group_level_preresult[i] <- TRUE
      }
    }
    if(FALSE %in% filter_group_level_preresult){
      assign("filter_group_level_result",FALSE,envir = .GlobalEnv)
    }else{
      message("PASS - All filter groups have fewer levels than their corresponding filter.")
      assign("filter_group_level_result",TRUE,envir = .GlobalEnv)
    }
  }
}
# -------------------------------------
### META FILE FILTER AND INDICATOR VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_filter_indicator_setup <- function(data,meta){
  indicator_group_check(meta)
  indicator_unit_validation(meta)
  indicator_unit_check(meta)
  filter_hint_check(meta)
  filter_group_check(meta)
  row_check(data,meta)
  filter_group_match(data,meta)
  filter_group_levels(data,meta)
}

# -------------------------------------
# indicator grouping - should be blank for all filters

indicator_group_check <- function(meta) {
  filters <- filter(meta,col_type == "Filter")
  if(any(!is.na(filters$indicator_grouping))){message('FAIL - Filters cannot have an indicator grouping.')}
    else{message('PASS - No filters have an indicator grouping.')}
}

# -------------------------------------
# Validation for the indicator units

indicator_unit_validation <- function(meta) {
  indicators <- filter(meta,col_type == "Indicator")
  real_indicators <- indicators$indicator_unit[!indicators$indicator_unit == ""]
  all <- length(unique(real_indicators))
  acceptable_indicator_units <- c("£","%")
  acceptable <- length(intersect(acceptable_indicator_units,real_indicators))
  invalid_indicator_units <- setdiff(unique(real_indicators),acceptable_indicator_units)
  if(acceptable == all){
    paste("PASS - The indicator units are valid.")
  } else {
    paste(c("FAIL - You have the following invalid i units in your metadata:",invalid_indicator_units))
  }
}

# -------------------------------------
# indicator unit should be blank for all filters

indicator_unit_check <- function(meta) {
  filters <- filter(meta,col_type =='Filter')
  if(any(!is.na(filters$indicator_unit))){message('FAIL - Filters cannot have an indicator unit.')}  
    else{message('PASS - No filters have an indicator unit.')}
}

# -------------------------------------
# filter_hint should be blank for indicators

filter_hint_check <- function(meta) {
  indicators <- filter(meta,col_type =='Indicator')
  if(any(!is.na(indicators$filter_hint))){message('FAIL - Indicators cannot have an filter hint.')}
    else{message('PASS - No indicators have a filter hint.')}
}

# -------------------------------------
# filter_grouping column is blank for all indicators

filter_group_check <- function(meta) {
  indicators <- filter(meta,col_type =='Indicator')
  if(any(!is.na(indicators$filter_grouping_column))){message('FAIL - Indicators cannot have a filter group assigned to them.')} 
    else{message('PASS - No indicators have a filter group.')}
}

# -------------------------------------
# rows in meta < cols in data file

row_check <- function(data,meta) {
  if(ncol(data)<nrow(meta)) stop('FAIL - Your metadata file has more rows than your data file has columns, this means that something is wrong.
        There are either too many rows in the metadata, or too few columns in the data file.
TRY - Check your .csv files in a text editor as this might help you find the problem.')
  message('PASS - You have fewer rows in your metadata than you have columns in your data file.')
}

# -------------------------------------
# filter groups should be in the vector for column names for the data file

filter_group_match <- function(data,meta) {
  filters <- filter(meta,col_type == "Filter")
  filtered_g <- filters %>% drop_na(filter_grouping_column)
  filter_groups <- c(filtered_g$filter_grouping_column)
    if(length(filter_groups)==0){message("IGNORE - There are no filter groups present to test.")}
  else{
    message("This will show if any of your filter groups do not have a matching variable in the data file:")
    for(i in filter_groups){
      if((i %in% names(data))==FALSE)
        warning("
    FAIL - ", i," is not a variable in the data file.")
    }
  }
}

# -------------------------------------
# Checking that filter groups have fewer levels than their filters

filter_group_levels <- function(data,meta){
  filters <- filter(meta,col_type == "Filter")
  filtered_g <- filters %>% drop_na(filter_grouping_column)
  filter_groups <- c(filtered_g$filter_grouping_column)
  if(length(filter_groups)==0){message("IGNORE - There are no filter groups present to test.")}
  else{
    filter_groups <- drop_na(metadata,filter_grouping_column)
    fgs_focus <- select(filter_groups,c(col_name,filter_grouping_column))
    fgs_list <- c(unlist(fgs_focus,use.names=FALSE))
    message("This will flag if any filter groups have more levels than their corresponding filters:")
    for(i in seq(1,by=1,len=(length(fgs_list)/2))){
      x <- i+(length(fgs_list)/2)
      y <- fgs_list[[i]]
      z <- fgs_list[[x]]
      if((length(unique(dataset[[y]])))<(length(unique(dataset[[z]]))))
        warning("
    WARNING - ",fgs_list[[i]]," has less levels than its filter group - ",fgs_list[[x]],".","
  You should check that you've entered the filter and filter group in the right columns in the metadata.")
    }
  }
}
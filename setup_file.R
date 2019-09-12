# -------------------------------------
### VALIDATION FUNCTIONS
# -------------------------------------
# check that the compulsory columns exist

data_comp_col <- function(data) {
  try(cat(if(!"geographic_level" %in% names(data)) stop("FAIL - The geographic_level variable is missing"), 
          message('PASS - geographic_level is present in the data file')),silent = FALSE)
  try(cat(if(!"time_period" %in% names(data)) stop("FAIL - The time_period variable is missing"), 
          message('PASS - time_period is present in the data file')),silent = FALSE)
  try(cat(if(!"time_identifier" %in% names(data)) stop("FAIL - The time_identifier variable is missing"), 
          message('PASS - time_identifier is present in the data file')),silent = FALSE)
  try(cat(if(!"country_name" %in% names(data)) stop("FAIL - The country_name variable is missing"), 
          message('PASS - country_name is present in the data file')),silent = FALSE)
  try(cat(if(!"country_code" %in% names(data)) stop("FAIL - The country_code variable is missing"), 
          message('PASS - country_code is present in the data file')),silent = FALSE)
}

# -------------------------------------
# This checks for a 4 or 6 digit number in the time_period column

time_period_check <- function(data) {
  try(cat(if(((any(grepl("^[0-9]{4,4}$",dataset$time_period)))==FALSE)&&((any(grepl("^[0-9]{6,6}$",dataset$time_period)))==FALSE)) 
    stop("FAIL - time_period must be a four or six digit number e.g. 2016 or 201617"),
    message('PASS - time_period is either 4 or 6 digits')))
}

time_period_check(dataset)

# -------------------------------------
# Checking that 6 digit numbers are for consecutive years

#time_period_check_consecutive <- function(data) {

#  currentyearend <- as.numeric(substr(dataset$time_period,3,4))
#  nextyearend <- as.numeric(substr(dataset$time_period,5,6))
  # add something to this to give a false when it's not
#  check_yearends <- any(((currentyearend+1)==nextyearend)==FALSE)

#if((!any(grepl("^[0-9]{6,6}$",data$time_period)))) warning("Ignore this test as there aren't six digit years")
    
#if(check_yearends==TRUE) stop("when time_period is 6 digits, the years must be consecutive")
      
#  message('PASS - Your 6digit time_period shows consecutive years')
#}

#time_period_check_consecutive(dataset)

# -------------------------------------
# checking the time identifier values are valid

time_identifier_check <- function(data) {
  acceptable_time_identifiers <- c("Spring term","Autumn term","Autumn and spring term","Up until 31st March",
                                   "January","February","March","April","May","June","July","August","September","October","November","Decemeber",
                                   "Calendar year","Calendar year Q1","Calendar year Q2","Calendar year Q3","Calendar year Q4",
                                   "Calendar year Q1-2","Calendar year Q1-3","Calendar year Q1-4","Calendar year Q2-3","Calendar year Q2-4","Calendar year Q3-4",
                                   "Financial year","Financial year Q1","Financial year Q2","Financial year Q3","Financial year Q4",
                                   "Financial year Q1-2","Financial year Q1-3","Financial year Q1-4","Financial year Q2-3","Financial year Q2-4","Financial year Q3-4",
                                   "Academic year","Academic year Q1","Academic year Q2","Academic year Q3","Academic year Q4",
                                   "Academic year Q1-2","Academic year Q1-3","Academic year Q1-4","Academic year Q2-3","Academic year Q2-4","Academic year Q3-4",
                                   "Tax year","Tax year Q1","Tax year Q2","Tax year Q3","Tax year Q4",
                                   "Tax year Q1-2","Tax year Q1-3","Tax year Q1-4","Tax year Q2-3","Tax year Q2-4","Tax year Q3-4")
  time_identifiers <- unique(data$time_identifier)
  identifier_test <- intersect(time_identifiers,acceptable_time_identifiers)
  try(cat(if(FALSE == identical(identifier_test,time_identifiers)) stop("FAIL - There is at least one invalid time_identifier present"),
  message('PASS - Your time identifier/s are valid')))
}

# -------------------------------------
# print the unique time_identifiers for conceptual checking

time_identifier_mix <- function(data) {
 cat(unique(data$time_identifier)) 
}

# -------------------------------------
# flag for commas across each column

comma_check <- function(data) {
  for (i in names(data)) {
    if(any(grepl(",",data[[i]]))) warning("FAIL - There are commas in ", i)
  }
message('If there are no warnings in this box, there are no commas in your data file')
}

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces_check <- function(data) {
  variable_names <- names(dataset)
  for (i in variable_names) {
    if(any(grepl('\\s',i))) warning("FAIL - There are spaces in ", i)
  }
  message('If there are no warnings in this box, there are no spaces in your variable names')
}

# -------------------------------------
# Do we have the right columns for the geographic level
#National_required <- c("country_code","country_name")
#Regional_required <- c("country_code","country_name","region_code","region_name")
#LA_required <- c("country_code","country_name","region_code","region_name","old_la_code","new_la_code","la_name")
#RSC_required <- c("country_code","country_name","rsc_name")
#PCon_required <- c("country_code","country_name","pcon_code","pcon_name")
#LAD_required <- c("country_code","country_name","lad_code","lad_name")
#LEP_required <- c("country_code","country_name","local_enterprise_partnership_code","local_enterprise_partnership_name")
#MCA_required <- c("country_code","country_name","mayoral_combined_authority_code","mayoral_combined_authority_name")
#OpportunityArea_required <- c("country_code","country_name","opportunity_area_code","opportunity_area_name")
#Ward_required <- c("country_code","country_name","ward_code","ward_name")
#MAT_required <- c("country_code","country_name","trust_id","trust_name")
#Sponsor_required <- c("country_code","country_name","sponsor_id","sponsor_name")

#geography_levels_present(dataset)

# -------------------------------------
# Are the geography columns completed for the right levels

#geography_level_completed <- function(data) {
  
#  National <- filter(data, geographic_level =='National')
  
#  if(any(is.na(National$country_name))) warning('The country_name column must be completed for national level data')  
  
#  message('country_name is present')
  
#  if(any(is.na(National$country_code))) warning('The country_code column must be completed for national level data')  
  
#  message('country_code is present')
  
#}

#geography_level_completed(dataset)

# -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one

# want to iterate over the columns of dfilters and then test if nrow(unique(x))>1
#filter_levels_check <- function(data) {
  
#  for (i in names(dfilters)) {
#    ifelse((length(unique(dataset[[i]])))>1,
#           message('Passed'),
#           warning('A filter should have more than 1 level, if they only have one level then remove them from the metadata so they are not in the table tool')
#    )
#  }
  
#}

#filter_levels_check(dataset)

# -------------------------------------
# Check for Total in all filters

#something
#total_check(dataset)

# -------------------------------------
# Check all compulsory columns exist

meta_comp_col <- function(data) {
  try(cat(if(!"col_name" %in% names(data)) stop("FAIL - The col_name variable is missing"), 
          message('PASS - col_name is present in the metadata')),silent = FALSE)
  try(cat(if(!"col_type" %in% names(data)) stop("FAIL - The col_type variable is missing"), 
          message('PASS - col_type is present in the metadata')),silent = FALSE)
  try(cat(if(!"label" %in% names(data)) stop("FAIL - The label variable is missing"), 
          message('PASS - label is present in the metadata')),silent = FALSE)
  try(cat(if(!"indicator_grouping" %in% names(data)) stop("FAIL - The indicator_grouping variable is missing"), 
          message('PASS - indicator_grouping is present in the metadata')),silent = FALSE)
  try(cat(if(!"indicator_unit" %in% names(data)) stop("FAIL - The indicator_unit variable is missing"), 
          message('PASS - indicator_unit is present in the metadata')),silent = FALSE)
  try(cat(if(!"filter_hint" %in% names(data)) stop("FAIL - The filter_hint variable is missing"), 
          message('PASS - filter_hint is present in the metadata')),silent = FALSE)
  try(cat(if(!"filter_grouping_column" %in% names(data)) stop("FAIL - The filter_grouping_column variable is missing"), 
          message('PASS - filter_grouping_column is present in the metadata')),silent = FALSE)
}

# -------------------------------------
# flag for commas across each column

meta_comma_check <- function(data) {
  for (i in names(data)) {
    if(any(grepl(",",data[[i]]))) warning("FAIL - There are commas in ", i)
  }
  message('If there are no warnings in this box, there are no commas in your metadata file')
}

# -------------------------------------
# is col_name completed for every row

col_name_completed <- function(data) {
  if(any(is.na(data$col_name))) stop(cat('FAIL - There are names missing in ', sum(is.na(data$col_name)), 'rows'))
    message('PASS - col_name is completed for all rows')
}

# -------------------------------------
# checking for duplicates in col_name

meta_duplicate_check <- function(data) {
  if(any(data$col_name %in% data$col_name[duplicated(data$col_name)])) stop('FAIL - At least one of the variable names is duplicated')
    message('PASS - All col_names are unique')
}

# -------------------------------------
# check that no value in col_name has any spaces

col_name_spaces_check <- function(data) {
  if(any(grepl('\\s',data$col_name))) stop("FAIL - There are spaces in the col_name values")
  message('PASS - There are no spaces in the col_name values')
}

# -------------------------------------
# check if any observational units are in the metadata

comp_col_check_meta <- function(data) {
  observational_units <- c("geographic_level","time_period","time_identifier","country_code","country_name",
                           "region_code","region_name","old_la_code","new_la_code","la_name","rsc_name",
                           "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                           "local_enterprise_partnership_name","mayoral_combined_authority_code",
                           "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                           "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name")
  for (i in observational_units) {
    try(cat(if(i %in% data$col_name) warning("FAIL - ", i, " should not be in the metadata. ")))
  }
  message("If there are no warnings in this box, there are no observational units in your metadata")
}

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

# This is currently checking that they are both present, rather than the column is only one of these two...
#col_type_check <- function(data) {
  
#  if((!"Filter" %in% data$col_type)&(!"Indicator" %in% data$col_type))
#    stop("col_type must either be 'Filter' or 'Indicator'")
  
#  message('PASS - col_type is always a Filter or an Indicator')
  
#}

#col_type_check(metadata)

# -------------------------------------
# is label completed for every row

label_check <- function(data) {
  if(any(is.na(data$label))) 
    stop(paste('FAIL - There are labels missing in', sum(is.na(data$label)), 'rows')) 
    message('PASS - label is completed for all rows')
}

# -------------------------------------
# checking for duplicate labels

duplicate_label_check <- function(data) {
  if(any(data$label %in% data$label[duplicated(data$label)])) stop('FAIL - At least one of the variable names is duplicated')
  message('PASS - All labels are unique')
}

# -------------------------------------
# indicator grouping - should be blank for all filters

indicator_group_check <- function(data) {
  filters <- filter(data,col_type == "Filter")
  if(any(!is.na(filters$indicator_grouping))) stop('FAIL - Filters cannot have an indicator grouping')  
  message('PASS - No filters have an indicator grouping')
}

# -------------------------------------
# Validation for the indicator units - NOT WORKING

#indicator_unit_validation <- function(data) {

  #remove after testing
#indicators <- filter(metadata,col_type == "Indicator")
#units_present <- unique(indicators$indicator_unit)  

#logic is wrong, need  to check what is there against our list, any()?
#  if(("£" %in% units_present)|
#     ("%" %in% units_present)|
#    (NA %in% units_present))
     
#  warning('There is at least one invalid indicator unit in the metadata')  
    
#  message('PASS - The indicator units are all valid')
  
#}

#indicator_unit_validation(metadata)

# -------------------------------------
# indicator unit should be blank for all filters

indicator_unit_check <- function(data) {
  filters <- filter(data,col_type =='Filter')
  if(any(!is.na(filters$indicator_unit))) stop('FAIL - Filters cannot have an indicator unit')  
  message('PASS - No filters have an indicator unit')
}

# -------------------------------------
# filter_hint should be blank for indicators

filter_hint_check <- function(data) {
  indicators <- filter(data,col_type =='Indicator')
  if(any(!is.na(indicators$filter_hint))) stop('FAIL - Indicators cannot have an filter hint')  
  message('PASS - No indicators have a filter hint')
}

# -------------------------------------
# filter_grouping column is blank for all indicators

filter_group_check <- function(data) {
  indicators <- filter(data,col_type =='Indicator')
  if(any(!is.na(indicators$filter_grouping_column))) stop('FAIL - Indicators cannot have a filter group assigned to them')  
  message('PASS - No indicators have a filter group')
}

# -------------------------------------
# for each col_name in the metadata check these each appear in the data file
# - flag any that aren't in the data file
# - list those in the data file that aren't in the metadata (or observational units)
#for(i in metadata$col_type) {
#  if(!i %in% names(dataset)) warning("You have listed a variable in the metadata that is not present in the data file")
  
#}
#column_crosscheck(data,meta)

# -------------------------------------
# rows in meta < cols in data file

row_check <- function(data,meta) {
  if(ncol(data)<nrow(meta)) stop('FAIL - Check your files in a text editor.
Your metadata file has more rows than your data file has columns, this is not right.
There are either too many rows in the metadata, or too few columns in the data file')
  message('PASS - You have fewer rows in your metadata than you have columns in your data file')
}

# -------------------------------------
# filter_grouping anything in this column should be in the vector for column names for the data file

#something about creating a variable of the filter_groups in the metadata
#comparing against names(data)

#filter_group_match(data)

# -------------------------------------
### WRAPPING UP THE FUNCTIONS ABOVE NEATLY INTO A FUNCTION
# -------------------------------------

screening_tests <- function(data,meta) {
  data_comp_col(data)
  time_period_check(data)
  #time_period_check_consecutive(data) - needs working out upstream to handle when the test isn't needed
  time_identifier_check(data)
  time_identifier_mix(data)
  comma_check(data)
  data_spaces_check(data)
  #geography_levels_present(data) - needs working on
  #geography_level_completed(data) - needs working on
  #filter_levels_check(data) - needs working on
  #total_check(data) - needs working on
  meta_comp_col(metadata)
  meta_comma_check(meta)
  col_name_completed(meta)
  meta_duplicate_check(meta)
  col_name_spaces_check(meta)
  comp_col_check_meta(meta)
  #col_type_check(meta) - needs reworking
  label_check(meta)
  duplicate_label_check(meta)
  indicator_group_check(meta)
  #indicator_unit_validation(meta) - needs reworking
  indicator_unit_check(meta)
  filter_hint_check(meta)
  filter_group_check(meta)
  #column_crosscheck(data,meta) - needs working on
  row_check(data,meta)
  #filter_group_match(data) - needs working on
}
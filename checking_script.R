# -------------------------------------
# PACKAGES
# -------------------------------------

# only need to run this line if you don't already have it installed
# install.packages('tidyverse')

library(tidyverse)

# -------------------------------------
# IMPORTING FILES
# -------------------------------------

metadata <- read_csv("data_metadata/EYFSP_APS_GLD_ELG_underlying_data_2013_2018.meta.csv")
dataset <- read_csv("data_metadata/APS_GLD_ELG_underlying_data_2013_2018.csv")

# -------------------------------------
# SETTING UP
# -------------------------------------

# Metadata slices
mfilters <- filter(metadata,col_type=="Filter")
#mfilter_groups <- filter group is not blank
mindicators <- filter(metadata,col_type=="Indicator")

# Datafile slices
filternames <- c(mfilters$col_name) 
indicatornames <- c(mindicators$col_name)

dfilters <- select(dataset, filternames)
dindicators <- select(dataset, indicatornames)

# -------------------------------------
### DATA FILE VALIDATION FUNCTIONS
# -------------------------------------

# all tests below this point will need testing with the test data to make sure they fail correctly as well as pass correctly

# check that the compulsory columns exist
comp_col_check <- function(data) {
  
  if(!"geographic_level" %in% names(data)) warning("geographic_level is missing")
  if(!"time_identifier" %in% names(data)) warning("time_identifier is missing")
  if(!"country_code" %in% names(data)) warning("country_code is missing")
  if(!"country_name" %in% names(data)) warning("country_name is missing")
  if(!"time_period" %in% names(data)) warning("time_period is missing") 
  
  message('passed')  
}

comp_col_check(dataset)

# -------------------------------------
# are the compulsory time and geography columns filled in and valid?
# This checks for a 4 or 6 digit number in the time_period column

time_period_check <- function(data) {
  
  if ((!any(grepl("^[0-9]{4,4}$",dataset$time_period)))&(!any(grepl("^[0-9]{6,6}$",dataset$time_period)))) 
    stop("time_period must be a four or six digit number e.g. 2016 or 201617")
  
  message('passed')
}

time_period_check(dataset)

# -------------------------------------
# Checking that 6 digit numbers are for consecutive years
# This is still a bit crude, but works for the majority of cases

time_period_check_consecutive <- function(data) {

if ((!any(grepl("^[0-9]{6,6}$",dataset$time_period)))) warning("Ignore this test as there aren't six digit years")
    
currentyearend <- as.numeric(substr(dataset$time_period,3,4))
nextyearend <- as.numeric(substr(dataset$time_period,5,6))

check_yearends <- any(((currentyearend+1)==nextyearend)==FALSE)
  
if(check_yearends==TRUE) warning("when time_period is 6 digits, the years must be consecutive")
      
  message('passed')
}

time_period_check_consecutive(dataset)

# -------------------------------------
# no crossing of time indentifiers - print the unique/distinct values from that column for now

time_identifier_list <- unique(dataset$time_identifier)

cat("Check the following list for crossing of conceptually different values:
    ", time_identifier_list) 

# -------------------------------------
# are the time identifier values valid?

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
  
  time_identifier <- unique(dataset$time_identifier)

  identifier_test <- intersect(time_identifier,acceptable_time_identifiers)
  
  if(FALSE == identical(identifier_test,time_identifier)) warning("There is an invalid time_identifier")
  
 message('passed')
  
 }

time_identifier_check(dataset)

# -------------------------------------
# flag for commas
# - FUTURE - use a loop across every column to flag the ones that have commas

#for i in x

#if(is.element(",",unlist(i))) stop("There are commas in column" i)

#message('passed')

comma_check <- function(data) {

  if(is.element(",",unlist(data))) stop("There are commas in your file")

  message('passed')
  
}

comma_check(dataset)

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces_check <- function(data) {
    
  if (any(grepl('\\s',names(dataset)))) stop("There are spaces in column names")

  message('passed')

}

data_spaces_check(dataset)

# -------------------------------------
# FUTURE - character limits? maybe something to decide on good practice once the platform is closer to ready
# FUTURE - for now just count the characters per variable name and then the maximumn per each column (mainly for my own interest)

# -------------------------------------
# NEEDS WORK - Do we have the right levels for the amount of data
# - Setting up the levels and required columns

National_required <- c("country_code","country_name")
Regional_required <- c("country_code","country_name","region_code","region_name")
LA_required <- c("country_code","country_name","region_code","region_name","old_la_code","new_la_code","la_name")
RSC_required <- c("country_code","country_name","rsc_name")
PCon_required <- c("country_code","country_name","pcon_code","pcon_name")
LAD_required <- c("country_code","country_name","lad_code","lad_name")
LEP_required <- c("country_code","country_name","local_enterprise_partnership_code","local_enterprise_partnership_name")
MCA_required <- c("country_code","country_name","mayoral_combined_authority_code","mayoral_combined_authority_name")
OpportunityArea_required <- c("country_code","country_name","opportunity_area_code","opportunity_area_name")
Ward_required <- c("country_code","country_name","ward_code","ward_name")
MAT_required <- c("country_code","country_name","trust_id","trust_name")
Sponsor_required <- c("country_code","country_name","sponsor_id","sponsor_name")

# SCHOOL LEVEL DATA IS NOT YET SUPPORTED

# - NEEDS WORK - actually testing this, are the above present?

# - NEEDS WORK - are the geography columns completed for the right levels

geography_level_check <- function(data) {
  
  # once i've finished setting these up I should shift the variable creation to the top
  National <- filter(dataset, geographic_level =='National')
  
  if(any(is.na(National$country_name))) warning('The country_name column must be completed for national level data')  
  
  message('country_name is present')
  
  if(any(is.na(National$country_code))) warning('The country_code column must be completed for national level data')  
  
  message('country_code is present')
  
}

geography_level_check(dataset)

# -------------------------------------
# FUTURE - geo codes are relevant to year of data (very optimistic, leave for now)

# -------------------------------------
### METADATA VALIDATION FUNCTIONS
# -------------------------------------

# check all columns exist
# - FUTURE - may want to add additional text at some point that highlights columns might be there but might just be labelled incorrectly

meta_col_check <- function(data) {
  
  if(!"col_name" %in% names(data)) warning("col_name is missing")
  if(!"col_type" %in% names(data)) warning("col_type is missing")
  if(!"label" %in% names(data)) warning("label is missing")
  if(!"indicator_grouping" %in% names(data)) warning("indicator_grouping is missing")
  if(!"indicator_unit" %in% names(data)) warning("indicator_unit") 
  if(!"filter_hint" %in% names(data)) warning("filter_hint is missing")
  if(!"filter_grouping_column" %in% names(data)) warning("filter_grouping_column is missing") 
  
  message('passed')  
  
}

meta_col_check(metadata)

# -------------------------------------
# flag for commas
# - FUTURE - more complex, but can we somehow flag for an individual column?

# currently commented out as the function is already defined
#comma_check <- function(data) {
  
 # if(is.element(",",unlist(data))) stop("There are commas in your file")
  
  #message('passed')
#}

comma_check(metadata)

# -------------------------------------
# is col_name completed for every row

meta_name_check <- function(data) {
  
  if(any(is.na(data$col_name))) warning(paste('There are names missing in ', sum(is.na(data$col_name)), 'rows'))
  if(any(metadata$col_name %in% metadata$col_name[duplicated(metadata$col_name)])) warning('At least one of the variable names is duplicated')
  
  message('passed')
  
}

meta_name_check(metadata)

# - check that no value in here has any spaces

meta_name_spaces_check <- function(data) {
  
  if (any(grepl('\\s',metadata$col_name))) stop("there are spaces in column names")
  
  message('passed')
  
}

meta_name_spaces_check(dataset)

# - also then something to check if it's a column that shouldn't be in? Maybe from the list of possible time/geography ones

comp_col_check_meta <- function(data) {
  
  if("geographic_level" %in% metadata$col_name) warning("geographic_level should not be in the metadata")
  if("time_identifier" %in% metadata$col_name) warning("time_identifier should not be in the metadata")
  if("country_code" %in% metadata$col_name) warning("country_code should not be in the metadata")
  if("country_name" %in% metadata$col_name) warning("country_name should not be in the metadata")
  if("time_period" %in% metadata$col_name) warning("time_period should not be in the metadata") 
  
  message('passed')  
  
}

comp_col_check_meta(metadata)

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type_check <- function(data) {
  
  if((!"Filter" %in% metadata$col_type)&(!"Indicator" %in% metadata$col_type))
    stop("col_type must either be 'Filter' or 'Indicator'")
  
  message('passed')
  
}

col_type_check(metadata)

# -------------------------------------
# label - is this filled in for every row - can we flag the specific row there isn't a label?
# - i.e. you need to add a label for your school_type column
# - there shouldn't be any duplicate values in this column

meta_label_check <- function(data) {
  
if(any(is.na(data$label))) warning(paste('There are labels missing in ', sum(is.na(data$label)), 'rows'))
if(any(metadata$label %in% metadata$label[duplicated(metadata$label)])) warning('At least one of the labels is duplicated')
  
  message('passed')
  
}

meta_label_check(metadata)

# -------------------------------------
# indicator grouping - is this blank for all filters?
# - FUTURE - can we extract these and show in a list
# - 'here are groups for your indicators as they will appear, please check these are correct'.

meta_indicator_group_check <- function(data) {
  
  filters <- data %>% filter(data$col_type =='Filter')
  if(any(!is.na(filters$indicator_grouping))) warning('Filter variables cannot have a indicator_grouping assigned to them')  
  
  message('passed')
  
}

meta_indicator_group_check(metadata)

# -------------------------------------
# Validation for the indicator units - NOT WORKING

meta_indicator_unit_check <- function(data) {
  
indicators <- data %>% filter(data$col_type =='Indicator')
  
  if((!"£" %in% indicators$indicator_unit)|
     (!"%" %in% indicators$indicator_unit)|
     (!"" %in% indicators$indicator_unit))
     
  warning('There is an invalid indicator unit in the metadata')  
    
  message('passed')
  
}

meta_indicator_unit_check(metadata)

# - should be blank for all filters

meta_filter_unit_check <- function(data) {
  
  filters <- data %>% filter(data$col_type =='Filter')
  
  if(any(!is.na(filters$indicator_unit))) warning('Filter variables cannot have an indicator unit assigned to them')  
  
  message('passed')
  
}

meta_filter_unit_check(metadata)

# -------------------------------------
# filter_hint should be blank for indicators
# - FUTURE - perhaps we can flag at row level?
# - FUTURE - a furtherthing would be add a message for filters where this isn't added so that we can say 'you don't have a hint for x, are you sure?

meta_filter_hint_check <- function(data) {
  
  indicators <- data %>% filter(data$col_type =='Indicator')
  
  if(any(!is.na(indicators$filter_hint))) warning('Indicator variables cannot have a filter hint assigned to them')  
  
  message('passed')
  
}

meta_filter_hint_check(metadata)

# -------------------------------------
# filter_grouping column
# - should be blank for all indicators

meta_filter_group_check <- function(data) {
  
  indicators <- data %>% filter(data$col_type =='Indicator')
    
  if(any(!is.na(indicators$filter_grouping_column))) warning('Indicator variables cannot have a filter group assigned to them')  
  
  message('passed')
  
}

meta_filter_group_check(metadata)

# -------------------------------------
### CROSS VALIDATION OF METADATA AND DATA FILE FUNCTIONS
# -------------------------------------

# for each col__name in the metadata check these each appear in the data file

for(i in metadata$col_type) {
  if(!i %in% names(dataset)) warning("You have listed a variable in the metadata that is not present in the data file")
  
}

# - flag any that aren't in the data file
# - list those in the data file that aren't in the metadata (or observational units)

# -------------------------------------
# rows in meta < cols in data file

row_check <- function(dataset,metadata) {

  if(ncol(dataset)<nrow(metadata)) stop('There are too many rows in the metadata, or too few columns in the data file')
  
  message('passed')
  
}

row_check(dataset,metadata)

# -------------------------------------
# filter_grouping anything in this column should be in the vector for column names for the data file

# -------------------------------------
# filter_group column has less levels than filter column in the data file

# this would need to iterate over each possible filter column and each corresponding grouping column
# comparing the unique levels in the datafile


# -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one
# want to iterate over the columns of dfilters and then test if nrow(unique(x))>1
filter_levels_check <- function(data) {
  
  for (i in names(dfilters)) {
    ifelse((length(unique(dataset[[i]])))>1,
           message('Passed'),
           warning('A filter should have more than 1 level, if they only have one level then remove them from the metadata so they are not in the table tool')
    )
  }
  
  }

filter_levels_check(dataset)

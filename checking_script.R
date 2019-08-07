# only need to run this line if you don't already have it installed
#install.packages('tidyverse')

library(tidyverse)

# -------------------------------------
# IMPORTING FILES
# -------------------------------------

metadata <- read_csv("data_metadata/absence_in_prus.meta.csv")
dataset <- read_csv("data_metadata/absence_in_prus.csv")

metadata <- read_csv("data_metadata/3digit_illegal.meta.csv")
dataset <- read_csv("data_metadata/3digit_illegal.csv")

# -------------------------------------
### DATA FILE VALIDATION
# -------------------------------------

# all tests below this point will need testing with the test data to make sure they fail correctly as well as pass correctly

# check that the compulsory columns exist
comp_col_check <- function(data) {
  
  if(!"geographic_level" %in% names(data)) warning("geographic_level is missing")
  if(!"time_identifier" %in% names(data)) warning("time_identifier is missing")
  if(!"country_code" %in% names(data)) warning("country_code is missing")
  if(!"country_name" %in% names(data)) warning("country_name is missing")
  if(!"time_period" %in% names(data)) warning("time_period is missing") 
  
  'passed'  
}

comp_col_check(dataset)

# -------------------------------------
# are the compulsory time and geography columns filled in and valid?
# This checks for a 4 or 6 digit number in the time_period column

time_period_check <- function(data) {
  
  if ((!any(grepl("^[0-9]{4,4}$",dataset$time_period)))&(!any(grepl("^[0-9]{6,6}$",dataset$time_period)))) 
    stop("time_period must be a four or six digit number e.g. 2016 or 201617")
  
  'passed'
}

time_period_check(dataset)

# -------------------------------------
# write a check for the 6 digit number only being consecutive years
# - could use the 101 thing to work it out

time_period_check_consecutive <- function(data) {
  
currentyear <- strtoi(substr(dataset$time_period,3,4))

nextyear <- strtoi(substr(dataset$time_period,5,6))
  
  if (((currentyear+1)!=nextyear)&(!any(grepl("^[0-9]{6,6}$",dataset$time_period))))
      stop("6 digit time_period values must show consecutive years, e.g. 201617")
  
  'passed'
}

time_period_check_consecutive(dataset)

# -------------------------------------
# no crossing of time indentifiers - print the unique/distinct values from that column for now

unique(dataset$time_identifier)

# -------------------------------------
# flag for commas
# - use a loop across every column to flag the ones that have commas

comma_check <- function(data) {

  if(is.element(",",unlist(data))) stop("There are commas in your file")

  'passed'
}

comma_check(dataset)

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces_check <- function(data) {
  
  names <- dataset$col_name
  
  if (any(grepl('\\s',names))) stop("there are spaces in column names")
  
  'passed'
}

data_spaces_check(dataset)

# -------------------------------------
# character limits? maybe something to decide on good practice once the platform is closer to ready
# - for now just count the characters per variable name and then the maximumn per each column (mainly for my own interest)

# -------------------------------------
# do any other valid geography columns exist
# - if so, are these valid?
# - if so, are the minimum ones there that we expect based on the level column

# -------------------------------------
# geo codes are relevant to year of data (optimistic?)

# -------------------------------------
# consistency in levels and ‘unique’ (geog and filter)

# -------------------------------------
# filters contain ‘total’ level

# -------------------------------------
# empty indicators - maybe output the percentage of all indicator values that are blank?

# -------------------------------------
### METADATA VALIDATION
# -------------------------------------

# check all columns exist
# - may want to add additional text at some point that highlights columns might be there but might just be labelled incorrectly

meta_col_check <- function(data) {
  
  if(!"col_name" %in% names(data)) warning("col_name is missing")
  if(!"col_type" %in% names(data)) warning("col_type is missing")
  if(!"label" %in% names(data)) warning("label is missing")
  if(!"indicator_grouping" %in% names(data)) warning("indicator_grouping is missing")
  if(!"indicator_unit" %in% names(data)) warning("indicator_unit") 
  if(!"filter_hint" %in% names(data)) warning("filter_hint is missing")
  if(!"filter_grouping_column" %in% names(data)) warning("filter_grouping_column is missing") 
  
  
  # need a way to make sure that it doesn't say passed when there is warnings?  
  'passed'  
}

meta_col_check(metadata)

# -------------------------------------
# flag for commas
# - more complex, but can we somehow flag for an individual column?
# checking for commas in the data file - currently commented out as the function is already defined

#comma_check <- function(data) {
  
 # if(is.element(",",unlist(data))) stop("There are commas in your file")
  
  #'passed'
#}

comma_check(metadata)

# -------------------------------------
# is col_name completed for every row
# - check that no value in here has any spaces
# - also then something to check if it's a column that shouldn't be in? Maybe from the list of possible time/geography ones

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type_check <- function(data) {
  if((!"Filter" %in% metadata$col_type)&(!"Indicator" %in% metadata$col_type))
    stop("col_type must either be 'Filter' or 'Indicator'")
  'passed'
}

col_type_check(metadata)

# -------------------------------------
# label - is this filled in for every row - can we flag the specific row there isn't a label?
# - i.e. you need to add a label for your school_type column
# - there shouldn't be any duplicate values in this column

# -------------------------------------
# indicator grouping - is this blank for all filters?
# - can we extract these and show in a list
# -- 'here are groups for your indicators as they will appear, please check these are correct'.

# -------------------------------------
# Unit validation? indicator unit column
# - something about units
# - should be blank for all filters

# -------------------------------------
# filter_hint should be blank for indicators, again can we flag at row level?
# - also add a message for filters where this isn't added so that we can say 'you don't have a hint for x, are you sure?

# -------------------------------------
# filter_grouping column
# - should be blank for all indicators

# -------------------------------------
### CROSS VALIDATION OF METADATA AND DATA FILE
# -------------------------------------

# for each col__name in the metadata check these each appear in the data file
# - flag any that aren't in the data file
# - list those in the data file that aren't in the metadata (or observational units)

# -------------------------------------
# rows in meta < cols in data file

# -------------------------------------
# filter_grouping anything in this column should be in the vector for column names for the data file

# -------------------------------------
# filter_group column has less levels than filter column in the data file

# -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one
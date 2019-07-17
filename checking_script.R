# We need notes here


# only need to run this line if you don't already have it installed
#install.packages('tidyverse')

library(tidyverse)

# These are the notes I took when we spoke about how we should do this 

# order for checking
# 1. load metadata and validate
# 2. load csv and validate against metadata
# 3. checks on csv file
# -------------------------------------

# I've (CR) split these out into the sections that will relate to each other

# case check -  I can't remember what this was for?

### DATA FILE VALIDATION
# flag for commas
# spaces in variable names
# character limits? maybe something to decide on good practice once the platform is closer to ready
#
# STARTED - check that the compulsory columns exist
# are the compulsory time and geography columns filled in and valid?
# no crossing of time indentifiers
# STARTED - time period is 4 or 6 digits
#
# do any other valid geography columns exist
# if so, are these valid?
# within geo level the rows have values in the columns we would expect
# (remember some may need to be flexible as they can be filled in or blank)
# depending on geo levels provided the columns are as expected
# geo codes are relevant to year of data
#
# consistency in levels and ‘unique’ (geog and filter)
# filters contain ‘total’ level
# empty indicators

### METADATA VALIDATION
# STARTED - check all columns exist (including filter group)
# STARTED - flag for commas
# - more complex, but can we somehow flag for an individual column?
# -- pull out the column names, and then run tests against that list as a way of doing it generically?
# is metadata filled in as expected? - each column individually
# is col_name completed for every row, and LATER check these each appear in the data file (bad if they don't)
# - also then something to check if it's a column that shouldn't be in? Maybe from the list of possible time/geography ones
# col_type - is this one of 'Filter' or 'Indicator'
# label - is this filled in for every row - can we flag the specific row there isn't a label?
# - i.e. you need to add a label for your school_type column
# indicator grouping - is this blank for all filters?
# - can we extract these and show in a list, so...
# -- 'here are groups for your indicators as they will appear, please check these are correct'.
# DISCUSS - unit validation? indicator unit column
# - should be blank for all filters
# filter_hint should be blank for indicators, again can we flag at row level?
# - also add a message for filters where this isn't added so that we can say 'you don't have a hint for x, are you sure?
# filter_grouping column
# - should be blank for all indicators
# - cross checked with data file later on

### CROSS VALIDATION OF METADATA AND DATA FILE
# for each col__name in the metadat check these each appear in the data file
# rows in meta < cols in data file
# filter_grouping anything in this column should be in the vector for column names for the data file
# filter_group column has less levels than filter column in the data file

# -------------------------------------
# will need to test each individual check with faulty data, come up with a systematic way to do this

# Importing csv files

metadata <- read_csv("data_metadata/absence_in_prus.meta.csv")
dataset <- read_csv("data_metadata/absence_in_prus.csv")

# -------------------------------------
# Checking metadata columns are all present
# may want to add additional text at some point that highlights columns might be there but might just be labelled incorrectly
# can this all just be done by comparing the names vector (I think that's the right term?) with one that has the right names in?
# although I guess we want to give feedback on each individual one
# also, do we need to make these all seperate, otherwise it fails on the first one and doesn't give them all? - STOP T OWARNING TO FIX THIS

meta_col_check <- function(data) {
  
  if(!"col_name" %in% names(data)) warning("col_name is missing")
  if(!"col_type" %in% names(data)) stop("col_type is missing")
  if(!"label" %in% names(data)) stop("label is missing")
  if(!"indicator_grouping" %in% names(data)) stop("indicator_grouping is missing")
  if(!"indicator_unit" %in% names(data)) stop("indicator_unit") 
  if(!"filter_hint" %in% names(data)) stop("filter_hint is missing")
  if(!"filter_grouping_column" %in% names(data)) stop("filter_grouping_column is missing") 
  
 
# way to make sure that it doesn't say passed when there is warnings?  
   'passed'  
}

meta_col_check(metadata)
# -------------------------------------
# checking for commas in the metadata

comma_check <- function(data) {
  
  if(is.element(",",unlist(data))) stop("There are commas in your file")
  
  'passed'
}

comma_check(metadata)

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces_check <- function(data) {
 
  names <- dataset$col_name
  
  if (any(grepl('\\s',names))) stop("there are spaces in column names")
  
  'passed'
}

data_spaces_check(dataset)

# -------------------------------------
# checking for commas in the data file - currently commented out as the function is already defined
# left in incase we do seperate the script

#comma_check <- function(data) {
  
#  if(is.element(",",unlist(data))) stop("There are commas in your file")
  
#  'passed'
#}

comma_check(dataset)

# -------------------------------------

## Check for specific columns in data

comp_col_check <- function(data) {
  
if(!"geographic_level" %in% names(data)) stop("geographic_level is missing")
if(!"time_identifier" %in% names(data)) stop("time_identifier is missing")
if(!"country_code" %in% names(data)) stop("country_code is missing")
if(!"country_name" %in% names(data)) stop("country_name is missing")
if(!"time_period" %in% names(data)) stop("time_period is missing") 

#commented out as it's not a requirement  
#if(!any(grepl('[:upper:]',names(data)))) stop("there are upper case letters in column names")
  
  'passed'  
}

comp_col_check(dataset)


# -------------------------------------

# This checks for a 4 or 6 digit number in the time_period column

time_period_check <- function(data) {

if ((!any(grepl("^[0-9]{4,4}$",dataset$time_period)))&(!any(grepl("^[0-9]{6,6}$",dataset$time_period)))) 
  stop("time_period must be a four or six digit number e.g. 2016 or 201617")

  'passed'
}

time_period_check(dataset)

# -------------------------------------
# Write a check for the 6 digit number only being consecutive years
# could use the 101 thing to work it out

# -------------------------------------
# check that the col_type column is one of filter or metadata

col_type_check <- function(data) {
  if((!"Filter" %in% metadata$col_type)&(!"Indicator" %in% metadata$col_type))
    stop("col_type must either be 'Filter' or 'Indicator'")
  'passed'
}

col_type_check(metadata)

# -------------------------------------
# -------------------------------------
# -------------------------------------


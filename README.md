# **Data-screener checks that we want**
These are the notes I had (CR), I've also reordered the main script so it resembles this

## DATA FILE VALIDATION
Check that the compulsory columns exist
***
Are the compulsory time and geography columns filled in and valid?
***
Time period is 4 or 6 digits
***
Write a check for the 6 digit number only being consecutive years
- could use the 101 thing to work it out
***
No crossing of time indentifiers - print the unique/distinct values from that column for now
***
Flag for commas
-use a loop across every column to flag the ones that have commas
***
Spaces in variable names
***
Character limits? maybe something to decide on good practice once the platform is closer to ready
- for now just count the characters per variable name and then the maximumn per each column (mainly for my own interest)
***
Do any other valid geography columns exist
- if so, are these valid?
- if so, are the minimum ones there that we expect based on the level column
***
Geo codes are relevant to year of data (optimistic?)
***
Consistency in levels and ‘unique’ (geog and filter)
***
Filters contain ‘total’ level
***
Empty indicators - maybe output the percentage of all indicator values that are blank?
***

## METADATA VALIDATION
Check all columns exist
***
Flag for commas
- more complex, but can we somehow flag for an individual column?
***
Is col_name completed for every row
- check that no value in here has any spaces
- also then something to check if it's a column that shouldn't be in? Maybe from the list of possible time/geography ones
***
Col_type - is this one of 'Filter' or 'Indicator'
***
Label - is this filled in for every row - can we flag the specific row there isn't a label?
- i.e. you need to add a label for your school_type column
***
Indicator grouping - is this blank for all filters?
- can we extract these and show in a list
- 'here are groups for your indicators as they will appear, please check these are correct'.
***
Unit validation? indicator unit column
- something about units
- should be blank for all filters
***
Filter_hint should be blank for indicators, again can we flag at row level?
- also add a message for filters where this isn't added so that we can say 'you don't have a hint for x, are you sure?
***
Filter_grouping column
- should be blank for all indicators
***

## CROSS VALIDATION OF METADATA AND DATA FILE
For each col__name in the metadata check these each appear in the data file
- flag any that aren't in the data file
- list those in the data file that aren't in the metadata (or observational units)
***
Rows in meta < cols in data file
***
Filter_grouping anything in this column should be in the vector for column names for the data file
***
Filter_group column has less levels than filter column in the data file
***
Filters in the metadata file should have more than one value - flag when they only have one
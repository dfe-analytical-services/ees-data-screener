## Future developments to this screener

# Future Ideas
Something that counts and outputs the maximum character lengths per column - more of a style guide to consider later
Anything on school level data that isn't already covered (thinking geographic levels etc)
Something checking if the geographic codes are relevant to the year of data - a very optimistic and complex one
Adding something to flag if columns such as time or geography are in the metadata and give a message that they shouldn't be
Checks that check if indicators have a certain unit, they are a specific type? Might not be needed nor possible
Extract the indicator groupings and print
Add a message for filters where a hint isn't added so that we can say 'you don't have a hint for x, are you sure?
Printing out observational unit and filter levels?

# Ideally needed if there is time
Need some way of checking for spaces at the end of variable names, R seems to be ignoring them on import?
check that the filter_group columns have less levels than the corresponding filter column in the data file (checking people have the two the right way around)
Count the number of spaces and commas in a file for those checks and print those

# Potential general code improvements
Soft coding the columns in the functions (not sure how to make this work), would only save a small amount of duplication so not hugely important
Fixing grammar so that singular counts have a different message

Outputting a pretty report
Make the script easier to use

# Future developments to this screener

## Future ideas
+ Something that counts and outputs the maximum character lengths per column - more of a style guide to consider later
+ Anything on school level data that isn't already covered (thinking geographic levels etc)
+ Something checking if the geographic codes are relevant to the year of data - a very optimistic and complex one
+ Adding something to flag if columns such as time or geography are in the metadata and give a message that they shouldn't be
+ Checks that check if indicators have a certain unit, they are a specific type? Might not be needed nor possible
+ Extract the indicator groupings and print
+ Add a message for filters where a hint isn't added so that we can say 'you don't have a hint for x, are you sure?
+ Printing out observational unit and filter levels?

## Ideally needed if there is time
+ Better error handling for things like the validity checks (thinking levels one in particular)
+ Expansions on the geography checks 
  + Could add a lookup list for countries and regions?
  + Could try to wrap it all into one, so it prints the levels you have, and then tests if the right columns are present and completed for the right levels, all in one. Basically so that it stops as soon as something isn't right (and stops without stopping the entire script).
+ Make the validity tests into loops that print out the incorrect values?
+ Need some way of checking for spaces at the end of variable names, R seems to be ignoring them on import?
+ Check that the filter_group columns have less levels than the corresponding filter column in the data file (checking people have the two the right way around)
+ Count the number of spaces and commas in a file for those checks and print those

## Potential general code improvements
+ Soft coding the columns in the functions (not sure how to make this work), would only save a small amount of duplication so not hugely important
+ Fixing grammar so that singular counts have a different message
+ Make the html output prettier
# Future developments to this screener

## Working task list
+ Make the validity + spaces tests into loops that print out the incorrect values
+ Neaten up the error messages by using break and next instead of stop to decide what messages are shown
+ Fix the issue with £ symbol

## Future ideas
+ Something that counts and outputs the maximum character lengths per column - more of a style guide to consider later
+ Count the number of spaces and commas in a file for those checks and print those

+ Anything on school level data that isn't already covered (thinking geographic levels etc)
+ Something checking if the geographic codes are relevant to the year of data - a very optimistic and complex one
+ Adding something to flag if columns such as time or geography are in the metadata and give a message that they shouldn't be
+ Checks that check if indicators have a certain unit, they are a specific type? Might not be needed nor possible
+ Extract the indicator groupings and print
+ Add a message for filters where a hint isn't added so that we can say 'you don't have a hint for x, are you sure?
+ Printing out observational unit and filter levels?

## Potential general code improvements
+ Soft coding the columns in the functions (not sure how to make this work), would only save a small amount of duplication so not hugely important
+ Fixing grammar so that singular counts have a different message
+ Make the html output prettier
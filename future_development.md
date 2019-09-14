# Future developments to this screener

## Working task list
+ **Fix the issue with the geography columns not present giving both pass and fail messages**

+ Neaten up the error messages by using break and next instead of stop to decide what messages are shown
 + Try using ifelse and break/next with the col_type check
 + Also see if I can remove the need for 'this will show warnings' messages and get everything to pass/fail
+ Fix the issue with £ symbol

## Future ideas
+ Make the validity + spaces tests into loops that print out the incorrect values
+ Something Laura said about making sure each row was 'unique' (presumably observational unit and filter combinations?)
+ Something that counts and outputs the maximum character lengths per column - more of a style guide to consider later
+ Count the number of spaces and commas in a file for those checks and print those

+ Anything on school level data that isn't already covered (thinking geographic levels etc)
+ Something checking if the geographic codes are relevant to the year of data - a very optimistic and complex one
+ Adding something to flag if columns such as time or geography are in the metadata and give a message that they shouldn't be
+ Checks that check if indicators have a certain unit, they are a specific type? Might not be needed nor possible
+ Extract the indicator groupings and print
+ Add a message for filters where a hint isn't added so that we can say 'you don't have a hint for x, are you sure?
+ Printing out observational unit and filter levels, so addind a 'what is in your data section'
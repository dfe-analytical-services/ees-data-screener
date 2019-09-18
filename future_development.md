# Future developments to the screener

<br>

#### Working task list
On reorganising-tests branch
+ Split up the tests into more sections for the accordions
+ Neaten up the error messages, add in the images for pass/fail/ignore 
  + Also add the count for the tests run and tests passed, and the % - these should go in a summary at the top

+ One combined function for the geography tests - being worked on combining-geography-testing branch - issue #14
+ Add something to fix issue #15
+ Fix the issue with £ symbol - issue #12
+ Program something that automates the check for conceptually different time_identifiers - issue #13

<br>

#### Future ideas
+ Additional tests
  + Something Laura said about making sure each row was 'unique' (presumably observational unit and filter combinations?)
  + Count the number of spaces and commas in a file for those checks and print the number to aid searching
  + Anything on school level data that isn't already covered
+ Adding a seperate section for "style testing"
  + Something that counts and outputs the maximum character lengths per column - more of a style guide to consider later
  + Check for use of proper case in the obv. units and filters?
  + Check all variables are lower case
  + Check the cases for labels and indicator groups used in the metadata
+ Add a summary of your data which prints the variables and levels that are useful
  + Extract the indicator groupings and print
  + Add a message for filters where a hint isn't added so that we can say 'you don't have a hint for x, are you sure?
  + Printing out observational unit and filter levels, so adding a 'what is in your data section'

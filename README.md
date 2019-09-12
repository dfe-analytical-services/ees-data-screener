# **EES-Data-Screener**
This is an R project that you can use to test data and metadata files against the standards required for Explore Education Statistics.

If you have any questions please contact cameron.race@education.gov.uk.

## Files
###setup_file.R
This file contains all of the functions used to screen the data.

###run.R
This is the script with the master functions which load packages, and create the validation functions.
In here you can also load files and run the screener within the console if you need.

###screener-report.Rmd
This file is where you input your data files and then run the other two scripts from.
This then outputs a html report of the results of your file's screening.
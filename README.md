# **EES-Data-Screener**
This is an R project that you can use to test data and metadata files against the standards required for Explore Education Statistics.

If you have any questions please contact cameron.race@education.gov.uk.

## Files
### setup_file.R
This file contains all of the functions used to screen the data.

### EES-data-screener-report.Rmd
This file is where you select your data files and then run the EES Data Screener from.
This then outputs a html report of the results of your screening.

### run.R
This is a script that can used to load packages, read in files, and run the functions in the console.

## Packages
#### Tidyverse
The majority of the data screener is based around the tidyverse packages that are downloadable through cran.

#### Govdown
If you're wanting to download the govdown package to run and edit the html output you'll need to run the following line.

    devtools::install_github("ukgovdatascience/govdown")

If you are struggling with proxy settings, run the following two lines in order, and then restart RStudio and try again.

    source("https://raw.githubusercontent.com/dfe-analytical-services/dfeR/master/R/proxy.R")
    setup_proxy()

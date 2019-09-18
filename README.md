# **EES-Data-Screener**
This is an R project developed that you can use to test data and metadata files against the standards required for statistical publications from the Deparment for Education, and for use on it's new dissemination platform - Explore Education Statistics. 

This will not QA the content of your data, instead it will assess the structure and formatting that is required in the [new data standards](https://teams.microsoft.com/l/channel/19%3A1bdf09280fd94df09f0d42e19cb251fb%40thread.skype/tab%3A%3A638782f8-c3cf-423f-b63c-2e5709c64b9b?groupId=679b2376-8c8c-4062-a1c9-0744ce5ac88f&tenantId=fad277c9-c60a-4da1-b5f3-b3b8b34a82f9). 

The [wiki](https://github.com/lauraselby/data-screener/wiki) contains pages on the working task list as well as providing a place to suggest future developments to this project.

If you have any questions about this project please contact cameron.race@education.gov.uk.

<br>

#### **Project**
**EES-data-screener-report.Rmd** <br>

>This file is where you select your data files and then run the EES Data Screener from. Knitting this then outputs a html report of the results of your screening.

**run.R** <br>

>This is a script that can used to knit the report, or to seperately load packages, read in files, and run the functions in the console.

**data_metadata** <br>

>This is the folder containing test data and metadata files.

**setup_functions** <br>

>This folder contains the files that create all of the functions used to screen the data.

<br>

#### **Packages**
**Tidyverse** <br>

>This data screener is based around the tidyverse packages that are accessible through cran.

**Govdown** <br>

>If you are wanting to download the govdown package to run and edit the html output you'll need to run the following line.
>
>    `devtools::install_github("ukgovdatascience/govdown")`
>
>If you are struggling with proxy settings, run the following two lines in order, and then restart RStudio and try again.
>
>   `source("https://raw.githubusercontent.com/dfe-analytical-services/dfeR/master/R/proxy.R")` <br>
>   `setup_proxy()`

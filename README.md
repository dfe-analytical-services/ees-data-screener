# **EES-Data-Screener**
This is an R project developed to test underlying data and metadata files against the standards required for statistical publications from the Deparment for Education, and for use on it's new dissemination platform - Explore Education Statistics. 

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

#### **Packagesand important things to make this work**
**Packrat** <br>

>The packages in this project are tracked by packrat. More information on packrat can be found [online](https://rstudio.github.io/packrat/limitations.html). This should install the packages you need for this project when you open it up in RStudio.

**Proxy settings**

>If as a DfE analyst you are struggling with proxy settings on your machine, run the following two lines in order, and then restart RStudio and try again.
>
>`source("https://raw.githubusercontent.com/dfe-analytical-services/dfeR/master/R/proxy.R")` <br>
>
>`setup_proxy()`

**Tidyverse** <br>

>This data screener is based around the tidyverse packages that are accessible through cran.

**Govdown** <br>

>More information on govdown can be found [online](https://ukgovdatascience.github.io/govdown/).
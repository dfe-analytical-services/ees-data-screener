# **EES-Data-Screener**
This is an R project developed to test underlying data and metadata files against the standards required for statistical publications from the Deparment for Education, and for use on it's new dissemination platform - [Explore Education Statistics](https://gss.civilservice.gov.uk/blog/how-we-listened-to-our-users-to-improve-our-education-statistics/). 

To use this project and run the data screener yourself you will need to download the zip file to your downloads folder on your computer, unzip the folder and then open the R project file with RStudio. You can then save this folder wherever is most convenient. If you've not used the screener for a while it is worth coming back to here to check for updates and download the latest version.

This will not QA the content of your data, instead it will assess the structure and formatting that is required in the [new data standards](https://teams.microsoft.com/l/channel/19%3A1bdf09280fd94df09f0d42e19cb251fb%40thread.skype/tab%3A%3A638782f8-c3cf-423f-b63c-2e5709c64b9b?groupId=679b2376-8c8c-4062-a1c9-0744ce5ac88f&tenantId=fad277c9-c60a-4da1-b5f3-b3b8b34a82f9). 

The [issues log](https://github.com/lauraselby/data-screener/issues) contains the working task list, and provides a place to raise bugs and suggest future developments to this project.

Note that when you open the project you will need to wait around 20-30 seconds until the following line appears in your console:

>Packrat bootstrap successfully completed. Restarting R and entering packrat mode...

You can then go ahead and use the project.

<br>

If you have any questions about this project please contact cameron.race@education.gov.uk.

<br>

### **Project**
**EES-data-screener.Rproj** <br>

This is the project file, and the file you should open after downloading to open the project in RStudio.

**run.R** <br>

This is the script that that you need to use to select the files, and run the screener.<br>

Once you have run this script, select your report in the file window within the project folder, and click to 'Open in browser' (Chrome works best and is recommended, though it still works in some other browsers).

**data_metadata** <br>

This is the folder containing test data and metadata files.

**setup_functions** <br>

This folder contains the files that create all of the functions used in this project.

**EES-data-screener-report.Rmd** <br>

This is the file that creates the .html report that is the output of the screener.

**packrat**

This folder contains the versions of the packages required for the project to run.

**all_geographies_report_2019-11-05-18_27_57.html**

This is an example .html report run on test data. It was created on the 11th November 2019 at 6:27pm and 57 seconds on the 'all_geographies' file.

<br>

### **Packages and proxy**
**Pandoc** <br>

You will need a version of pandoc that is 2.x.x or greater to run this project and create the .html report. RStudio comes with an older version as standard. As such, there is a function in the run.r file that you can run to check if your version is up to date, and if it is not it will automatically download this for you and start up the install wizard.

**Packrat** <br>

The packages in this project are tracked by packrat. More information on packrat can be found [online](https://rstudio.github.io/packrat/limitations.html). This should install the packages you need to run this project in RStudio automatically, allowing you to focus on running the screener on your data.

**Proxy settings**

If as a DfE analyst you are struggling with proxy settings on your machine, run the following two lines in order, and then restart RStudio and try again.

>`source("https://raw.githubusercontent.com/dfe-analytical-services/dfeR/master/R/proxy.R")` <br>
>
>`setup_proxy()`

**Tidyverse** <br>

The functions in this data screener are based around the [tidyverse packages](https://www.tidyverse.org/).

**Govdown** <br>

More information on govdown, a package used to format the output report in this project, can be found [online](https://ukgovdatascience.github.io/govdown/).

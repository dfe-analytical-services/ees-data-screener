# **EES-Data-Screener**

# **IMPORTANT - THIS PROJECT IS NOW NO LONGER THE ROUTE YOU SHOULD USE TO SCREEN DATA FILES. FOR MORE INFORMATION SEE OUR [ANALYSTS' GUIDE](https://dfe-analytical-services.github.io/analysts-guide/statistics-production/ud.html#how-to-check-against-these-standards) OR CONTACT EXPLORE.STATISTICS@EDUCATION.GOV.UK.**

**How to use this project** <br>

This is an R project that used to be used to test underlying data and metadata files against the standards required for statistical publications from the Deparment for Education, and for use on it's new dissemination platform - [Explore Education Statistics](https://gss.civilservice.gov.uk/blog/how-we-listened-to-our-users-to-improve-our-education-statistics/). 

[How to: Run the Data-screener on your machine](https://www.youtube.com/watch?v=D60UU5r_TrM&feature=youtu.be) is a walkthrough video that takes you through how to use this project on your own machine, starting from the pre-requisites referred to below, right through to the final output report. **Note that the video is slightly out of date, and you should refer to the below for the latest versions of software.**

To use this project and run the data screener yourself you will need to download the zip file to your downloads folder on your computer, unzip the folder and then open the R project file with RStudio. You can then save this folder wherever is most convenient across your machine, ideally on your c: drive. Do not save this in a shared drive or area, or you may run into difficulties during setup. If you've not used the screener for a while it is worth going back to the github repo to check for updates and download the latest version.

This will not QA the content of your data, instead it will assess the structure and formatting that is required in the [new data standards](https://teams.microsoft.com/l/channel/19%3A1bdf09280fd94df09f0d42e19cb251fb%40thread.skype/tab%3A%3A638782f8-c3cf-423f-b63c-2e5709c64b9b?groupId=679b2376-8c8c-4062-a1c9-0744ce5ac88f&tenantId=fad277c9-c60a-4da1-b5f3-b3b8b34a82f9). 

The [issues log](https://github.com/lauraselby/data-screener/issues) contains the working task list, and provides a place to raise bugs and suggest future developments to this project.

<br>

If you have any questions about this project, or would like your files to be screened for you, please contact cameron.race@education.gov.uk.

<br>

### **Project**
**EES-data-screener.Rproj** <br>

This is the project file, and the file you should open after downloading to open the project in RStudio.

**run.R** <br>

This is the script that you need to use to setup your environment, designate the files, and run the screener.<br>

You will be presented with three options, type the number of the option you want to choose in the console to start the screener.<br>

Once you have run the screener, select your report in the file window within the project folder, and click to 'Open in browser' (Chrome works best and is recommended for full functionality, though it still works in some other browsers such as Edge).

**data_metadata** <br>

This is the folder where you need to save data files and their corresponding metadata files to be screened.

**reports** <br>

This is the folder where output reports are saved.

**setup_functions** <br>

This folder contains the files that create all of the functions used in this project.

**EES-data-screener-report.Rmd** <br>

This is the file that creates the .html report that is the output of the screener.

**project_library** <br>

This folder contains the packages required for the project, and is set as your library for the project on opening.

**Updating and this project** <br>

Most of the files in this project should be self-explanatory when looking around. The setup files contain the functions used for each test referred to and called by the .rmd file. 

<br>

Hard-coded variables, such as acceptable time_identifiers, are defined in the environment_setup script. If you're looking at a function and can't work out where a variable is from, also check the function_setup file as this contains a number of variables based on the data being screened that are reused across functions.

<br>

### **Packages and proxy**
**R** <br>

This project assumes a few prerequisites for those on DfE machines:
- You have installed the latest version of R (3.6.2) and RStudio (1.2.5033) from the software centre.
- You have installed the latest version of RTools (35) from the software centre (note that this is a separate installation to the above one).

Once these are installed you should be good to go. It is possible that this will run on older versions of R/RStudio/RTools if you have them already, but this has not been tested.

**Pandoc** <br>

You will need a version of pandoc that is 2.x.x or greater to run this project and create the .html report. RStudio comes with an older version as standard. As such, the screener will automatically check if your version is up to date, and if it is not it will download this for you and start up the install wizard. If this happens, follow the instructions to complete the installation, restart as required and then try again. Whilst you do this, make sure to uncheck the ‘install for all users’ option during the install wizard, this will allow you to install it without the need for an admin password.

**Proxy settings** <br>

If as a DfE analyst you are struggling with proxy settings on your machine, run the following two lines in order, follow any instructions that appear, and then restart RStudio and try again. This will also help if you are having other issues with proxy settings and R generally.

>`source("https://raw.githubusercontent.com/dfe-analytical-services/dfeR/master/R/proxy.R")` <br>
>`setup_proxy()`

**Tidyverse** <br>

Many of functions in this data screener are based around the [Tidyverse packages](https://www.tidyverse.org/).

**Govdown** <br>

More information on govdown, a package used to format the output report in this project, can be found [online](https://ukgovdatascience.github.io/govdown/).

**StyleR** <br>

The code in this project has been styled using the [StyleR package](https://styler.r-lib.org/).
The following will load the package, and then run through and check all relevant code files in the project.

>`library(styler)` <br>
>`styler::style_dir(filetype = c("r","rmd"),recursive = FALSE)` <br>
>`styler::style_dir(path = "setup_functions",filetype = "r",recursive = FALSE)` <br>
>`styler::style_dir(path = "project_library",filetype = "r",recursive = FALSE)`

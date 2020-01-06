# -------------------------------------
### SETTING UP THE ENVIRONMENT
# -------------------------------------

# function to check the pandoc version and install the latest if not 2.7.3 or later
cam_pandoc_install <- function(){
  if(rmarkdown::pandoc_version()>='2.7.3'){print("You already have version 2.7.3 or later of Pandoc installed, you're good to use the screener.")}
  else{if(!require(installr)){install.packages("installr");require(installr)}
  install.pandoc()}
}

# function to install/load the packages needed to run the screener
cam_envrionment_setup <- function(){
  if(!require(govdown)){install.packages("govdown");require(govdown)}
  if(!require(tidyr)){install.packages("tidyr");require(tidyr)}
  if(!require(readr)){install.packages("readr");require(readr)}
  if(!require(stringr)){install.packages("stringr");require(stringr)}
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
  if(!require(knitr)){install.packages("knitr");require(knitr)}
  if(!require(rmarkdown)){install.packages("rmarkdown");require(rmarkdown)}
  print("Your environment has successfully been setup, you should now be able to run the screener.")
}
cam_envrionment_setup()
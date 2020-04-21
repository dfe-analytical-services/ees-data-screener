## Running the Screener
# using ugbly force to make renv read dependencies


library(gitignore)
library(rmarkdown)
library(installr)
library(readr)
library(govdown)
library(knitr)
library(tidyr)
library(stringr)
library(dplyr)
library(svDialogs)
library(janitor)
library(renv)



# Setup your environment by running the following line
source("setup_functions/setup_environment.R", encoding = "UTF-8")

# The following line will load the packages you need into your environment to run the screener
environment_setup()

# Screen your files by running the following line and responding to the prompts.
screening_results()

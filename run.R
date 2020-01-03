## Running the Screener

# Setup your environment by running the following line
source("setup_functions/setup_environment.R")

# The following line will check your version of Pandoc and either give you the go ahead to screen your files or automatically update it for you if needed.
# If it needs updating, this may take a couple of minutes. It will download it for you and automatically start the install wizard.
# Follow the instructions for the install, restart RStudio, and then run this again, it won't redownload pandoc if you have version 2.7.3 or later.
pandoc_install()

# Screen your files by running the following line and responding to the prompts.
screening_results()

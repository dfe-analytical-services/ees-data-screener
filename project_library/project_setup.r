# Project setup

if (!require(here)) {
  install.packages("here")
  library(here)
}

project_library <- paste0(here(), "/project_library/library")

.libPaths(project_library)

# Setup your environment by running the following line
source("setup_functions/setup_environment.R", encoding = "UTF-8")

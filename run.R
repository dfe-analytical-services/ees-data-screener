## Running Script - turn this to a markdown?

# Load packages
library(tidyverse)
library(govdown)

# Load your files
dataset <- read_csv("data_metadata/All_geographies.csv",trim_ws = FALSE)
metadata <- read_csv("data_metadata/filter_group.meta.csv",trim_ws = FALSE)

# Build the screener functions
source("setup_file.R")

# Run the screener tests if needed for testing
screening_tests(dataset,metadata)
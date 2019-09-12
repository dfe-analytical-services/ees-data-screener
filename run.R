## Running Script - turn this to a markdown?

# Load packages
library(tidyverse)

# Load your files
dataset <- read_csv("data_metadata/absence_in_prus.csv")
metadata <- read_csv("data_metadata/absence_in_prus.meta.csv")

# Build the screener functions
source("setup_file.R")

# Run the screener tests
screening_tests(dataset,metadata)
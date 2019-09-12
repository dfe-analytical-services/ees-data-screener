## Running Script - turn this to a markdown?

# Load packages
library(tidyverse)

# Load your files
dataset <- read_csv("data_metadata/dynamic_test_data.csv")
metadata <- read_csv("data_metadata/dynamic_test_data.meta.csv")

# Build the screener functions
source("setup_file.R")

# Run the screener tests if needed for testing
# screening_tests(dataset,metadata)
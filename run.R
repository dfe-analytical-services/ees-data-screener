## Running Script

# Load your files
library(tidyverse)
dataset <- read_csv("data_metadata/dynamic_test_data.csv")
metadata <- read_csv("data_metadata/dynamic_test_data.meta.csv")

# Run the setup script
# something

# Seperate script for the set up of variables to be re run each time new data is loaded?

# Run the screener
screening_tests(dataset,metadata)
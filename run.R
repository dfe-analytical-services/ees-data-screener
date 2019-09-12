## Running Script - turn this to a markdown?

# Load packages
library(tidyverse)
<<<<<<< HEAD

# Load your files
dataset <- read_csv("data_metadata/absence_in_prus.csv")
metadata <- read_csv("data_metadata/absence_in_prus.meta.csv")

=======

# Load your files
dataset <- read_csv("data_metadata/dynamic_test_data.csv")
metadata <- read_csv("data_metadata/dynamic_test_data.meta.csv")

>>>>>>> 4ac7ddc4420c4064844b52bd96fe0086b82813a6
# Build the screener functions
source("setup_file.R")

# Run the screener tests
screening_tests(dataset,metadata)
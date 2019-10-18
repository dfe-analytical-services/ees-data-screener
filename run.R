## Running Script

# Load packages
packrat::restore()

# Run the report - MAKE SURE THAT THE FILE NAME IS CORRECT IN THE .Rmd FILE ITSELF
rmarkdown::render("EES-data-screener-report.Rmd")

# Load your files
#dataset <- read_csv("data_metadata/filter_group.csv",trim_ws = FALSE)
#metadata <- read_csv("data_metadata/filter_group.meta.csv",trim_ws = FALSE)

# Build the screener validation functions
#source("setup_functions/all_setup.R")

# Run the screener tests if needed for testing
#screening_tests(dataset,metadata)
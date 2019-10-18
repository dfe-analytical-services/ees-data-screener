## Running Script

# Ensure the files you want to screen are saved in the data_metadata folder.
# Enter the names of those files below.
your_data_file <- "all_geographies.csv"
your_meta_file <- "all_geographies.meta.csv"

# Enter the name you wish to save the report as and then run the report.
rmarkdown::render("EES-data-screener-report.Rmd")



# IGNORE THE BELOW unless you are running things only in the console and are not creating a report.

# Load your files
#dataset <- read_csv("data_metadata/filter_group.csv",trim_ws = FALSE)
#metadata <- read_csv("data_metadata/filter_group.meta.csv",trim_ws = FALSE)

# Build the screener validation functions
#source("setup_functions/all_setup.R")

# Run the screener tests if needed for testing
#screening_tests(dataset,metadata)
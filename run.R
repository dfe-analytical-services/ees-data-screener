## Running the Screener

# Ensure the data file and corresponding metadata file you want to screen are saved in the data_metadata folder.
# Enter the name of the data file below, note that you do not need to include the extension '.csv'.
your_data_file <- "all_geographies"

# The metadata file will automatically be recognised if you have followed the naming convention - mydatafilename.meta.csv.

# Run the report.
rmarkdown::render("EES-data-screener-report.Rmd",
                  output_file = paste(your_data_file,"_", Sys.Date(), '.html', sep = ''))


# IGNORE THE BELOW unless you are running things only in the console and are not creating a report.

# Load your files
#dataset <- read_csv("data_metadata/filter_group.csv",trim_ws = FALSE)
#metadata <- read_csv("data_metadata/filter_group.meta.csv",trim_ws = FALSE)

# Build the screener validation functions
#source("setup_functions/all_setup.R")

# Run the screener tests if needed for testing
#screening_tests(dataset,metadata)
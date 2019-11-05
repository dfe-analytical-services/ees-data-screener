## Running the Screener

# Ensure the data file and corresponding metadata file you want to screen are saved in the data_metadata folder.
# Enter the name of the data file below, note that you do not need to include the extension '.csv'.
# The metadata file will automatically be recognised if you have followed the naming convention - mydatafilename.meta.csv.
your_data_file <- "all_geographies"

# Run the report.
rmarkdown::render("EES-data-screener-report.Rmd",
                  output_file = paste(gsub(":",".",gsub("\\s","_",paste(your_data_file,"_","report_", Sys.time(),'.html',sep='')))))


# If you are having issues with pandoc, run the following two lines. These will check your version and automatically update it for you if needed.
# This may take a couple of minutes, and will download it for you and automatically start the install wizard.
# Follow the instructions for the install, restart RStudio, and then run these again, it won't download pandoc if you have version 2.7.3 or later.
source('setup_functions/all_setup.r')
cam_pandoc_install()
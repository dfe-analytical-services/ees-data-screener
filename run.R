## Running the Screener

# Setup your environment by running the following line
source("setup_functions/setup_environment.R", encoding = "UTF-8")

# The following line will check your version of Pandoc and either give you the go ahead to screen your files or automatically update it for you if needed.
# If it needs updating, this may take a couple of minutes. It will download it for you and automatically start the install wizard.
# Follow the instructions for the install, restart RStudio, and then run this again, it won't redownload pandoc if you have version 2.7.3 or later.
pandoc_install()

<<<<<<< HEAD
# Screen your files by running the following line and responding to the prompts.
screening_results()
=======
# Ensure the data file and corresponding metadata file you want to screen are saved in the data_metadata folder.
# Enter the name of the data file below, note that you do not need to include the extension '.csv'.
# The metadata file will automatically be recognised if you have followed the naming convention - mydatafilename.meta.csv.
your_data_file <- "all_geographies_cam_EES-770"

# Run the report.
rmarkdown::render("EES-data-screener-report.Rmd",
                  output_file = paste(gsub(":",".",gsub("\\s","_",paste(your_data_file,"_","report_", Sys.time(),'.html',sep='')))))
>>>>>>> master

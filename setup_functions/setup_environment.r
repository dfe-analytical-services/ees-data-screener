# -------------------------------------
### SETTING UP THE ENVIRONMENT
# -------------------------------------
# function to check the pandoc version and install the latest if not 2.7.3 or later
pandoc_install <- function() {
  if (rmarkdown::pandoc_version() >= "2.7.3") {
    message("You already have version 2.7.3 or later of Pandoc installed, you can now run the screener.")
    message("")
  }
  else {
    if (!require(installr)) {
      install.packages("installr")
      require(installr)
    }
    install.pandoc()
  }
}

# -------------------------------------
# function to install/load the packages needed to run the screener
environment_setup <- function() {
  
  renv::restore()
  
  suppressWarnings(suppressMessages(library(rmarkdown)))
  suppressWarnings(suppressMessages(library(govdown)))
  suppressWarnings(suppressMessages(library(knitr)))
  suppressWarnings(suppressMessages(library(tidyr)))
  suppressWarnings(suppressMessages(library(readr)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(svDialogs)))
  
  message("")
  message("Your environment has successfully been setup, you can now run the screener.")
  message("")
}

# -------------------------------------
# quote check function

quote_check <- function(data_quote_test, meta_quote_test) {
  if (sum(stringi::stri_count(c(as.vector(as.matrix(data_quote_test))), fixed = '"')) != 0) {
    stop(
      message(""),
      message(your_data_file, ".csv contains quotes."),
      message(""),
      message("Please remove these and then try again."),
      message("One way to do this is to open the file using Wordpad or Notepad, and delete or find/replace the quote marks."),
      message("")
    )
  }
  
  if (sum(stringi::stri_count(c(as.vector(as.matrix(meta_quote_test))), fixed = '"')) != 0) {
    stop(
      message(""),
      message(your_meta_file, ".meta.csv contains quotes."),
      message(""),
      message("Please remove these and then try again."),
      message("One way to do this is to open the file using Wordpad or Notepad, and delete or find/replace the quote marks."),
      message("")
    )
  }
}


# -------------------------------------
# pre-check function

prechecks <- function(data, meta) {
  for (i in c("geographic_level", "time_period", "time_identifier", "country_code", "country_name")) {
    if (i %in% names(data)) {
      
    } else {
      stop(
        message(""),
        message("The ", i, " column is missing from ", paste(your_data_file, ".csv", sep = ""), "."),
        message("Please review the data standards and amend your data before trying to screen it again."),
        message("")
      )
    }
  }
  
  for (i in meta_cols) {
    if (i %in% names(meta)) {
      
    } else {
      stop(
        message(""),
        message("The ", i, " column is missing from ", paste(your_meta_file, ".meta.csv", sep = ""), "."),
        message("Please review the data standards and amend your data before trying to screen it again."),
        message("")
      )
    }
  }
  if (length(setdiff(names(meta), meta_cols)) != 0) {
    stop(
      message(""),
      message("You have the following invalid columns in ", paste(your_meta_file, ".meta.csv", sep = ""), ":"),
      message(paste(setdiff(names(meta), meta_cols), collapse = "  ")),
      message(""),
      message("Please review the data standards and amend your data before trying to screen it again."),
      message("")
    )
  }
}

# -------------------------------------
# run function

screening_results <- function() {
  user_input <- svDialogs::dlg_list(c(
    "My files are saved in the data_metadata folder and I want to type the name.",
    "I want to select my data and meta data files separately using file explorer.",
    "I want to screen all files in the data_metadata folder."
  ),
  preselect = NULL,
  title = "Select how you would like to use the screener by entering the number next to your desired method below.",
  gui = .GUI
  )$res
  
  if (user_input == "My files are saved in the data_metadata folder and I want to type the name.") {
    assign("your_data_file", dlg_input(message = "Enter the name of your data file:", default = NULL, gui = .GUI)$res, envir = .GlobalEnv)
    assign("your_meta_file", your_data_file, envir = .GlobalEnv)
    
    data_quote_test <- read.table(paste("data_metadata/", your_data_file, ".csv", sep = ""), fill = TRUE)
    meta_quote_test <- read.table(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), fill = TRUE)
    
    quote_check(your_data_file, your_meta_file)
    
    assign("dataset", read_csv(paste("data_metadata/", your_data_file, ".csv", sep = ""), trim_ws = FALSE), envir = .GlobalEnv)
    assign("metadata", read_csv(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), trim_ws = FALSE), envir = .GlobalEnv)
    assign("metadata_utf16", read.csv(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), stringsAsFactors = FALSE, encoding = "UTF-16"), envir = .GlobalEnv)
    
    prechecks(dataset, metadata)
    
    rmarkdown::render("EES-data-screener-report.Rmd",
                      output_file = paste(gsub(":", ".", gsub("\\s", "_", paste(your_data_file, "_", "report_", Sys.time(), ".html", sep = "")))),
                      output_dir = "reports",
                      envir = .GlobalEnv
    )
    
    message("")
    message("Screening results breakdown:")
    message("")
    screening_tests(dataset, metadata, metadata_utf16)
    message("")
    message("Screening results at a glance:")
    message("")
    message(Sys.time(), " screening of ", your_data_file, " files.")
    message("")
    message(total_percent, " of tests passed.")
    message("Number of applicable tests: ", sum(pass, fail))
    message("Tests passed: ", pass)
    message("Tests failed: ", fail)
    message("")
    message("Your report has been saved in the /reports folder.")
    
    if (total_percent == "100%") {
      message("")
      message("Your data file has passed the screening and may be uploaded.")
    } else {
      message("")
      message("Please check the report as your files have not passed the screening.")
    }
  }
  
  if (user_input == "I want to select my data and meta data files separately using file explorer.") {
    assign("dataset_path", dlg_open("default", "Select your data file", multiple = FALSE, gui = .GUI)$res, envir = .GlobalEnv)
    assign("metadata_path", dlg_open("default", "Select your metadata file", multiple = FALSE, gui = .GUI)$res, envir = .GlobalEnv)
    
    assign("your_data_file", str_remove(basename(dataset_path), ".csv"), envir = .GlobalEnv)
    assign("your_meta_file", str_remove(basename(metadata_path), ".meta.csv"), envir = .GlobalEnv)
    
    data_quote_test <- read.table(dataset_path, fill = TRUE)
    meta_quote_test <- read.table(metadata_path, fill = TRUE)
    
    quote_check(data_quote_test, meta_quote_test)
    
    assign("dataset", read_csv(dataset_path, trim_ws = FALSE), envir = .GlobalEnv)
    assign("metadata", read_csv(metadata_path, trim_ws = FALSE), envir = .GlobalEnv)
    assign("metadata_utf16", read.csv(metadata_path, stringsAsFactors = FALSE, encoding = "UTF-16"), envir = .GlobalEnv)
    
    prechecks(dataset, metadata)
    
    rmarkdown::render("EES-data-screener-report.Rmd",
                      output_file = paste(gsub(":", ".", gsub("\\s", "_", paste(your_data_file, "_report_", Sys.time(), ".html", sep = "")))),
                      output_dir = "reports",
                      envir = .GlobalEnv
    )
    
    message("")
    message("Screening results breakdown:")
    message("")
    screening_tests(dataset, metadata, metadata_utf16)
    message("")
    message("Screening results at a glance:")
    message("")
    message(Sys.time(), " screening of")
    message(dataset_path)
    message("and")
    message(metadata_path)
    message("")
    message(total_percent, " of tests passed.")
    message("Number of applicable tests: ", sum(pass, fail))
    message("Tests passed: ", pass)
    message("Tests failed: ", fail)
    message("")
    message("Your report has been saved in the /reports folder.")
    message("")
    
    if (total_percent == "100%") {
      message("Your data file has passed the screening and may be uploaded.")
    } else {
      message("Please check the report as your files have not passed the screening.")
    }
  }
  
  if (user_input == "I want to screen all files in the data_metadata folder.") {

    assign("file_list", list.files(path = "./data_metadata/", pattern = "*.csv", full.names = T), envir = .GlobalEnv)
    
    if ((length(file_list) %% 2) == 1) {
      stop("There is an odd number of .csv files in the data_metadata folder, please check the contents of the folder and try again, or choose a different option.")
    } else {
      file_list1 <- gsub("^.*?/", "", file_list)
      file_list2 <- gsub("^.*?/", "", file_list1)
      file_list3 <- gsub("^(.[^.]*).*$", "\\1", file_list2)
      myfiles <- unique(file_list3)
      
      for (i in myfiles) {
        assign("your_data_file", i, envir = .GlobalEnv)
        assign("your_meta_file", your_data_file, envir = .GlobalEnv)
        
        assign("data_quote_test", read.table(paste("data_metadata/", your_data_file, ".csv", sep = ""), fill = TRUE), envir = .GlobalEnv)
        assign("meta_quote_test", read.table(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), fill = TRUE), envir = .GlobalEnv)
        
        if (sum(stringi::stri_count(c(as.vector(as.matrix(data_quote_test))), fixed = '"')) != 0) {
          stop(
            message(""),
            message(your_data_file, ".csv contains quotes."),
            message(""),
            message("Please remove these and then try again."),
            message("One way to do this is to open the file using Wordpad or Notepad, and delete or find/replace the quote marks."),
            message("")
          )
        }
        
        if (sum(stringi::stri_count(c(as.vector(as.matrix(meta_quote_test))), fixed = '"')) != 0) {
          stop(
            message(""),
            message(your_meta_file, ".meta.csv contains quotes."),
            message(""),
            message("Please remove these and then try again."),
            message("One way to do this is to open the file using Wordpad or Notepad, and delete or find/replace the quote marks."),
            message("")
          )
        }
        
        assign("dataset", read_csv(paste("data_metadata/", your_data_file, ".csv", sep = ""), trim_ws = FALSE), envir = .GlobalEnv)
        assign("metadata", read_csv(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), trim_ws = FALSE), envir = .GlobalEnv)
        assign("metadata_utf16", read.csv(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), stringsAsFactors = FALSE, encoding = "UTF-16"), envir = .GlobalEnv)
        
        prechecks(dataset, metadata)
        
        rmarkdown::render("EES-data-screener-report.Rmd",
                          output_file = paste(gsub(":", ".", gsub("\\s", "_", paste(your_data_file, "_", "report_", Sys.time(), ".html", sep = "")))),
                          output_dir = "reports",
                          envir = .GlobalEnv
        )
        
        message("Screening results at a glance:")
        message("")
        message(Sys.time(), " screening of ", your_data_file, " files.")
        message(total_percent, " of tests passed.")
        message("Number of applicable tests: ", sum(pass, fail))
        message("Tests passed: ", pass)
        message("Tests failed: ", fail)
        message("")
        message("Your report has been saved in the /reports folder.")
        message("")
        
        if (total_percent == "100%") {
          message("Your data file has passed the screening and may be uploaded.")
          message("")
        } else {
          message("Please check the report as your files have not passed the screening.")
          message("")
        }
      }
    }
  }
}

# -------------------------------------
# hard-coded variables

acceptable_time_identifiers <- c(
  "Spring term", "Autumn term", "Autumn and spring term",
  "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "Decemeber",
  "Calendar year", "Calendar year Q1", "Calendar year Q2", "Calendar year Q3", "Calendar year Q4",
  "Financial year", "Financial year Q1", "Financial year Q2", "Financial year Q3", "Financial year Q4",
  "Academic year", "Academic year Q1", "Academic year Q2", "Academic year Q3", "Academic year Q4",
  "Tax year", "Tax year Q1", "Tax year Q2", "Tax year Q3", "Tax year Q4"
)

acceptable_observational_units <- c(
  "country_code", "country_name", "time_period", "time_identifier", "geographic_level",
  "region_code", "region_name", "old_la_code", "new_la_code", "la_name", "rsc_region_lead_name",
  "pcon_code", "pcon_name", "lad_code", "lad_name", "local_enterprise_partnership_code",
  "local_enterprise_partnership_name", "mayoral_combined_authority_code",
  "mayoral_combined_authority_name", "opportunity_area_code", "opportunity_area_name",
  "ward_code", "ward_name", "trust_id", "trust_name", "sponsor_id", "sponsor_name",
  "school_laestab", "school_name", "school_urn", "school_estab", "school_postcode",
  "provider_urn", "provider_name", "provider_ukprn", "provider_upin",
  "institution_id", "institution_name"
)

acceptable_levels <- c(
  "National", "Regional", "Local authority", "RSC region", "Parliamentary constituency",
  "Local authority district", "Local enterprise partnership", "Mayoral combined authority",
  "Opportunity area", "Ward", "MAT", "Sponsor", "School", "Provider", "Institution"
)

all_required <- c("country_code", "country_name")
regional_required <- c("region_code", "region_name")
la_required <- c("region_code", "region_name", "old_la_code", "new_la_code", "la_name")
rsc_required <- c("rsc_region_lead_name")
pcon_required <- c("pcon_code", "pcon_name")
lad_required <- c("lad_code", "lad_name")
lep_required <- c("local_enterprise_partnership_code", "local_enterprise_partnership_name")
mca_required <- c("mayoral_combined_authority_code", "mayoral_combined_authority_name")
oa_required <- c("opportunity_area_code", "opportunity_area_name")
ward_required <- c("ward_code", "ward_name")
MAT_required <- c("trust_id", "trust_name")
sponsor_required <- c("sponsor_id", "sponsor_name")
school_required <- c("school_laestab", "school_name")
provider_required <- c("provider_urn", "provider_name")
institution_required <- c("institution_id", "institution_id")

meta_cols <- c("col_name", "col_type", "label", "indicator_grouping", "indicator_unit", "filter_hint", "filter_grouping_column")

acceptable_indicatorunits <- c("£", "%")

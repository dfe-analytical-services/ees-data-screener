# -------------------------------------
### SETTING UP THE ENVIRONMENT
# -------------------------------------
# function to install/load the packages needed to run the screener
environment_setup <- function() {
  message("")
  renv::restore()
  message("")

  suppressWarnings(suppressMessages(library(gitignore)))
  suppressWarnings(suppressMessages(library(rmarkdown)))
  suppressWarnings(suppressMessages(library(installr)))
  suppressWarnings(suppressMessages(library(readr)))

  if (rmarkdown::pandoc_version() >= "2.7.3") {
    message("You already have version 2.7.3 or later of Pandoc installed.")
    message("")
  }
  else {
    message("Your version of Pandoc is out of date, the latest version will now download, please follow the instructions on the install wizard to continue.")
    message("")
    install.pandoc()
  }

  suppressWarnings(suppressMessages(library(govdown)))
  suppressWarnings(suppressMessages(library(knitr)))
  suppressWarnings(suppressMessages(library(tidyr)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(svDialogs)))
  suppressWarnings(suppressMessages(library(janitor)))


  message("Your environment has successfully been setup, you can now run the screener.")
  message("")
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

    assign("dataset", read_csv(paste("data_metadata/", your_data_file, ".csv", sep = ""), guess_max = 999999, na = c("NA"), trim_ws = FALSE), envir = .GlobalEnv)
    assign("metadata", read_csv(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), guess_max = 999999, trim_ws = FALSE), envir = .GlobalEnv)
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
    if (advisory > 0) {
      message("Number of recommendations: ", advisory, " - please check the tests for recommended changes.")
      message("")
    }
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

    assign("dataset", read_csv(dataset_path, trim_ws = FALSE, guess_max = 999999, na = c("NA")), envir = .GlobalEnv)
    assign("metadata", read_csv(metadata_path, trim_ws = FALSE, guess_max = 999999), envir = .GlobalEnv)
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
    if (advisory > 0) {
      message("Number of recommendations: ", advisory, " - please check the tests for recommended changes.")
      message("")
    }

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

        assign("dataset", read_csv(paste("data_metadata/", your_data_file, ".csv", sep = ""), guess_max = 999999, na = c("NA"), trim_ws = FALSE), envir = .GlobalEnv)
        assign("metadata", read_csv(paste("data_metadata/", your_meta_file, ".meta.csv", sep = ""), guess_max = 999999, trim_ws = FALSE), envir = .GlobalEnv)
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
        if (advisory > 0) {
          message("Number of recommendations: ", advisory, " - please check the tests for recommended changes.")
          message("")
        }
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
# setting pre-defined variables

source("setup_functions/pre-defined_variables.R", encoding = "UTF-8")

# -------------------------------------
### DATA FILE TIME VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_time_setup <- function(data) {
  time_period(data)
  time_period_six(data)
  time_identifier(data)
  time_identifier_mix(data)
}

data_time_results_function <- function() {
  assign("data_time_results", c(
    time_period_result,
    time_period_six_result,
    time_identifier_result,
    time_identifier_mix_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# This checks for a 4 or 6 digit number in the time_period column

time_period <- function(data) {
  time_length <- dataset
  time_length[["digits"]] <- str_count(time_length[["time_period"]])

  if ((nrow(filter(time_length, digits == 4)) + nrow(filter(time_length, digits == 6)) == nrow(time_length)) == FALSE) {
    message("FAIL - time period must be a four or six digit number e.g. 2016 or 201617.")
    message("Here are the time period values in your file: ", paste0(unique(data[["time_period"]]), sep = " "))
    assign("time_period_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - time period is always a four or six digit number.")
    assign("time_period_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# then if 6 digit if it shows consecutive years

time_period_six <- function(data) {
  time_length <- data
  time_length$digits <- str_count(time_length$time_period)

  six_digit_years <- filter(time_length, digits == 6)

  yearends_preresult <- c()

  for (period in unique(six_digit_years$time_period)) {
    currentyearend <- as.numeric(substr(period, 3, 4))
    nextyearend <- as.numeric(substr(period, 5, 6))
    if (currentyearend == 99 && nextyearend == 0) {
      yearends_preresult[paste(period, "check", sep = "_")] <- TRUE
    } else {
      yearends_preresult[paste(period, "check", sep = "_")] <- ((currentyearend + 1) == nextyearend)
    }
  }

  if (nrow(filter(time_length, digits == 6)) == 0) {
    message("IGNORE - You don't have any 6 digit time_period values to test.")
    assign("time_period_six_result", NA, envir = .GlobalEnv)
  } else {
    if (FALSE %in% yearends_preresult) {
      message("FAIL - When the time period is 6 digits, the years must be consecutive such as 201920.")
      assign("time_period_six_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - Your 6 digit time period/s show consecutive years.")
      assign("time_period_six_result", TRUE, envir = .GlobalEnv)
    }
  }
}


# -------------------------------------
# checking the time identifier values are valid

time_identifier <- function(data) {
  time_identifier_preresult <- c()

  for (i in unique(data$time_identifier)) {
    if ((i %in% acceptable_time_identifiers) == FALSE) {
      message("FAIL - ", i, " is not a valid time identifier.")
      time_identifier_preresult[i] <- FALSE
    } else {
      time_identifier_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% time_identifier_preresult) {
    assign("time_identifier_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - Your time_identifier values are valid.")
    assign("time_identifier_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# print the unique time_identifiers for conceptual checking

time_identifier_mix <- function(data) {
  base_identifier <- data$time_identifier[1]

  if ((base_identifier %in% c("Reporting year")) == TRUE) {
    lev <- c("Reporting year")
  }
  if ((base_identifier %in% c("Spring term", "Autumn term", "Autumn and spring term")) == TRUE) {
    lev <- c("Spring term", "Autumn term", "Autumn and spring term")
  }
  if ((base_identifier %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "Decemeber")) == TRUE) {
    lev <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "Decemeber")
  }
  if ((base_identifier %in% c("Calendar year")) == TRUE) {
    lev <- c("Calendar year")
  }
  if ((base_identifier %in% c("Calendar year Q1", "Calendar year Q2", "Calendar year Q3", "Calendar year Q4")) == TRUE) {
    lev <- c("Calendar year Q1", "Calendar year Q2", "Calendar year Q3", "Calendar year Q4")
  }
  if ((base_identifier %in% c("Financial year")) == TRUE) {
    lev <- c("Financial year")
  }
  if ((base_identifier %in% c("Financial year Q1", "Financial year Q2", "Financial year Q3", "Financial year Q4")) == TRUE) {
    lev <- c("Financial year Q1", "Financial year Q2", "Financial year Q3", "Financial year Q4")
  }
  if ((base_identifier %in% c("Academic year")) == TRUE) {
    lev <- c("Academic year")
  }
  if ((base_identifier %in% c("Academic year Q1", "Academic year Q2", "Academic year Q3", "Academic year Q4")) == TRUE) {
    lev <- c("Academic year Q1", "Academic year Q2", "Academic year Q3", "Academic year Q4")
  }
  if ((base_identifier %in% c("Tax year")) == TRUE) {
    lev <- c("Tax year")
  }
  if ((base_identifier %in% c("Tax year Q1", "Tax year Q2", "Tax year Q3", "Tax year Q4")) == TRUE) {
    lev <- c("Tax year Q1", "Tax year Q2", "Tax year Q3", "Tax year Q4")
  }

  if (time_identifier_result == FALSE) {
    message("IGNORE - You have invalid time_identifiers so this test cannot run.")
    assign("time_identifier_mix_result", NA, envir = .GlobalEnv)
  } else {
    if (any(is.na(factor(unique(data$time_identifier), lev))) == TRUE) {
      message("FAIL - data is mixing time identifiers. Allowable values given you've included ", base_identifier, " are: ", paste(lev, sep = " "), ".")
      assign("time_identifier_mix_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - Your time_identifier values are compatible.")
      assign("time_identifier_mix_result", TRUE, envir = .GlobalEnv)
    }
  }
}

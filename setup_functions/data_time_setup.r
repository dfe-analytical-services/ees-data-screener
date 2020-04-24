# -------------------------------------
### DATA FILE TIME VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_time_setup <- function(data) {
  # note that the identifier tests come first as they are needed for the period tests
  time_identifier(data)
  time_identifier_mix(data)
  time_period(data)
  time_period_six(data)
  three_years(data)
}

data_time_results_function <- function() {
  assign("data_time_results", c(
    time_period_result,
    time_period_six_result,
    time_identifier_result,
    time_identifier_mix_result,
    three_years_result
  ),
  envir = .GlobalEnv
  )
}


# -------------------------------------
# IN PROGRESS
# checking that if the time_identifer is X, then the time_period is Y
# can and should be refactored at some point

time_period <- function(data) {
  if (time_identifier_result == FALSE) {
    message("IGNORE - You have invalid time_identifiers so this test cannot run.")
    assign("time_period_result", NA, envir = .GlobalEnv)
  } else {
    if (time_identifier_mix_result == FALSE) {
      message("IGNORE - You have mixed time_identifiers so this test cannot run.")
      assign("time_period_result", NA, envir = .GlobalEnv)
    } else {
      base_identifier <- data$time_identifier[1]
      time_length <- dataset
      time_length[["digits"]] <- str_count(time_length[["time_period"]])
      time_period_preresult <- c()

      if ((base_identifier %in% c("Reporting year")) == TRUE) {
        if ((nrow(filter(time_length, digits == 4)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for Reporting year must be a four digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

      if ((base_identifier %in% c("Spring term", "Autumn term", "Summer term")) == TRUE) {
        if ((nrow(filter(time_length, digits == 4)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for terms must be a four digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }
      
      if ((base_identifier %in% c("Autumn and spring term")) == TRUE) {
        if ((nrow(filter(time_length, digits == 6)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for combined terms must be a six digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

      if ((base_identifier %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) == TRUE) {
        if ((nrow(filter(time_length, digits == 4)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for months must be a four digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

      if ((base_identifier %in% c("Calendar year")) == TRUE) {
        if ((nrow(filter(time_length, digits == 4)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for Calendar year must be a four digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

     if ((base_identifier %in% c("Financial year")) == TRUE) {
        if ((nrow(filter(time_length, digits == 6)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for Financial year must be a six digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

     if ((base_identifier %in% c("Academic year")) == TRUE) {
        if ((nrow(filter(time_length, digits == 6)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for Academic year must be a six digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

      if ((base_identifier %in% c("Tax year")) == TRUE) {
        if ((nrow(filter(time_length, digits == 6)) == nrow(time_length)) == FALSE) {
          message("FAIL - the time period for Tax year must be a six digit number.")
          message("Guidance - https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values.")
          time_period_preresult <- FALSE
        } else {
          message("PASS - time period is the correct number of digits for the time_identifer.")
          time_period_preresult <- TRUE
        }
      }

      if (time_period_preresult == FALSE) {
        assign("time_period_result", FALSE, envir = .GlobalEnv)
      } else {
        message("PASS - Your time_identifier values are compatible.")
        assign("time_period_result", TRUE, envir = .GlobalEnv)
      }
    }
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
  if ((base_identifier %in% c("Spring term", "Autumn term", "Summer term")) == TRUE) {
    lev <- c("Spring term", "Autumn term", "Summer term")
  }
  if ((base_identifier %in% c("Autumn and spring term")) == TRUE) {
    lev <- c("Autumn and spring term")
  }
  if ((base_identifier %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) == TRUE) {
    lev <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  }
  if ((base_identifier %in% c("Calendar year")) == TRUE) {
    lev <- c("Calendar year")
  }
  if ((base_identifier %in% c("Financial year")) == TRUE) {
    lev <- c("Financial year")
  }
  if ((base_identifier %in% c("Academic year")) == TRUE) {
    lev <- c("Academic year")
  }
  if ((base_identifier %in% c("Tax year")) == TRUE) {
    lev <- c("Tax year")
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

# -------------------------------------
# produce a warning if there are fewer than 3 years of data in the file

three_years <- function(data) {
  if (length(unique(data$time_period)) < 3) {
    message("ADVISORY - Your file contains fewer than 3 years.")
    message("Where it exists, you should include at least 3 years of data in your file.")
    message("This is to meet the upcoming change in accessibility legislation.")
    assign("three_years_result", "Advisory", envir = .GlobalEnv)
  } else {
    message("PASS - Your file contains at 3 years or more years of data.")
    assign("three_years_result", TRUE, envir = .GlobalEnv)
  }
}

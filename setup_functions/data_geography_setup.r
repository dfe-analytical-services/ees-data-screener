# -------------------------------------
### DATA FILE GEOGRAPHY VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_geography_setup <- function(data) {
  level_validity(data)
  geography_level_present(data)
  geography_level_completed(data)
  region_col_completed(data)
  new_la_code(data)
  incorrect_level_national(data)
  incorrect_level_regional(data)
}

data_geography_results_function <- function() {
  assign("data_geography_results", c(
    level_validity_result,
    geography_level_present_result,
    geography_level_completed_result,
    region_col_completed_result,
    new_la_code_result,
    incorrect_level_national_result,
    incorrect_level_regional_result
  ),
  envir = .GlobalEnv
  )
}

# -------------------------------------
# Do we have acceptable values for the geographic level

level_validity <- function(data) {
  level_validity_preresult <- c()

  for (i in unique(data[["geographic_level"]])) {
    if ((i %in% acceptable_levels) == FALSE) {
      message("FAIL - ", i, " is not a valid geographic level.")
      level_validity_preresult[i] <- FALSE
    } else {
      level_validity_preresult[i] <- TRUE
    }
  }
  if (FALSE %in% level_validity_preresult) {
    assign("level_validity_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - Your geographic_level values are valid.")
    assign("level_validity_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# Do we have the right columns for the geographic level

geography_level_present <- function(data) {
  geography_level_present_preresult <- c()

  for (i in all_required) {
    if ((i %in% names(data)) == FALSE) {
      message("FAIL - ", i, " must be present for all levels of data.")
      geography_level_present_preresult[i] <- FALSE
    } else {
      geography_level_present_preresult[i] <- TRUE
    }
  }

  if ("Regional" %in% data[["geographic_level"]]) {
    for (i in regional_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for regional level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Local authority" %in% data[["geographic_level"]]) {
    for (i in la_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for local authority level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("RSC region" %in% data[["geographic_level"]]) {
    for (i in rsc_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for RSC region level data.")
        geography_levels_preresult[i] <- FALSE
      } else {
        geography_levels_preresult[i] <- TRUE
      }
    }
  }

  if ("Parliamentary constituency" %in% data[["geographic_level"]]) {
    for (i in pcon_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for parliamentary constituency level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Local authority district" %in% data[["geographic_level"]]) {
    for (i in lad_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for local authority district level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Local enterprise partnership" %in% data[["geographic_level"]]) {
    for (i in lep_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for local enterprise partnership level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Mayoral combined authority" %in% data[["geographic_level"]]) {
    for (i in mca_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for mayoral combined authority level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Opportunity area" %in% data[["geographic_level"]]) {
    for (i in oa_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for opportunity level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Ward" %in% data[["geographic_level"]]) {
    for (i in ward_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for ward level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("MAT" %in% data[["geographic_level"]]) {
    for (i in MAT_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for MAT level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if ("Sponsor" %in% data[["geographic_level"]]) {
    for (i in sponsor_required) {
      if ((i %in% names(data)) == FALSE) {
        message("FAIL - ", i, " must be present for sponsor level data.")
        geography_level_present_preresult[i] <- FALSE
      } else {
        geography_level_present_preresult[i] <- TRUE
      }
    }
  }

  if (FALSE %in% geography_level_present_preresult) {
    assign("geography_level_present_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - Your geographic columns are valid.")
    assign("geography_level_present_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# Are the geography levels completed as expected

geography_level_completed <- function(data) {
  geography_level_completed_preresult <- c()

  if (any(is.na(data$country_name))) {
    message("FAIL - The country_name column must be completed for all data.")
    geography_level_completed_preresult[["country_name"]] <- FALSE
  }

  if (any(is.na(data$country_code))) {
    message("FAIL - The country_code column must be completed for all data.")
    geography_level_completed_preresult[["country_code"]] <- FALSE
  }

  if ("Regional" %in% data[["geographic_level"]]) {
    Regional <- filter(data, geographic_level == "Regional")
    if (any(is.na(Regional$region_name))) {
      message("FAIL - The region_name column must be completed for all regional data.")
      geography_level_completed_preresult[["region_name"]] <- FALSE
    }
    if (any(is.na(Regional$region_code))) {
      message("FAIL - The region_code column must be completed for all regional data.")
      geography_level_completed_preresult[["region_code"]] <- FALSE
    }
  }

  if ("Local authority" %in% data[["geographic_level"]]) {
    LA <- filter(data, geographic_level == "Local authority")
    if (any(is.na(LA$old_la_code))) {
      message("FAIL - The old_la_code column must be completed for all local authority data.")
      geography_level_completed_preresult[["old_la_code"]] <- FALSE
    }

    if (any(is.na(LA$la_name))) {
      message("FAIL - The la_name column must be completed for all local authority data.")
      geography_level_completed_preresult[["la_name"]] <- FALSE
    }
  }

  if ("RSC region" %in% data[["geographic_level"]]) {
    RSC <- filter(data, geographic_level == "RSC region")
    if (any(is.na(RSC$rsc_region_lead_name))) {
      message("FAIL - The rsc_region_lead_name column must be completed for all RSC region data.")
      geography_level_completed_preresult[["rsc_region_lead_name"]] <- FALSE
    }
  }

  if ("Parliamentary constituency" %in% data[["geographic_level"]]) {
    pcon <- filter(data, geographic_level == "Parliamentary constituency")
    if (any(is.na(pcon$pcon_name))) {
      message("FAIL - The pcon_name column must be completed for all parliamentary constituency data.")
      geography_level_completed_preresult[pcon_name] <- FALSE
    }
    if (any(is.na(pcon$pcon_code))) {
      message("FAIL - The pcon_code column must be completed for all parliamentary constituency data.")
      geography_level_completed_preresult[pcon_code] <- FALSE
    }
  }

  if ("Local authority district" %in% data[["geographic_level"]]) {
    lad <- filter(data, geographic_level == "Local authority district")
    if (any(is.na(lad$lad_name))) {
      message("FAIL - The lad_name column must be completed for all local authority district data.")
      geography_level_completed_preresult[lad_name] <- FALSE
    }
    if (any(is.na(lad$lad_code))) {
      message("FAIL - The lad_code column must be completed for all local authority district data.")
      geography_level_completed_preresult[lad_code] <- FALSE
    }
  }

  if ("Local enterprise partnership" %in% data[["geographic_level"]]) {
    lep <- filter(data, geographic_level == "Local enterprise partnership")
    if (any(is.na(lep$local_enterprise_partnership_name))) {
      message("FAIL - The local_enterprise_partnership_name column must be completed for all local enterprise partnership data.")
      geography_level_completed_preresult[local_enterprise_partnership_name] <- FALSE
    }
    if (any(is.na(lep$local_enterprise_partnership_code))) {
      message("FAIL - The local_enterprise_partnership_code column must be completed for all local enterprise partnership data.")
      geography_level_completed_preresult[local_enterprise_partnership_code] <- FALSE
    }
  }

  if ("Mayoral combined authority" %in% data[["geographic_level"]]) {
    mca <- filter(data, geographic_level == "Mayoral combined authority")
    if (any(is.na(mca$mayoral_combined_authority_name))) {
      message("FAIL - The mayoral_combined_authority_name column must be completed for all mayoral combined authority data.")
      geography_level_completed_preresult[mayoral_combined_authority_name] <- FALSE
    }
    if (any(is.na(mca$mayoral_combined_authority_code))) {
      message("FAIL - The mayoral_combined_authority_code column must be completed for all mayoral combined authority data.")
      geography_level_completed_preresult[mayoral_combined_authority_code] <- FALSE
    }
  }

  if ("Opportunity area" %in% data[["geographic_level"]]) {
    oa <- filter(data, geographic_level == "Opportunity area")
    if (any(is.na(oa$opportunity_area_name))) {
      message("FAIL - The opportunity_area_name column must be completed for all opportunity area data.")
      geography_level_completed_preresult[opportunity_area_name] <- FALSE
    }
    if (any(is.na(oa$opportunity_area_code))) {
      message("FAIL - The opportunity_area_code column must be completed for all opportunity area data.")
      geography_level_completed_preresult[opportunity_area_code] <- FALSE
    }
  }

  if ("Ward" %in% data[["geographic_level"]]) {
    ward <- filter(data, geographic_level == "Ward")
    if (any(is.na(ward$ward_name))) {
      message("FAIL - The ward_name column must be completed for all ward data.")
      geography_level_completed_preresult[ward_name] <- FALSE
    }
    if (any(is.na(ward$ward_code))) {
      message("FAIL - The ward_code column must be completed for all ward data.")
      geography_level_completed_preresult[ward_code] <- FALSE
    }
  }

  if ("MAT" %in% data[["geographic_level"]]) {
    mat <- filter(data, geographic_level == "MAT")
    if (any(is.na(mat$trust_name))) {
      message("FAIL - The trust_name column must be completed for all MAT data.")
      geography_level_completed_preresult[trust_name] <- FALSE
    }
    if (any(is.na(mat$trust_id))) {
      message("FAIL - The trust_id column must be completed for all MAT data.")
      geography_level_completed_preresult[trust_id] <- FALSE
    }
  }

  if ("Sponsor" %in% data[["geographic_level"]]) {
    sponsor <- filter(data, geographic_level == "Sponsor")
    if (any(is.na(sponsor$sponsor_name))) {
      message("FAIL - The sponsor_name column must be completed for all sponsor data.")
      geography_level_completed_preresult[sponsor_name] <- FALSE
    }
    if (any(is.na(sponsor$sponsor_id))) {
      message("FAIL - The sponsor_id column must be completed for all sponsor data.")
      geography_level_completed_preresult[sponsor_id] <- FALSE
    }
  }

  if (FALSE %in% geography_level_completed_preresult) {
    assign("geography_level_completed_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - Your geographic columns are completed as expected.")
    assign("geography_level_completed_result", TRUE, envir = .GlobalEnv)
  }
}

# -------------------------------------
# When one of region name and code is completed, is the other also?

region_col_completed <- function(data) {
  
  # THIS ISN'T APPLYING WHAT I INTENDED CORRECTLY
  
  # NA in the character columns are now "". Need to handle them
  
  
  region_col_completed_preresult <- c()
  
  region_both_complete_check <- function(data) {
    if (("region_code" %in% names(data)) && ("region_name" %in% names(data))) {
      if (is.na(data[["region_code"]]) && !is.na(data[["region_name"]])) {
        message("FAIL - You must include the region_code when there is a region_name.")
        region_col_completed_preresult[["rcode_missing"]] <<- FALSE
      }
      if (is.na(data[["region_name"]]) && !is.na(data[["region_code"]])) {
        message("FAIL - You must include the region_name when there is a region_code.")
        region_col_completed_preresult[["rname_missing"]] <<- FALSE
      }
    }
  }
  
  apply(data, 1, region_both_complete_check)
  
  if (FALSE %in% region_col_completed_preresult) {
    assign("region_col_completed_result", FALSE, envir = .GlobalEnv)
  } else {
    message("PASS - Your regional columns relate to each other as expected.")
    assign("region_col_completed_result", TRUE, envir = .GlobalEnv)
  }
  
}

# -------------------------------------
# Is the new LA code always either 9 digits or blank?

new_la_code <- function(data) {
  if ("new_la_code" %in% names(data)) {
    new_la_length <- data
    new_la_length$code_length <- str_count(new_la_length$new_la_code)

    if (((nrow(filter(new_la_length, code_length == 9)) + nrow(filter(new_la_length, is.na(code_length) ))) == nrow(new_la_length))) {
      message("FAIL - new_la_code must be either a 9 digit code or blank.")
      assign("new_la_code_result", FALSE, envir = .GlobalEnv)
    } else {
      message("PASS - new_la_code is always a 9 digit code or blank.")
      assign("new_la_code_result", TRUE, envir = .GlobalEnv)
    }
  } else {
    message("IGNORE - You don't have a new_la_code column to validate.")
    assign("new_la_code_result", NA, envir = .GlobalEnv)
  }
}

# -------------------------------------
# Are any la or regional rows completed for national columns?

incorrect_level_national <- function(data) {
    national_rows <- filter(data, geographic_level == "National") 
    incorrect_level_national_preresult <- c()

    # NA in the character columns are now "". Need to handle them
    
    if(nrow(national_rows) == 0){
      incorrect_level_national_preresult <- NA
    } else {
      for(i in c(regional_required,la_required)) {
        if(i %in% names(national_rows)) {
          if (any(!is.na(national_rows[i]))) {
            incorrect_level_national_preresult[i] <- FALSE
          } else {
            incorrect_level_national_preresult[i] <- TRUE
          }
        }
      }
    }
    if (NA %in% incorrect_level_national_preresult) {
      message("IGNORE - There are no national level rows to test.")
      assign("incorrect_level_national_result", NA, envir = .GlobalEnv)
    } else {
      if (FALSE %in% incorrect_level_national_preresult) {
        message("FAIL - You have regional or LA data in your national rows, please double check your file.")
        assign("incorrect_level_national_result", FALSE, envir = .GlobalEnv)
      } else {
          message("PASS - Your national rows do not have LA or regional data in them.")
          assign("incorrect_level_national_result", TRUE, envir = .GlobalEnv)
        }
      }
}


# -------------------------------------
# Are any la rows completed for regional columns?

# NA in the character columns are now "". Need to handle them

incorrect_level_regional <- function(data) {
  regional_rows <- filter(data, geographic_level == "Regional") 
  incorrect_level_regional_preresult <- c()
  
  if(nrow(regional_rows) == 0) {
    incorrect_level_regional_preresult <- NA
  } else {
    for(i in la_required) {
      if(i %in% names(regional_rows)) {
        if(any(!is.na(regional_rows[i]))) {
          incorrect_level_regional_preresult[i] <- FALSE
        } else {
          incorrect_level_regional_preresult[i] <- TRUE
        }
      }
    }
  }
  
  
  if (NA %in% incorrect_level_regional_preresult) {
    message("IGNORE - There are no regional level rows to test.")
    assign("incorrect_level_regional_result", NA, envir = .GlobalEnv)
  } else {
      if (FALSE %in% incorrect_level_regional_preresult) {
        message("FAIL - You have LA data in your regional rows, please double check your file.")
        assign("incorrect_level_regional_result", FALSE, envir = .GlobalEnv)
      } else {
        message("PASS - Your regional rows do not have LA data in them..")
        assign("incorrect_level_regional_result", TRUE, envir = .GlobalEnv)
      }
  }
}


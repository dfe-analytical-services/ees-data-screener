# -------------------------------------
### META FILE VARIABLE AND LABEL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_variable_label_setup <- function(meta){
  col_name_completed(meta)
  meta_duplicate_check(meta)
  col_name_spaces_check(meta)
  comp_col_check_meta(meta)
  col_type_check(meta)
  label_check(meta)
  duplicate_label_check(meta)
}

# -------------------------------------
# is col_name completed for every row

col_name_completed <- function(meta) {
  if(any(is.na(meta$col_name))){cat('FAIL - There is a col name missing in ', sum(is.na(meta$col_name)), 'row/s.')}
    else{message('PASS - col_name is completed for all rows.')}
}

# -------------------------------------
# checking for duplicates in col_name

meta_duplicate_check <- function(meta) {
  if(any(meta$col_name %in% meta$col_name[duplicated(meta$col_name)])){message('FAIL - At least one of the variable names is duplicated.')}
    else{message('PASS - All col names are unique.')}
}

# -------------------------------------
# check that no value in col_name has any spaces

col_name_spaces_check <- function(meta) {
  if(any(grepl('\\s',meta$col_name))){message("FAIL - There are spaces in the col name values.")}
    else{message('PASS - There are no spaces in the col name values.')}
}

# -------------------------------------
# check if any observational units are in the metadata

comp_col_check_meta <- function(meta) {
  observational_units <- c("geographic_level","time_period","time_identifier","country_code","country_name",
                           "region_code","region_name","old_la_code","new_la_code","la_name","rsc_name",
                           "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                           "local_enterprise_partnership_name","mayoral_combined_authority_code",
                           "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                           "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name",
                           "school_laestab","school_name","school_urn","school_estab","school_postcode",
                           "provider_urn","provider_name","provider_ukprn","provider_upin",
                           "institution_id","institution_name")
  message("This will show if there are any observational units erroneously in your metadata:")
  for (i in observational_units) {
    try(cat(if(i %in% meta$col_name) warning("FAIL - ", i, " should not be in the metadata. ")))
  }
}

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type_check <- function(meta) {
  f <- nrow(filter(meta,col_type == "Filter"))
  i <- nrow(filter(meta,col_type == "Indicator"))
  m <- nrow(meta)
  col_types <- unique(meta$col_type)
  if((f + i == m)==FALSE){writeLines(c("FAIL - col_type must be either 'Filter' or 'Indicator', and cannot be blank.",
                 "Here are the col type values in your file:","",col_types,""))}
    else{message("PASS - col_type is always 'Filter' or 'Indicator'.")}
}

# -------------------------------------
# is label completed for every row

label_check <- function(meta) {
  if(any(is.na(meta$label))){message('FAIL - There is a label missing in', sum(is.na(meta$label)), 'row/s.')}
    else{message('PASS - label is completed for all rows.')}
}

# -------------------------------------
# checking for duplicate labels

duplicate_label_check <- function(meta) {
  if(any(meta$label %in% meta$label[duplicated(meta$label)])){message('FAIL - At least one of the variable names is duplicated.')}
    else{message('PASS - All labels are unique.')}
}
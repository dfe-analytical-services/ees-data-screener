# -------------------------------------
### ,ETA FILE GENERAL VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

meta_general_setup <- function(data,meta){
        meta_comp_col(meta)
        column_crosscheck(data,meta)
        meta_crosscheck(data,meta)
        meta_comma(meta)
}

meta_general_results_function <- function(){
  assign("meta_general_results",c(meta_comp_col_result,
                                  column_crosscheck_result,
                                  meta_crosscheck_result,
                                  meta_comma_result)
         ,envir = .GlobalEnv)
}

# -------------------------------------
# Check all compulsory columns exist

meta_comp_col <- function(meta) {
  
  meta_cols <- c("col_name","col_type","label","indicator_grouping","indicator_unit","filter_hint","filter_grouping_column")
  meta_comp_col_preresult
  
  for(i in meta_cols){
    if(!i %in% names(meta)){
      message("FAIL - The ",i," variable is missing.")
      meta_comp_col_preresult[i] <- FALSE
    }else{
      meta_comp_col_preresult[i] <- TRUE
    }
  }
  if(any(in))
  if(FALSE %in% meta_comp_col_preresult){
    assign("meta_comp_col_result",FALSE,envir = .GlobalEnv)
  }else{
    message("PASS - The metadata columns are present.")
    assign("meta_comp_col_result",TRUE,envir = .GlobalEnv)
  }
}
  
  

# -------------------------------------
# For each col_name in the metadata check these each appear in the data file
column_crosscheck <- function(data,meta) {
  m_variables <- c(meta$col_name)
  message("This will show rows in the metadata that do not have a matching variable in the data file:")
  for(i in m_variables){
    if((i %in% names(data))==FALSE)
      warning("
              FAIL - ", i," is not a variable in the data file.")
  }
}

# -------------------------------------
# List those in the data file that aren't in the metadata (or observational units, or variables with only one level)

meta_crosscheck <- function(data,meta) {
  
  observational_units <- c("geographic_level","time_period","time_identifier","country_code","country_name",
                           "region_code","region_name","old_la_code","new_la_code","la_name","rsc_region_lead_name",
                           "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                           "local_enterprise_partnership_name","mayoral_combined_authority_code",
                           "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                           "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name",
                           "school_laestab","school_name","school_urn","school_estab","school_postcode",
                           "provider_urn","provider_name","provider_ukprn","provider_upin",
                           "institution_id","institution_name")
  
  meta_variables <- setdiff(c(meta$col_name,meta$filter_grouping_column),observational_units)
  data_variables <- setdiff(names(data),observational_units)
  possible_variables <- setdiff(data_variables,meta_variables)
  missing_and_meta_variables <- meta_variables[!is.na(meta_variables)]
  
  for(i in possible_variables){
    if((length(unique(data[[i]])) > 1)&&(i %in% names(data))){
        missing_and_meta_variables <- c(missing_and_meta_variables,i)
    }
  }
  
  message("This will show variables in the data file that are not present in the metadata:")
    for (i in unique(missing_and_meta_variables)) {
    try(cat(if((i %in% meta_variables)==FALSE) warning(i, " is not in the metadata or a recognised observational unit.
")))
 
 }
}

# -------------------------------------
# flag for commas across each column

meta_comma_check <- function(meta) {
  message('This will show if there are commas in your metadata file:')
  for (i in names(meta)) {
    if(any(grepl(",",meta[[i]]))) warning("FAIL - There are commas in ", i)
  }
}
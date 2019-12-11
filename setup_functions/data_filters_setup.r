# -------------------------------------
### DATA FILE FILTER VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file
data_filters_setup <-function(data,meta){
  filter_levels_check(data,meta)
  total_check(data,meta)
  observational_total_check(data)
}

data_filters_results_function <- function(){
  data_filters_results <- c(filter_levels_check_result, total_check_result, observational_total_check_result)
}

# -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one

filter_levels_check <- function(data,meta) {
  mfilters <- filter(meta,col_type=="Filter")
  filternames <- c(mfilters$col_name)
  dfilters <- select(data,filternames)
  if(ncol(dfilters)==0){
    message("IGNORE - There are no filters in your data to test.")
    assign("filter_levels_check_result",c(NA),envir = .GlobalEnv)
    }else{
    filter_levels_check_preresult <- c()
  for (i in names(dfilters)) {
    if((length(unique(data[[i]])))<2){
    message("FAIL - There are fewer than two levels in ", i,".")
    filter_levels_check_preresult[i] <- FALSE
    }else{
    filter_levels_check_preresult[i] <- TRUE
    }
  }
      if(FALSE %in% filter_levels_check_preresult){
        assign("filter_levels_check_result",FALSE,envir = .GlobalEnv)
      }else{
        message("PASS - No filters have fewer than two levels.")
        assign("filter_levels_check_result",TRUE,envir = .GlobalEnv)
      }
    }
}

# -------------------------------------
# Check for Total in all filters

total_check <- function(data,meta) {
  if(!"Filter" %in% meta$col_type){message("IGNORE - This test does not apply to your data.")}
  else{
    mfilters <- filter(meta,col_type=="Filter")
    filter_names <- c(mfilters$col_name)
    dfilters <- select(data,filter_names)
    for(i in names(dfilters)) {
      if(!"Total" %in% dfilters[[i]]){warning("
    WARNING - There is no total value in ", i,".")
  }else(message("Pass - every filter has a total level."))
  }} 
}

# -------------------------------------
# Check if Total has been used errorneously in any observational units

observational_total_check <- function(data){
  observational_units <- c("country_code","country_name",
                           "region_code","region_name","old_la_code","new_la_code","la_name","rsc_region_lead_name",
                           "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                           "local_enterprise_partnership_name","mayoral_combined_authority_code",
                           "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                           "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name",
                           "school_laestab","school_name","school_urn","school_estab","school_postcode",
                           "provider_urn","provider_name","provider_ukprn","provider_upin",
                           "institution_id","institution_name")
  present_ob_units <- intersect(observational_units,names(data))
  for(i in present_ob_units) {
    if("Total" %in% data[[i]] || "total" %in% data[[i]]){message("FAIL - A total value is present in ",i,", this should be replaced with a blank.")
  }else{message("PASS - there are no total values in ",i,".")}
}}
# -------------------------------------
### DATA FILE FILTER VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file
data_filters_setup <-function(data,meta){
  filter_levels_check(data,meta)
  total_check(data,meta)
  observational_total_check(data)
}

# -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one

filter_levels_check <- function(data,meta) {
  message("This will show if there are any filters with fewer than 2 levels:")
  mfilters <- filter(meta,col_type=="Filter")
  filternames <- c(mfilters$col_name)
  dfilters <- select(data,filternames)
  for (i in names(dfilters)) {
    if((length(unique(data[[i]])))<2) warning("
  WARNING - There are fewer than two levels in ", i,".")
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
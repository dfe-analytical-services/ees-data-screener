# -------------------------------------
### DATA FILE GEOGRAPHY VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file

data_geography_setup <- function(data){
  level_validity_check(data)
  geography_levels_present(data)
  geography_level_completed(data)
}

# -------------------------------------
# Do we have acceptable values for the geographic level

level_validity_check <- function(data) {
  acceptable_levels <- c("National","Regional","Local authority","RSC region","Parliamentary constituency",
                         "Local authority district","Local enterprise partnerships","Mayoral combined authorities",
                         "Opportunity area","Ward","MAT","Sponsor","School","Provider","Institution")
  levels <- unique(data$geographic_level)
  # Original function that gave a firm PASS rather than 'If there are no warnings'
  #levels_test <- intersect(levels,acceptable_levels)
  #try(cat(if(FALSE == identical(levels_test,levels)) 
    #stop(writeLines(c("FAIL - There is at least one invalid geographic_level present.",
       #"Here are the geographic levels in your file:","",levels,""))),
  message("This will show if any of your geographic levels are invalid:")
    for(i in levels){
      if((i %in% acceptable_levels)==FALSE)
        warning("
             FAIL - ", i, " is not a valid geographic level.")
    }
}

# -------------------------------------
# Do we have the right columns for the geographic level

geography_levels_present <- function(data){
  message("This will show if there are any expected geography columns missing from the data file:")
 regional_required <- c("country_code","country_name","region_code","region_name")
 if("Regional" %in% data$geographic_level)
 {for(i in regional_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for regional level data.")
 }
 }
 la_required <- c("country_code","country_name","region_code","region_name","old_la_code","new_la_code","la_name")
 if("Local authority" %in% data$geographic_level)
 {for(i in la_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for local authority level data.")
 }
 }
 rsc_required <- c("country_code","country_name","rsc_region_lead_name")
 if("RSC region" %in% data$geographic_level)
 {for(i in rsc_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for RSC region level data.")
 }
 }
 pcon_required <- c("country_code","country_name","pcon_code","pcon_name")
 if("Parliamentary constituency" %in% data$geographic_level)
 {for(i in pcon_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for parliamentary constituency level data.")
 }
 }
 lad_required <- c("country_code","country_name","lad_code","lad_name")
 if("Local authority district" %in% data$geographic_level)
 {for(i in lad_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for local authority district level data.")
 }
 }
 lep_required <- c("country_code","country_name","local_enterprise_partnership_code","local_enterprise_partnership_name")
 if("Local enterprise partnership" %in% data$geographic_level)
 {for(i in lep_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for local enterprise partnership level data.")
 }
 }
 mca_required <- c("country_code","country_name","mayoral_combined_authority_code","mayoral_combined_authority_name")
 if("Mayoral combined authority" %in% data$geographic_level)
 {for(i in mca_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for mayoral combined authority level data.")
 }
 }
 oa_required <- c("country_code","country_name","opportunity_area_code","opportunity_area_name")
 if("Opportunity area" %in% data$geographic_level)
 {for(i in oa_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for opportunity level data.")
 }
 }
 ward_required <- c("country_code","country_name","ward_code","ward_name")
 if("Ward" %in% data$geographic_level)
 {for(i in ward_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for ward level data.")
 }
 }
 MAT_required <- c("country_code","country_name","trust_id","trust_name")
 if("MAT" %in% data$geographic_level)
 {for(i in MAT_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for MAT level data.")
 }
 }
 sponsor_required <- c("country_code","country_name","sponsor_id","sponsor_name")
 if("Sponsor" %in% data$geographic_level)
 {for(i in sponsor_required){
   if((i %in% names(data))==FALSE)
     message("FAIL - ", i, " must be present for sponsor level data.")
 }
 }
}

# -------------------------------------
# Are the geography columns completed for the right levels

national_level_completed <- function(data) {
  if(any(is.na(data$country_name))) {message('FAIL - The country_name column must be completed for all data.')}
  if(any(is.na(data$country_code))) {message('FAIL - The country_code column must be completed for all data.')}
}
regional_level_completed <- function(data) {
  Regional <- filter(data, geographic_level =='Regional')
  if(any(is.na(Regional$region_name))) {message('FAIL - The region_name column must be completed for all regional data.')}
  if(any(is.na(Regional$region_code))) {message('FAIL - The region_code column must be completed for all regional data.')}
}
la_level_completed <- function(data) {
  LA <- filter(data, geographic_level =='Local authority')
  if(any(is.na(LA$old_la_code))) {message('FAIL - The old_la_code column must be completed for all local authority data.')}
  if(any(is.na(LA$new_la_code))) {message('FAIL - The new_la_code column must be completed for all local authority data.')}  
  if(any(is.na(LA$la_name))) {message('FAIL - The la_name column must be completed for all local authority data.')}
}
rsc_level_completed <- function(data) {
  RSC <- filter(data, geographic_level =='RSC region')
  if(any(is.na(RSC$rsc_region_lead_name))) {message('FAIL - The rsc_region_lead_name column must be completed for all RSC region data.')}

}
pcon_level_completed <- function(data) {
  pcon <- filter(data, geographic_level =='Parliamentary constituency')
  if(any(is.na(pcon$pcon_name))) {message('FAIL - The pcon_name column must be completed for all parliamentary constituency data.')}
  if(any(is.na(pcon$pcon_code))) {message('FAIL - The pcon_code column must be completed for all parliamentary constituency data.')}
}
lad_level_completed <- function(data) {
  lad <- filter(data, geographic_level =='Local authority district')
  if(any(is.na(lad$lad_name))) {message('FAIL - The lad_name column must be completed for all local authority district data.')}
  if(any(is.na(lad$lad_code))) {message('FAIL - The lad_code column must be completed for all local authority district data.')}
}
lep_level_completed <- function(data) {
  lep <- filter(data, geographic_level =='Local enterprise partnership')
  if(any(is.na(lep$local_enterprise_partnership_name))) {message('FAIL - The local_enterprise_partnership_name column must be completed for all local enterprise partnership data.')}
  if(any(is.na(lep$local_enterprise_partnership_code))) {message('FAIL - The local_enterprise_partnership_code column must be completed for all local enterprise partnership data.')}
}
mca_level_completed <- function(data) {
  mca <- filter(data, geographic_level =='Mayoral combined authority')
  if(any(is.na(mca$mayoral_combined_authority_name))) {message('FAIL - The mayoral_combined_authority_name column must be completed for all mayoral combined authority data.')}
  if(any(is.na(mca$mayoral_combined_authority_code))) {message('FAIL - The mayoral_combined_authority_code column must be completed for all mayoral combined authority data.')}
}
oa_level_completed <- function(data) {
  oa <- filter(data, geographic_level =='Opportunity area')
  if(any(is.na(oa$opportunity_area_name))) {message('FAIL - The opportunity_area_name column must be completed for all opportunity area data.')}
  if(any(is.na(oa$opportunity_area_code))) {message('FAIL - The opportunity_area_code column must be completed for all opportunity area data.')}
}
ward_level_completed <- function(data) {
  ward <- filter(data, geographic_level =='Ward')
  if(any(is.na(ward$ward_name))) {message('FAIL - The ward_name column must be completed for all ward data.')}
  if(any(is.na(ward$ward_code))) {message('FAIL - The ward_code column must be completed for all ward data.')}
}
mat_level_completed <- function(data) {
  mat <- filter(data, geographic_level =='MAT')
  if(any(is.na(mat$trust_name))) {message('FAIL - The trust_name column must be completed for all MAT data.')}
  if(any(is.na(mat$trust_id))) {message('FAIL - The trust_id column must be completed for all MAT data.')}
}
sponsor_level_completed <- function(data) {
  sponsor <- filter(data, geographic_level =='Sponsor')
  if(any(is.na(sponsor$sponsor_name))) {message('FAIL - The trust_name column must be completed for all sponsor data.')}
  if(any(is.na(sponsor$sponsor_id))) {message('FAIL - The trust_id column must be completed for all sponsor data.')}
}
geography_level_completed <- function(data) {
  message("This will show if any of your geography columns are not completed for the relevant rows:")
  try(cat(national_level_completed(data)))
  if("Regional" %in% data$geographic_level){cat(regional_level_completed(data))}
  if("Local authority" %in% data$geographic_level){cat(la_level_completed(data))}
  if("RSC region" %in% data$geographic_level){cat(rsc_level_completed(data))}
  if("Parliamentary constituency" %in% data$geographic_level){cat(pcon_level_completed(data))}
  if("Local authority district" %in% data$geographic_level){cat(lad_level_completed(data))}
  if("Local enterprise partnership" %in% data$geographic_level){cat(lep_level_completed(data))}
  if("Mayoral combined authority" %in% data$geographic_level){cat(mca_level_completed(data))}
  if("Opportunity area" %in% data$geographic_level){cat(oa_level_completed(data))}
  if("Ward" %in% data$geographic_level){cat(ward_level_completed(data))}
  if("MAT" %in% data$geographic_level){cat(mat_level_completed(data))}
  if("Sponsor" %in% data$geographic_level){cat(sponsor_level_completed(data))} 
}
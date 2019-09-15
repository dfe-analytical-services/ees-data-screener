# -------------------------------------
### VALIDATION FUNCTIONS
# -------------------------------------
# check that the compulsory columns exist

data_comp_col <- function(data) {
  comp_col <- c("geographic_level","time_period","time_indentifier")
  for(i in comp_col){
    if(i %in% names(data)){message("PASS - ",i," is present in the data file.")}
    else{message("FAIL - The ",i," variable is missing from the data file.")}
  }
}

# -------------------------------------
# This checks for a 4 or 6 digit number in the time_period column, and then if 6 digit if it shows consecutive years

time_period_check <- function(data) {
  time_length <- data
  time_length$digits <- str_count(time_length$time_period)
  four <- nrow(filter(time_length,digits == 4))
  six <- nrow(filter(time_length,digits == 6))
  all <- nrow(time_length)
  time_periods <- unique(data$time_period)
  try(cat(if((four + six == all)==FALSE) 
    stop(writeLines(c("FAIL - time period must be a four or six digit number e.g. 2016 or 201617.",
                      "Here are the time period values in your file:","",time_periods,""))),
    message("PASS - time period is always a four or six digit number.")))
        consecutive_mini_function <- function(data) {
          six_digit_years <- filter(time_length,digits==6)
          currentyearend <- as.numeric(substr(six_digit_years$time_period,3,4))
          nextyearend <- as.numeric(substr(six_digit_years$time_period,5,6))
          check_yearends <- any(((currentyearend+1)==nextyearend)==FALSE)
          try(cat(if(check_yearends==TRUE) stop("FAIL - when the time period is 6 digits, the years must be consecutive."),
                  message('PASS - Your 6 digit time period/s show consecutive years.')))
        }
        if(six==0){message("IGNORE - There are no 6 digit time periods in the data file.")
        }else{
          consecutive_mini_function(data)
        }
}

# -------------------------------------
# checking the time identifier values are valid

time_identifier_check <- function(data) {
  acceptable_time_identifiers <- c("Spring term","Autumn term","Autumn and spring term","Up until 31st March",
                                   "January","February","March","April","May","June","July","August","September","October","November","Decemeber",
                                   "Calendar year","Calendar year Q1","Calendar year Q2","Calendar year Q3","Calendar year Q4",
                                   "Calendar year Q1-2","Calendar year Q1-3","Calendar year Q1-4","Calendar year Q2-3","Calendar year Q2-4","Calendar year Q3-4",
                                   "Financial year","Financial year Q1","Financial year Q2","Financial year Q3","Financial year Q4",
                                   "Financial year Q1-2","Financial year Q1-3","Financial year Q1-4","Financial year Q2-3","Financial year Q2-4","Financial year Q3-4",
                                   "Academic year","Academic year Q1","Academic year Q2","Academic year Q3","Academic year Q4",
                                   "Academic year Q1-2","Academic year Q1-3","Academic year Q1-4","Academic year Q2-3","Academic year Q2-4","Academic year Q3-4",
                                   "Tax year","Tax year Q1","Tax year Q2","Tax year Q3","Tax year Q4",
                                   "Tax year Q1-2","Tax year Q1-3","Tax year Q1-4","Tax year Q2-3","Tax year Q2-4","Tax year Q3-4")
  time_identifiers <- unique(data$time_identifier)
  #identifier_test <- intersect(time_identifiers,acceptable_time_identifiers)
  #try(cat(if(FALSE == identical(identifier_test,time_identifiers)) 
  #  stop(writeLines(c("FAIL - There is at least one invalid time_identifier present.",
   #                   "Here are the time_identifiers in your file:","",time_identifiers,""))),
  #message('PASS - Your time identifier/s are valid')))
  message('This will show if any of your time identifier/s are invalid:')
  for(i in time_identifiers){
    if((i %in% acceptable_time_identifiers)==FALSE)
      warning("
             FAIL - ", i, " is not a valid time identifier.")
  }
}

# -------------------------------------
# print the unique time_identifiers for conceptual checking

time_identifier_mix <- function(data) {
 cat(unique(dataset$time_identifier),sep = ", ") 
}

# -------------------------------------
# flag for commas across each column

comma_check <- function(data) {
  message("This will show if there are commas present in your data file:")
  for (i in names(data)) {
    if(any(grepl(",",data[[i]]))) warning("FAIL - Comma/s present in ",i,".")
  }
}

# -------------------------------------
# Checking datafile for spaces in variable names

data_spaces_check <- function(data) {
  variable_names <- names(dataset)
  message("This will show if there are any spaces in your variable names:")
  for (i in variable_names) {
    if(any(grepl('\\s',i))) warning("
FAIL - There are spaces in ", i,".")
  }
}

# -------------------------------------
# Do we have acceptable values for the geographic level

level_validity_check <- function(data) {
  acceptable_levels <- c("National","Regional","Local authority","RSC region","Parliamentary constituency",
                         "Local authority district","Local enterprise partnership","Mayoral combined authorities",
                         "Opportunity area","Ward","MAT","Sponsor")
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
  if("Local enterprise partnerships" %in% data$geographic_level){cat(lep_level_completed(data))}
  if("Mayoral combined authorities" %in% data$geographic_level){cat(mca_level_completed(data))}
  if("Opportunity area" %in% data$geographic_level){cat(oa_level_completed(data))}
  if("Ward" %in% data$geographic_level){cat(ward_level_completed(data))}
  if("MAT" %in% data$geographic_level){cat(mat_level_completed(data))}
  if("Sponsor" %in% data$geographic_level){cat(sponsor_level_completed(data))} 
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
  WARNING - There are fewer than two levels in: ", i,".")
  }
}

# -------------------------------------
# Check for Total in all filters

total_check <- function(data,meta) {
  if(!"Filter" %in% meta$col_type){message("IGNORE - This test does not apply to your data.")}
  else{message("This will show if there are 'Total' levels missing from any of your filters:")
    mfilters <- filter(meta,col_type=="Filter")
    filter_names <- c(mfilters$col_name)
    dfilters <- select(data,filter_names)
    for(i in names(dfilters)) {
      if(!"Total" %in% dfilters[[i]]) warning("
    WARNING - There is no 'Total' value in: ", i,".")
  }
  } 
}

# -------------------------------------
# Check all compulsory columns exist

meta_comp_col <- function(data) {
  try(cat(if(!"col_name" %in% names(data)) stop("FAIL - The col_name variable is missing."), 
          message('PASS - col_name is present in the metadata.')),silent = FALSE)
  try(cat(if(!"col_type" %in% names(data)) stop("FAIL - The col_type variable is missing."), 
          message('PASS - col_type is present in the metadata.')),silent = FALSE)
  try(cat(if(!"label" %in% names(data)) stop("FAIL - The label variable is missing."), 
          message('PASS - label is present in the metadata.')),silent = FALSE)
  try(cat(if(!"indicator_grouping" %in% names(data)) stop("FAIL - The indicator_grouping variable is missing."), 
          message('PASS - indicator_grouping is present in the metadata.')),silent = FALSE)
  try(cat(if(!"indicator_unit" %in% names(data)) stop("FAIL - The indicator_unit variable is missing."), 
          message('PASS - indicator_unit is present in the metadata.')),silent = FALSE)
  try(cat(if(!"filter_hint" %in% names(data)) stop("FAIL - The filter_hint variable is missing."), 
          message('PASS - filter_hint is present in the metadata')),silent = FALSE)
  try(cat(if(!"filter_grouping_column" %in% names(data)) stop("FAIL - The filter_grouping_column variable is missing."), 
          message('PASS - filter_grouping_column is present in the metadata.')),silent = FALSE)
}

# -------------------------------------
# For each col_name in the metadata check these each appear in the data file
column_crosscheck <- function(data,meta) {
  m_variables <- c(meta$col_name)
  message("This will show if any rows in the metadata do not have a matching variable in the data file:")
  for(i in m_variables){
    if((i %in% names(data))==FALSE)
      warning("
              FAIL - ", i," is not a variable in the data file.")
  }
}

# -------------------------------------
# List those in the data file that aren't in the metadata (or observational units)

meta_crosscheck <- function(data,meta) {
  observational_units <- c("geographic_level","time_period","time_identifier","country_code","country_name",
                           "region_code","region_name","old_la_code","new_la_code","la_name","rsc_region_lead_name",
                           "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                           "local_enterprise_partnership_name","mayoral_combined_authority_code",
                           "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                           "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name")
  n_ob_units <- setdiff(names(data),observational_units)
  message("This will show if there are variables in the data file that are not present in the metadata:")
    for (i in n_ob_units) {
    try(cat(if((i %in% meta$col_name)==FALSE) warning(i, " is not in the metadata or a recognised observational unit.
")))
  }
}

# -------------------------------------
# flag for commas across each column

meta_comma_check <- function(data) {
  message('This will show if there are commas in your metadata file:')
  for (i in names(data)) {
    if(any(grepl(",",data[[i]]))) warning("FAIL - There are commas in ", i)
  }
}

# -------------------------------------
# is col_name completed for every row

col_name_completed <- function(data) {
  if(any(is.na(data$col_name))){cat('FAIL - There is a col name missing in ', sum(is.na(data$col_name)), 'row/s.')}
    else{message('PASS - col_name is completed for all rows.')}
}

# -------------------------------------
# checking for duplicates in col_name

meta_duplicate_check <- function(data) {
  if(any(data$col_name %in% data$col_name[duplicated(data$col_name)])){message('FAIL - At least one of the variable names is duplicated.')}
    else{message('PASS - All col names are unique.')}
}

# -------------------------------------
# check that no value in col_name has any spaces

col_name_spaces_check <- function(data) {
  if(any(grepl('\\s',data$col_name))){message("FAIL - There are spaces in the col name values.")}
    else{message('PASS - There are no spaces in the col name values.')}
}

# -------------------------------------
# check if any observational units are in the metadata

comp_col_check_meta <- function(data) {
  observational_units <- c("geographic_level","time_period","time_identifier","country_code","country_name",
                           "region_code","region_name","old_la_code","new_la_code","la_name","rsc_name",
                           "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                           "local_enterprise_partnership_name","mayoral_combined_authority_code",
                           "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                           "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name")
  message("This will show if there are any observational units erroneously in your metadata:")
  for (i in observational_units) {
    try(cat(if(i %in% data$col_name) warning("FAIL - ", i, " should not be in the metadata. ")))
  }
}

# -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type_check <- function(data) {
  f <- nrow(filter(data,col_type == "Filter"))
  i <- nrow(filter(data,col_type == "Indicator"))
  m <- nrow(data)
  col_types <- unique(data$col_type)
  if((f + i == m)==FALSE){writeLines(c("FAIL - col_type must be either 'Filter' or 'Indicator', and cannot be blank.",
                 "Here are the col type values in your file:","",col_types,""))}
    else{message("PASS - col_type is always 'Filter' or 'Indicator'.")}
}

# -------------------------------------
# is label completed for every row

label_check <- function(data) {
  if(any(is.na(data$label))){message('FAIL - There is a label missing in', sum(is.na(data$label)), 'row/s.')}
    else{message('PASS - label is completed for all rows.')}
}

# -------------------------------------
# checking for duplicate labels

duplicate_label_check <- function(data) {
  if(any(data$label %in% data$label[duplicated(data$label)])){message('FAIL - At least one of the variable names is duplicated.')}
    else{message('PASS - All labels are unique.')}
}

# -------------------------------------
# indicator grouping - should be blank for all filters

indicator_group_check <- function(data) {
  filters <- filter(data,col_type == "Filter")
  if(any(!is.na(filters$indicator_grouping))){message('FAIL - Filters cannot have an indicator grouping.')}
    else{message('PASS - No filters have an indicator grouping.')}
}

# -------------------------------------
# Validation for the indicator units
# Currently the £ symbol is causing problems so have shortcutted the test for the time being

indicator_unit_validation <- function(data) {
#  indicators <- filter(metadata,col_type == "Indicator")
#  real_indicators <- indicators %>% drop_na(indicator_unit)
#  percent <- nrow(filter(real_indicators,indicator_unit=="%"))
#  pound <- nrow(filter(real_indicators,indicator_unit== "£"))
#  all <- nrow(real_indicators)
  indicator_units <- unique(data$indicator_unit)
#  if((percent + pound == all)==FALSE)
#    stop(
  writeLines(c("MANUAL CHECK - indicator unit must be either a percentage '%', a pound sign '<U+00A3>', or blank 'NA'.","",
                     "Here is a list of the indicator units in your file:","", indicator_units,"",
               "Make sure to manually check that the above are all valid units!"))
  #)
#    message("PASS - The indicator units are valid")
}

# -------------------------------------
# indicator unit should be blank for all filters

indicator_unit_check <- function(data) {
  filters <- filter(data,col_type =='Filter')
  if(any(!is.na(filters$indicator_unit))){message('FAIL - Filters cannot have an indicator unit.')}  
    else{message('PASS - No filters have an indicator unit.')}
}

# -------------------------------------
# filter_hint should be blank for indicators

filter_hint_check <- function(data) {
  indicators <- filter(data,col_type =='Indicator')
  if(any(!is.na(indicators$filter_hint))){message('FAIL - Indicators cannot have an filter hint.')}
    else{message('PASS - No indicators have a filter hint.')}
}

# -------------------------------------
# filter_grouping column is blank for all indicators

filter_group_check <- function(data) {
  indicators <- filter(data,col_type =='Indicator')
  if(any(!is.na(indicators$filter_grouping_column))){message('FAIL - Indicators cannot have a filter group assigned to them.')} 
    else{message('PASS - No indicators have a filter group.')}
}

# -------------------------------------
# rows in meta < cols in data file

row_check <- function(data,meta) {
  if(ncol(data)<nrow(meta)) stop('FAIL - Your metadata file has more rows than your data file has columns, this means that something is wrong.
        There are either too many rows in the metadata, or too few columns in the data file.
TRY - Check your csv files in a text editor as this might help you find the problem.')
  message('PASS - You have fewer rows in your metadata than you have columns in your data file.')
}

# -------------------------------------
# filter groups should be in the vector for column names for the data file

filter_group_match <- function(data,meta) {
  filters <- filter(meta,col_type == "Filter")
  filtered_g <- filters %>% drop_na(filter_grouping_column)
  filter_groups <- c(filtered_g$filter_grouping_column)
    if(length(filter_groups)==0){message("IGNORE - There are no filter groups present to test.")}
  else{
    message("This will show if any of your filter groups do not have a matching variable in the data file:")
    for(i in filter_groups){
      if((i %in% names(data))==FALSE)
        warning("
    FAIL - ", i," is not a variable in the data file.")
    }
  }
}

# -------------------------------------
# Checking that filter groups have fewer levels than their filters

filter_group_levels <- function(data,meta){
  filters <- filter(meta,col_type == "Filter")
  filtered_g <- filters %>% drop_na(filter_grouping_column)
  filter_groups <- c(filtered_g$filter_grouping_column)
  if(length(filter_groups)==0){message("IGNORE - There are no filter groups present to test.")}
  else{
    filter_groups <- drop_na(metadata,filter_grouping_column)
    fgs_focus <- select(filter_groups,c(col_name,filter_grouping_column))
    fgs_list <- c(unlist(fgs_focus,use.names=FALSE))
    message("This will flag if any filter groups have more levels than their corresponding filters:")
    for(i in seq(1,by=1,len=(length(fgs_list)/2))){
      x <- i+(length(fgs_list)/2)
      y <- fgs_list[[i]]
      z <- fgs_list[[x]]
      if((length(unique(dataset[[y]])))<(length(unique(dataset[[z]]))))
        warning("
    WARNING - ",fgs_list[[i]]," has less levels than its filter group - ",fgs_list[[x]],".","
  You should check that you've entered the filter and filter group in the right columns in the metadata.")
    }
  }
}

# -------------------------------------
### WRAPPING UP THE FUNCTIONS ABOVE NEATLY INTO A FUNCTION
# -------------------------------------

screening_tests <- function(data,meta) {
  data_comp_col(data)
  time_period_check(data)
  time_identifier_check(data)
  time_identifier_mix(data)
  comma_check(data)
  data_spaces_check(data)
  level_validity_check(data)
  geography_levels_present(data)
  geography_level_completed(data)
  filter_levels_check(data,meta)
  total_check(data)
  meta_comp_col(metadata)
  column_crosscheck(data,meta)
  meta_crosscheck(data,meta)
  meta_comma_check(meta)
  col_name_completed(meta)
  meta_duplicate_check(meta)
  col_name_spaces_check(meta)
  comp_col_check_meta(meta)
  col_type_check(meta)
  label_check(meta)
  duplicate_label_check(meta)
  indicator_group_check(meta)
  indicator_unit_validation(meta)
  indicator_unit_check(meta)
  filter_hint_check(meta)
  filter_group_check(meta)
  row_check(data,meta)
  filter_group_match(data,meta)
  filter_group_levels(data,meta)
}
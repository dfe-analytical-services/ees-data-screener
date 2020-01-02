# -------------------------------------
### SETTING UP THE ENVIRONMENT
# -------------------------------------

# function to check the pandoc version and install the latest if not 2.7.3 or later
cam_pandoc_install <- function(){
  if(rmarkdown::pandoc_version()>='2.7.3'){print("You already have version 2.7.3 or later of Pandoc installed, you're good to use the screener.")}
  else{if(!require(installr)){install.packages("installr");require(installr)}
  install.pandoc()}
}

# function to install/load the packages needed to run the screener
cam_envrionment_setup <- function(){
  if(!require(govdown)){install.packages("govdown");require(govdown)}
  if(!require(tidyr)){install.packages("tidyr");require(tidyr)}
  if(!require(readr)){install.packages("readr");require(readr)}
  if(!require(stringr)){install.packages("stringr");require(stringr)}
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
  if(!require(rmarkdown)){install.packages("rmarkdown");require(rmarkdown)}
  if(!require(knitr)){install.packages("knitr");require(knitr)}
  print("Your environment has successfully been setup, you should now be able to run the screener.")
}
cam_envrionment_setup()

# hard-coded variables
acceptable_time_identifiers <- c("Spring term","Autumn term","Autumn and spring term",
                                 "January","February","March","April","May","June","July","August","September","October","November","Decemeber",
                                 "Calendar year","Calendar year Q1","Calendar year Q2","Calendar year Q3","Calendar year Q4",
                                 "Financial year","Financial year Q1","Financial year Q2","Financial year Q3","Financial year Q4",
                                 "Academic year","Academic year Q1","Academic year Q2","Academic year Q3","Academic year Q4",
                                 "Tax year","Tax year Q1","Tax year Q2","Tax year Q3","Tax year Q4")

acceptable_observational_units <- c("country_code","country_name",
                                    "region_code","region_name","old_la_code","new_la_code","la_name","rsc_region_lead_name",
                                    "pcon_code","pcon_name","lad_code","lad_name","local_enterprise_partnership_code",
                                    "local_enterprise_partnership_name","mayoral_combined_authority_code",
                                    "mayoral_combined_authority_name","opportunity_area_code","opportunity_area_name",
                                    "ward_code","ward_name","trust_id","trust_name","sponsor_id","sponsor_name",
                                    "school_laestab","school_name","school_urn","school_estab","school_postcode",
                                    "provider_urn","provider_name","provider_ukprn","provider_upin",
                                    "institution_id","institution_name")

acceptable_levels <- c("National","Regional","Local authority","RSC region","Parliamentary constituency",
                       "Local authority district","Local enterprise partnership","Mayoral combined authority",
                       "Opportunity area","Ward","MAT","Sponsor","School","Provider","Institution")

all_required <- c("country_code","country_name")
regional_required <- c("region_code","region_name")
la_required <- c("region_code","region_name","old_la_code","new_la_code","la_name")
rsc_required <- c("rsc_region_lead_name")
pcon_required <- c("pcon_code","pcon_name")
lad_required <- c("lad_code","lad_name")
lep_required <- c("local_enterprise_partnership_code","local_enterprise_partnership_name")
mca_required <- c("mayoral_combined_authority_code","mayoral_combined_authority_name")
oa_required <- c("opportunity_area_code","opportunity_area_name")
ward_required <- c("ward_code","ward_name")
MAT_required <- c("trust_id","trust_name")
sponsor_required <- c("sponsor_id","sponsor_name")
school_required <- c("school_laestab","school_name")
provider_required <- c("provider_urn","provider_name")
institution_required <- c("institution_id","institution_id")

acceptable_indicator_units <- c('£',"%")

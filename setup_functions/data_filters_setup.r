# -------------------------------------
### DATA FILE FILTER VALIDATION FUNCTIONS
# -------------------------------------
# Checks in this file
data_filters_setup <-function(data,meta){
  filter_levels_check(data,meta)
  total_check(data,meta)
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
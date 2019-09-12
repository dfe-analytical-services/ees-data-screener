# FUNCTIONS TO RUN

# Datafile

datafile_checks <- function(data) {
  comp_col_check(data)
  time_period_check(data)
  time_period_check_consecutive(data)
  time_identifier_mix(data)
  time_identifier_check(data)
  comma_check(data)
}

datafile_checks(dataset)
# NEEDS WORK - geography_level_check(dataset)

# Metadata
metadata_checks <- function(data) {
  meta_col_check(data)
  comma_check(data)
  meta_name_check(data)
  meta_duplicate_check(data)
  meta_name_spaces_check(dataset)
  comp_col_check_meta(data)
  col_type_check(data)
  meta_label_check(data)
  meta_duplicate_label_check(data)
  meta_indicator_group_check(data)
# NEEDS WORK - meta_indicator_unit_check(metadata)
  meta_filter_unit_check(data)
  meta_filter_hint_check(data)
  meta_filter_group_check(data)
}
metadata_checks(metadata)


# Cross-validation - could possible split into the above if the variables are set up right at the start
# as will only need access to the variables and one file
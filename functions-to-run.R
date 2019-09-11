# FUNCTIONS TO RUN

# Datafile
comp_col_check(dataset)
time_period_check(dataset)
time_period_check_consecutive(dataset)
time_identifier_mix(dataset)
time_identifier_check(dataset)
comma_check(dataset)
# NEEDS WORK - geography_level_check(dataset)

# Metadata
meta_col_check(metadata)
comma_check(metadata)
meta_name_check(metadata)
meta_duplicate_check(metadata)
meta_name_spaces_check(dataset)
comp_col_check_meta(metadata)
col_type_check(metadata)
meta_label_check(metadata)
meta_duplicate_label_check(metadata)
meta_indicator_group_check(metadata)
# NEEDS WORK - meta_indicator_unit_check(metadata)
meta_filter_unit_check(metadata)
meta_filter_hint_check(metadata)
meta_filter_group_check(metadata)

# Cross-validation - could possible split into the above if the variables are set up right at the start
# as will only need access to the variables and one file
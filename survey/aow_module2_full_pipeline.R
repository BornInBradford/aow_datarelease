# Survey module 2 pipeline


# 1. merge online/offline versions and tag missing values according to survey version where possible
source("survey/module2/aow_module2_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
source("survey/module2/aow_module2_administrative.R")

# 3. derive new variables, do any required question version merging, drop unnecessary vars
source("survey/module2/aow_module2_derived_vars.R")

# 4. final labelling of variables and values and general tidying
source("survey/module2/aow_module2_labelling.R")

# Survey module 2 pipeline


# 1. merge online/offline versions and tag missing values according to survey version where possible
source("survey/module2/aow_module2_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
source("survey/module2/aow_module2_administrative.R")


# Survey module 4 pipeline


# 1. merge online/offline versions and tag missing values according to survey version where possible
source("survey/module4/aow_module4_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
source("survey/module4/aow_module4_administrative.R")


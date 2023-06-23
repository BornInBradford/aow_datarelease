# Survey module 1 pipeline


# 1. merge online/offline versions and tag missing values according to survey version where possible
source("survey/module1/aow_module1_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
source("survey/module1/aow_module1_administrative.R")


# full build pipeline is broken into stages

# choose whether to export/import at each stage
export_import <- TRUE

# 1. merge online/offline versions and tag missing values according to survey version where possible
source("survey/aow_module2_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
source("survey/aow_module2_administrative.R")


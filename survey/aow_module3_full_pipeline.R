# Survey module 3 pipeline

options(aow_output_reports = TRUE)

source("tools/aow_tools.R")

# 1. merge online/offline versions and tag missing values according to survey version where possible
#    output = module 3 merged
source("survey/module3/aow_module3_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
#    output = module 3 linked
source("survey/module3/aow_module3_administrative.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_linked.rds",
                                                   "Survey module 3 linked data frame")

# 3. derive new variables, do any required question version merging, drop unnecessary vars
#    output = module 3 derived
source("survey/module3/aow_module3_derived_vars.R")

# 4. final labelling of variables and values and general tidying
#    output = module 3 labelled
source("survey/module3/aow_module3_labelling.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_labelled.rds",
                                                   "Survey module 3 labelled data frame")

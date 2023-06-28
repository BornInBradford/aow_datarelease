# Survey module 2 pipeline

options(aow_output_reports = TRUE)

source("tools/aow_tools.R")

# 1. merge online/offline versions and tag missing values according to survey version where possible
#    output = module 2 merged
source("survey/module2/aow_module2_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
#    output = module 2 linked
source("survey/module2/aow_module2_administrative.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_linked.rds",
                                                   "Survey module 2 linked data frame")

# 3. derive new variables, do any required question version merging, drop unnecessary vars
#    output = module 2 derived
source("survey/module2/aow_module2_derived_vars.R")

# 4. final labelling of variables and values and general tidying
#    output = module 2 labelled
source("survey/module2/aow_module2_labelling.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_labelled.rds",
                                                   "Survey module 2 labelled data frame")

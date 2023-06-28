# Survey module 1 pipeline

options(aow_output_reports = TRUE)

source("tools/aow_tools.R")

# 1. merge online/offline versions and tag missing values according to survey version where possible
#    output = module 1 merged
source("survey/module1/aow_module1_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
#    output = module 1 linked
source("survey/module1/aow_module1_administrative.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module1_linked.rds",
                                                   "Survey module 1 linked data frame")

# 3. derive new variables, do any required question version merging, drop unnecessary vars
#    output = module 1 derived
source("survey/module1/aow_module1_derived_vars.R")

# 4. final labelling of variables and values and general tidying
#    output = module 1 labelled
source("survey/module1/aow_module1_labelling.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module1_labelled.rds",
                                                   "Survey module 1 labelled data frame")

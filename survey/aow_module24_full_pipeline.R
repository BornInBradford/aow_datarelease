# Survey module 24 pipeline

options(aow_output_reports = TRUE)

source("tools/aow_tools.R")

# 1. merge online/offline versions and tag missing values according to survey version where possible
#    output = module 24 merged
source("survey/module24/aow_module24_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
#    output = module 24 linked
source("survey/module24/aow_module24_administrative.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_linked.rds",
                                                   "Survey module 24 linked data frame")
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_notlinked.rds",
                                                   "Survey module 24 *unlinked* data frame")

# 3. integrate with existing survey releases: union with previous output, merge/recode questions if needed, drop unnecessary vars
#    output = module 231 integrated
source("survey/module231/aow_module231_integration.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_integrated.rds",
                                                   "Survey module 231 integrated with previous release")


# 4. final labelling of variables and values and general tidying
#    output = module 231 labelled
source("survey/module231/aow_module231_labelling.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_labelled.rds",
                                                   "Survey module 231 main labelled data frame")


# 5. generate derived variables for module 231
#    output = module 231 derived
source("survey/module231/aow_module231_derived_vars.R")

# 6. final labelling of derived variables and values and general tidying
#    output = module 231 derived labelled
source("survey/module231/aow_module231_derived_labelling.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_derived_labelled.rds",
                                                   "Survey module 231 derived variables labelled")


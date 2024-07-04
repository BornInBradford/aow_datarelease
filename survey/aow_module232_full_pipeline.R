# Survey module 232 pipeline

options(aow_output_reports = TRUE)

source("tools/aow_tools.R")

# 1. merge online/offline versions and tag missing values according to survey version where possible
#    output = module 232 merged
source("survey/module232/aow_module232_survey_merge_miss.R")

# 2. add administrative vars such as person ID, survey date and ages then basic tidy up
#    output = module 232 linked
source("survey/module232/aow_module232_administrative.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_linked.rds",
                                                   "Survey module 232 linked data frame")
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_notlinked.rds",
                                                   "Survey module 232 *unlinked* data frame")

# 3. integrate with existing survey releases: union with previous output, merge/recode questions if needed, drop unnecessary vars
#    output = module 232 integrated
source("survey/module232/aow_module232_integration.R")

# output report if required
if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_integrated.rds",
                                                   "Survey module 232 integrated with previous release")


# 4. final labelling of variables and values and general tidying
#    output = module 232 labelled
#source("survey/module232/aow_module232_labelling.R")

# output report if required
#if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_labelled.rds",
#                                                   "Survey module 232 main labelled data frame")


# 5. generate derived variables for module 232
#    output = module 232 derived
#source("survey/module232/aow_module232_derived_vars.R")

# output report if required
#if(getOption("aow_output_reports")) aow_df_summary("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_derived.rds",
#                                                   "Survey module 232 derived variables")



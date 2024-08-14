# Module 231 survey labelling and tidying

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_derived.rds")


# 80 chars:                           
################################################################################

## Labelling derived variables

module <- module |> set_variable_labels(own_fin_total = "Own Financial Resources (higher = more sources)",
                                        own_fin_nas = "Own Financial Resources NAs",
                                        own_fin_missing = "Own Financial Resources missing",
                                        food_avail_total = "Food Availability (higher = more food insecurity)",
                                        food_avail_nas = "Food Availability NAs",
                                        food_avail_missing = "Food Availability missing",
                                        edeqs_total = "Eating Disorder Examination (higher = more disordered)",
                                        edeqs_nas = "Eating Disorder Examination NAs",
                                        edeqs_missing = "Eating Disorder Examination missing",
                                        edeqs_cat = "Eating Disorder Examination category",
                                        paqa_total = "Physical Activity Questionnaire (higher = more active)",
                                        paqa_nas = "Physical Activity Questionnaire NAs",
                                        paqa_missing = "Physical Activity Questionnaire missing",
                                        paqa_mean = "Physical Activity Questionnaire (higher = more active)",
                                        yapsed_total = "Youth Activity Profile (higher = more sedentary)",
                                        yapsed_nas = "Youth Activity Profile NAs",
                                        yapsed_missing = "Youth Activity Profile missing",
                                        yapsed_mean = "Youth Activity Profile mean (higher = more sedentary)"
)


# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_derived_labelled.rds")



  
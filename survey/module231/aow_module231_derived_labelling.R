# Module 231 survey labelling and tidying

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_derived.rds")


# 80 chars:                           
################################################################################

## Labelling derived variables

module <- module |> set_variable_labels(own_fin_total = "Own Financial Resources (higher = more sources)",
                                        food_avail_total = "Food Availability (higher = more food insecurity)",
                                        edeqs_total = "Eating Disorder Examination (higher = more disordered)",
                                        edeqs_cat = "Eating Disorder Examination category",
                                        paqa_total = "Physical Activity Questionnaire (higher = more active)",
                                        paqa_mean = "Physical Activity Questionnaire (higher = more active)",
                                        yapsed_total = "Youth Activity Profile (higher = more sedentary)",
                                        yapsed_mean = "Youth Activity Profile mean (higher = more sedentary)"
)


# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_derived_labelled.rds")



  
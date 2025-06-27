# Module 231 survey derived variables

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_labelled.rds")

# survey cols, leaving out admin cols
survey_cols <- names(module)[!names(module) %in% aow_survey_column_order()]


# load lookups

rcads_lookup <- read_csv("resources/RCADS25_C_lookup_table.csv")

rcads_lookup <- rcads_lookup %>%
  rename(year_group = YearGroup) %>%
  rename(gender = Sex) %>%
  mutate(gender = ifelse(gender == "Female",1,2))


swemwbs_lookup <- read_csv("resources/SWEMWBS_lookup_table.csv")

remove_na <- function(x) ifelse(is.na(x), 0, x)

# CHANGES from mod 1
#
# SAIS dropped as two  aw3_6_comparison vars dropped
# FAS dropped as some awb3_1_assets vars dropped
# own financial resources recoded as two awb3_4_personal_assts are dropped

# CHANGES from mod 3
#
# edeqs_cat is now labelled categorical (1, 2) instead of text (normal, possible disorder)

################################################################################
# dvs inherited from module 1 in 2023 release

# Sum derived variables

module <-
  module %>%
  #Own Financial Resources - Bespoke
  #sum scores
  mutate(own_fin_total = remove_na(awb3_4_personal_assts_1) + 
           remove_na(awb3_4_personal_assts_2) + 
           remove_na(awb3_4_personal_assts_3) + 
           remove_na(awb3_4_personal_assts_4) +
           remove_na(awb3_4_personal_assts_5),
         own_fin_nas = is.na(awb3_4_personal_assts_1) +
                             is.na(awb3_4_personal_assts_2) +
                             is.na(awb3_4_personal_assts_3) +
                             is.na(awb3_4_personal_assts_4) +
                             is.na(awb3_4_personal_assts_5),
         own_fin_missing = ifelse(own_fin_nas == 5, 1, 0),
         own_fin_total = ifelse(own_fin_missing == 1, NA, own_fin_total)) %>%
  #recode inconsistent responses
  #Food Availability - Bespoke
  #sum scores
  mutate(food_avail_total = remove_na(aw3_5_food_1) + 
           remove_na(aw3_5_food_2) + 
           remove_na(aw3_5_food_3) + 
           remove_na(aw3_5_food_4) + 
           remove_na(aw3_5_food_5),
         food_avail_nas = is.na(aw3_5_food_1) +
                                is.na(aw3_5_food_2) +
                                is.na(aw3_5_food_3) +
                                is.na(aw3_5_food_4) +
                                is.na(aw3_5_food_5),
         food_avail_missing = ifelse(food_avail_nas == 5, 1, 0),
         food_avail_total = ifelse(food_avail_missing == 1, NA, food_avail_total))
           






################################################################################
# dvs inherited from module 3 in 2023 release

module <-
  module %>%
  mutate(TMPVAR_awb2_12_eat_hbt_1_a5 = awb2_12_eat_hbt_1_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_2_a5 = awb2_12_eat_hbt_2_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_3_a5 = awb2_12_eat_hbt_3_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_4_a5 = awb2_12_eat_hbt_4_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_5_a5 = awb2_12_eat_hbt_5_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_6_a5 = awb2_12_eat_hbt_6_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_7_a5 = awb2_12_eat_hbt_7_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_8_a5 = awb2_12_eat_hbt_8_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_9_a5 = awb2_12_eat_hbt_9_a5 - 1,
         TMPVAR_awb2_12_eat_hbt_10_a5 = awb2_12_eat_hbt_10_a5 - 1,
         TMPVAR_awb2_12_wght_1_a5 = awb2_12_wght_1_a5 - 1,
         TMPVAR_awb2_12_wght_2_a5 = awb2_12_wght_2_a5 - 1)

module <-
  module %>%
  
  
  #EDE-QS
  #sum scores
  mutate(edeqs_total = remove_na(TMPVAR_awb2_12_eat_hbt_1_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_2_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_3_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_4_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_5_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_6_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_7_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_8_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_9_a5) +
           remove_na(TMPVAR_awb2_12_eat_hbt_10_a5) +
           remove_na(TMPVAR_awb2_12_wght_1_a5) +
           remove_na(TMPVAR_awb2_12_wght_2_a5),
         edeqs_nas = is.na(TMPVAR_awb2_12_eat_hbt_1_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_2_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_3_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_4_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_5_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_6_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_7_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_8_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_9_a5) +
           is.na(TMPVAR_awb2_12_eat_hbt_10_a5) +
           is.na(TMPVAR_awb2_12_wght_1_a5) +
           is.na(TMPVAR_awb2_12_wght_2_a5),
         edeqs_missing = ifelse(edeqs_nas == 12, 1, 0),
         edeqs_total = ifelse(edeqs_missing == 1, NA, edeqs_total)) %>%
  #categorise
  mutate(edeqs_cat = ifelse(edeqs_total < 15, 
                            1, # normal
                            2)) |> # possible disorder
  set_value_labels(edeqs_cat = c("normal" = 1, "possible disorder" = 2))

                            

module <- module %>%
  
  #PAQ-A
  #sum scores
  mutate(paqa_total = remove_na(awb4_1_physical_actvty_1_a5) +
           remove_na(awb4_1_physical_actvty_2_a5) +
           remove_na(awb4_1_physical_actvty_3_a5) +
           remove_na(awb4_1_physical_actvty_4_a5) +
           remove_na(awb4_1_physical_actvty_5_a5) +
           remove_na(awb4_1_physical_actvty_6_a5) +
           remove_na(awb4_1_physical_actvty_7_a5) +
           remove_na(awb4_1_physical_actvty_8_a5),
         paqa_nas = is.na(awb4_1_physical_actvty_1_a5) +
           is.na(awb4_1_physical_actvty_2_a5) +
           is.na(awb4_1_physical_actvty_3_a5) +
           is.na(awb4_1_physical_actvty_4_a5) +
           is.na(awb4_1_physical_actvty_5_a5) +
           is.na(awb4_1_physical_actvty_6_a5) +
           is.na(awb4_1_physical_actvty_7_a5) +
           is.na(awb4_1_physical_actvty_8_a5),
         paqa_missing = ifelse(paqa_nas == 8, 1, 0),
         paqa_total = ifelse(paqa_missing == 1, NA, paqa_total)) %>%
  #compute score
  mutate(paqa_mean = paqa_total/8) 

module <- module %>%
  
  #YAP (sedentary scale)
  #Own Financial Resources - Bespoke
  #sum scores
  mutate(yapsed_total = remove_na(awb4_2_outside_schl_1_r7) +
           remove_na(awb4_2_outside_schl_2_r7) +
           remove_na(awb4_2_outside_schl_3_r7) +
           remove_na(awb4_2_outside_schl_4_r7) +
           remove_na(awb4_2_overall_a5),
         yapsed_nas = is.na(awb4_2_outside_schl_1_r7) +
           is.na(awb4_2_outside_schl_2_r7) +
           is.na(awb4_2_outside_schl_3_r7) +
           is.na(awb4_2_outside_schl_4_r7) +
           is.na(awb4_2_overall_a5),
         yapsed_missing = ifelse(yapsed_nas == 5, 1, 0),
         yapsed_total = ifelse(yapsed_missing == 1, NA, yapsed_total)) %>%
  #compute score
  mutate(yapsed_mean = yapsed_total/5)



# trim down to admin and derived variables

vars_to_drop <- c("own_fin_nas",
                  "own_fin_missing",
                  "food_avail_nas",
                  "food_avail_missing",
                  "edeqs_nas",
                  "edeqs_missing",
                  "paqa_nas",
                  "paqa_missing",
                  "yapsed_nas",
                  "yapsed_missing")

derived_vars <- module |> select(-starts_with("TMPVAR_"), -all_of(survey_cols), -all_of(vars_to_drop))

# export
saveRDS(derived_vars, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_derived.rds")


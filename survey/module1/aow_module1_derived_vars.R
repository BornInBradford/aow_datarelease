# Module 1 survey derived variables

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module1_linked.rds")

# Re-scaling variables
module <- module %>%
  
  # rescale striving to avoid inferiority scale(SAIS) (subtract 1, reverse third option)
  mutate(TMPVAR_aw3_6_comparison_1 = aw3_6_comparison_1 - 1,
         TMPVAR_aw3_6_comparison_2 = aw3_6_comparison_2 - 1,
         TMPVAR_aw3_6_comparison_3 = aw3_6_comparison_3 - 1) %>%
  mutate(TMPVAR_aw3_6_comparison_3 = recode(TMPVAR_aw3_6_comparison_3,
                                     `0` = 4,
                                     `1` = 3,
                                     `2` = 2,
                                     `3` = 1,
                                     `4` = 0)) %>%
  mutate(TMPVAR_awb3_7_violence = recode(awb3_7_violence,
                                  `1` = 0,
                                  `2` = 2,
                                  `3` = 1)) %>%
  set_value_labels(TMPVAR_awb3_7_violence = c("No" = 0, "Yes" = 1, "Not sure" = 2))


# Sum derived variables

module <-
  module %>%
  rowwise() %>%
  #Family Affluence Scale (FAS) - Adapted
  #sum scores
  mutate(fas_total = sum(c_across(awb3_1_assets_1:awb3_1_assets_9), na.rm = TRUE),
         fas_nas = sum(is.na(c_across(awb3_1_assets_1:awb3_1_assets_9))),
         fas_missing = ifelse(fas_nas == 9, 1, 0)) %>%
  #categorise
  mutate(fas_cat = ifelse(fas_total < 3, "low",
                          ifelse(fas_total < 6, "medium", "high"))) %>%
  #Own Financial Resources - Bespoke
  #sum scores
  mutate(own_fin_total = sum(c_across(awb3_4_personal_assts_1:awb3_4_personal_assts_5), na.rm = TRUE),
         own_fin_nas = sum(is.na(c_across(awb3_4_personal_assts_1:awb3_4_personal_assts_7))),
         own_fin_missing = ifelse(own_fin_nas == 7, 1, 0)) %>%
  #recode inconsistent responses
  mutate(own_fin_total = ifelse(awb3_4_personal_assts_6 == 1 && own_fin_total > 0, -1, own_fin_total)) %>%
  #Food Availability - Bespoke
  #sum scores
  mutate(food_avail_total = sum(c_across(aw3_5_food_1:aw3_5_food_5), na.rm = TRUE),
         food_avail_nas = sum(is.na(c_across(aw3_5_food_1:aw3_5_food_5))),
         food_avail_missing = ifelse(food_avail_nas == 5, 1, 0)) %>%
  #Striving to Avoid Inferiority Scale (SAIS) - Adapted
  #sum scores
  mutate(sais_total = sum(c_across(TMPVAR_aw3_6_comparison_1:TMPVAR_aw3_6_comparison_3), na.rm = TRUE),
         sais_nas = sum(is.na(c_across(TMPVAR_aw3_6_comparison_1:TMPVAR_aw3_6_comparison_3))),
         sais_missing = ifelse(sais_nas == 3, 1, 0)) %>%
  #compute score
  mutate(sais_mean = sais_total/3)


# fas cat - code
module <- module %>%
  mutate(fas_cat = case_when(
    fas_cat == "low" ~ 1,
    fas_cat == "medium" ~ 2,
    fas_cat == "high" ~ 3)) %>%
  set_value_labels(fas_cat = c("Low" = 1,
                               "Medium" = 2,
                               "High" = 3))

module <- module |> select(-starts_with("TMPVAR_"))

# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module1_derived.rds")



  
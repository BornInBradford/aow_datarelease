# Module 3 survey derived variables

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_linked.rds")

module <-
  module %>%
  mutate(awb5_1_hearing_sght_4 = ifelse(awb5_1_hearing_sght_4 == 1, 0,
                                        ifelse(awb5_1_hearing_sght_4 == 3, 1, awb5_1_hearing_sght_4))) %>%
  mutate(across(awb2_12_eat_hbt_1_a5:awb2_12_wght_2_a5,
                ~ .x - 1)) %>%
  mutate(awb5_1_oral_hlth_2 = awb5_1_oral_hlth_2 - 1) %>%
  mutate(awb5_1_oral_hlth_3 = ifelse(awb5_1_oral_hlth_3 == 1, 0,
                                     ifelse(awb5_1_oral_hlth_3 == 3, 1, awb5_1_oral_hlth_3))) %>%
  mutate(awb4_4y_gendersex_f_2 = ifelse(awb4_4y_gendersex_f_2 == 2, 0, awb4_4y_gendersex_f_2)) %>%
  mutate(awb5_2_smokevape_prnt = ifelse(awb5_2_smokevape_prnt == 2, 0, awb5_2_smokevape_prnt)) %>%
  mutate(aw5_2_vape_prnt = ifelse(aw5_2_vape_prnt == 2, 0, aw5_2_vape_prnt)) %>%
  mutate(awb5_2y_alcohol_qntty = ifelse(awb5_2y_alcohol_qntty == 2, 0, awb5_2y_alcohol_qntty)) %>%
  mutate(across(awb5_2_gambling_1_a5:awb5_2_gambling_14_a5,
                ~ ifelse(.x == 5, 0, .x))) %>%
  mutate(awb5_2y_knife = ifelse(awb5_2y_knife == 2, 0, awb5_2y_knife)) %>%
  mutate(awb5_2y_gang = recode(awb5_2y_gang, 
                               `1` = 1,
                               `2` = 0,
                               `3` = 2)) %>%
  mutate(awb5_2y_gang_6 = ifelse(awb5_2y_gang_6 == 2, 0, awb5_2y_gang_6)) %>%
  mutate(awb4_1_sick_a5 = ifelse(awb4_1_sick_a5 == 2, 0, awb4_1_sick_a5)) %>%
  mutate(awb4_3_nap_a5 = awb4_3_nap_a5 - 1)

module <-
  module %>%
  rowwise() %>%
  #EDE-QS
  #sum scores
  mutate(edeqs_total = sum(c_across(awb2_12_eat_hbt_1_a5:awb2_12_wght_2_a5), na.rm = TRUE),
         edeqs_nas = sum(is.na(c_across(awb2_12_eat_hbt_1_a5:awb2_12_wght_2_a5))),
         edeqs_missing = ifelse(edeqs_nas == 12, 1, 0)) %>%
  #categorise
  mutate(edeqs_cat = ifelse(edeqs_total < 15, "normal", "possible disorder")) 

module <- module %>%
  #PAQ-A
  #sum scores
  mutate(paqa_total = sum(c_across(awb4_1_physical_actvty_1_a5:awb4_1_physical_actvty_8_a5), na.rm = TRUE),
         paqa_nas = sum(is.na(c_across(awb4_1_physical_actvty_1_a5:awb4_1_physical_actvty_8_a5))),
         paqa_missing = ifelse(paqa_nas == 8, 1, 0)) %>%
  #compute score
  mutate(paqa_mean = paqa_total/8) 

module <- module %>%
    #YAP (sedentary scale)
  #Own Financial Resources - Bespoke
  #sum scores
  mutate(yapsed_total = sum(c_across(awb4_2_outside_schl_1_a5:awb4_2_overall_a5), na.rm = TRUE),
         yapsed_nas = sum(is.na(c_across(awb4_2_outside_schl_1_a5:awb4_2_overall_a5))),
         yapsed_missing = ifelse(yapsed_nas == 5, 1, 0)) %>%
  #compute score
  mutate(yapsed_mean = yapsed_total/5)

# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_derived.rds")



  
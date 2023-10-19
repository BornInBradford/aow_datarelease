# Module 3 survey derived variables

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_linked.rds")

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
  rowwise() %>%
  #EDE-QS
  #sum scores
  mutate(edeqs_total = sum(c(TMPVAR_awb2_12_eat_hbt_1_a5,
                             TMPVAR_awb2_12_eat_hbt_2_a5,
                             TMPVAR_awb2_12_eat_hbt_3_a5,
                             TMPVAR_awb2_12_eat_hbt_4_a5,
                             TMPVAR_awb2_12_eat_hbt_5_a5,
                             TMPVAR_awb2_12_eat_hbt_6_a5,
                             TMPVAR_awb2_12_eat_hbt_7_a5,
                             TMPVAR_awb2_12_eat_hbt_8_a5,
                             TMPVAR_awb2_12_eat_hbt_9_a5,
                             TMPVAR_awb2_12_eat_hbt_10_a5,
                             TMPVAR_awb2_12_wght_1_a5,
                             TMPVAR_awb2_12_wght_2_a5), na.rm = TRUE),
         edeqs_nas = sum(is.na(c(TMPVAR_awb2_12_eat_hbt_1_a5,
                                 TMPVAR_awb2_12_eat_hbt_2_a5,
                                 TMPVAR_awb2_12_eat_hbt_3_a5,
                                 TMPVAR_awb2_12_eat_hbt_4_a5,
                                 TMPVAR_awb2_12_eat_hbt_5_a5,
                                 TMPVAR_awb2_12_eat_hbt_6_a5,
                                 TMPVAR_awb2_12_eat_hbt_7_a5,
                                 TMPVAR_awb2_12_eat_hbt_8_a5,
                                 TMPVAR_awb2_12_eat_hbt_9_a5,
                                 TMPVAR_awb2_12_eat_hbt_10_a5,
                                 TMPVAR_awb2_12_wght_1_a5,
                                 TMPVAR_awb2_12_wght_2_a5))),
         edeqs_missing = ifelse(edeqs_nas == 12, 1, 0)) %>%
  #categorise
  mutate(edeqs_cat = ifelse(edeqs_total < 15, "normal", "possible disorder")) 

module <- module %>%
  #PAQ-A
  #sum scores
  mutate(paqa_total = sum(c(awb4_1_physical_actvty_1_a5,
                            awb4_1_physical_actvty_2_a5,
                            awb4_1_physical_actvty_3_a5,
                            awb4_1_physical_actvty_4_a5,
                            awb4_1_physical_actvty_5_a5,
                            awb4_1_physical_actvty_6_a5,
                            awb4_1_physical_actvty_7_a5,
                            awb4_1_physical_actvty_8_a5), na.rm = TRUE),
         paqa_nas = sum(is.na(c(awb4_1_physical_actvty_1_a5,
                                awb4_1_physical_actvty_2_a5,
                                awb4_1_physical_actvty_3_a5,
                                awb4_1_physical_actvty_4_a5,
                                awb4_1_physical_actvty_5_a5,
                                awb4_1_physical_actvty_6_a5,
                                awb4_1_physical_actvty_7_a5,
                                awb4_1_physical_actvty_8_a5))),
         paqa_missing = ifelse(paqa_nas == 8, 1, 0)) %>%
  #compute score
  mutate(paqa_mean = paqa_total/8) 

module <- module %>%
    #YAP (sedentary scale)
  #Own Financial Resources - Bespoke
  #sum scores
  mutate(yapsed_total = sum(c(awb4_2_outside_schl_1_r7,
                              awb4_2_outside_schl_2_r7,
                              awb4_2_outside_schl_3_r7,
                              awb4_2_outside_schl_4_r7,
                              awb4_2_overall_a5), na.rm = TRUE),
         yapsed_nas = sum(is.na(c(awb4_2_outside_schl_1_r7,
                                  awb4_2_outside_schl_2_r7,
                                  awb4_2_outside_schl_3_r7,
                                  awb4_2_outside_schl_4_r7,
                                  awb4_2_overall_a5))),
         yapsed_missing = ifelse(yapsed_nas == 5, 1, 0)) %>%
  #compute score
  mutate(yapsed_mean = yapsed_total/5)

module <- module |> select(-starts_with("TMPVAR_"))

# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_derived.rds")



  
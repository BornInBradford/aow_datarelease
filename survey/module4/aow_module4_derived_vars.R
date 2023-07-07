# Module 4 survey derived variables

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_linked.rds")

module <-
  module %>%
  mutate(awb7_1_bullying_2 = case_when(awb7_1_bullying_2 == 1 ~ 0,
                                       awb7_1_bullying_2 == 2 ~ 2,
                                       awb7_1_bullying_2 == 3 ~ 1)) %>%
  mutate(awb7_1_future = ifelse(awb7_1_future == 4, 0, awb7_1_future)) %>%
  mutate(awb7_2_water_1 = case_when(awb7_2_water_1 == 1 ~ 1,
                                    awb7_2_water_1 == 2 ~ 0,
                                    awb7_2_water_1 == 3 ~ 2)) %>%
  mutate(awb6_5_dev_n_home = awb6_5_dev_n_home - 1) %>%
  mutate(awb6_6_int_hme_hrs = awb6_6_int_hme_hrs - 1) %>%
  mutate(awb6_6_int_hme_hrs_wknd = awb6_6_int_hme_hrs_wknd - 1) %>%
  mutate(awb6_8_attd_tech_10 = case_when(awb6_8_attd_tech_10 == 1 ~ 5,
                                         awb6_8_attd_tech_10 == 2 ~ 4,
                                         awb6_8_attd_tech_10 == 3 ~ 3,
                                         awb6_8_attd_tech_10 == 4 ~ 2,
                                         awb6_8_attd_tech_10 == 5 ~ 1))



module <-
  module %>%
  rowwise() %>%
  #ADDI
  #sum scores
  mutate(addi_inst_exp = sum(c(awb8_2_age_3,
                               awb8_2_police_5,
                               awb8_2_shop_6,
                               awb8_2_service_8,
                               awb8_2_int_9,
                               awb8_2_afraid_10), na.rm = TRUE),
         addi_peer_exp = sum(c(awb8_2_club_1,
                               awb8_2_excl_2,
                               awb8_2_lang_4,
                               awb8_2_names_7,
                               awb8_2_threat_11), na.rm = TRUE),
         addi_total = addi_inst_exp + addi_peer_exp,
         addi_nas = sum(is.na(c(awb8_2_age_3,
                                awb8_2_police_5,
                                awb8_2_shop_6,
                                awb8_2_service_8,
                                awb8_2_int_9,
                                awb8_2_afraid_10,
                                awb8_2_club_1,
                                awb8_2_excl_2,
                                awb8_2_lang_4,
                                awb8_2_names_7,
                                awb8_2_threat_11))),
         addi_missing = ifelse(addi_nas == 11, 1, 0)) %>%
  #PATT-SQ
  mutate(patt_career = mean(c(awb6_8_attd_tech_1,
                              awb6_8_attd_tech_11), na.rm = TRUE),
         patt_interest = mean(c(awb6_8_attd_tech_2, awb6_8_attd_tech_12),
                              na.rm = TRUE),
         patt_tedious = mean(c(awb6_8_attd_tech_3, awb6_8_attd_tech_4),
                             na.rm = TRUE),
         patt_conseq = mean(c(awb6_8_attd_tech_5, awb6_8_attd_tech_6),
                            na.rm = TRUE),
         patt_diff = mean(c(awb6_8_attd_tech_7, awb6_8_attd_tech_8),
                          na.rm = TRUE),
         patt_gender = mean(c(awb6_8_attd_tech_9, awb6_8_attd_tech_10),
                            na.rm = TRUE),
         patt_nas = sum(is.na(c_across(awb6_8_attd_tech_1:awb6_8_attd_tech_12))),
         patt_missing = ifelse(patt_nas == 12, 1, 0))


# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_derived.rds")



  
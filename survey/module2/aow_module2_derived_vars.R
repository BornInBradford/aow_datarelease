# Module 2 survey derived variables

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_linked.rds")

module <- module %>%
  mutate(TMPVAR_awb2_1_illhealth_1 = awb2_1_illhealth_1 - 1,
         TMPVAR_awb2_1_illhealth_2 = awb2_1_illhealth_2 - 1,
         TMPVAR_awb2_1_illhealth_3 = awb2_1_illhealth_3 - 1,
         TMPVAR_awb2_1_illhealth_4 = awb2_1_illhealth_4 - 1,
         TMPVAR_awb2_1_illhealth_5 = awb2_1_illhealth_5 - 1,
         TMPVAR_awb2_1_illhealth_6 = awb2_1_illhealth_6 - 1,
         TMPVAR_awb2_1_illhealth_7 = awb2_1_illhealth_7 - 1,
         TMPVAR_awb2_1_illhealth_8 = awb2_1_illhealth_8 - 1,
         TMPVAR_awb2_1_illhealth_9 = awb2_1_illhealth_9 - 1,
         TMPVAR_awb2_1_illhealth_10 = awb2_1_illhealth_10 - 1,
         TMPVAR_awb2_1_illhealth_11 = awb2_1_illhealth_11 - 1,
         TMPVAR_awb2_1_illhealth_12 = awb2_1_illhealth_12 - 1,
         TMPVAR_awb2_1_illhealth_13 = awb2_1_illhealth_13 - 1,
         TMPVAR_awb2_1_illhealth_14 = awb2_1_illhealth_14 - 1,
         TMPVAR_awb2_1_illhealth_15 = awb2_1_illhealth_15 - 1,
         TMPVAR_awb2_1_illhealth_16 = awb2_1_illhealth_16 - 1,
         TMPVAR_awb2_1_illhealth_17 = awb2_1_illhealth_17 - 1,
         TMPVAR_awb2_1_illhealth_18 = awb2_1_illhealth_18 - 1,
         TMPVAR_awb2_1_illhealth_19 = awb2_1_illhealth_19 - 1,
         TMPVAR_awb2_1_illhealth_20 = awb2_1_illhealth_20 - 1,
         TMPVAR_awb2_1_illhealth_21 = awb2_1_illhealth_21 - 1,
         TMPVAR_awb2_1_illhealth_22 = awb2_1_illhealth_22 - 1,
         TMPVAR_awb2_1_illhealth_23 = awb2_1_illhealth_23 - 1,
         TMPVAR_awb2_1_illhealth_24 = awb2_1_illhealth_24 - 1,
         TMPVAR_awb2_1_illhealth_25 = awb2_1_illhealth_25 - 1) %>%
  mutate(TMPVAR_awb2_4_loneliness_1 = awb2_4_loneliness_1 - 1,
         TMPVAR_awb2_4_loneliness_2 = awb2_4_loneliness_2 - 1,
         TMPVAR_awb2_4_loneliness_3 = awb2_4_loneliness_3 - 1,
         TMPVAR_awb2_4_loneliness_4 = awb2_4_loneliness_4 - 1) %>%
  mutate(TMPVAR_awb2_5_social_spprt_1 = case_match(awb2_5_social_spprt_1, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_2 = case_match(awb2_5_social_spprt_2, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_3 = case_match(awb2_5_social_spprt_3, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_4 = case_match(awb2_5_social_spprt_4, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_5 = case_match(awb2_5_social_spprt_5, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_6 = case_match(awb2_5_social_spprt_6, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_7 = case_match(awb2_5_social_spprt_7, 1~2, 2~1, 3~0),
         TMPVAR_awb2_5_social_spprt_8 = case_match(awb2_5_social_spprt_8, 1~2, 2~1, 3~0)) %>%
  mutate(TMPVAR_awb2_9_resil2_a5 = case_match(awb2_9_resil2_a5, 1~5, 2~4, 3~3, 4~2, 5~1),
         TMPVAR_awb2_9_resil4_a5 = case_match(awb2_9_resil2_a5, 1~5, 2~4, 3~3, 4~2, 5~1),
         TMPVAR_awb2_9_resil6_a5 = case_match(awb2_9_resil2_a5, 1~5, 2~4, 3~3, 4~2, 5~1)) %>%
  mutate(TMPVAR_awb2_11_psychosis_3_r4 = ifelse(awb2_11_psychosis_3_r4 == 3, 0, awb2_11_psychosis_3_r4),
         TMPVAR_awb2_11_psychosis_5_r4 = ifelse(awb2_11_psychosis_5_r4 == 3, 0, awb2_11_psychosis_5_r4),
         TMPVAR_awb2_11_psychosis_2_r4 = ifelse(awb2_11_psychosis_2_r4 == 3, 0, awb2_11_psychosis_2_r4),
         TMPVAR_awb2_11_psychosis_10_r4 = ifelse(awb2_11_psychosis_10_r4 == 3, 0, awb2_11_psychosis_10_r4),
         TMPVAR_awb2_11_pwrs_read_a4 = ifelse(awb2_11_pwrs_read_a4 == 3, 0, awb2_11_pwrs_read_a4),
         TMPVAR_awb2_11_psychosis_1_r4 = ifelse(awb2_11_psychosis_1_r4 == 3, 0, awb2_11_psychosis_1_r4),
         TMPVAR_awb2_11_psychosis_4_r4 = ifelse(awb2_11_psychosis_4_r4 == 3, 0, awb2_11_psychosis_4_r4),
         TMPVAR_awb2_11_psychosis_9_r4 = ifelse(awb2_11_psychosis_9_r4 == 3, 0, awb2_11_psychosis_9_r4)) %>%
  mutate(TMPVAR_awb2_9_seek_hlp_ppl_1_r4 = ifelse(awb2_9_seek_hlp_ppl_1_r4 == 8, 0, awb2_9_seek_hlp_ppl_1_r4))


rcads_lookup <- read_csv("resources/RCADS25_C_lookup_table.csv")

rcads_lookup <- rcads_lookup %>%
  rename(year_group = YearGroup) %>%
  rename(gender = Sex) %>%
  mutate(gender = ifelse(gender == "Female",1,2))

rcads_lookup$year_group <- as.character(rcads_lookup$year_group)

swemwbs_lookup <- read_csv("resources/SWEMWBS_lookup_table.csv")


#----------------Compute DVs

#RCADS-25
#sum scores 
module <-
  module %>%
  rowwise() %>%
  mutate(rcad_ga = sum(c(TMPVAR_awb2_1_illhealth_2,                      
                                       TMPVAR_awb2_1_illhealth_3,                     
                                       TMPVAR_awb2_1_illhealth_5,
                                       TMPVAR_awb2_1_illhealth_6,                      
                                       TMPVAR_awb2_1_illhealth_7,
                                       TMPVAR_awb2_1_illhealth_9,
                                       TMPVAR_awb2_1_illhealth_11,                     
                                       TMPVAR_awb2_1_illhealth_12,
                                       TMPVAR_awb2_1_illhealth_14,
                                       TMPVAR_awb2_1_illhealth_17,                      
                                       TMPVAR_awb2_1_illhealth_18,
                                       TMPVAR_awb2_1_illhealth_20,
                                       TMPVAR_awb2_1_illhealth_22,                      
                                       TMPVAR_awb2_1_illhealth_23,
                                       TMPVAR_awb2_1_illhealth_25), na.rm = TRUE),
         rcad_md = sum(c(TMPVAR_awb2_1_illhealth_1,
                                       TMPVAR_awb2_1_illhealth_4,
                                       TMPVAR_awb2_1_illhealth_8,
                                       TMPVAR_awb2_1_illhealth_10,
                                       TMPVAR_awb2_1_illhealth_13,
                                       TMPVAR_awb2_1_illhealth_15,
                                       TMPVAR_awb2_1_illhealth_16,
                                       TMPVAR_awb2_1_illhealth_19,
                                       TMPVAR_awb2_1_illhealth_21,
                                       TMPVAR_awb2_1_illhealth_24), na.rm = TRUE),
         rcad_total = sum(c(rcad_ga,
                            rcad_md)),
         rcad_nas = sum(is.na(c(TMPVAR_awb2_1_illhealth_1,
                                              TMPVAR_awb2_1_illhealth_2,
                                              TMPVAR_awb2_1_illhealth_3,
                                              TMPVAR_awb2_1_illhealth_4,
                                              TMPVAR_awb2_1_illhealth_5,
                                              TMPVAR_awb2_1_illhealth_6,
                                              TMPVAR_awb2_1_illhealth_7,
                                              TMPVAR_awb2_1_illhealth_8,
                                              TMPVAR_awb2_1_illhealth_9,
                                              TMPVAR_awb2_1_illhealth_10,
                                              TMPVAR_awb2_1_illhealth_11,
                                              TMPVAR_awb2_1_illhealth_12,
                                              TMPVAR_awb2_1_illhealth_13,
                                              TMPVAR_awb2_1_illhealth_14,
                                              TMPVAR_awb2_1_illhealth_15,
                                              TMPVAR_awb2_1_illhealth_16,
                                              TMPVAR_awb2_1_illhealth_17,
                                              TMPVAR_awb2_1_illhealth_18,
                                              TMPVAR_awb2_1_illhealth_19,
                                              TMPVAR_awb2_1_illhealth_20,
                                              TMPVAR_awb2_1_illhealth_21,
                                              TMPVAR_awb2_1_illhealth_22,
                                              TMPVAR_awb2_1_illhealth_23,
                                              TMPVAR_awb2_1_illhealth_24,
                                              TMPVAR_awb2_1_illhealth_25))),
         rcad_ga_miss = sum(is.na(c(TMPVAR_awb2_1_illhealth_2,                      
                                                  TMPVAR_awb2_1_illhealth_3,                     
                                                  TMPVAR_awb2_1_illhealth_5,
                                                  TMPVAR_awb2_1_illhealth_6,                      
                                                  TMPVAR_awb2_1_illhealth_7,
                                                  TMPVAR_awb2_1_illhealth_9,
                                                  TMPVAR_awb2_1_illhealth_11,                     
                                                  TMPVAR_awb2_1_illhealth_12,
                                                  TMPVAR_awb2_1_illhealth_14,
                                                  TMPVAR_awb2_1_illhealth_17,                      
                                                  TMPVAR_awb2_1_illhealth_18,
                                                  TMPVAR_awb2_1_illhealth_20,
                                                  TMPVAR_awb2_1_illhealth_22,                      
                                                  TMPVAR_awb2_1_illhealth_23,
                                                  TMPVAR_awb2_1_illhealth_25))),
         rcad_md_miss = sum(is.na(c(TMPVAR_awb2_1_illhealth_1,
                                                  TMPVAR_awb2_1_illhealth_4,
                                                  TMPVAR_awb2_1_illhealth_8,
                                                  TMPVAR_awb2_1_illhealth_10,
                                                  TMPVAR_awb2_1_illhealth_13,
                                                  TMPVAR_awb2_1_illhealth_15,
                                                  TMPVAR_awb2_1_illhealth_16,
                                                  TMPVAR_awb2_1_illhealth_19,
                                                  TMPVAR_awb2_1_illhealth_21,
                                                  TMPVAR_awb2_1_illhealth_24))),
         rcad_missing = ifelse(rcad_nas == 25, 1, 0)) %>%
  mutate(rcad_ga = ifelse(rcad_ga_miss <= 3, rcad_ga/(15-rcad_ga_miss)*15, NA),
         rcad_md = ifelse(rcad_md_miss <= 2, rcad_md/(10-rcad_md_miss)*10, NA),
         rcad_total = ifelse(rcad_nas <= 4, rcad_total/(25-rcad_nas)*25, NA)) %>%
  left_join(rcads_lookup, by = c(year_group = "year_group", gender = "gender")) %>%
  mutate(rcad_md_t = ((rcad_md- depression_int)*10)/depression_factor + 50,
         rcad_ga_t = ((rcad_ga - anxiety_int)*10)/anxiety_factor + 50,
         rcad_total_t = ((rcad_total - total_int)*10)/total_factor + 50,
         rcad_md_cat = ifelse(rcad_md_t < 65, "Normal",
                              ifelse(rcad_md_t < 70, "Borderline", "Clinical")),
         rcad_ga_cat = ifelse(rcad_ga_t < 65, "Normal",
                              ifelse(rcad_ga_t < 70, "Borderline", "Clinical")),
         rcad_total_cat = ifelse(rcad_total_t < 65, "Normal",
                                 ifelse(rcad_total_t < 70, "Borderline", "Clinical"))) %>%
  #SWEMWBs
  #sum scores
  mutate(wellbeing = sum(c(awb2_2_optmstc_1_a4,
                                         awb2_2_useful_2_a4,
                                         awb2_2_relxed_3_a4,
                                         awb2_2_problems_4_a4,
                                         awb2_2_think_clr_5_a4,
                                         awb2_2_close_othrs_6_a4,
                                         awb2_2_own_mnd_7_a4,
                                         awb2_3_self_effccy), na.rm = TRUE),
         wellbeing_nas = sum(is.na(c(awb2_2_optmstc_1_a4,
                                                   awb2_2_useful_2_a4,
                                                   awb2_2_relxed_3_a4,
                                                   awb2_2_problems_4_a4,
                                                   awb2_2_think_clr_5_a4,
                                                   awb2_2_close_othrs_6_a4,
                                                   awb2_2_own_mnd_7_a4,
                                                   awb2_3_self_effccy))),
         wellbeing_missing = ifelse(wellbeing_nas == 7, 1, 0)) %>%
  #compute scores
  left_join(swemwbs_lookup) %>%
  mutate(swemwbs_cat = ifelse(swemwbs_total <= 19.5, "Low", 
                              ifelse(swemwbs_total < 27.5, "Normal", "High"))) %>%
  #ULS-4
  #sum scores
  mutate(loneliness = sum(c(TMPVAR_awb2_4_loneliness_1,
                                          TMPVAR_awb2_4_loneliness_2,
                                          TMPVAR_awb2_4_loneliness_3,
                                          TMPVAR_awb2_4_loneliness_4), na.rm = TRUE),
         loneliness_nas = sum(is.na(c(TMPVAR_awb2_4_loneliness_1,
                                                    TMPVAR_awb2_4_loneliness_2,
                                                    TMPVAR_awb2_4_loneliness_3,
                                                    TMPVAR_awb2_4_loneliness_4))),
         loneliness_missing = ifelse(loneliness_nas == 4, 1, 0)) %>%
  #GHSQ
  #emotional subscales
  mutate(emo_inf = sum(c(TMPVAR_awb2_9_seek_hlp_ppl_1_r4,
                                       awb2_9_seek_hlp_ppl_2,
                                       awb2_9_seek_hlp_ppl_3,
                                       awb2_9_seek_hlp_ppl_4), na.rm = TRUE),
         emo_form = sum(c(awb2_9_seek_hlp_ppl_5,
                                        awb2_9_seek_hlp_ppl_6,
                                        awb2_9_seek_hlp_ppl_7,
                                        awb2_9_seek_hlp_ppl_8), na.rm = TRUE),
         emo_sch = awb2_9_seek_hlp_ppl_9_a_4,
         #totals
         emo_total = sum(c(emo_inf, emo_form, emo_sch), na.rm = TRUE),
         emo_nas = sum(is.na(c(TMPVAR_awb2_9_seek_hlp_ppl_1_r4,
                                             awb2_9_seek_hlp_ppl_2,
                                             awb2_9_seek_hlp_ppl_3,
                                             awb2_9_seek_hlp_ppl_4,
                                             awb2_9_seek_hlp_ppl_5,
                                             awb2_9_seek_hlp_ppl_6,
                                             awb2_9_seek_hlp_ppl_7,
                                             awb2_9_seek_hlp_ppl_8,
                                             awb2_9_seek_hlp_ppl_9_a_4,
                                             awb2_9_seek_hlp_ppl_10))),
         emo_missing = ifelse(emo_nas == 10, 1, 0)) %>%
  #BRS
  #sum scores
  mutate(brs_total = sum(c(awb2_9_resil1_a5,
                                         TMPVAR_awb2_9_resil2_a5,
                                         awb2_9_resil3_a5,
                                         TMPVAR_awb2_9_resil4_a5,
                                         awb2_9_resil5_a5,
                                         TMPVAR_awb2_9_resil6_a5), na.rm = TRUE),
         brs_nas = sum(is.na(c(awb2_9_resil1_a5,
                                            TMPVAR_awb2_9_resil2_a5,
                                            awb2_9_resil3_a5,
                                            TMPVAR_awb2_9_resil4_a5,
                                            awb2_9_resil5_a5,
                                            TMPVAR_awb2_9_resil6_a5))),
         brs_missing = ifelse(brs_nas == 6, 1, 0)) %>%
  #compute_scores %>%
  mutate(brs_mean = brs_total/6,
         brs_cat = ifelse(brs_mean < 3, "Low",
                          ifelse(brs_mean <= 4.3, "Normal", "High"))) %>%
  #MSPSS
  #subscales
  mutate(soc_sup_fam = sum(c(TMPVAR_awb2_5_social_spprt_3,                  #####SOCIAL SUPPORT#####
                             TMPVAR_awb2_5_social_spprt_4,
                             TMPVAR_awb2_5_social_spprt_6,
                             TMPVAR_awb2_5_social_spprt_8), na.rm = TRUE),
         soc_sup_frnd = sum(c(TMPVAR_awb2_5_social_spprt_1,
                              TMPVAR_awb2_5_social_spprt_2,
                              TMPVAR_awb2_5_social_spprt_5,
                              TMPVAR_awb2_5_social_spprt_7), na.rm = TRUE),
         #totals
         soc_sup_total = sum(soc_sup_fam, soc_sup_frnd),
         soc_sup_nas = sum(is.na(c(TMPVAR_awb2_5_social_spprt_1,
                                                 TMPVAR_awb2_5_social_spprt_2,
                                                 TMPVAR_awb2_5_social_spprt_3,
                                                 TMPVAR_awb2_5_social_spprt_4,
                                                 TMPVAR_awb2_5_social_spprt_5,
                                                 TMPVAR_awb2_5_social_spprt_6,
                                                 TMPVAR_awb2_5_social_spprt_7,
                                                 TMPVAR_awb2_5_social_spprt_8))),
         soc_sup_missing = ifelse(soc_sup_nas == 8, 1, 0))

module <- module |> select(-starts_with("TMPVAR_"))

# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_derived.rds")



  
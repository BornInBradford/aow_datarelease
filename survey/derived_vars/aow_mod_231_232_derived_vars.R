# Modules 231 and 232 survey derived variables

# easier to split into two different datasets, one for each module
# only if each dv only needs vars from one module
# makes admin data much easier to deal with and link
# would like these to be standalone as many will want just these vars 
# and linkage needed to the other two modules could cause confusion among users

source("tools/aow_survey_functions.R")

module231 <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_labelled.rds")
module232 <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_labelled.rds")

# joining cols - admin vars used to join modules and to derive some variables
joining_cols <- c("aow_recruitment_id", "year_group", "gender")

# admin cols, leaving out joining cols
admin_cols <- aow_survey_column_order()[!aow_survey_column_order() %in% joining_cols]

# survey cols, leaving out joining cols
survey_cols <- c(names(module231), names(module232))[!c(names(module231), names(module232)) %in% joining_cols]

# start combining module survey data
module231 <- module231 |> select(-all_of(admin_cols))
module232 <- module232 |> select(-all_of(admin_cols))

# combined surveys
module_all <- module231 |> full_join(module232, by = joining_cols)

# check duplicates
module_all$aow_recruitment_id |> duplicated() |> any()

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

# CHANGES from mod 2
#
# MSPSS dropped as awb2_5_social_spprt vars dropped

# CHANGES from mod 3
#
# No changes

# CHANGES from mod 4
#
# PATT-SQ dropped as awb6_8_attd_tech vars dropped

################################################################################
# dvs inherited from module 1 in 2023 release

# Sum derived variables

module_all <-
  module_all %>%
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
# dvs inherited from module 2 in 2023 release

module_all <- module_all %>%
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


#RCADS-25
#sum scores 
module_all <-
  module_all %>%
  mutate(rcad_ga = remove_na(TMPVAR_awb2_1_illhealth_2) +                      
           remove_na(TMPVAR_awb2_1_illhealth_3) +                  
           remove_na(TMPVAR_awb2_1_illhealth_5) +
           remove_na(TMPVAR_awb2_1_illhealth_6) +                      
           remove_na(TMPVAR_awb2_1_illhealth_7) +
           remove_na(TMPVAR_awb2_1_illhealth_9) +
           remove_na(TMPVAR_awb2_1_illhealth_11) +                     
           remove_na(TMPVAR_awb2_1_illhealth_12) +
           remove_na(TMPVAR_awb2_1_illhealth_14) +
           remove_na(TMPVAR_awb2_1_illhealth_17) +                      
           remove_na(TMPVAR_awb2_1_illhealth_18) +
           remove_na(TMPVAR_awb2_1_illhealth_20) +
           remove_na(TMPVAR_awb2_1_illhealth_22) +                      
           remove_na(TMPVAR_awb2_1_illhealth_23) +
           remove_na(TMPVAR_awb2_1_illhealth_25),
         rcad_md = remove_na(TMPVAR_awb2_1_illhealth_1) +
           remove_na(TMPVAR_awb2_1_illhealth_4) +
           remove_na(TMPVAR_awb2_1_illhealth_8) +
           remove_na(TMPVAR_awb2_1_illhealth_10) +
           remove_na(TMPVAR_awb2_1_illhealth_13) +
           remove_na(TMPVAR_awb2_1_illhealth_15) +
           remove_na(TMPVAR_awb2_1_illhealth_16) +
           remove_na(TMPVAR_awb2_1_illhealth_19) +
           remove_na(TMPVAR_awb2_1_illhealth_21) +
           remove_na(TMPVAR_awb2_1_illhealth_24),
         rcad_total = rcad_ga +  rcad_md,
         rcad_nas = is.na(TMPVAR_awb2_1_illhealth_1) +
           is.na(TMPVAR_awb2_1_illhealth_2) +
           is.na(TMPVAR_awb2_1_illhealth_3) +
           is.na(TMPVAR_awb2_1_illhealth_4) +
           is.na(TMPVAR_awb2_1_illhealth_5) +
           is.na(TMPVAR_awb2_1_illhealth_6) +
           is.na(TMPVAR_awb2_1_illhealth_7) +
           is.na(TMPVAR_awb2_1_illhealth_8) +
           is.na(TMPVAR_awb2_1_illhealth_9) +
           is.na(TMPVAR_awb2_1_illhealth_10) +
           is.na(TMPVAR_awb2_1_illhealth_11) +
           is.na(TMPVAR_awb2_1_illhealth_12) +
           is.na(TMPVAR_awb2_1_illhealth_13) +
           is.na(TMPVAR_awb2_1_illhealth_14) +
           is.na(TMPVAR_awb2_1_illhealth_15) +
           is.na(TMPVAR_awb2_1_illhealth_16) +
           is.na(TMPVAR_awb2_1_illhealth_17) +
           is.na(TMPVAR_awb2_1_illhealth_18) +
           is.na(TMPVAR_awb2_1_illhealth_19) +
           is.na(TMPVAR_awb2_1_illhealth_20) +
           is.na(TMPVAR_awb2_1_illhealth_21) +
           is.na(TMPVAR_awb2_1_illhealth_22) +
           is.na(TMPVAR_awb2_1_illhealth_23) +
           is.na(TMPVAR_awb2_1_illhealth_24) +
           is.na(TMPVAR_awb2_1_illhealth_25),
         rcad_ga_miss = is.na(TMPVAR_awb2_1_illhealth_2) +                     
           is.na(TMPVAR_awb2_1_illhealth_3) +                    
           is.na(TMPVAR_awb2_1_illhealth_5) +
           is.na(TMPVAR_awb2_1_illhealth_6) +                    
           is.na(TMPVAR_awb2_1_illhealth_7) +
           is.na(TMPVAR_awb2_1_illhealth_9) +
           is.na(TMPVAR_awb2_1_illhealth_11) +                    
           is.na(TMPVAR_awb2_1_illhealth_12) +
           is.na(TMPVAR_awb2_1_illhealth_14) +
           is.na(TMPVAR_awb2_1_illhealth_17) +                    
           is.na(TMPVAR_awb2_1_illhealth_18) +
           is.na(TMPVAR_awb2_1_illhealth_20) +
           is.na(TMPVAR_awb2_1_illhealth_22) +                    
           is.na(TMPVAR_awb2_1_illhealth_23) +
           is.na(TMPVAR_awb2_1_illhealth_25),
         rcad_md_miss = is.na(TMPVAR_awb2_1_illhealth_1) +
           is.na(TMPVAR_awb2_1_illhealth_4) +
           is.na(TMPVAR_awb2_1_illhealth_8) +
           is.na(TMPVAR_awb2_1_illhealth_10) +
           is.na(TMPVAR_awb2_1_illhealth_13) +
           is.na(TMPVAR_awb2_1_illhealth_15) +
           is.na(TMPVAR_awb2_1_illhealth_16) +
           is.na(TMPVAR_awb2_1_illhealth_19) +
           is.na(TMPVAR_awb2_1_illhealth_21) +
           is.na(TMPVAR_awb2_1_illhealth_24),
         rcad_missing = ifelse(rcad_nas == 25, 1, 0),
         rcad_ga = ifelse(rcad_ga_miss == 15, NA, rcad_ga),
         rcad_md = ifelse(rcad_md_miss == 10, NA, rcad_md),
         rcad_total = ifelse(rcad_missing == 1, NA, rcad_total)) %>%
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
  mutate(wellbeing = remove_na(awb2_2_optmstc_1_a4) +
           remove_na(awb2_2_useful_2_a4) +
           remove_na(awb2_2_relxed_3_a4) +
           remove_na(awb2_2_problems_4_a4) +
           remove_na(awb2_2_think_clr_5_a4) +
           remove_na(awb2_2_close_othrs_6_a4) +
           remove_na(awb2_2_own_mnd_7_a4) +
           remove_na(awb2_3_self_effccy),
         wellbeing_nas = is.na(awb2_2_optmstc_1_a4) +
           is.na(awb2_2_useful_2_a4) +
           is.na(awb2_2_relxed_3_a4) +
           is.na(awb2_2_problems_4_a4) +
           is.na(awb2_2_think_clr_5_a4) +
           is.na(awb2_2_close_othrs_6_a4) +
           is.na(awb2_2_own_mnd_7_a4) +
           is.na(awb2_3_self_effccy),
         wellbeing_missing = ifelse(wellbeing_nas == 7, 1, 0),
         wellbeing = ifelse(wellbeing_missing == 1, NA, wellbeing))%>%
  #compute scores
  left_join(swemwbs_lookup) %>%
  mutate(swemwbs_cat = ifelse(swemwbs_total <= 19.5, "Low", 
                              ifelse(swemwbs_total < 27.5, "Normal", "High"))) %>%
  #ULS-4
  #sum scores
  mutate(loneliness = remove_na(TMPVAR_awb2_4_loneliness_1) +
           remove_na(TMPVAR_awb2_4_loneliness_2) +
           remove_na(TMPVAR_awb2_4_loneliness_3) +
           remove_na(TMPVAR_awb2_4_loneliness_4),
         loneliness_nas = is.na(TMPVAR_awb2_4_loneliness_1) +
           is.na(TMPVAR_awb2_4_loneliness_2) +
           is.na(TMPVAR_awb2_4_loneliness_3) +
           is.na(TMPVAR_awb2_4_loneliness_4),
         loneliness_missing = ifelse(loneliness_nas == 4, 1, 0),
         loneliness = ifelse(loneliness_missing == 1, NA, loneliness)) %>%
  #GHSQ
  #emotional subscales
  mutate(emo_inf = remove_na(TMPVAR_awb2_9_seek_hlp_ppl_1_r4) +
           remove_na(awb2_9_seek_hlp_ppl_2) +
           remove_na(awb2_9_seek_hlp_ppl_3) +
           remove_na(awb2_9_seek_hlp_ppl_4),
         emo_form = remove_na(awb2_9_seek_hlp_ppl_5) +
           remove_na(awb2_9_seek_hlp_ppl_6) +
           remove_na(awb2_9_seek_hlp_ppl_7) +
           remove_na(awb2_9_seek_hlp_ppl_8),
         emo_sch = remove_na(awb2_9_seek_hlp_ppl_9_a_4),
         #totals
         emo_total = emo_inf + emo_form + emo_sch,
         emo_nas = is.na(TMPVAR_awb2_9_seek_hlp_ppl_1_r4) +
           is.na(awb2_9_seek_hlp_ppl_2) +
           is.na(awb2_9_seek_hlp_ppl_3) +
           is.na(awb2_9_seek_hlp_ppl_4) +
           is.na(awb2_9_seek_hlp_ppl_5) +
           is.na(awb2_9_seek_hlp_ppl_6) +
           is.na(awb2_9_seek_hlp_ppl_7) +
           is.na(awb2_9_seek_hlp_ppl_8) +
           is.na(awb2_9_seek_hlp_ppl_9_a_4) +
           is.na(awb2_9_seek_hlp_ppl_10),
         emo_missing = ifelse(emo_nas == 10, 1, 0),
         emo_inf = ifelse(emo_missing == 1, NA, emo_inf),
         emo_form = ifelse(emo_missing == 1, NA, emo_form),
         emo_sch = ifelse(emo_missing == 1, NA, emo_sch),
         emo_total = ifelse(emo_missing == 1, NA, emo_total)) %>%
  #BRS
  #sum scores
  mutate(brs_total = remove_na(awb2_9_resil1_a5) +
           remove_na(TMPVAR_awb2_9_resil2_a5) +
           remove_na(awb2_9_resil3_a5) +
           remove_na(TMPVAR_awb2_9_resil4_a5) +
           remove_na(awb2_9_resil5_a5) +
           remove_na(TMPVAR_awb2_9_resil6_a5),
         brs_nas = is.na(awb2_9_resil1_a5) +
           is.na(TMPVAR_awb2_9_resil2_a5) +
           is.na(awb2_9_resil3_a5) +
           is.na(TMPVAR_awb2_9_resil4_a5) +
           is.na(awb2_9_resil5_a5) +
           is.na(TMPVAR_awb2_9_resil6_a5),
         brs_missing = ifelse(brs_nas == 6, 1, 0),
         brs_total = ifelse(brs_missing == 1, NA, brs_total)) %>%
  #compute_scores %>%
  mutate(brs_mean = brs_total/6,
         brs_cat = ifelse(brs_mean < 3, "Low",
                          ifelse(brs_mean <= 4.3, "Normal", "High")))




################################################################################
# dvs inherited from module 3 in 2023 release

module_all <-
  module_all %>%
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

module_all <-
  module_all %>%
  
  
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
  mutate(edeqs_cat = ifelse(edeqs_total < 15, "normal", "possible disorder")) 

module_all <- module_all %>%
  
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

module_all <- module_all %>%
  
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







################################################################################
# dvs inherited from module 4 in 2023 release

module_all <-
  module_all %>%
  
  #ADDI
  #sum scores
  mutate(addi_inst_exp = remove_na(awb8_2_age_3) +
           remove_na(awb8_2_police_5) +
           remove_na(awb8_2_shop_6) +
           remove_na(awb8_2_service_8) +
           remove_na(awb8_2_int_9) +
           remove_na(awb8_2_afraid_10),
         addi_peer_exp = remove_na(awb8_2_club_1) +
           remove_na(awb8_2_excl_2) +
           remove_na(awb8_2_lang_4) +
           remove_na(awb8_2_names_7) +
           remove_na(awb8_2_threat_11),
         addi_total = addi_inst_exp + addi_peer_exp,
         addi_nas = is.na(awb8_2_age_3) +
           is.na(awb8_2_police_5) +
           is.na(awb8_2_shop_6) +
           is.na(awb8_2_service_8) +
           is.na(awb8_2_int_9) +
           is.na(awb8_2_afraid_10) +
           is.na(awb8_2_club_1) +
           is.na(awb8_2_excl_2) +
           is.na(awb8_2_lang_4) +
           is.na(awb8_2_names_7) +
           is.na(awb8_2_threat_11),
         addi_missing = ifelse(addi_nas == 11, 1, 0))


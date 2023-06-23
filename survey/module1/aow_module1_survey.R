# Appending AoW Online and Offline Module 1 Survey
# Amy Hough
# 04/04/2023

# packages
library(tidyverse)
library(haven)
library(labelled)

# data
online <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module1_Online.dta")
offline <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module1_Offline.dta")

# data dictionary
online_dict <- read_csv("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data_dictionary\\AoWModule1OnlineSurvey_DataDictionary_2023-06-15.csv")
offline_dict <- read_csv("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data_dictionary\\AoWModule1OfflineForm_DataDictionary_2023-06-15.csv")

# add survey indicator to each dataset --- label values once appended 
online <- online %>% mutate(survey_type = 1) # 1=online
offline <- offline %>% mutate(survey_type = 2) # 2=offline

# determine which cols are in both and which are in one survey only
on_col <- as.data.frame(colnames(online))
on_col <- on_col %>% rename(names = `colnames(online)`) %>% mutate(has_on = 1)
off_col <- as.data.frame(colnames(offline))
off_col <- off_col %>% rename(names = `colnames(offline)`) %>% mutate(has_off = 1)
on_off <- full_join(on_col, off_col, by = "names")
on_off <- on_off %>% mutate_at(vars(2:3), ~replace(., is.na(.), 0)) %>% mutate(has_1 = ifelse(has_on == 1 & has_off == 1,0,1))
on_off <- on_off %>% filter(!has_1 == 0)
# change variable names
online <- online %>%
  rename(aow_id = redcap_survey_identifier) %>%
  rename(date_time_collection = module_1_timestamp) %>%
  rename(awb2_6_family_brth_n_a5 = awb_2_6_family_brth_n_a5) %>%
  set_variable_labels(aow_id = "AoW Person ID",
                      date_time_collection = "Date & time of collection",
                      awb2_6_family_brth_n_a5 = "Select your birth order. I am...")
  
offline <- offline %>%
  rename(aw1_2_years_lvd_a4 = aw1_2_years_lvd_r4) %>%
  rename(awb1_2_ethnicity_arb_a4 = awb1_2_ethnicity_arb_r4) %>%
  set_variable_labels(aw1_2_years_lvd_a4 = "How many years have you lived in the UK?",
                      awb1_2_ethnicity_arb_a4 = "Arab") %>%
  rename(awb3_activities_1 = awb3_4_activities_1,
         awb3_activities_2 = awb3_4_activities_2,
         awb3_activities_3 = awb3_4_activities_3,
         awb3_activities_4 = awb3_4_activities_4,
         awb3_activities_5 = awb3_4_activities_5,
         awb3_activities_6 = awb3_4_activities_6,
         awb3_activities_7 = awb3_4_activities_7,
         awb3_activities_8 = awb3_4_activities_8,
         awb3_activities_9 = awb3_4_activities_9,
         awb3_activities_10 = awb3_4_activities_10,
         awb3_activities_11 = awb3_4_activities_11,
         awb3_activities_12 = awb3_4_activities_12,
         awb3_activities_13 = awb3_4_activities_13,
         awb3_activities_14 = awb3_4_activities_14,
         awb3_activities_15 = awb3_4_activities_15,
         awb3_activities_16 = awb3_4_activities_16,
         awb3_activities_17 = awb3_4_activities_17,
         awb3_activities_18 = awb3_4_activities_18)

# create new cols in awb3_4_talent_in_arts for __1,__2:5 (strongly disagree:strongly agree)
offline <- offline %>%
  mutate(awb3_4_talent_in_arts___1 = ifelse(awb3_4_talent_in_arts == 1,1,0)) %>%
  mutate(awb3_4_talent_in_arts___2 = ifelse(awb3_4_talent_in_arts == 2,1,0)) %>%
  mutate(awb3_4_talent_in_arts___3 = ifelse(awb3_4_talent_in_arts == 3,1,0)) %>%
  mutate(awb3_4_talent_in_arts___4 = ifelse(awb3_4_talent_in_arts == 4,1,0)) %>%
  mutate(awb3_4_talent_in_arts___5 = ifelse(awb3_4_talent_in_arts == 5,1,0)) %>%
  set_variable_labels(awb3_4_talent_in_arts___1 = "I have great talent in art. Strongly disagree.",
                      awb3_4_talent_in_arts___2 = "I have great talent in art. Disagree",
                      awb3_4_talent_in_arts___3 = "I have great talent in art. Neither agree/disagree",
                      awb3_4_talent_in_arts___4 = "I have great talent in art. Agree",
                      awb3_4_talent_in_arts___5 = "I have great talent in art. Strongly agree") %>%
  select(-awb3_4_talent_in_arts)

# append online and offline 
mod1 <- rbind(online, offline)

# relabel any variables where label is too long
mod1 <- mod1 %>%
  set_variable_labels(awb1_1_id = "Participant Study ID",
                      aow_id = "Age of Wonder ID",
                      date_time_collection = "Date and time of survey collection",
                      mod1_version = "Module 1 version",
                      year_group = "School year group",
                      aw1_2_name_a4 = "4) What is the first letter of your surname?",
                      aw1_2_dob_r4 = "5) What day of the month is your birthday?",
                      awb1_2_country_brth = "7) What is your country of birth?",
                      awb1_2_date_emgrtd = "8) What date did you most recently arrive in the UK?",
                      aw1_2_years_lvd_a4 = "9) How many years have you lived in the UK?",
                      awb1_2_date_emgrtd_year_a3 = "10) What year did you most recently arrive in the UK?",
                      awb1_2_date_emgrtd_month_a3 = "11) What month did you most recently arrive in the UK?",
                      awb1_2_uknation_idntty_1___1 = "12) Describe your national identitity: British",
                      awb1_2_uknation_idntty_1___2 = "12) Describe your national identity: English",
                      awb1_2_uknation_idntty_1___3 = "12) Describe your national identity: Welsh",
                      awb1_2_uknation_idntty_1___4 = "12) Describe your national identity: Scottish",
                      awb1_2_uknation_idntty_1___5 = "12) Describe your national identity: Northern Irish",
                      awb1_2_uknation_idntty_1___6 = "12) Describe your national identity: Other",
                      awb1_2_uknation_idntty_2 = "13) Other national identity, please describe",
                      awb1_2_ethnicity = "15) What is your ethnic group?",
                      awb1_2_ethnicity_r4 = "16) What is your ethnicity?",
                      awb1_2_ethnicity_whte = "17) Ethnicity: white",
                      awb1_2_ethnicity_whte_othr = "18) Ethnicity: white other",
                      awb1_2_ethnicity_mix = "19) Ethnicity: Mixed or multiple ethnic groups",
                      awb1_2_ethnicity_mix_othr = "20) Ethnicity: Mixed or multiple ethnic groups other",
                      awb1_2_ethnicity_asn = "21) Ethnicity: Asian or Asian British",
                      awb1_2_ethnicity_asn_othr = "22) Ethnicity: Other Asian background",
                      awb1_2_ethnicity_blck = "23) Ethnicity: Black, Black British, Caribbean or African",
                      awb1_2_ethnicity_blck_afrcn = "24) Ethnicity: Other African background",
                      awb1_2_ethnicity_whte_crrbn = "25) Ethnicity: Other Black/Black British/Caribbean background",
                      awb1_2_ethnicity_arb_a4 = "26) Ethnicity: Arab",
                      awb1_2_ethnicity_othr = "27) Ethnicity: Other ethnic groups",
                      awb1_2_ethnicity_othr_othr = "28) Ethnicity: Any other ethnic groups",
                      awb1_2_language_hme___1 = "30) Language spoken at home: English",
                      awb1_2_language_hme___2 = "30) Language spoken at home: Urdu",
                      awb1_2_language_hme___3 = "30) Language spoken at home: Punjabi",
                      awb1_2_language_hme___4 = "30) Language spoken at home: Gujarati",
                      awb1_2_language_hme___5 = "30) Language spoken at home: Bengali",
                      awb1_2_language_hme___6 = "30) Language spoken at home: Hindko",
                      awb1_2_language_hme___7 = "30) Language spoken at home: Polish",
                      awb1_2_language_hme___8 = "30) Language spoken at home: Pashto",
                      awb1_2_language_hme___9 = "30) Language spoken at home: Other",
                      awb1_2_language_hme_othr = "31) Language spoken at home: Other",
                      awb1_2_religion = "33) Do you consider yourself to have a religion?",
                      awb1_2y_religion = "34) What is your religion?",
                      awb1_2y_religion_r4 = "35) What is your religion?",
                      awb1_2y_religion_othr = "36) Other religion, please specify",
                      awb1_2_sex = "37) What is your biological sex on your birth certificate?",
                      awb1_2_sex_othr = "38) What is your biological sex? Self describe",
                      awb1_2_gender_r4 = "39) What is your gender?",
                      awb1_2_gender_othr_r4 = "40) What is your gender? Self describe",
                      awb1_2_gender = "41) Is the gender you identify with the same as your sex registered at birth?",
                      awb1_2_gender_othr = "42) If no, what term do you use to describe your gender?",
                      awb1_2_disability = "44) Do you have any physical/metal health conditions/illnesses?",
                      awb1_2_disability_tme_a4 = "45) Expected to last/lasted for >12 months?",
                      awb1_2_disability_impct_a4 = "46) Do illnesses/conditions reduce ability to carry out daily activities?",
                      awb1_3_worries = "48) What are 3 things that are worrying you currently?",
                      awb1_3_happy = "49) What is currently making you happy?",
                      awb1_4_q_self_a3 = "50) What question would you ask yourself in 3y time?",
                      awb3_1_assets_1 = "53) Do you have 3 meals every day?",
                      awb3_1_assets_2 = "54) Do you have a warm winter coat?",
                      awb3_1_assets_3 = "55) Do you have clothes you think your friends like?",
                      awb3_1_assets_4 = "56) Do you have your own mobile phone?",
                      awb3_1_assets_5 = "57) Do you have a computer/laptop/tablet with internet at home?",
                      awb3_1_assets_6 = "58) Do you have at least one family holiday per year?",
                      awb3_1_assets_7 = "59) Do you have a family car, van, or truck?",
                      awb3_1_assets_8 = "60) Do you have a bedroom for yourself?",
                      awb3_1_assets_9 = "61) Do you have a dishwasher at home?",
                      awb3_1_compare_frnds = "62) Compared to your friends, are your family richer/same/poorer?",
                      awb3_1_money_wrry = "63) How often do you worry about how much money your family has?",
                      awb3_1_warm_engh_a5 = "64) Are you/household warm enough at home in winter?",
                      awb3_1_save_mny_a5 = "65) My parents talked about cutting back to save money",
                      awb3_1y_save_mny_a5___1 = "66) Parents discussed cutting back on gas/electric",
                      awb3_1y_save_mny_a5___2 = "66) Parents discussed cutting back on luxuries (takeaways,new clothes)",
                      awb3_1y_save_mny_a5___3 = "66) Parents discussed cutting back on holidays/leisure activities",
                      awb3_1y_save_mny_a5___4 = "66) Parents discussed cutting back on food shopping",
                      awb3_1y_save_mny_a5___5 = "66) Parents discussed cutting back on car journeys",
                      awb3_1y_save_mny_a5___6 = "66) Parents discussed cutting back on other",
                      awb3_1y_save_mny_othr_a5 = "67) Parents discussed cutting back on other, specify",
                      awb3_2_hm_live = "68) How many homes do you live in?",
                      awb3_2_homes_1_ppl___1 = "69) Mother lives in first home",
                      awb3_2_homes_1_ppl___2 = "69) Father lives in first home",
                      awb3_2_homes_1_ppl___3 = "69) Guardian lives in first home",
                      awb3_2_homes_1_ppl___4 = "69) Foster carer lives in first home",
                      awb3_2_homes_1_ppl___5 = "69) Step mother lives in first home",
                      awb3_2_homes_1_ppl___6 = "69) Step father lives in first home",
                      awb3_2_homes_1_ppl___7 = "69) Mother's partner lives in first home",
                      awb3_2_homes_1_ppl___8 = "69) Father's partner lives in first home",
                      awb3_2_homes_1_ppl___9 = "69) Siblings live in first home",
                      awb3_2_homes_1_ppl___10 = "69) Auntie lives in first home",
                      awb3_2_homes_1_ppl___11 = "69) Uncle lives in first home",
                      awb3_2_homes_1_ppl___12 = "69) Grandmother lives in first home",
                      awb3_2_homes_1_ppl___13 = "69) Grandfather lives in first home",
                      awb3_2_homes_1_ppl___14 = "69) Cousins live in first home",
                      awb3_2_homes_1_ppl___15 = "69) Others live in first home",
                      awb3_2_homes_1_ppl_othr = "70) Other, specify who lives in first home",
                      awb3_2_homes_1_mthr_1 = "71) How many of your mothers live in this home?",
                      awb3_2_homes_1_fthr_2 = "72) How many of your fathers live in this home?",
                      awb3_2_homes_1_grd_3 = "73) How many of your guardians live in this home?",
                      awb3_2_homes_1_fstcr_4 = "74) How many of your foster carers live in this home?",
                      awb3_2_homes_1_stpm_5 = "75) How many of your step mothers live in this home?",
                      awb3_2_homes_1_stpf_6 = "76) How many of your step fathers live in this home?",
                      awb3_2_homes_1_mthpt_7 = "77) How many of your mothers partners live in this home?",
                      awb3_2_homes_1_fthpt_8 = "78) How many of your fathers partners live in this home?",
                      awb3_2_homes_1_sib_9 = "79) How many of your siblings live in this home?",
                      awb3_2_homes_1_ant_10 = "80) How many of your aunties live in this home?",
                      awb3_2_homes_1_unc_11 = "81) How many of your uncles live in this home?",
                      awb3_2_homes_1_gmthr_12 = "82) How many of your grandmothers live in this home?",
                      awb3_2_homes_1_gfthr_13 = "83) How many of your grandfathers live in this home?",
                      awb3_2_homes_1_cus_14 = "84) How many of your cousins live in this home?",
                      awb3_2_homes_2_ppl___1 = "85) Mother lives in second home",
                      awb3_2_homes_2_ppl___2 = "85) Father lives in second home",
                      awb3_2_homes_2_ppl___3 = "85) Guardian lives in second home",
                      awb3_2_homes_2_ppl___4 = "85) Foster carer lives in second home",
                      awb3_2_homes_2_ppl___5 = "85) Step mother lives in second home",
                      awb3_2_homes_2_ppl___6 = "85) Step father lives in second home",
                      awb3_2_homes_2_ppl___7 = "85) Mothers partner lives in second home",
                      awb3_2_homes_2_ppl___8 = "85) Fathers partner lives in second home",
                      awb3_2_homes_2_ppl___9 = "85) Siblings live in second home",
                      awb3_2_homes_2_ppl___10 = "85) Auntie lives in second home",
                      awb3_2_homes_2_ppl___11 = "85) Uncle lives in second home",
                      awb3_2_homes_2_ppl___12 = "85) Grandmother lives in second home",
                      awb3_2_homes_2_ppl___13 = "85) Grandfather lives in second home",
                      awb3_2_homes_2_ppl___14 = "85) Cousins live in second home",
                      awb3_2_homes_2_ppl___15 = "85) Others live in second home",
                      awb3_2_homes_2_ppl_othr = "86) Other, specify who lives in second home",
                      awb3_2_homes_2_mthr_1 = "87) How many of your mothers live in this home?",
                      awb3_2_homes_2_fthr_2 = "88) How many of your fathers live in this home?",
                      awb3_2_homes_2_grd_3 = "89) How many of your guardians live in this home?",
                      awb3_2_homes_2_fstcr_4 = "90) How many of your foster carers live in this home?",
                      awb3_2_homes_2_stpm_5 = "91) How many of your step mothers live in this home?",
                      awb3_2_homes_2_stpf_6 = "92) How many of your step fathers live in this home?",
                      awb3_2_homes_2_mthpt_7 = "93) How many of your mothers partners live in this home?",
                      awb3_2_homes_2_fthpt_8 = "94) How many of your fathers partners live in this home?",
                      awb3_2_homes_2_ppl_sib_9 = "95) How many of your siblings live in this home?",
                      awb3_2_homes_2_ant_10 = "96) How many of your aunties live in this home?",
                      awb3_2_homes_2_ppl_unc_11 = "97) How many of your uncles live in this home?",
                      awb3_2_homes_2_gmthr_12 = "98) How many of your grandmothers live in this home?",
                      awb3_2_homes_2_gfthr_13 = "99) How many of your grandfathers live in this home?",
                      awb3_2_homes_2_cus_14 = "100) How many of your cousins live in this home?",
                      awb2_6_family_brth_n_a5 = "102) Select your birth order",
                      awb2_6_family_rltnshp_1_a5 = "103) How often does your family get along together?",
                      awb2_6_family_rltnshp_2_a5 = "104) How often do you get along with siblings?",
                      awb3_3_home_1_jb = "105) Do any adults in first home have a job?",
                      awb3_3_home_1_jb_relatn_1 = "107) Adult 1: What is their relation to you?",
                      awb3_3_home_1_jb_othr_1 = "108) Adult 1: Other relation, specify",
                      awb3_3_home_1_jb_work_1 = "109) Adult 1: What is their place of work?",
                      awb3_3_home_1_jb_job_1 = "110) Adult 1: What job do they do?",
                      awb3_3_home_1_jb_relatn_2 = "112) Adult 2: What is their relation to you?",
                      awb3_3_home_1_jb_othr_2 = "113) Adult 2: Other relation, specify",
                      awb3_3_home_1_jb_work_2 = "114) Adult 2: What is their place of work?",
                      awb3_3_home_1_jb_job_2 = "115) Adult 2: What job do they do?",
                      awb3_3_home_1_jb_relatn_3 = "117) Adult 3: What is their relation to you?",
                      awb3_3_home_1_jb_othr_3 = "118) Adult 3: Other relation, specify",
                      awb3_3_home_1_jb_work_3 = "119) Adult 3: What is their place of work?",
                      awb3_3_home_1_jb_no___1 = "121) Why adults in 1st home not work: Sick/retired/student",
                      awb3_3_home_1_jb_no___2 = "121) Why adults in 1st home not work: Looking for job",
                      awb3_3_home_1_jb_no___3 = "121) Why adults in 1st home not work: Care for others",
                      awb3_3_home_1_jb_no___4 = "121) Why adults in 1st home not work: Don't know",
                      awb3_3_home_2_jb = "122) Do adults in second home have a job?",
                      awb3_3_home_2_jb_relatn_1 = "124) Adult 1: What is their relation to you?",
                      awb3_3_home_2_jb_othr_1 = "125) Adult 1: Other relation, specify",
                      awb3_3_home_2_jb_work_1 = "126) Adult 1: What is their place of work?",
                      awb3_3_home_2_jb_job_1 = "127) Adult 1: What job do they do?",
                      awb3_3_home_2_jb_relatn_2 = "129) Adult 2: What is their relation to you?",
                      awb3_3_home_2_jb_othr_2 = "130) Adult 2: Other relation, specify",
                      awb3_3_home_2_jb_work_2 = "131) Adult 2: What is their place of work?",
                      awb3_3_home_2_jb_job_2 = "132) Adult 2: What job do they do?",
                      awb3_3_home_2_jb_relatn_3 = "134) Adult 3: What is their relation to you?",
                      awb3_3_home_2_jb_othr_3 = "135) Adult 3: Other relation, specify",
                      awb3_3_home_2_jb_work_3 = "136) Adult 3: What is their place of work?",
                      awb3_3_home_2_jb_job_3 = "137) Adult 3: What job do they do?",
                      awb3_3_home_2_jb_no___1 = "138) Why adults in 2nd home not work: sick/retired/student",
                      awb3_3_home_2_jb_no___2 = "138) Why adults in 2nd home not work: Looking for job",
                      awb3_3_home_2_jb_no___3 = "138) Why adults in 2nd home not work: Care for others",
                      awb3_3_home_2_jb_no___4 = "138) Why adults in 2nd home not work: Don't know",
                      awb3_4_personal_assts_1 = "140) Do you get regular pocket money?",
                      awb3_4_personal_assts_2 = "141) Do you get money from chores/babysitting?",
                      awb3_4_personal_assts_3 = "142) Do you get money from working in family business?",
                      awb3_4_personal_assts_4 = "143) Do you get money from a paid job?",
                      awb3_4_personal_assts_5 = "144) Are you given money by parents when needed?",
                      awb3_4_personal_assts_6 = "145) I never get money to spend on myself",
                      awb3_4_personal_assts_7 = "146) Do you get money from another source?",
                      awb3_4_prsnl_assts_7_othr = "147) Other source, specify?",
                      aw3_5_food_1 = "148) Not enough money to get food that we want",
                      aw3_5_food_2 = "149) I worry about not having enough to eat",
                      aw3_5_food_3 = "150) I worry about my parents getting enough food for us",
                      aw3_5_food_4 = "151) I feel hungry because theres not enough food to eat",
                      aw3_5_food_5 = "152) I try not to eat alot so that our food will last",
                      aw3_6_comparison_1 = "154) If I dont strive to achieve I'll be seen as inferior",
                      aw3_6_comparison_2 = "155) People compare me to others to see if I match up",
                      aw3_6_comparison_3 = "156) Others will accept me even if I fail",
                      awb3_7_prsnl_sfty_1 = "158) Your safety when going out after dark",
                      awb3_7_prsnl_sfty_2 = "159) Your safety when going out during the day",
                      awb3_7_prsnl_sfty_3 = "160) Your safety at school",
                      awb3_7_prsnl_sfty_4 = "161) Your safety going to and from school",
                      awb3_7_violence = "162) Have you been victim of violence in past 12mth near home?",
                      aw3_7_decisions = "163) I can influence decisions affecting my local area",
                      awb3_activities_1 = "166) Did you go to a party/dance in past month?",
                      awb3_activities_1_r5 = "167) Did you go to a party/dance in past month?",
                      awb3_activities_2 = "168) Did you watch live sport in past month?",
                      awb3_activities_2_r5 = "169) Did you watch live sport in past month?",
                      awb3_activities_3 = "170) Did you sing/play instrument/make music in past month?",
                      awb3_activities_3_r5 = "171) Did you sing/play instrument/make music in past month?",
                      awb3_activities_4 = "172) Did you go to a live concert/gig in past month?",
                      awb3_activities_4_r5 = "173) Did you go to a live concert/gig in past month?",
                      awb3_activities_5 = "174) Did you go to other live performance in past month?",
                      awb3_activities_5_r5 = "175) Did you go to other live performance in past month?",
                      awb3_activities_6 = "176) Did you read for enjoyment in past month?",
                      awb3_activities_6_r5 = "177) Did you read for enjoyment in past month?",
                      awb3_activities_7 = "178) Did you go to youth clubs in past month?",
                      awb3_activities_7_r5 = "179) Did you go to youth clubs in past month?",
                      awb3_activities_8 = "180) Did you go to scouts/guides in past month?",
                      awb3_activities_8_r5 = "181) Did you go to scouts/guides in past month?",
                      awb3_activities_9 = "182) Did you go to a library in past month?",
                      awb3_activities_9_r5 = "183) Did you go to a library in past month?",
                      awb3_activities_10 = "184) Did you go to museums/galleries in past month?",
                      awb3_activities_10_r5 = "185) Did you go to museums/galleries in past month?",
                      awb3_activities_11 = "186) Did you do voluntary/charity/community work in past month?",
                      awb3_activities_11_r5 = "187) Did you do voluntary/charity/community work in past month?",
                      awb3_activities_12 = "188) Did you go to political meeting in past month?",
                      awb3_activities_12_r5 = "189) Did you go to political meeting in past month?",
                      awb3_activities_13 = "190) Did you attend a religious service in past month?",
                      awb3_activities_13_r5 = "191) Did you attend a religious service in past month?",
                      awb3_activities_14 = "192) Did you participate in poetry in past month?",
                      awb3_activities_14_r5 = "193) Did you participate in poetry in past month?",
                      awb3_activities_15 = "194) Have you done creative writing in past month?",
                      awb3_activities_15_r5 = "195) Have you done creative writing in past month?",
                      awb3_activities_16 = "196) Did you take part in a performance in past month?",
                      awb3_activities_16_r5 = "197) Did you take part in performance in past month?",
                      awb3_activities_17 = "198) Did you do any art work in past month?",
                      awb3_activities_17_r5 = "199) Did you do any art work in past month?",
                      awb3_activities_18 = "200) Did you make your own videos in past month?",
                      awb3_activities_18_r5 = "201) Did you make your own videos in past month?")
                      


mod1 <- mod1 %>%
  set_value_labels(survey_type = c("Online" = 1,
                                   "Offline" = 2)) %>%
  set_variable_labels(survey_type = "Was survey completed online or offline?")

# mod1 <- mod1 %>%
#   mutate(national_identity = rowSums(14:19)) %>%
#   relocate(national_identity, .before = awb1_2_uknation_idntty_1___1)


# add pseudo id 
pseudo <- read_csv("U:\\Born In Bradford - Confidential - Data\\BiB\\processing\\AoW\\denom\\data\\denom_pseudo.csv")
pseudo <- pseudo %>% select(aow_person_id, aow_recruitment_id) %>% rename(aow_id = aow_recruitment_id)
m1 <- left_join(mod1, pseudo, by = "aow_id")
mod1 <- m1 %>%
  relocate(aow_person_id, .before = aow_id) %>%
  relocate(survey_type, .after = date_time_collection)
mod1 <- mod1 %>%
  set_variable_labels(aow_person_id = "Age of Wonder Person Identifier")


# save dataset
write_dta(mod1, path = "U:\\Born In Bradford - Confidential - Data\\BiB\\processing\\AoW\\survey\\data\\Survey_Module_1.dta")

# save dataset
write_dta(mod1, path = "U:\\Born In Bradford - Confidential - Data\\BiB\\processing\\AoW\\survey\\data\\Survey_Module_1.dta")










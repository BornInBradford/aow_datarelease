# Module 4 survey labelling and tidying

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_derived.rds")

module <- module %>%
  set_variable_labels(aow_recruitment_id = "Age of Wonder recruitment ID",
                      age_survey_y = "Age (years) at survey date",
                      age_survey_m = "Age (months) at survey date",
                      survey_date = "Date survey taken",
                      survey_version = "Survey version",
                      survey_mode = "Survey taken online or offline?",
                      awb7_1_bullying_2 = "Do you think your school takes bullying seriously?",
                      awb7_1_future = "My school provides me with information about my next steps?",
                      awb7_2_water_1 = "Are you able to get water at school?",
                      awb7_2_water_3 = "If you can, where can you get it from? Describe",
                      awb7_1_ask_any_q = "If you could ask any teenager in Bradford any question about school, what would it be?",
                      awb8_2y_migration = "Which country did they migrate from?",
                      awb8_2_club_rsn_2 = "Why have you been discouraged for joining a club? Describe",
                      awb8_2_excl_rsn_2 = "Why did others your age not include you in activities? Describe",
                      awb8_2_age_rsn_2 = "Why do people expect less of you than others? Describe",
                      awb8_2_lang_rsn_2 = "Why do people assume your English is poor? Describe",
                      awb8_2_police_rsn_2 = "Why were you hassled by police? Describe",
                      awb8_2_shop_rsn_2 = "Why were you hassled by staff in a shop? Describe",
                      awb8_2_names_rsn_2 = "Why were you called insulting names? Describe",
                      awb8_2_service_rsn_2 = "Why did you receive poor service in a shop? Describe",
                      awb8_2_int_rsn_2 = "Why did people act as though you were not intelligent? Describe",
                      awb8_2_afraid_rsn_2 = "Why did people act as if they were afraid of you? Describe",
                      awb8_2_threat_rsn_2 = "Why were you threatened? Describe",
                      awb6_1_social_media_othr = "Which social media platforms do you use? Describe",
                      awb6_1_positive_exp_othr = "What have been your positive experiences of using social media? Describe",
                      awb6_1_neg_exp_othr_r5 = "What have been your negative experiences of using social media? Describe",
                      awb6_2_dgt_school_othr = "What digital devices do you have access to at school? Describe",
                      awb6_4_device_help_othr = "When using digital devices at school, who do you ask for help? Describe",
                      awb6_5_dev_n_home = "How many digital devices are available to you at home?",
                      awb6_5_dev_help_hm_othr = "Who do you ask for help when using digital devices at home? Describe",
                      awb6_6_int_hme_hrs = "How much time do you spend on the internet at home on a school day?",
                      awb6_6_int_hme_hrs_wknd = "How much time do you spend on the internet at home on a weekend day?",
                      awb6_7_dig_sklls_othr = "Have you ever done any classes in digital skils? Describe",
                      awb6_7_opp_dig_sklls_othr = "Where would you find out about opportunities to learn digital skills? Describe",
                      awb6_8_attd_tech_10 = "How much do you agree? Girls are more capable of doing technical jobs than boys",
                      awb6_8_smedia_habits = "If you could asked teenagers in Bradford their digital habits, what would it be?",
                      awb6_5_dgt_home_r8 = "What digital devices do you have access to at home?",
                      awb6_2_dgt_home_r8 = "What digital devices do you have access to at home?",
                      awb6_1_negative_exp_r5 = "What have been your negative experiences of using social media?",
                      awb8_2_problem_1 = "When you have a problem, what do you do about it?",
                      awb6_1_search_links = "When searching/following links online, what are they about most of the time?",
                      addi_inst_exp = "Adolescent Discrimination Distress Index Institutional",
                      addi_peer_exp = "Adolescent Discrimination Distress Index Peer",
                      addi_total = "Adolescent Discrimination Distress Index total",
                      addi_nas = "Adolescent Discrimination Distress Index NAs",
                      addi_missing = "Adolescent Discrimination Distress Index missing",
                      patt_career = "Pupils attitudes toward technology: Careers",
                      patt_interest = "Pupils attitudes toward technology: Interest",
                      patt_tedious = "Pupils attitudes toward technology: Tedious",
                      patt_conseq = "Pupils attitudes toward technology: Consequences",
                      patt_diff = "Pupils attitudes toward technology: Diff",
                      patt_gender = "Pupils attitudes toward technology: Gender",
                      patt_nas = "Pupils attitudes toward technology: NAs",
                      patt_missing = "Pupils attitudes toward technology: Missing")

# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_labelled.rds")



  
# Appending AoW Online and Offline Module 3 Survey
# Amy Hough
# 04/04/2023

# packages
library(tidyverse)
library(haven)
library(labelled)

# data
online <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module3_Online.dta")
offline <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module3_Offline.dta")

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
on_off <- on_off %>% filter(has_1 == 1)
view(on_off)

# change var names to append
online <- online %>%
  rename(aow_id = redcap_survey_identifier) %>%
  rename(date_time_collection = module_3_timestamp,
         awb5_2_smrn_pstyr = awb5_2_smrn_pstyr_a5)
  
offline <- offline %>%
  mutate(mod3_version = 5) %>%
  rename(awb4_2_outside_schl_1 = awb4_2_outside_schl_2,
         awb4_2_outside_schl_2 = awb4_2_outside_schl_3,
         awb4_2_outside_schl_3 = awb4_2_outside_schl_4,
         awb4_2_outside_schl_4 = awb4_2_outside_schl_5,
         awb4_2_outside_schl_1_a5 = awb4_2_outside_schl_2_a5,
         awb4_2_outside_schl_2_a5 = awb4_2_outside_schl_3_a5,
         awb4_2_outside_schl_3_a5 = awb4_2_outside_schl_4_a5,
         awb4_2_outside_schl_4_a5 = awb4_2_outside_schl_5_a5) 

# determine which cols are in both and which are in one survey only
on_col <- as.data.frame(colnames(online))
on_col <- on_col %>% rename(names = `colnames(online)`) %>% mutate(has_on = 1)
off_col <- as.data.frame(colnames(offline))
off_col <- off_col %>% rename(names = `colnames(offline)`) %>% mutate(has_off = 1)
on_off <- full_join(on_col, off_col, by = "names")
on_off <- on_off %>% mutate_at(vars(2:3), ~replace(., is.na(.), 0)) %>% mutate(has_1 = ifelse(has_on == 1 & has_off == 1,0,1))
on_off <- on_off %>% filter(has_1 == 1)
view(on_off)

# change name of food_dt_6 in online to food_dt_6_a6 (offline name). food_dt_6 in online is different to offline version. 
# awb5_1_food_dt_6 == awb5_1_food_dt_5 in offline and online version
offline <- offline %>%
  mutate(awb5_1_food_dt_5 = case_when(
    awb5_1_food_dt_6 == 1 | awb5_1_food_dt_5 == 1 ~ 1,
    awb5_1_food_dt_6 == 2 | awb5_1_food_dt_5 == 2 ~ 2,
    awb5_1_food_dt_6 == 3 | awb5_1_food_dt_5 == 3 ~ 3,
    awb5_1_food_dt_6 == 4 | awb5_1_food_dt_5 == 4 ~ 4,
    awb5_1_food_dt_6 == 5 | awb5_1_food_dt_5 == 5 ~ 5)) %>%
  select(-awb5_1_food_dt_6) %>%
  rename(awb5_1_food_dt_6 = awb5_1_food_dt_6_a6)



online$awb5_2y_alcohol_lctn_2 <- as.character(online$awb5_2y_alcohol_lctn_2)

# bind online and offline
mod3 <- bind_rows(online, offline)

# set variable labels
mod3 <- mod3 %>%
  set_variable_labels(awb1_1_id = "Participant Study ID",
                      aow_id = "Age of Wonder ID",
                      date_time_collection = "Date and time of collection",
                      mod3_version = "Version",
                      year_group = "School year group",
                      awb5_1_general_hlth = "6) How good is your health in general?",
                      awb5_1_hearing_sght_1 = "8) Do you wear glasses or contact lenses?",
                      awb5_1_hearing_sght_2 = "9) Do you have any difficulty seeing?",
                      awb5_1_hearing_sght_3 = "10) Do you have difficulty hearing/wear hearing aids?",
                      awb5_1_hearing_sght_4 = "11) Have you had your eyes tested outside of school?",
                      awb5_1_food_dt_1 = "12) How often does your family have meals together?",
                      awb5_1_food_dt_2 = "13) How often do you eat breakfast in a week?",
                      awb5_1_food_dt_3 = "14) How often do you eat 2+ portions fruit/day?",
                      awb5_1_food_dt_4 = "15) How often do you eat 2+ portions veg/day?",
                      awb5_1_food_dt_5 = "16) How often drink diet/sugar free drinks?",
                      awb5_1_food_dt_6 = "16) How often drink diet/sugar free drinks?",
                      awb5_1_food_dt_7 = "18) How often do you eat fast food?",
                      awb5_1_food_dt_8 = "19) How would you describe your diet?",
                      awb5_1_food_dt_9 = "20) Do you consider your health when choosing meal?",
                      awb4_5_location_1 = "21) Where do you normally eat your meals?",
                      awb4_5_location_2 = "22) Describe where else you eat meals",
                      awb2_12_eat_hbt_1_a5 = "24) Have you been deliberately limiting food intake to influence weight?",
                      awb2_12_eat_hbt_2_a5 = "25) Long periods of time without eating to influence weight?",
                      awb2_12_eat_hbt_3_a5 = "26) Thinking about food made it difficult to concentrate?",
                      awb2_12_eat_hbt_4_a5 = "27) Thinking about weight made it difficult to concentrate?",
                      awb2_12_eat_hbt_5_a5 = "28) Have you feared you might gain weight?",
                      awb2_12_eat_hbt_6_a5 = "29) Have you had strong desire to lose weight?",
                      awb2_12_eat_hbt_7_a5 = "30) Made yourself sick/taken laxatives to control weight?",
                      awb2_12_eat_hbt_8_a5 = "31) Exercised compulsively to lose weight?",
                      awb2_12_eat_hbt_9_a5 = "32) Have you felt loss of control over eating?",
                      awb2_12_eat_hbt_10_a5 = "33) How many days eat large amount of food in one go?",
                      awb2_12_wght_1_a5 = "34) Has your weight made you judge yourself in past week?",
                      awb2_12_wght_2_a5 = "35) How dissatisfied been with weight in past week?",
                      awb5_1_oral_hlth_1 = "36) How often do you brush your teeth?",
                      awb5_1_oral_hlth_2 = "37) How many times brush teeth yesterday?",
                      awb5_1_oral_hlth_3 = "38) Do you have dentist you see every 6mnth?",
                      awb5_1_oral_hlth_4 = "39) Why did you last go to the dentist?",
                      awb5_1_oral_hlth_5 = "40) Describe last dentist visit reason",
                      awb4_1_physical_actvty_1 = "42) How many mornings actively travel to school past week?",
                      awb4_1_physical_actvty_2 = "43) How often active during PE in past week?",
                      awb4_1_physical_actvty_3 = "44) What did you do at lunch in past week?",
                      awb4_1_physical_actvty_4 = "45) What did you do at break in past week?",
                      awb4_1_physical_actvty_5 = "46) How many afternoons actively travel in past week?",
                      awb4_1_physical_actvty_6 = "47) How many days after school were you active in past week?",
                      awb4_1_physical_actvty_7 = "48) How many evenings were you active in past week?",
                      awb4_1_physical_actvty_8 = "49) How many times very active last weekend?",
                      awb4_1_physical_actvty_1_a5 = "202) How many mornings actively travel to school past week?",
                      awb4_1_physical_actvty_2_a5 = "203) How often active during PE in past week?",
                      awb4_1_physical_actvty_3_a5 = "204) What did you do at lunch in past week?",
                      awb4_1_physical_actvty_4_a5 = "205) What did you do at break in past week?",
                      awb4_1_physical_actvty_5_a5 = "206) How many afternoons actively travel in past week?",
                      awb4_1_physical_actvty_6_a5 = "207) How many days after school were you active in past week?",
                      awb4_1_physical_actvty_7_a5 = "208) How many evenings were you active in past week?",
                      awb4_1_physical_actvty_8_a5 = "209) How many times very active last weekend?",
                      awb4_1_sick = "50) Were you sick last week?",
                      awb4_1y_sick = "51) What prevented you doing physical activities?",
                      awb4_4_sex = "53) What is your biological sex at birth?",
                      awb4_4_sex_othr = "54) Space to self describe",
                      awb4_4_growth_sprt = "55) How would you describe a growth spurt during puberty?",
                      awb4_4_body_hr = "56) How would you describe body hair growth during puberty?",
                      awb4_4_skin = "57) How would you describe skin changes during puberty?",
                      awb4_4by_gendersex_m = "58) How would you describe voice during puberty?",
                      awb4_4y_gendersex_y_f_2 = "62) Age when started periods?",
                      awb4_2_outside_schl_1 = "63) Time spent watching TV outside of school?",
                      awb4_2_outside_schl_2 = "64) Time spent playing video games outside/school?",
                      awb4_2_outside_schl_3 = "65) Time spent using PC/tablets outside/school?",
                      awb4_2_outside_schl_4 = "66) Time spent using mobile outside/school?",
                      awb4_2_overall = "67) Which describes sedentary habits at home?",
                      awb4_3_times2 = "69) Weekday - what time switch lights off to sleep?",
                      awb4_3_times3 = "70) Weekday - what time do you wake up?",
                      awb4_3_times4 = "71) Weekend - what time switch lights off to sleep?",
                      awb4_3_times5 = "72) Weekend - what time normally wake up?",
                      awb4_3_bed = "73) Did you go to sleep soon after going to bed?",
                      awb4_3n_bed_1___1 = "74) Didn't sleep - Worried",
                      awb4_3n_bed_1___2 = "74) Didn't sleep - Noisy",
                      awb4_3n_bed_1___3 = "74) Didn't sleep - Hungry/Thirsty",
                      awb4_3n_bed_1___4 = "74) Didn't sleep - Never sleep well",
                      awb4_3n_bed_1___5 = "74) Didn't sleep - Watching videos",
                      awb4_3n_bed_1___6 = "74) Didn't sleep - Reading",
                      awb4_3n_bed_1___7 = "74) Didn't sleep - Playing on phone",
                      awb4_3n_bed_1___8 = "74) Didn't sleep - Too hot/cold",
                      awb4_3n_bed_1___9 = "74) Didn't sleep - Other",
                      awb4_4y_ph_otherq = "76) What would you ask about physical health?",
                      awb5_1_cigs_a5 = "79) Have you ever smoked e-cig/cigs?",
                      awb5_1_cigs2_a5 = "80) How often do you smoke cigs?",
                      awb5_2_smoke = "81) Which best describes your smoking habits?",
                      awb5_2_smoke_r4 = "82) Which best describes your smoking habits?",
                      awb5_3_smoke = "83) How old when first tried smoking?",
                      awb5_2_evr_vaped_a5 = "85) Have you ever vaped?",
                      awb5_2_vape = "86) Which best describes your vaping habits?",
                      awb5_2_vape_r4 = "87) Which best describes your vaping habits?",
                      awb5_2_smokevape_prnt = "88) Do your parents/carers smoke/vape?",
                      aw5_2_vape_prnt = "89) Does anyone smoke indoors at home?",
                      awb5_2_alcohol = "91) Have you ever had more than a few sips of alcohol?",
                      awb5_2y_alcohol_age = "92) How old when first had alcohol?",
                      awb5_2y_alcohol_frqncy = "93) How many times had alcohol in past year?",
                      awb5_2y_alcohol_qntty = "94) Have you had 5+ alcoholic drinks at a time?",
                      awb5_2_yalcohol_y_qntty_age = "95) How old when have 5+ alcoholic drinks?",
                      awb5_2_yalcohol_y_qntty_frqncy = "96) How often 5+ alcoholic drinks in past year?",
                      awb5_2y_alcohol_lctn___1 = "97) Past week, got alcohol from parents to drink with them",
                      awb5_2y_alcohol_lctn___2 = "97) Past week, got alcohol from parents to drink with friends",
                      awb5_2y_alcohol_lctn___3 = "97) Past week, stole alcohol from home",
                      awb5_2y_alcohol_lctn___4 = "97) Past week, I bought alcohol from supermarket",
                      awb5_2y_alcohol_lctn___5 = "97) Past week, I bought alcohol from off licence",
                      awb5_2y_alcohol_lctn___6 = "97) Past week, I got stranger to buy alcohol from supermarket",
                      awb5_2y_alcohol_lctn___7 = "97) Past week, I got stranger to buy alcohol from off licence",
                      awb5_2y_alcohol_lctn___8 = "97) Past week, I got friend/sibling to buy alcohol",
                      awb5_2y_alcohol_lctn___9 = "97) Past week, I bought alcohol in pub/club",
                      awb5_2y_alcohol_lctn___10 = "97) I got alcohol some other way (describe)",
                      awb5_2y_alcohol_lctn_1 = "98) I got alcohol some other way - describe",
                      awb5_2y_alcohol_whch_1___1 = "99) Past week, no alcohol",
                      awb5_2y_alcohol_whch_1___2 = "99) Past week, pre-mixed spirits",
                      awb5_2y_alcohol_whch_1___3 = "99) Past week, beer or lager",
                      awb5_2y_alcohol_whch_1___4 = "99) Past week, spirits",
                      awb5_2y_alcohol_whch_1___5 = "99) Past week, cider",
                      awb5_2y_alcohol_whch_1___6 = "99) Past week, fortified wines",
                      awb5_2y_alcohol_whch_1___7 = "99) Past week, wine",
                      awb5_2y_alcohol_whch_1___8 = "99) Past week, other (describe)",
                      awb5_2y_alcohol_lctn_2 = "100) Other alcohol in past week, describe",
                      awb5_2y_alcohol_prnts = "101) Do your parents/carers know you drink alcohol?",
                      awb5_2y_alcohol_drnk = "102) When were you last drunk?",
                      awb5_drugs = "103) Have you ever taken drugs?",
                      awb5_2_drugs_othrs = "104) Do you know anyone who takes drugs?",
                      awb5_2_drugs_1 = "105) Have you ever taken cannibis?",
                      awb5_2_cannabis_pstyr = "106) Past year, how often taken cannibis?",
                      awb5_2_drugs_2 = "107) Have you ever taken cocaine?",
                      awb5_2_cocaine_pstyr = "108) Past year, how many times taken cocaine?",
                      awb5_2_drugs_3 = "109) Have you ever taken acid or LSD?",
                      awb5_2_acid_pstyr = "110) Past year, how often taken acid or LSD?",
                      awb5_2_drugs_4 = "111) Have you ever taken ecstasy?",
                      awb5_2_ecstasy_pstyr = "112) Past year, how many times taken ecstasy?",
                      awb5_2_drugs_5 = "113) Have you ever taken heroin?",
                      awb5_2_heroin_pstyr = "114) Past year, how many times taken heroin?",
                      awb5_2_drugs_6 = "115) Have you ever taken crack?",
                      awb5_2_crack_pstyr = "116) Past year, how many times taken crack?",
                      awb5_2_drugs_7 = "117) Have you ever taken speed or amphetamines?",
                      awb5_2_speed_pstyr = "118) Past year, how many times taken speed/amphetamines?",
                      awb5_2_drugs_8 = "119) Have you ever taken methamphetamine?",
                      awb5_2_meth_pstyr = "120) Past year, how many times taken methamphetamine?",
                      awb5_2_drugs_9 = "121) Have you ever taken semeron?",
                      awb5_2_smrn_pstyr = "122) Past year, how many times taken semeron?",
                      awb5_2_drugs_10 = "123) Have you ever taken ketamine?",
                      awb5_2_ket_pstyr = "124) Past year, how many times taken ketamine?",
                      awb5_2_drugs_11 = "125) Have you ever taken mephedrone?",
                      awb5_2_mephedrone_pstyr = "126) Past year, how many times taken mephedrone?",
                      awb5_2_drugs_12 = "127) Have you ever taken spice?",
                      awb5_2_spice_pstyr = "128) Past year, how many times taken spice?",
                      awb5_2_drugs_13 = "129) Have you ever taken magic mushrooms?",
                      awb5_2_mushrooms_pstyr = "130) Past year, how many times taken magic mushrooms?",
                      awb5_2_drugs_14 = "131) Have you ever taken salvia?",
                      awb5_2_salvia_pstyr = "132) Past year, how many times taken salvia?",
                      awb5_2_drugs_15 = "133) Have you ever taken nitrous oxide?",
                      awb5_2_nitrous_pstyr = "134) Past year, how many times taken nitrous oxide?",
                      awb5_2_drugs_16 = "135) Have you ever taken poppers?",
                      awb5_2_poppers_pstyr = "136) Past year, how many times taken poppers?",
                      awb5_2_drugs_17 = "137) Have you ever taken prescription drugs not prescribed to you?",
                      awb5_2_prescription_pstyr = "138) Past year, how many times taken prescription drugs recreationally?",
                      awb5_2_gambling_1_a5 = "139) Have you spent money on lotto?",
                      awb5_2_gambling_2_a5 = "140) Have you spent money on national lottery scratchcards?",
                      awb5_2_gambling_3_a5 = "141) Have you spent money on national lottery instant win games?",
                      awb5_2_gambling_4_a5 = "142) Have you spent money on other national lottery games?",
                      awb5_2_gambling_5_a5 = "143) Have you spent money on other lotteries?",
                      awb5_2_gambling_6_a5 = "144) Have you spent money on fruit/slot machines?",
                      awb5_2_gambling_7_a5 = "145) Have you ever placed a private bet for money?",
                      awb5_2_gambling_8_a5 = "146) Have you ever played cards for money?",
                      awb5_2_gambling_9_a5 = "147) Have you spent money on bingo at a bingo club?",
                      awb5_2_gambling_10_a5 = "148) Have you spent money on bingo elsewhere?",
                      awb5_2_gambling_11_a5 = "149) Have you visited a betting shop to play gaming machines?",
                      awb5_2_gambling_12_a5 = "150) Have you personally placed a bet at a betting shop?",
                      awb5_2_gambling_13_a5 = "151) Have you visited a casino to play games?",
                      awb5_2_gambling_14_a5 = "152) Have you been on gambling apps where you can win money?",
                      awb5_2_gambling_pstyr1_a5 = "153) Past year, has your gambling led to arguments with family/friends?",
                      awb5_2_gambling_pstyr2_a5 = "154) Past year, has your gambling led to telling lies to family/friends?",
                      awb5_2_gambling_pstyr3_a5 = "155) Past year, has your gambling led to you missing school?",
                      awb5_2_tkn_mny_a5 = "156) Past year, have you taken money without permission for gambling?",
                      awb5_2_tkn_mny_a5___1 = "156) Past year, never taken money without permission for gambling",
                      awb5_2_tkn_mny_a5___2 = "156) Past year, taken dinner/fare money for gambling",
                      awb5_2_tkn_mny_a5___3 = "156) Past year, taken money from family for gambling",
                      awb5_2_tkn_mny_a5___4 = "156) Past year, taken money from things you've sold for gambling",
                      awb5_2_tkn_mny_a5___5 = "156) Past year, taken money from outside family for gambling",
                      awb5_2_tkn_mny_a5___6 = "156) Past year, taken money from somewhere else for gambling",
                      awb5_2_tkn_mny_a5___7 = "156) Taken money for gambling - Prefer not to say",
                      awb5_2_fam_spend1_a5 = "158) Has family spent money on lotto?",
                      awb5_2_fam_spend2_a5 = "159) Has family spent money on fruit/slot machines?",
                      awb5_2_fam_spend3_a5 = "160) Have family visited betting shop to play machines?",
                      awb5_2_fam_spend4_a5 = "161) Have family place bet in betting shop?",
                      awb5_2_fam_spend5_a5 = "162) Have family played bingo at bingo club?",
                      awb5_2_fam_spend6_a5 = "163) Have family playing bingo elsewhere?",
                      awb5_2_fam_spend7_a5 = "164) Have family visited casino to play games?",
                      awb5_2_fam_spend8_a5 = "165) Have family played on gambling apps?",
                      awb5_2_fam_spend9_a5 = "166) Have family gambled for money/items?",
                      awb5_2_online_gamb1_a5 = "168) When did you last play online gambling game?",
                      awb5_2_online_gamb2_a5___1 = "169) Did you gamble using social networking sites?",
                      awb5_2_online_gamb2_a5___2 = "169) Did you gamble using social networking apps?",
                      awb5_2_online_gamb2_a5___3 = "169) Did you gamble using free demo games gambling site?",
                      awb5_2_online_gamb2_a5___4 = "169) Did you gamble using free demo games gambling apps?",
                      awb5_2_online_gamb2_a5___5 = "169) Did you gamble using other apps?",
                      awb5_2_online_gamb2_a5___6 = "169) Did you gamble on other websites?",
                      awb5_2_online_gamb2_a5___7 = "169) Did you gamble any other way?",
                      awb5_2_online_gamb2_a5___8 = "169) Did you gamble? Don't know/can't remember",
                      awb5_2_online_gamb3_a5___1 = "171) Video games - have you paid to buy in game items?",
                      awb5_2_online_gamb3_a5___2 = "171) Video games - have you paid to open loot?",
                      awb5_2_online_gamb3_a5___3 = "171) Video games - have you bet in game items on other websites?",
                      awb5_2_online_gamb3_a5___4 = "171) Video games - none of these",
                      awb5_2_gambling_1 = "172) Spent own money placing private bet for money",
                      awb5_2_gambling_2 = "173) Spent own money purchasing in game items?",
                      awb5_2_gambling_3 = "174) Spent own money on any other gambling?",
                      awb5_2_gambling_money_1 = "175) How much spent placing private bet - past year?",
                      awb5_2_gambling_money_2 = "176) How much spent purchasing in game items - past year?",
                      awb5_2_gambling_money_3 = "177) How much spent any other gambling past year?",
                      awb_5_gambling = "178) Have you spent your own money gambling - past week?",
                      awb5_2_gambling_prnts = "179) Do your parents know you're gambling?",
                      awb5_2_worried = "180) How often worried about your gambling - past year?",
                      awb5_2_worried_2 = "181) How often gambled more money - past year?",
                      awb5_2_worried_3 = "182) How often... past year?",
                      awb5_2_worried_4 = "183) How often argued with family/friends about gambling - past year?",
                      awb5_2_worried_5 = "184) How often lied to family/friends about gambling - past year?",
                      awb5_gambling_family = "185) How often worried about family members gambling?",
                      awb5_2y_knife = "186) Have you carried knife/weapon in past year?",
                      awb5_2y_gang = "187) Are you a member of a gang?",
                      awb5_2_contactpolice_1 = "189) Have you been stopped & questioned by police?",
                      awb5_2_contactpolice_2 = "190) Have you been given formal warning/caution by police?",
                      awb5_2_contactpolice_3 = "191) Have you been arrested/taken to police station?",
                      awb5_2y_gang_6 = "192) Have you appeared in court for being accused of a crime?",
                      awb5_2y_gang_5 = "193) Were you found guily or not guilty?",
                      awb5_2_services_1 = "195) Are you aware of stop smoking service?",
                      awb5_2_services_2 = "196) Are you aware of Step2/young people health project?",
                      awb5_2_services_3 = "197) Are you aware of C-card/free condom schemes?",
                      awb5_2_services_4 = "198) Are you aware of contraception/sexual health services?",
                      awb5_2_services_5 = "199) Are you aware of drug/alcohol services?",
                      awb5_2_services_6 = "200) Are you aware of childline?",
                      awb5_2_services_7 = "201) Are you aware of Kooth?",
                      awb4_1_sick_a5 = "210) Were you sick last week/prevent normal activities?",
                      awb4_1y_sick_a5 = "211) What prevented you doing normal activities?",
                      awb4_2_outside_schl_1_a5 = "213) How long do you watch TV outside of school?",
                      awb4_2_outside_schl_2_a5 = "214) How long spent playing video games outside of school?",
                      awb4_2_outside_schl_3_a5 = "215) How long spent using PC/tablet outside of school?",
                      awb4_2_outside_schl_4_a5 = "216) How long spent using mobile outside of school?",
                      awb4_2_overall_a5 = "217) What are your sedentary habits like at home?",
                      awb4_3_times2_a5 = "219) On school nights, what time do you fall asleep?",
                      awb4_3_times3_a5 = "220) On school days, what time do you wake up?",
                      awb4_3_times4_a5 = "221) On non-school night, what time do you fall asleep?",
                      awb4_3_times5_a5 = "222) On non-school days, what time do you wake up?",
                      awb4_3_nap_a5 = "223) Do you ever take naps during the day?",
                      awb4_3_alert_tod_a5 = "224) Which time of day are you more active/alert?",
                      awb4_3_fall_asleep_a5 = "225) How quickly do you fall asleep when in bed/lights off?",
                      awb4_3_wake_drng_nght_a5 = "226) Do you wake up during the night?",
                      awb4_3_wake_drng_nght2_a5 = "227) How quickly do you fall asleep when woken during night?",
                      awb4_3_sleep_well_nght_a5 = "228) Do you sleep well at night?",
                      awb4_3_sleep_drngday_a5 = "229) How likely are you to feel sleepy during the day?",
                      module_3_complete = "230) Module 3 complete?",
                      survey_type = "Online or offline survey?")
                      


# add pseudo id 
pseudo <- read_csv("U:\\Born In Bradford - Confidential - Data\\BiB\\processing\\AoW\\denom\\data\\denom_pseudo.csv")
pseudo <- pseudo %>% select(aow_person_id, aow_recruitment_id) %>% rename(aow_id = aow_recruitment_id)
m3 <- left_join(mod3, pseudo, by = "aow_id")
mod3 <- m3 %>%
  relocate(aow_person_id, .before = aow_id) %>%
  relocate(survey_type, .after = date_time_collection)
mod3 <- mod3 %>%
  set_variable_labels(aow_person_id = "Age of Wonder Person Identifier")


# if date time == [not completed], NA
mod3 <- mod3 %>% replace_with_na(replace = list(date_time_collection = "[not completed]"))
                      
# save dataset
write_dta(mod3, path = "U:\\Born In Bradford - Confidential - Data\\BiB\\processing\\AoW\\survey\\data\\Survey_Module_3.dta")

                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      














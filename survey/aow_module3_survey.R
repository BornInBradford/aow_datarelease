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
                      awb5_1_food_dt_6_a6 = "17) How often drink sugary drinks?",
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
                      awb4_4by_gendersex_m1 = "59) How would you describe facial hair growth/puberty?",
                      awb4_4by_gendersex_f1 = "60) How would you describe breast growth/puberty?",
                      awb4_4by_gendersex_f2 = "61) Have you started your periods?",
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
                      awb4_3_bed_1 = "74) If not - why didnt you sleep soon after?",
                      awb4_3_bed_2 = "75) Describe why didn't sleep soon after",
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
                      awb5_2_acid_pstyr = "110_ Past year, how often taken acid or LSD?"
                      
                      
                      
                      














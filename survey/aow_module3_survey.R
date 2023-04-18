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


















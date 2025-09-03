
# ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
# check <- readRDS(file.path(ckat_input, "all_dvs_processeddata_cognitive.rds"))


# ParticipantInfo.rds
# Basic participant and session details for CKAT and cognitive testing
source("ckat/build_ckat_cog_denominator.R")

# CogAndMotorTasksRecorded.rds
# long list of tasks recorded against each AoWRecruitmentID
# e.g.
# AIM  BDR  CRS  FDR  FLK  PS_  QB_  STE  TRK 
# 4878 5144   89 5227 4967 4877 4351 4875 4893 


# Events_data_ckat.rds
# Millisecond timings of events occurring throughout a session
# Split by task e.g.
#
# AIM_instr_events.csv       AIM_task_events.csv    STE_A_instr_events.csv     STE_A_task_events.csv 
# 33810                    867328                     39459                     16908 
# STE_B_task_events.csv   TRK_NO_instr_events.csv    TRK_NO_task_events.csv TRK_WITH_instr_events.csv 
# 16899                     28290                     28290                     16950 
# TRK_WITH_task_events.csv 
# 28240
source("ckat/ckat_events_data.R")

# all_dvs_processeddata_ckat.rds
# key-value pairs containing the processed data
# needs splitting by task type and reshaping i.e.
# AIM_dvs.csv STE_dvs.csv TRK_dvs.csv 
# 3901260       87874      361146

# AIM_movement_data_ckat.rds
# TRK_STE_Movement_data_ckat.rds



# Events_data_cognitive.rds
# empty file

# FLK_Trials_data_cognitive.rds
# FLK trials data, straightforward coding and labelling required

# PS_Trials_data_cognitive.rds
# PS trials data, straightforward coding and labelling required

# WM_Trials_data_cognitive.rds
# WM trials data, needs coding and labelling
# then split by task i.e.
# BDR_trials.csv CRS_trials.csv FDR_trials.csv 
# 451052           9256         567456

# QB_dvs_processeddata_cognitive.rds
# this is survey data in a very odd long format, needs some work
# and more info on the survey structure

# RCq_dvs_processeddata_cognitive.rds
# also survey-type data in unusual long format
# only seems to contain data for 10 individuals

# RCR_dvs_processeddata_cognitive.rds
# appears to be task timing and other process data
# only appears to have data from 10 individuals

# all_dvs_processeddata_cognitive.rds
# processed dvs for all cognitive tasks in long format
# needs splitting by task and reshaping, i.e.
#
# BDR_dvs.csv CRS_dvs.csv FDR_dvs.csv FLK_dvs.csv  PS_dvs.csv 
# 89264        1424       90784       43032       95148





# ParticipantInfo.rds
# Basic participant and session details for CKAT and cognitive testing
source("ckat/build_ckat_cog_denominator.R")


# Events_data_ckat.rds
# Millisecond timings of events occurring throughout a session
source("ckat/ckat_events_data.R")


# all_dvs_processeddata_ckat.rds
# key-value pairs containing the processed data
source("ckat/ckat_processed_data.R")


# AIM_movement_data_ckat.rds
# TRK_STE_Movement_data_ckat.rds

# format these to compress but might be too big to add denominator?


# all_dvs_processeddata_cognitive.rds
# processed dvs for all cognitive tasks in long format
# needs to run before trials data as it's easier to dedupe this file
# then we can use the output to dedupe the trials
source("ckat/cognitive_processed_data.R")


# FLK_Trials_data_cognitive.rds
# PS_Trials_data_cognitive.rds
# WM_Trials_data_cognitive.rds
# Cognitive tasks trials data, WM file needs splitting by task
source("ckat/cognitive_trials_data.R")


# QB_dvs_processeddata_cognitive.rds
# this is survey data in a very odd long format, needs some work
# and more info on the survey structure

# find out how to process this




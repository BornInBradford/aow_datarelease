# ckat events data

library(tidyr)
library(dplyr)
library(labelled)

ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
ckat_output <- file.path("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data")

# get ckat cog denominator
denom <- readRDS(file.path(ckat_output, "ckat_participant_sessions.rds"))

# get raw events data
events <- readRDS(file.path(ckat_input, "Events_data_ckat.rds"))

# explore
table(events$Event)
table(events$FileName)

length(unique(events$Event))
length(unique(events$FileName))

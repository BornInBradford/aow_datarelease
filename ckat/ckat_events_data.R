# ckat events data

library(tidyr)
library(dplyr)
library(labelled)

ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
ckat_output <- file.path("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data")

# get ckat cog denominator
denom <- readRDS(file.path(ckat_output, "ckat_linkage_denominator.rds"))

# get raw events data
events <- readRDS(file.path(ckat_input, "Events_data_ckat.rds"))

# explore
table(events$Event)
table(events$FileName)

length(unique(events$Event))
length(unique(events$FileName))

# reformat events data
events <- events |> transmute(aow_recruitment_id = tolower(FilePath),
                              task = substr(FileName, 1, nchar(FileName) - 11) |> tolower(),
                              event_time = as.numeric(TimeMs),
                              event = Event) |>
  set_variable_labels(task = "CKAT task type",
                      event_time = "CKAT event time in milliseconds",
                      event = "CKAT event type")

# anyone missing?
with_events <- denom |> semi_join(events, by = "aow_recruitment_id")
without_events <- denom |> anti_join(events, by = "aow_recruitment_id")
in_denom <- events |> semi_join(denom, by = "aow_recruitment_id") 
not_in_denom <- events |> anti_join(denom, by = "aow_recruitment_id")

# 36 missing - a few tests, a few invalid ids
table(not_in_denom$aow_recruitment_id)

# join denominator
events_linked <- denom |> inner_join(events, by = "aow_recruitment_id")

# task list
tasks <- events$task |> unique()

# export
for(t in tasks) {
  
  df <- events_linked |> filter(task == t)
  saveRDS(df, file.path(ckat_output, paste0("ckat_events_", t, ".rds")))
  
}


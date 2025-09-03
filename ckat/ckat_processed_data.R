# ckat processed data

library(tidyr)
library(dplyr)
library(labelled)
library(readxl)

ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
ckat_output <- file.path("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data")

# get ckat cog denominator
denom <- readRDS(file.path(ckat_output, "ckat_linkage_denominator.rds"))

# get raw processed data
procdat <- readRDS(file.path(ckat_input, "all_dvs_processeddata_ckat.rds"))

# no session ids so for deduplicating we're going to need to know original row number
# (and trust they are still in order!)


# explore
table(procdat$FileName)

length(unique(procdat$Variable))
length(unique(procdat$FileName))

# are variables the same?
# table(procdat$Variable, procdat$FileName)

# no, so split first, after a bit of reformatting
procdat <- procdat |> transmute(aow_recruitment_id = tolower(FilePath),
                                task = tolower(substr(FileName, 1, 3)),
                                value = as.numeric(Value),
                                variable = Variable)

# also remove simple duplicates
aim <- procdat |> filter(task == "aim") |> select(-task) |> unique()
ste <- procdat |> filter(task == "ste") |> select(-task) |> unique()
trk <- procdat |> filter(task == "trk") |> select(-task) |> unique()



# there are duplicate ids in here which means we can't pivot without somehow removing them

## AIM
# first try removing NAs and those not in denom
aim <- aim |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
aim_dupes <- aim$aow_recruitment_id[select(aim, aow_recruitment_id, variable) |> duplicated()] |> unique()
aim <- aim |> filter(!aow_recruitment_id %in% aim_dupes) |>
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

## STE
# first try removing NAs and those not in denom
ste <- ste |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
ste_dupes <- ste$aow_recruitment_id[select(ste, aow_recruitment_id, variable) |> duplicated()] |> unique()
ste <- ste |> filter(!aow_recruitment_id %in% ste_dupes) |> 
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

## TRK
# first try removing NAs and those not in denom
trk <- trk |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
trk_dupes <- trk$aow_recruitment_id[select(trk, aow_recruitment_id, variable) |> duplicated()] |> unique()
trk <- trk |> filter(!aow_recruitment_id %in% trk_dupes) |> 
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

# join to session info
aim_joined <- denom |> inner_join(aim, by = "aow_recruitment_id")
ste_joined <- denom |> inner_join(ste, by = "aow_recruitment_id")
trk_joined <- denom |> inner_join(trk, by = "aow_recruitment_id")


# read labels
ckat_labels <- read_xlsx("U:/Born in Bradford - AOW Raw Data/sql/ckat/docs/ckat_cog_variable_labelling.xlsx")


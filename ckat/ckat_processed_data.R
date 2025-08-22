# ckat processed data

library(tidyr)
library(dplyr)
library(labelled)

ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
ckat_output <- file.path("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data")

# get ckat cog denominator
denom <- readRDS(file.path(ckat_output, "ckat_linkage_denominator.rds"))

# get raw processed data
procdat <- readRDS(file.path(ckat_input, "all_dvs_processeddata_ckat.rds"))

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

aim <- procdat |> filter(task == "aim") |> select(-task)
ste <- procdat |> filter(task == "ste") |> select(-task)
trk <- procdat |> filter(task == "trk") |> select(-task)

# there are duplicate ids in here which emans we can't pivot without somehow removing them
ste <- ste |> pivot_wider(id_cols = "aow_recruitment_id",
                          names_from = "variable",
                          values_from = "value")


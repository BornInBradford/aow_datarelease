# cognitive processed data

library(tidyr)
library(dplyr)
library(labelled)
library(readxl)

cog_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
cog_output <- file.path("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data")

# get ckat cog denominator
denom <- readRDS(file.path(cog_output, "ckat_linkage_denominator.rds"))

# get raw processed data
procdat <- readRDS(file.path(cog_input, "all_dvs_processeddata_cognitive.rds"))

# no session ids so for deduplicating we're going to need to know original row number
# (and trust they are still in order!)


# explore
table(procdat$FileName)

length(unique(procdat$Variable))
length(unique(procdat$FileName))

# are variables the same?
# table(procdat$Variable, procdat$FileName)

# so split first, after a bit of reformatting
procdat <- procdat |> transmute(aow_recruitment_id = tolower(FilePath),
                                task = tolower(substr(FileName, 1, 3)),
                                value = as.numeric(Value),
                                variable = Variable)

# also remove simple duplicates
bdr <- procdat |> filter(task == "bdr") |> select(-task) |> unique()
crs <- procdat |> filter(task == "crs") |> select(-task) |> unique()
fdr <- procdat |> filter(task == "fdr") |> select(-task) |> unique()
flk <- procdat |> filter(task == "flk") |> select(-task) |> unique()
ps <- procdat |> filter(task == "ps_") |> select(-task) |> unique()



# there are duplicate ids in here which means we can't pivot without somehow removing them

## bdr
# first try removing NAs and those not in denom
bdr <- bdr |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
bdr_dupes <- bdr$aow_recruitment_id[select(bdr, aow_recruitment_id, variable) |> duplicated()] |> unique()
bdr <- bdr |> filter(!aow_recruitment_id %in% bdr_dupes) |>
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

## crs
# first try removing NAs and those not in denom
crs <- crs |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
crs_dupes <- crs$aow_recruitment_id[select(crs, aow_recruitment_id, variable) |> duplicated()] |> unique()
crs <- crs |> filter(!aow_recruitment_id %in% crs_dupes) |> 
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

## fdr
# first try removing NAs and those not in denom
fdr <- fdr |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
fdr_dupes <- fdr$aow_recruitment_id[select(fdr, aow_recruitment_id, variable) |> duplicated()] |> unique()
fdr <- fdr |> filter(!aow_recruitment_id %in% fdr_dupes) |> 
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

## flk
# first try removing NAs and those not in denom
flk <- flk |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
flk_dupes <- flk$aow_recruitment_id[select(flk, aow_recruitment_id, variable) |> duplicated()] |> unique()
flk <- flk |> filter(!aow_recruitment_id %in% flk_dupes) |> 
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")

## ps
# first try removing NAs and those not in denom
ps <- ps |> filter(!is.na(value)) |>
  semi_join(denom, by = "aow_recruitment_id")
# find the dupes
ps_dupes <- ps$aow_recruitment_id[select(ps, aow_recruitment_id, variable) |> duplicated()] |> unique()
ps <- ps |> filter(!aow_recruitment_id %in% ps_dupes) |> 
  pivot_wider(id_cols = "aow_recruitment_id",
              names_from = "variable",
              values_from = "value")


# join to session info
bdr_joined <- denom |> inner_join(bdr, by = "aow_recruitment_id")
crs_joined <- denom |> inner_join(crs, by = "aow_recruitment_id")
fdr_joined <- denom |> inner_join(fdr, by = "aow_recruitment_id")
flk_joined <- denom |> inner_join(fdr, by = "aow_recruitment_id")
ps_joined <- denom |> inner_join(ps, by = "aow_recruitment_id")


# do labelling
cog_labels <- read_xlsx("U:/Born in Bradford - AOW Raw Data/sql/ckat/docs/ckat_cog_variable_labelling.xlsx")

set_cog_labels <- as.list(cog_labels$label)
names(set_cog_labels) <- cog_labels$variable

bdr_joined <- bdr_joined |> set_variable_labels(.labels = set_cog_labels, .strict = FALSE)
crs_joined <- crs_joined |> set_variable_labels(.labels = set_cog_labels, .strict = FALSE)
fdr_joined <- fdr_joined |> set_variable_labels(.labels = set_cog_labels, .strict = FALSE)
flk_joined <- flk_joined |> set_variable_labels(.labels = set_cog_labels, .strict = FALSE)
ps_joined <- ps_joined |> set_variable_labels(.labels = set_cog_labels, .strict = FALSE)

# export
saveRDS(bdr_joined, file.path(ckat_output, "cog_processed_dvs_bdr.rds"))
saveRDS(crs_joined, file.path(ckat_output, "cog_processed_dvs_crs.rds"))
saveRDS(fdr_joined, file.path(ckat_output, "cog_processed_dvs_fdr.rds"))
saveRDS(flk_joined, file.path(ckat_output, "cog_processed_dvs_flk.rds"))
saveRDS(ps_joined, file.path(ckat_output, "cog_processed_dvs_ps.rds"))



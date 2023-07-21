
# whether to export files
options(aow_export_denom = TRUE)


library(tidyverse)
library(labelled)
library(readr)
library(haven)

output_path <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/"


# load data frames

denom_pseudo <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\denom\\data\\denom_pseudo.rds")
denom <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\denom\\data\\denom_identifiable.rds")
bioimp <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\measures\\data\\aow_bioimpedance.rds")
bp <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\measures\\data\\aow_bp.rds")
sk <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\measures\\data\\aow_sk.rds")
htwt <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\measures\\data\\aow_heightweight.rds")
srv1 <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\survey\\data\\aow_survey_module1_labelled.rds")
srv2 <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\survey\\data\\aow_survey_module2_labelled.rds")
srv3 <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\survey\\data\\aow_survey_module3_labelled.rds")
srv4 <- readRDS("U:\\Born In Bradford - Confidential\\Data\\BiB\\processing\\AoW\\survey\\data\\aow_survey_module4_labelled.rds")


dat_df <- denom |> select(aow_recruitment_id) |>
  left_join(srv1 |> select(aow_recruitment_id) |> mutate(has_survey_m1 = 1) |> unique()) |>
  left_join(srv2 |> select(aow_recruitment_id) |> mutate(has_survey_m2 = 1) |> unique()) |>
  left_join(srv3 |> select(aow_recruitment_id) |> mutate(has_survey_m3 = 1) |> unique()) |>
  left_join(srv4 |> select(aow_recruitment_id) |> mutate(has_survey_m4 = 1) |> unique()) |>
  left_join(bioimp |> select(aow_recruitment_id) |> mutate(has_bioimp = 1) |> unique()) |>
  left_join(bp |> select(aow_recruitment_id) |> mutate(has_bp = 1) |> unique()) |>
  left_join(sk |> select(aow_recruitment_id) |> mutate(has_skinfld = 1) |> unique()) |>
  left_join(htwt |> select(aow_recruitment_id) |> mutate(has_htwt = 1) |> unique())

dat_df <- dat_df |> mutate(across(starts_with("has_"), ~if_else(!is.na(.), 1, 0))) |>
  mutate(has_survey = ifelse(has_survey_m1 == 1 | has_survey_m2 == 1 | has_survey_m3 == 1 | has_survey_m4 == 1, 1, 0),
         has_measure = ifelse(has_bioimp == 1 | has_bp == 1 | has_skinfld == 1 | has_htwt == 1, 1, 0),
         has_data = ifelse(has_survey == 1 | has_measure == 1, 1, 0))

dat_df <- dat_df |> set_variable_labels(has_survey_m1 = "Has AoW survey module 1",
                                        has_survey_m2 = "Has AoW survey module 2",
                                        has_survey_m3 = "Has AoW survey module 3",
                                        has_survey_m4 = "Has AoW survey module 4",
                                        has_survey = "Has any AoW survey",
                                        has_bioimp = "Has AoW bioimpedance",
                                        has_bp = "Has AoW blood pressures",
                                        has_skinfld = "Has AoW skinfolds",
                                        has_htwt = "Has AoW height/weight",
                                        has_measure = "Has any AoW health measure",
                                        has_data = "Has any AoW data"
)

has_labs <- c(Yes = 1, No = 0)

dat_df <- dat_df |> set_value_labels(has_survey_m1 = has_labs,
                                        has_survey_m2 = has_labs,
                                        has_survey_m3 = has_labs,
                                        has_survey_m4 = has_labs,
                                        has_survey = has_labs,
                                        has_bioimp = has_labs,
                                        has_bp = has_labs,
                                        has_skinfld = has_labs,
                                        has_htwt = has_labs,
                                        has_measure = has_labs,
                                        has_data = has_labs
)

dat_df <- dat_df |> select(aow_recruitment_id,
                           has_survey_m1,
                           has_survey_m2,
                           has_survey_m3,
                           has_survey_m4,
                           has_survey,
                           has_bioimp,
                           has_bp,
                           has_skinfld,
                           has_htwt,
                           has_measure,
                           has_data)

has_cols <- names(dat_df |> select(starts_with("has_")))

denom <- denom |> select(-any_of(has_cols)) |> left_join(dat_df)
denom_pseudo <- denom_pseudo |> select(-any_of(has_cols)) |> left_join(dat_df)


# export
if(getOption("aow_export_denom")) {
  
  saveRDS(denom, file.path(output_path, "denom_identifiable.rds"))
  write_dta(denom, file.path(output_path, "denom_identifiable.dta"))
  write_csv(denom, file.path(output_path, "denom_identifiable.csv"), na = "")
  
  saveRDS(denom_pseudo, file.path(output_path, "denom_pseudo.rds"))
  write_dta(denom_pseudo, file.path(output_path, "denom_pseudo.dta"))
  write_csv(denom_pseudo, file.path(output_path, "denom_pseudo.csv"), na = "")
  
}



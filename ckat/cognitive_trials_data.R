# cognitive trials data

library(tidyr)
library(dplyr)
library(labelled)
library(readxl)

cog_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")
cog_output <- file.path("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data")


## FLANKER

# get raw flanker trials data
rawtrials <- readRDS(file.path(cog_input, "FLK_Trials_data_cognitive.rds"))

# denominator is best taken from the processed data as it was easier to
# identify and remove ids with repeat sessions from here
denom <- readRDS(file.path(cog_output, "cog_processed_dvs_flk.rds")) |> select(aow_recruitment_id:p_handedness)

rawtrials <- rawtrials |> transmute(aow_recruitment_id = tolower(FilePath),
                                    test = as.integer(Test),
                                    trial = as.integer(Trial),
                                    congruence = case_when(Congruence == "Congruent" ~ 1,
                                                           Congruence == "Incongruent" ~ 2),
                                    direction = case_when(Direction == "Right" ~ 1,
                                                          Direction == "Left" ~ 2),
                                    response = case_when(Response == "Right" ~ 1,
                                                         Response == "Left" ~ 2),
                                    ans = as.integer(Ans),
                                    rt = as.numeric(RT)) |>
  set_value_labels(test = c(Practice = 0, Test = 1),
                   congruence = c(Congruent = 1, Incongruent = 2),
                   direction = c(Right = 1, Left = 2),
                   response = c(Right = 1, Left = 2),
                   ans = c(Incorrect = 0, Correct = 1)) |>
  set_variable_labels(test = "Flanker task trial - practice or test",
                      trial = "Flanker task trial number",
                      congruence = "Flanker task trial congruence",
                      direction = "Flanker task trial direction",
                      response = "Flanker task trial response",
                      ans = "Flanker task trial reponse correct or not",
                      rt = "Flanker task trial reaction time")

flk_trials <- denom |> inner_join(rawtrials, by = "aow_recruitment_id")


# export
saveRDS(flk_trials, file.path(cog_output, "cog_trials_flk.rds"))




## PROCESSING SPEED

# get raw processing speed trials data
rawtrials <- readRDS(file.path(cog_input, "PS_Trials_data_cognitive.rds"))

# denominator is best taken from the processed data as it was easier to
# identify and remove ids with repeat sessions from here
denom <- readRDS(file.path(cog_output, "cog_processed_dvs_ps.rds")) |> select(aow_recruitment_id:p_handedness)

rawtrials <- rawtrials |> transmute(aow_recruitment_id = tolower(FilePath),
                                    test = as.integer(Test),
                                    trial = as.integer(Trial),
                                    red_circles = as.integer(N_Red_Circles),
                                    red_triangles = as.integer(N_Red_Triangles),
                                    blue_circles = as.integer(N_Blue_Circles),
                                    response = as.integer(Response),
                                    ans = as.integer(Ans),
                                    rt = as.numeric(RT)) |>
  set_value_labels(test = c(Practice = 0, Test = 1),
                   ans = c(Incorrect = 0, Correct = 1)) |>
  set_variable_labels(test = "Processing speed trial - practice or test",
                      trial = "Processing speed trial number",
                      red_circles = "Processing speed number of red circles",
                      red_triangles = "Processing speed number of red triangles",
                      blue_circles = "Processing speed number of blue circles",
                      response = "Processing speed trial response",
                      ans = "Processing speed trial reponse correct or not",
                      rt = "Processing speed trial reaction time")

ps_trials <- denom |> inner_join(rawtrials, by = "aow_recruitment_id")


# export
saveRDS(ps_trials, file.path(cog_output, "cog_trials_ps.rds"))





## WORKING MEMORY

# get raw processing speed trials data
rawtrials <- readRDS(file.path(cog_input, "WM_Trials_data_cognitive.rds"))

# denominator is best taken from the processed data as it was easier to
# identify and remove ids with repeat sessions from here
denom_bdr <- readRDS(file.path(cog_output, "cog_processed_dvs_bdr.rds")) |> select(aow_recruitment_id:p_handedness)
denom_crs <- readRDS(file.path(cog_output, "cog_processed_dvs_crs.rds")) |> select(aow_recruitment_id:p_handedness)
denom_fdr <- readRDS(file.path(cog_output, "cog_processed_dvs_fdr.rds")) |> select(aow_recruitment_id:p_handedness)

# split by task
bdr_raw <- rawtrials |> filter(FileName == "BDR_trials.csv")
crs_raw <- rawtrials |> filter(FileName == "CRS_trials.csv")
fdr_raw <- rawtrials |> filter(FileName == "FDR_trials.csv")

# format and label bdr
bdr_raw <- bdr_raw |> transmute(aow_recruitment_id = tolower(FilePath),
                                test = as.integer(Test),
                                trial = as.integer(Trial),
                                stim_length = as.integer(Stim_length),
                                stim_presented = Presented,
                                current_digit = as.integer(Current_digit),
                                response = as.integer(Response),
                                ans = as.integer(Ans),
                                rt = as.numeric(RT)) |>
  set_value_labels(test = c(Practice = 0, Test = 1),
                   ans = c(Incorrect = 0, Correct = 1)) |>
  set_variable_labels(test = "Backward digit recall trial - practice or test",
                      trial = "Backward digit recall trial number",
                      stim_length = "Backward digit recall stimuli length",
                      stim_presented = "Backward digit recall stimuli presented",
                      current_digit = "Backward digit recall currently presented digit",
                      response = "Backward digit recall trial response",
                      ans = "Backward digit recall trial reponse correct or not",
                      rt = "Backward digit recall trial reaction time")


# format and label crs
crs_raw <- crs_raw |> transmute(aow_recruitment_id = tolower(FilePath),
                                test = as.integer(Test),
                                trial = as.integer(Trial),
                                stim_length = as.integer(Stim_length),
                                stim_presented = Presented,
                                current_digit = as.integer(Current_digit),
                                response = as.integer(Response),
                                ans = as.integer(Ans),
                                rt = as.numeric(RT)) |>
  set_value_labels(test = c(Practice = 0, Test = 1),
                   ans = c(Incorrect = 0, Correct = 1)) |>
  set_variable_labels(test = "Corsi trial - practice or test",
                      trial = "Corsi trial number",
                      stim_length = "Corsi stimuli length",
                      stim_presented = "Corsi stimuli presented",
                      current_digit = "Corsi currently presented digit",
                      response = "Corsi trial response",
                      ans = "Corsi trial reponse correct or not",
                      rt = "Corsi trial reaction time")


# format and label fdr
fdr_raw <- fdr_raw |> transmute(aow_recruitment_id = tolower(FilePath),
                                test = as.integer(Test),
                                trial = as.integer(Trial),
                                stim_length = as.integer(Stim_length),
                                stim_presented = Presented,
                                current_digit = as.integer(Current_digit),
                                response = as.integer(Response),
                                ans = as.integer(Ans),
                                rt = as.numeric(RT)) |>
  set_value_labels(test = c(Practice = 0, Test = 1),
                   ans = c(Incorrect = 0, Correct = 1)) |>
  set_variable_labels(test = "Forward digit recall trial - practice or test",
                      trial = "Forward digit recall trial number",
                      stim_length = "Forward digit recall stimuli length",
                      stim_presented = "Forward digit recall stimuli presented",
                      current_digit = "Forward digit recall currently presented digit",
                      response = "Forward digit recall trial response",
                      ans = "Forward digit recall trial reponse correct or not",
                      rt = "Forward digit recall trial reaction time")


# join each to their own processed dvs as a denominator
bdr_trials <- denom_bdr |> inner_join(bdr_raw, by = "aow_recruitment_id")
crs_trials <- denom_crs |> inner_join(crs_raw, by = "aow_recruitment_id")
fdr_trials <- denom_fdr |> inner_join(fdr_raw, by = "aow_recruitment_id")


# export
saveRDS(bdr_trials, file.path(cog_output, "cog_trials_bdr.rds"))
saveRDS(crs_trials, file.path(cog_output, "cog_trials_crs.rds"))
saveRDS(fdr_trials, file.path(cog_output, "cog_trials_fdr.rds"))




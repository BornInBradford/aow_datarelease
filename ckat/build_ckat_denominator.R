# ckat denominator cleaning

library(tidyr)
library(dplyr)
library(labelled)

# get denominator
denom <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/denom_identifiable.rds")

denom <- denom |> select(aow_person_id,
                         BiBPersonID,
                         is_bib,
                         aow_recruitment_id,
                         birth_date,
                         birth_year,
                         birth_month,
                         recruitment_era,
                         age_recruitment_y,
                         age_recruitment_m,
                         school_id,
                         form_tutor_id,
                         gender,
                         ethnicity_1,
                         ethnicity_2)

ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")

participants <- readRDS(file.path(ckat_input, "ParticipantInfo.rds"))

# duplicates check
participants |>
  dplyr::summarise(n = dplyr::n(), .by = c(FilePath, UploadTime, variable)) |>
  dplyr::filter(n > 1L) 

# drop "empty" variable names as we get a duplicate on this
participants <- participants |> filter(variable != "empty")

# duplicates check
participants |>
  dplyr::summarise(n = dplyr::n(), .by = c(FilePath, UploadTime, variable)) |>
  dplyr::filter(n > 1L) 

# do initial pivot
participants <- participants |> pivot_wider(id_cols = c("FilePath", "UploadTime"), names_from = "variable", values_from = "value")

# format AOW_ID
participants <- participants |> mutate(AOW_ID = gsub("[^aowAOW0-9]", "", AOW_ID),
                                      AOW_ID = tolower(AOW_ID))
# do linkage
unlinked_records <- participants |> anti_join(denom, by = c("AOW_ID" = "aow_recruitment_id"))
participants <- denom |> inner_join(participants, by = c("aow_recruitment_id" = "AOW_ID"))

# still getting some duplicates that appear to be due to data being uploaded twice
participants |> duplicated() |> which() |> length()
participants |> select(-UploadTime) |> duplicated() |> which() |> length()

# take the first recorded upload where duplicated
participants <- participants |> arrange(UploadTime)
participants <- participants[!(participants |> select(-UploadTime) |> duplicated()), ]

# build ckat denominator with session info
ckat_sessions <- transmute(participants,
                           aow_person_id,
                           BiBPersonID,
                           is_bib,
                           aow_recruitment_id,
                           birth_year,
                           birth_month,
                           recruitment_era,
                           age_recruitment_y,
                           age_recruitment_m,
                           school_id,
                           form_tutor_id,
                           gender,
                           ethnicity_1,
                           ethnicity_2,
                           test_device = DEV_ID,
                           test_date = as.Date(TEST_DATE, format = "%d/%m/%Y"),
                           test_start_time = case_when(TEST_START == "empty" ~ NA,
                                                       .default = TEST_START),
                           test_end_time = case_when(TEST_END == "empty" ~ NA,
                                                     .default = TEST_END),
                           participant_handedness = case_when(HANDEDNESS == "Left" ~ 1L,
                                                              HANDEDNESS == "Right" ~ 2L,
                                                              HANDEDNESS == "empty" ~ -1L),
                           participant_presentation = case_when(ASSENT == "No" | MIA == "Declined" ~ 1L,
                                                                ASSENT == "ABSENT" | tolower(trimws(MIA)) == "absent" ~ 2L,
                                                                MIA == "Present" ~ 3L,
                                                                MIA == "empty" ~ -1L),
                           session_completed = case_when(tolower(trimws(COMPLETED)) == "true" ~ 1L,
                                                         tolower(trimws(COMPLETED)) == "false" ~ 0L),
                           session_interrupted = case_when(tolower(trimws(INTERRUPTED)) == "true" ~ 1L,
                                                           tolower(trimws(INTERRUPTED)) == "false" ~ 0L),
                           session_errors = case_when(tolower(trimws(ERRORS)) == "true" ~ 1L,
                                                      tolower(trimws(ERRORS)) == "false" ~ 0L),
                           session_notes = coalesce(`Extra Info`, info, `Etxra info`, `NOTES`, `NOTE`),
                           session_notes = case_when(session_notes == "empty" ~ NA,
                                                     .default = session_notes),
                           ckat_version = CKAT_VERSION) |>
  set_value_labels(participant_handedness = c("Left" = 1,
                                              "Right" = 2,
                                              "missing" = -1),
                   participant_presentation = c("Declined consent" = 1,
                                                "Absent" = 2,
                                                "Present" = 3,
                                                "missing" = -1),
                   session_completed = c("Yes" = 1,
                                         "No" = 0),
                   session_interrupted = c("Yes" = 1,
                                           "No" = 0),
                   session_errors = c("Yes" = 1,
                                      "No" = 0)) |>
  set_variable_labels(test_device = "CKAT tablet PC ID used for testing session",
                      test_date = "CKAT test session date",
                      test_start_time = "CKAT test session start time",
                      test_end_time = "CKAT test session end time",
                      participant_handedness = "Participant handedness recorded during CKAT session",
                      participant_presentation = "Particpant present, absent or declined consent for CKAT session",
                      session_completed = "CKAT test session complete - flagged by software",
                      session_interrupted = "CKAT test session interrupted - flagged by software",
                      session_errors = "CKAT test session errors - flagged by software",
                      session_notes = "CKAT test session operator notes",
                      ckat_version = "CKAT test session software version")




saveRDS(ckat_sessions, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/ckat/data/ckat_participant_sessions.rds")




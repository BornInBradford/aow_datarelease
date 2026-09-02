library(tidyverse)

build_wave_data <- function(raw_df) {
  
  years <- c("2022-23", "2023-24", "2024-25", "2025-26")
  skeleton <- expand_grid(id = unique(raw_df$id), year = years)
  
  # imputing dsa and ht_agreed from downstream indicators
  full_df <- skeleton |>
    left_join(raw_df, by = c("id", "year")) |>
    mutate(
      any_data_collected = coalesce(data_y8, 0) > 0 | coalesce(data_y9, 0) > 0 | coalesce(data_y10, 0) > 0,
      any_received       = coalesce(received_y8, 0) > 0 | coalesce(received_y9, 0) > 0 | coalesce(received_y10, 0) > 0,
      dsa = case_when(
        dsa == 1 ~ 1,
        any_data_collected | any_received ~ 1,
        TRUE ~ dsa
      ),
      ht_agreed = case_when(
        ht_agreed == 1 ~ 1,
        dsa == 1 ~ 1,
        TRUE ~ ht_agreed
      )
    ) |>
    select(-any_data_collected, -any_received)
  
  # reshape long
  long_df <- full_df |>
    pivot_longer(
      cols = c(n_y8, n_y9, n_y10, 
               received_y8, received_y9, received_y10,
               received_new_y8, received_new_y9, received_new_y10,
               data_y8, data_y9, data_y10,
               data_new_y8, data_new_y9, data_new_y10),
      names_to = c(".value", "year_group"),
      names_pattern = "(n|received|received_new|data|data_new)_y(\\d+)"
    ) |>
    rename(roll = n, received_n = received, received_new_n = received_new, data_n = data, data_new_n = data_new) |>
    mutate(
      year_group = as.integer(year_group),
      year_start = as.integer(str_sub(year, 1, 4)),
      cohort_id  = year_start - year_group
    )
  
  # cohort-based roll imputation
  long_df <- long_df |>
    arrange(id, cohort_id, year_group) |>
    group_by(id, cohort_id) |>
    mutate(
      roll_observed = roll,
      roll = zoo::na.locf(roll, na.rm = FALSE) |>
        zoo::na.locf(fromLast = TRUE, na.rm = FALSE),
      roll_status = case_when(
        !is.na(roll_observed) ~ "observed",
        is.na(roll_observed) & !is.na(roll) ~ "imputed_from_cohort",
        TRUE ~ "unknown"
      )
    ) |>
    ungroup() |>
    select(-roll_observed)
  
  # roll correction against received_n
  long_df <- long_df |>
    mutate(
      roll_pre_adjustment = roll,
      kids_estimated = if_else(
        !is.na(received_n) & (is.na(roll) | received_n > roll),
        received_n - coalesce(roll, 0),
        0
      ),
      roll = if_else(
        !is.na(received_n) & (is.na(roll) | received_n > roll),
        received_n,
        roll
      ),
      roll_status = if_else(kids_estimated > 0, "adjusted_to_received", roll_status)
    ) |>
    select(-roll_pre_adjustment)
  
  # setup_completed: row-level per year_group
  long_df <- long_df |>
    group_by(id, year) |>
    mutate(
      setup_completed = case_when(
        sum(received_n, na.rm = TRUE) > 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      received_exceeds_roll = FALSE, # no longer used
      data_exceeds_received = !is.na(received_n) & !is.na(data_n) & data_n > received_n
    ) |>
    ungroup()
  
  # year_group_excluded: school sent details for at least one year group this year, but this one was zero
  long_df <- long_df |>
    group_by(id, year) |> 
    mutate(year_group_excluded = !is.na(dsa) & !is.na(received_n) & any(dsa == 1) & received_n == 0 & !all(received_n == 0)) |>
    ungroup()
  
  long_df
}

output_dir <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/school_onboarding/data/"

raw_records <- readRDS(paste0(output_dir, "aow_school_process.rds"))
wave_data <- build_wave_data(raw_records)
saveRDS(wave_data, paste0(output_dir, "aow_wave_data.rds"))



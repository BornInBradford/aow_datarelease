library(tidyverse)

# schools affected and sessions per level
roll_adjustment_summary <- function(wave_data) {
  
  adjusted_rows <- wave_data |>
    filter(roll_status == "adjusted_to_received")
  
  n_schools_affected <- n_distinct(adjusted_rows$id)
  
  n_sessions <- adjusted_rows |>
    summarise(
      year_group_sessions = n_distinct(paste(id, year_group)),
      year_sessions = n_distinct(paste(id, year)),
      year_group_in_year_sessions = n_distinct(paste(id, year, year_group))
    )
  
  list(
    n_schools_affected = n_schools_affected,
    sessions = n_sessions,
    affected_rows = adjusted_rows |>
      select(id, year, year_group, roll, received_n, kids_estimated, roll_status)
  )
}

# retention rate: all rolls inc estimates vs. observed roll only
retention_rate_comparison <- function(wave_data, group_vars = NULL) {
  
  wave_data_flagged <- wave_data |>
    filter(!is.na(roll)) |>
    mutate(
      roll_observed_only = if_else(roll_status == "observed" | roll_status == "imputed_from_cohort",
                                   roll, NA_real_)
    )
  
  if (is.null(group_vars)) {
    wave_data_flagged |>
      summarise(
        setup_completed_n = sum(roll[setup_completed == TRUE], na.rm = TRUE),
        received_n_total = sum(received_n, na.rm = TRUE),
        retention_rate_corrected = received_n_total / setup_completed_n,
        
        setup_completed_n_obs = sum(roll_observed_only[setup_completed == TRUE], na.rm = TRUE),
        received_n_total_obs = sum(received_n[!is.na(roll_observed_only)], na.rm = TRUE),
        retention_rate_observed_only = received_n_total_obs / setup_completed_n_obs
      )
  } else {
    wave_data_flagged |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        setup_completed_n = sum(roll[setup_completed == TRUE], na.rm = TRUE),
        received_n_total = sum(received_n, na.rm = TRUE),
        retention_rate_corrected = received_n_total / setup_completed_n,
        
        setup_completed_n_obs = sum(roll_observed_only[setup_completed == TRUE], na.rm = TRUE),
        received_n_total_obs = sum(received_n[!is.na(roll_observed_only)], na.rm = TRUE),
        retention_rate_observed_only = received_n_total_obs / setup_completed_n_obs,
        .groups = "drop"
      )
  }
}


output_dir <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/school_onboarding/data/"

wave_data <- readRDS(paste0(output_dir, "aow_wave_data.rds"))


roll_qc <- roll_adjustment_summary(wave_data)

rate_ovr <- retention_rate_comparison(wave_data)                                   
rate_yr <- retention_rate_comparison(wave_data, group_vars = "year")              
rate_yg <- retention_rate_comparison(wave_data, group_vars = "year_group")        
rate_session <- retention_rate_comparison(wave_data, group_vars = c("year","year_group"))


save(roll_qc, rate_ovr, rate_yr, rate_yg, rate_session, file = paste0(output_dir, "qc_rates.RData"))


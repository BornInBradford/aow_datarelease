library(tidyverse)


table_col_order <- function() {
  
  col_order <- c("id",
                 "year",
                 "year_group",
                 "schools_eligible",
                 "eligible",
                 "estimated_n",
                 "schools_agreed",
                 "ht_agreed_n",
                 "schools_yr_not_agreed",
                 "schools_never_agreed",
                 "ch_not_agreed",
                 "schools_dsa",
                 "dsa_n",
                 "schools_yr_no_dsa",
                 "ch_no_dsa",
                 "schools_setup",
                 "setup_completed_n",
                 "schools_yr_no_setup",
                 "ch_no_setup",
                 "schools_received",
                 "schools_yr_yg_exclusions",
                 "year_group_excluded_n",
                 "received_n_total",
                 "received_new_total",
                 "received_rpt_total",
                 "retention_rate",
                 "schools_yr_exclusions",
                 "ch_excluded",
                 "schools_with_data",
                 "data_obtained_n",
                 "data_obtained_new_n",
                 "data_obtained_rpt_n",
                 "response_rate",
                 "overall_yield",
                 "ch_no_data"
  )
  
  return(col_order)
  
}

# collapse row-level flags to school/year level
school_year_summary <- function(wave_data) {
  wave_data |>
    group_by(id, year) |>
    summarise(
      ht_agreed_year = as.integer(any(ht_agreed == 1, na.rm = TRUE)),
      dsa_year = as.integer(any(dsa == 1, na.rm = TRUE)),
      setup_completed_year = case_when(
        any(setup_completed == TRUE, na.rm = TRUE) ~ TRUE,
        all(is.na(setup_completed)) ~ NA,
        TRUE ~ FALSE
      ),
      .groups = "drop"
    )
}

# collapse to school level
school_ever_summary <- function(syr_summary) {
  syr_summary |>
    group_by(id) |>
    summarise(
      ht_agreed_ever = as.integer(any(ht_agreed_year == 1, na.rm = TRUE)),
      dsa_ever = as.integer(any(dsa_year == 1, na.rm = TRUE)),
      setup_completed_ever = case_when(
        any(setup_completed_year == TRUE, na.rm = TRUE) ~ TRUE,
        all(is.na(setup_completed_year)) ~ NA,
        TRUE ~ FALSE
      ),
      .groups = "drop"
    )
}


consort_by_wave <- function(wave_data) {
  syr <- school_year_summary(wave_data)
  ever <- school_ever_summary(syr)
  
  school_counts <- tibble(year_group = c(8L, 9L, 10L)) |>
    mutate(
      schools_eligible = n_distinct(wave_data$id),
      schools_agreed = sum(ever$ht_agreed_ever == 1, na.rm = TRUE),
      schools_yr_not_agreed = n_distinct(syr$id[syr$ht_agreed_year == 0]),
      schools_never_agreed = sum(ever$ht_agreed_ever == 0, na.rm = TRUE),
      schools_dsa = sum(ever$dsa_ever == 1, na.rm = TRUE),
      schools_yr_no_dsa = n_distinct(syr$id[syr$ht_agreed_year == 1 & syr$dsa_year == 0]),
      schools_setup = sum(ever$setup_completed_ever == TRUE, na.rm = TRUE),
      schools_yr_no_setup = n_distinct(syr$id[syr$dsa_year == 1 & !syr$setup_completed_year])
    )
  
  child_counts <- wave_data |>
    filter(!is.na(roll)) |>
    group_by(year_group) |>
    summarise(
      eligible = sum(roll, na.rm = TRUE),
      estimated_n = sum(kids_estimated, na.rm = TRUE),
      ht_agreed_n = sum(roll[ht_agreed == 1], na.rm = TRUE),
      dsa_n = sum(roll[dsa == 1], na.rm = TRUE),
      setup_completed_n = sum(roll[setup_completed == TRUE], na.rm = TRUE),
      year_group_excluded_n = sum(roll[setup_completed == TRUE & year_group_excluded == TRUE], na.rm = TRUE),
      received_n_total = sum(received_n, na.rm = TRUE),
      received_new_total = sum(received_new_n, na.rm = TRUE),
      schools_received = n_distinct(id[!is.na(received_n) & received_n > 0]),
      schools_yr_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & !year_group_excluded & received_n < roll]),
      schools_yr_yg_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & year_group_excluded & received_n < roll]),
      data_obtained_n = sum(data_n, na.rm = TRUE),
      data_obtained_new_n = sum(data_new_n, na.rm = TRUE),
      schools_with_data = n_distinct(id[!is.na(data_n) & data_n > 0]),
      .groups = "drop"
    ) |>
    mutate(
      ch_not_agreed  = eligible - ht_agreed_n,
      ch_no_dsa = ht_agreed_n - dsa_n,
      ch_no_setup = dsa_n - setup_completed_n,
      ch_excluded = (setup_completed_n - year_group_excluded_n) - received_n_total,
      ch_no_data = received_n_total - data_obtained_n,
      received_rpt_total = received_n_total - received_new_total,
      data_obtained_rpt_n = data_obtained_n - data_obtained_new_n,
      retention_rate = received_n_total / setup_completed_n,
      response_rate = data_obtained_n / received_n_total,
      overall_yield = data_obtained_n / eligible
    )
  
  ct <- school_counts |> left_join(child_counts, by = "year_group")
  
  ct <- ct |> select(any_of(table_col_order()))
  
}

consort_by_year <- function(wave_data) {
  syr <- school_year_summary(wave_data)
  
  school_counts <- syr |>
    group_by(year) |>
    summarise(
      schools_eligible = n_distinct(id),
      schools_agreed = sum(ht_agreed_year == 1, na.rm = TRUE),
      schools_yr_not_agreed = sum(ht_agreed_year == 0, na.rm = TRUE),
      schools_dsa = sum(dsa_year == 1, na.rm = TRUE),
      schools_yr_no_dsa = sum(ht_agreed_year == 1 & dsa_year == 0, na.rm = TRUE),
      schools_setup = sum(setup_completed_year == TRUE, na.rm = TRUE),
      schools_yr_no_setup = sum(dsa_year == 1 & !setup_completed_year, na.rm = TRUE),
      .groups = "drop"
    )
  
  child_counts <- wave_data |>
    filter(!is.na(roll)) |>
    group_by(year) |>
    summarise(
      eligible = sum(roll, na.rm = TRUE),
      estimated_n = sum(kids_estimated, na.rm = TRUE),
      ht_agreed_n = sum(roll[ht_agreed == 1], na.rm = TRUE),
      dsa_n  = sum(roll[dsa == 1], na.rm = TRUE),
      setup_completed_n = sum(roll[setup_completed == TRUE], na.rm = TRUE),
      year_group_excluded_n = sum(roll[setup_completed == TRUE & year_group_excluded == TRUE], na.rm = TRUE),
      received_n_total = sum(received_n, na.rm = TRUE),
      received_new_total = sum(received_new_n, na.rm = TRUE),
      schools_received = n_distinct(id[!is.na(received_n) & received_n > 0]),
      schools_yr_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & !year_group_excluded & received_n < roll]),
      schools_yr_yg_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & year_group_excluded & received_n < roll]),
      data_obtained_n = sum(data_n, na.rm = TRUE),
      data_obtained_new_n = sum(data_new_n, na.rm = TRUE),
      schools_with_data = n_distinct(id[!is.na(data_n) & data_n > 0]),
      .groups = "drop"
    ) |>
    mutate(
      ch_not_agreed = eligible - ht_agreed_n,
      ch_no_dsa = ht_agreed_n - dsa_n,
      ch_no_setup = dsa_n - setup_completed_n,
      ch_excluded = (setup_completed_n - year_group_excluded_n) - received_n_total,
      ch_no_data = received_n_total - data_obtained_n,
      received_rpt_total = received_n_total - received_new_total,
      data_obtained_rpt_n = data_obtained_n - data_obtained_new_n,
      retention_rate = received_n_total / setup_completed_n,
      response_rate = data_obtained_n / received_n_total,
      overall_yield = data_obtained_n / eligible
    )
  
  ct <- school_counts |> left_join(child_counts, by = "year")
  
  ct <- ct |> select(any_of(table_col_order()))
  
}

consort_by_year_and_wave <- function(wave_data) {
  syr <- school_year_summary(wave_data)
  
  school_counts <- syr |>
    group_by(year) |>
    summarise(
      schools_eligible = n_distinct(id),
      schools_agreed = sum(ht_agreed_year == 1, na.rm = TRUE),
      schools_yr_not_agreed = sum(ht_agreed_year == 0, na.rm = TRUE),
      schools_dsa = sum(dsa_year == 1, na.rm = TRUE),
      schools_yr_no_dsa = sum(ht_agreed_year == 1 & dsa_year == 0, na.rm = TRUE),
      schools_setup = sum(setup_completed_year == TRUE, na.rm = TRUE),
      schools_yr_no_setup = sum(dsa_year == 1 & !setup_completed_year, na.rm = TRUE),
      .groups = "drop"
    )
  
  child_counts <- wave_data |>
    filter(!is.na(roll)) |>
    group_by(year, year_group) |>
    summarise(
      eligible = sum(roll, na.rm = TRUE),
      estimated_n = sum(kids_estimated, na.rm = TRUE),
      ht_agreed_n = sum(roll[ht_agreed == 1], na.rm = TRUE),
      dsa_n = sum(roll[dsa == 1], na.rm = TRUE),
      setup_completed_n = sum(roll[setup_completed == TRUE], na.rm = TRUE),
      year_group_excluded_n = sum(roll[setup_completed == TRUE & year_group_excluded == TRUE], na.rm = TRUE),
      received_n_total = sum(received_n, na.rm = TRUE),
      received_new_total = sum(received_new_n, na.rm = TRUE),
      schools_received = n_distinct(id[!is.na(received_n) & received_n > 0]),
      schools_yr_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & !year_group_excluded & received_n < roll]),
      schools_yr_yg_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & year_group_excluded & received_n < roll]),
      data_obtained_n = sum(data_n, na.rm = TRUE),
      data_obtained_new_n = sum(data_new_n, na.rm = TRUE),
      schools_with_data = n_distinct(id[!is.na(data_n) & data_n > 0]),
      .groups = "drop"
    )
  
  ct <- child_counts |> 
    left_join(school_counts, by = "year") |>
    mutate(
      ch_not_agreed = eligible - ht_agreed_n,
      ch_no_dsa = ht_agreed_n - dsa_n,
      ch_no_setup = dsa_n - setup_completed_n,
      ch_excluded = (setup_completed_n - year_group_excluded_n) - received_n_total,
      ch_no_data = received_n_total - data_obtained_n,
      received_rpt_total = received_n_total - received_new_total,
      data_obtained_rpt_n = data_obtained_n - data_obtained_new_n,
      retention_rate = received_n_total / setup_completed_n,
      response_rate = data_obtained_n / received_n_total,
      overall_yield = data_obtained_n / eligible
    )
  
  ct <- ct |> select(any_of(table_col_order()))
  
}

consort_combined <- function(wave_data) {
  syr <- school_year_summary(wave_data)
  ever <- school_ever_summary(syr)
  
  school_counts <- tibble(
    schools_eligible = n_distinct(wave_data$id),
    schools_agreed = sum(ever$ht_agreed_ever == 1, na.rm = TRUE),
    schools_yr_not_agreed = n_distinct(syr$id[syr$ht_agreed_year == 0]),
    schools_never_agreed = sum(ever$ht_agreed_ever == 0, na.rm = TRUE),
    schools_dsa = sum(ever$dsa_ever == 1, na.rm = TRUE),
    schools_yr_no_dsa = n_distinct(syr$id[syr$ht_agreed_year == 1 & syr$dsa_year == 0]),
    schools_setup = sum(ever$setup_completed_ever == TRUE, na.rm = TRUE),
    schools_yr_no_setup = n_distinct(syr$id[syr$dsa_year == 1 & !syr$setup_completed_year])
  )
  
  child_counts <- wave_data |>
    filter(!is.na(roll)) |>
    summarise(
      eligible = sum(roll, na.rm = TRUE),
      estimated_n = sum(kids_estimated, na.rm = TRUE),
      ht_agreed_n = sum(roll[ht_agreed == 1], na.rm = TRUE),
      dsa_n  = sum(roll[dsa == 1], na.rm = TRUE),
      setup_completed_n = sum(roll[setup_completed == TRUE], na.rm = TRUE),
      year_group_excluded_n = sum(roll[setup_completed == TRUE & year_group_excluded == TRUE], na.rm = TRUE),
      received_n_total = sum(received_n, na.rm = TRUE),
      received_new_total = sum(received_new_n, na.rm = TRUE),
      schools_received = n_distinct(id[!is.na(received_n) & received_n > 0]),
      schools_yr_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & !year_group_excluded & received_n < roll]),
      schools_yr_yg_exclusions = n_distinct(id[!is.na(received_n) & setup_completed & year_group_excluded & received_n < roll]),
      data_obtained_n = sum(data_n, na.rm = TRUE),
      data_obtained_new_n = sum(data_new_n, na.rm = TRUE),
      schools_with_data = n_distinct(id[!is.na(data_n) & data_n > 0])
    ) |>
    mutate(
      ch_not_agreed = eligible - ht_agreed_n,
      ch_no_dsa = ht_agreed_n - dsa_n,
      ch_no_setup = dsa_n - setup_completed_n,
      ch_excluded = (setup_completed_n - year_group_excluded_n) - received_n_total,
      ch_no_data = received_n_total - data_obtained_n,
      received_rpt_total = received_n_total - received_new_total,
      data_obtained_rpt_n = data_obtained_n - data_obtained_new_n,
      retention_rate = received_n_total / setup_completed_n,
      response_rate = data_obtained_n / received_n_total,
      overall_yield = data_obtained_n / eligible
    )
  
  ct <- bind_cols(school_counts, child_counts)
  
  ct <- ct |> select(any_of(table_col_order()))
  
}

check_dsa_roll_gap <- function(wave_data) {
  wave_data |>
    filter(dsa == 1, is.na(roll)) |>
    select(id, year, year_group, dsa, ht_agreed, roll, roll_status, received_n)
}

output_dir <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/school_onboarding/data/"

wave_data <- readRDS(paste0(output_dir, "aow_wave_data.rds"))

ct_combined <- consort_combined(wave_data)
ct_by_wave <- consort_by_wave(wave_data)
ct_by_year <- consort_by_year(wave_data)
ct_yr_wave <- consort_by_year_and_wave(wave_data)
chk <- check_dsa_roll_gap(wave_data)

save(ct_by_wave, ct_by_year, ct_combined, ct_yr_wave, file = paste0(output_dir, "consort_tables.RData"))


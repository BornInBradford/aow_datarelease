
source("tools/aow_survey_functions.R")

new_module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_linked.rds")

pre_mod1 <- readRDS("U:/Born in Bradford - AOW Raw Data/previous_release/survey/aow_survey_module1_labelled.rds")
pre_mod2 <- readRDS("U:/Born in Bradford - AOW Raw Data/previous_release/survey/aow_survey_module2_labelled.rds")
pre_mod3 <- readRDS("U:/Born in Bradford - AOW Raw Data/previous_release/survey/aow_survey_module3_labelled.rds")
pre_mod4 <- readRDS("U:/Born in Bradford - AOW Raw Data/previous_release/survey/aow_survey_module4_labelled.rds")


# check duplicates in aow_recruitment_id in previous release
pre_mod1$aow_recruitment_id |> duplicated() |> any()
pre_mod2$aow_recruitment_id |> duplicated() |> any()
pre_mod3$aow_recruitment_id |> duplicated() |> any()
pre_mod4$aow_recruitment_id |> duplicated() |> any()

new_names <- names(new_module)

pre_rec_ids <- c(pre_mod1$aow_recruitment_id, pre_mod2$aow_recruitment_id, pre_mod3$aow_recruitment_id, pre_mod4$aow_recruitment_id) |>
  unique()

# derive previous module merged administrative dataset
pre_mod_admin <- pre_mod1 |> select(any_of(aow_survey_column_order())) |>
  bind_rows(pre_mod2 |> select(any_of(aow_survey_column_order()))) |>
  bind_rows(pre_mod3 |> select(any_of(aow_survey_column_order()))) |>
  bind_rows(pre_mod4 |> select(any_of(aow_survey_column_order())))

# year_group value type has changed
pre_mod_admin <- pre_mod_admin |>
  mutate(year_group = year_group |> trimws() |> as.integer()) |>
  set_value_labels(year_group = c("Year 7" = 7,
                                  "Year 8" = 8,
                                  "Year 9" = 9,
                                  "Year 10" = 10,
                                  "Year 11" = 11,
                                  "Year 12" = 12,
                                  "Year 13" = 13,
                                  "various - see module specific data" = -1))

pre_mod_admin <- pre_mod_admin |> aow_pre_mod_admin_data()

# trim previous modules down to variables carried forward and drop admin cols
pre_mod1 <- pre_mod1 |> select(any_of(new_names)) |> select(-any_of(aow_pre_mod_drop_cols()))
pre_mod2 <- pre_mod2 |> select(any_of(new_names)) |> select(-any_of(aow_pre_mod_drop_cols()))
pre_mod3 <- pre_mod3 |> select(any_of(new_names)) |> select(-any_of(aow_pre_mod_drop_cols()))
pre_mod4 <- pre_mod4 |> select(any_of(new_names)) |> select(-any_of(aow_pre_mod_drop_cols()))

# merge all previous module data to be carried forward
pre_merged <- pre_mod_admin |>
  left_join(pre_mod1, by = "aow_recruitment_id") |>
  left_join(pre_mod2, by = "aow_recruitment_id") |>
  left_join(pre_mod3, by = "aow_recruitment_id") |>
  left_join(pre_mod4, by = "aow_recruitment_id")
  
# merge previous module data with new module data
integrated <- pre_merged |> bind_rows(new_module) |>
  set_variable_labels(.labels = aow_denom_col_labels(full = FALSE)) |>
  set_variable_labels(.labels = aow_survey_process_labels())

# restore column order
integrated <- integrated |> select(all_of(new_names))

# export
saveRDS(integrated, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_integrated.rds")


# Age of Wonder: survey

These scripts produce the Age of Wonder survey data files. The surveys are organised into separate data collection instruments called 'modules', and the pipeline and outputs follow this modular structure.

There is a separate pipeline for each survey module, each of which follows the same processing and output steps. The modular pipelines can be run end to end a module at a time from the `aow_module{x}_full_pipeline.R` scripts. Or, for convenience, all four modules can be run in one go from `run_all.R`.

Each of the scripts described below starts by importing and ends by exporting independently named files, so each script can be run independently if required. However, to run the pipeline from end to end follow these steps in order:

1. `aow_module{x}_survey_merge_miss.R` merges online/offline versions of the survey and tags missing values according to survey version where possible. Outputs `aow_survey_module{x}_merged.rds`

2. `aow_module{x}_administrative.R` adds administrative variables such as person ids, basic demographics and survey dates, then does a basic tidy up. Outputs `aow_survey_module{x}_linked.rds`

An html report of the `linked` data frame is produced to assist with the next steps.

3. `aow_module{x}_integration.R` integrates the current release with any previous survey releases where longitudinal variables can be formed across releases. Outputs `aow_survey_module{x}_integrated.rds`

An html report of the `integrated` data frame is produced to assist with the next steps.

4. `aow_module{x}_labelling.R` carries out final labelling of raw survey module variables and values and does any general tidying required. Outputs `aow_survey_module{x}_labelled.rds`

An html report of the `labelled` data frame is produced. This is the final output data file for the raw survey variables.

5. `aow_module{x}_derived_vars.R` derives new variables, e.g. scale scores, and adds these to a new dataset. Outputs `aow_survey_module{x}_derived.rds`

6. `aow_module{x}_derived_labelling.R` carries out final labelling of raw survey module variables and values and does any general tidying required. Outputs `aow_survey_module{x}_derived_labelled.rds`

An html report of the `derived labelled` data frame is produced. This is the final output data file for the derived variables.

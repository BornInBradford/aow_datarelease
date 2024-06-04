# BiB Age of Wonder data release

Age of Wonder is the secondary school data collection sweep of the Born in Bradford cohort study. This repository contains the scripts used in production of Age of Wonder data releases. The scripts mostly run in R with some Stata required. 

There is **no data** in the repository. To apply for data access, please see the [BiB website](https://borninbradford.nhs.uk/research/how-to-access-data/).

For those with direct access to the source data files, the instructions below explain how to set up and run the pipeline to reproduce the data release. The information is provided in full for anyone looking to understand more about the data release production process.

## Set up ignored files and directories

Under the repo root directory, create directories called `config` and `reports`.

### `config`

In `config` create a file called `set_aow_salt.R` containing one line as follows:

```R
options(aow_salt = "xyz")
```

You can replace "xyz" with any random string.

Unzip the postcode_lsoa_lookup.zip file into the `resources` folder, as the scripts need access to the `PCD_OA_LSOA_MSOA_LAD_NOV22_UK_LU.csv` file.

### `reports`

This is where html reports will go when they are produced by the data release scripts described below.

## Run the pipeline

### 1. Recruitment denominator

Build the recruitment denominator first by running `denom/load_and_tidy_denominator_inputs.R`

For more info on the output files, see the `denom` directory README. These output files are needed by the data output production scripts.

### 2. School visits measures

The scripts for creating the bioimpedance, heights/weights, skinfolds and blood pressures data files are in `measures/DataCleaning` and need to be run in Stata.

Data frame reports can be generated, but for this the data needs to be converted to .rds format. To do the conversion, run `measures/measures_convert_to_rds.R`. Then create the html reports by running `measures/measures_reports.R`.

See the `measures` directory README for more information on the output files.

### 3. Surveys

The surveys are split into different data collection instruments called 'modules'. This is the most complicated part of the pipeline, and there is more information on the process and how to run individual chunks of it in the `survey` directory README. 

If you just want to reproduce the whole pipeline in one go, run `survey/run_all.R`.

### 4. Data availability indicators

Now that the data outputs are generated, we can return to complete the denominator.

Run `denom/denom_add_data_availability_indicators.R` to add variable to the denominator files indicating which participants have data from each instrument.

Finally, run `denom/denom_data_frame_reports.R` to create html reports describing the denominator data output files.

### 5. Data release summary report

If a summary report of all data outputs is required, run `tools/aow_data_release_summary.R`.

## Other useful stuff

The `survey/redcap` directory contains the REDCap data dictionary .csv files used to generate the survey data collection instruments, and used in processing of the raw REDCap survey data.





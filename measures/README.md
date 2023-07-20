# aow_datarelease/measures

These scripts produce the Age of Wonder school-based health measures data files. 

### `DataCleaning/1_CleanBioimpedanceData.do`

Stata script to create the bioimpedance output data file.

### `DataCleaning/2_CleanY9SchoolVisitData.do`

Stata script to create the school visit measures that are captured in REDCap. This produces outputs for:
* height and weight, including derived variables such as BMI and z scores
* blood pressure
* skinfolds

### `measures_convert_to_rds.R`

Converts the Stata output .dta files to .rds format. This is a necessary step if you want to produce html output reports for these files.

### `measures_reports.R`

Creates html reports of the output data files in the `reports` directory. Uses the .rds files, so don't forget to run the conversion script if you recently updated the Stata data files.





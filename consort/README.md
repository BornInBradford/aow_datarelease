# CONSORT: school and pupil recruitment

## Data processing and output

The CONSORT process tracks a recruitment denominator consisting of the populations of all eligible schools through successive stages of school sign-up, ending with pupil recruitment and data collection.

The school onboarding dataset derives from information about contact with headteachers and progress with Data Sharing Agreements recorded contemporaneously by the study team during recruitment.

37 schools are identified as eligible, and 3 year groups (8, 9, 10) are included over four academic years (22-23, 23-24, 24-25, 25-26). So, there are `37*3*4=444` eligible pupil subcohorts in the recruitment denominator. The core CONSORT dataset takes these 444 subcohorts and computes:

* The number of pupils reported by the school as on roll in that year in that year group
* The number of pupils whose details BiB received from the school prior to data collection
* The number of pupils subsequently providing data

The CONSORT output is built in five steps:

1. `1_wrangle_school_onboarding_data.R` performs basic QC checks, cleaning and shaping of the raw data captured by the study team.

2. `2_build_recruitment_wave_data.R` links and shapes the data and derives the required counts for each of the 444 subcohorts.

3. `3_build_consort_tables.R` cross-tabulates the data by recruitment year and year group to derive the counts for the CONSORT diagram.

4. `qc_check_roll_estimate_impacts.R` performs a simple sensitivity analysis to estimate the impact of the way some subcohort counts are adjusted.

5. `aow_consort_design.qmd` outputs an html report describing the assumptions and calculations that form the basis of the CONSORT data, and brings together all the output counts.

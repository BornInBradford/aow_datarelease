# Age of Wonder: denominator

These scripts produce the Age of Wonder data release denominator data files. The denominator contains a record for every consented participant in each academic year. These come from lists provided by the schools.

## `load_and_tidy_denominator_inputs.R` 

Creates the main denominator output files. These need to be created before the main data scripts can be run as they need the denominator files for cleaning and linkage. 

Four outputs are generated in .rds, .dta and .csv formats:

* `denom_identifiable` complete denominator information, including some school identifying info such as school name and establishment number; pupil personal information is dropped, apart from upn and postcode
* `denom_pseudo` only includes pseudo versions of upn and the school identifying info, e.g. as aow_person_id, school_id; also postcode is dropped
* `aow_consent` indicators of activities each participant is consented into
* `id_lookup` a lookup table linking aow_recruitment_id and aow_person_id

## `denom_add_data_availability_indicators.R`

Adds variables to the denominator data files indicating which participants have data from each source (surveys, measures etc). The denominator and data output files need to be present for this to work.

The data availability indicators are added to `denom_identifiable` and `denom_pseudo`.

## `denom_data_frame_reports.R`

Finally, if requried, output html reports decribing the denominator data frames into the `reports` directory.


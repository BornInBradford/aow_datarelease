# Age of Wonder: Resources
This folder contains any open source data used in the creation of the Age of Wonder data. This currently includes geographic boundary information and IMD data. 

## imd2019lsoa.csv
Downloaded from: [Open Data Communities](https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices) 
Download date: 14/02/24. 

Includes all IMD indicies for 2019, including IDACI. Data is in long format (one record per LSOA code/IMD value combination).

## write_imd2019_for_aow.R
Processes imd2019lsoa.csv and converts from long to wide format, with one row per LSOA. Extracts data for IMD 2019 score and deciles. This is saved as imd2019_for_aow.csv

## imd2019_for_aow.csv
IMD data used to create IMD_2019_score and IMD_2019_deciles variables in denominator file. 

## postcode_lsoa_lookup.zip
Contains a compressed version of PCD_OA_LSOA_MSOA_LAD_NOV22_UK_LU.csv. File is compressed to enable it to be stored on GitHub. 

### PCD_OA_LSOA_MSOA_LAD_NOV22_UK_LU.csv
Downloaded from [Open Geography Portal](https://geoportal.statistics.gov.uk/datasets/9c5ebee4163d435aa4defdaf348ba3c2/about)
Download date: 14/02/2024

Lookup table for UK postcodes to OA, LSOA, MSOA and Local authorities, as of November 2022. Uses 2011 census boundaries. This the most recent (and final) lookup for the 2011 census boundaries. Using the lookup for 2011 boundaries as IMD is only produced using 2011 boundaries.

This is not a BiB product, and we are sharing this under the terms of the Open Government License. 
Contains OS data © Crown copyright and database right [2024]
Contains Royal Mail data © Royal Mail copyright and database right [2024]
Source: Office for National Statistics licensed under the Open Government Licence v.3.0
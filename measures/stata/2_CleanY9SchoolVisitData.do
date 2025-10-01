/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW year 9 measurements data

	Date created				: 	13th July 2024
	
	Amended						:	29th September 2025
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all


// Import data
use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Keep relevant variables
keep hw_aow_id date_time_collection hw_height hw_weight bp_arm_circ_a4 bp_cuff_size_a4 bp_clothing_a4 bp_sys_1 bp_dia_1 bp_sys_2 bp_dia_2 sk_tricep sk_subscap

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id
lab var aow_recruitment_id "Age of Wonder recruitment ID"

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate
order aow_recruitment_id date_measurement
lab var date_measurement "Date of measurement"

* Drop duplicates
duplicates drop	// n=456

* Drop if no measurement data
drop if hw_height==. & hw_weight==. & bp_sys_1==. & bp_dia_1==. & sk_tricep==. & sk_subscap==.	// n=1,103

* Drop records where we cannot identify the recruitment era
*drop if aow_recruitment_id=="aow1002641"
*drop if aow_recruitment_id=="aow1002666"


* Check for duplicate recruitment IDs
duplicates tag aow_recruitment_id, generate(tag)
tab tag

* There are a number of entries for the same recruitment ID where the date is the same but the measurements are different, or the measurements are the same but the date is different. Export and send to Theresa for checking. 

preserve
keep if tag==1
export delimited using "U:\Born In Bradford - Confidential - Data\BiB\processing\AoW\measures\data\Y9MeasurementsForChecking_20250930.csv", replace
restore

/* Received some information from Theresa, but insufficient to make decisions. Therefore, drop these. */

drop if tag==1
drop tag


// Check recruitment IDs, should only be one per child
codebook aow_recruitment_id	/* Someone is in twice, which is weird because I thought I had dropped all these above */
duplicates tag aow_recruitment_id, generate(tag)	// No duplicates
sort aow_recruitment_id	// Ah, one is blank. There are lots of non-sensical ones too 
drop if aow_recruitment_id==""
drop tag


* Merge with denominator 
merge 1:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keep(3) nogen keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id)

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date
lab var age_m "Age in months at measurement"
lab var age_y "Age in years at measurement"

order aow_recruitment_id aow_person_id BiBPersonID - age_y 
drop bp_arm_circ_a4 bp_cuff_size_a4 bp_clothing_a4

// Check data\
codebook

compress
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta", replace


*------------------------------------------------------------------------------*
* Height and weight
*------------------------------------------------------------------------------*

use "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta", clear

* Drop variables not required
keep aow_recruitment_id - hw_weight

* Drop if no height and weight measurement	
drop if hw_height==. & hw_weight==.	/* n=34 */

* Drop duplicates
duplicates drop	/* n=0 */

* Check to see whether bioimpedance heights/weights are in this dataset
merge 1:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance.dta", keepusing(height weight) nogen keep(1 3)

* For matched variables, replace any measurements missing from Y9 measurements with those from bioimpedance
replace hw_height = height if height!=. & hw_height==.
replace hw_weight = weight if weight!=. & hw_weight==.

* Check values 
sum hw_height hw_weight, det
scatter hw_height hw_weight, mlabel(aow_recruitment_id)

list aow_recruitment_id if hw_weight>800 & hw_weight<.
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1203082"
replace hw_weight=51.2 if aow_recruitment_id=="aow1203082"

edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1186105"

edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1212273"
replace hw_height=. if aow_recruitment_id=="aow1212273"
// Incorrect height (16)

* Some implausible values here. Check them against bioimpedance data

* Identify height differences between the datasets
gen heightdiff = abs(hw_height - height)
sum heightdiff

* There are many where the difference betweent he height from school with height from bioimpedance if difference is <1. This is generally because the school measurements are more precise, so I'm going to leave these. 

* However, if the height difference is >=1, I will set these to missing
replace hw_height=. if heightdiff>=1 & heightdiff<.

// Export this because I need to drop these from the bioimpedance dataset
preserve
keep if heightdiff>1 & heightdiff<.
keep aow_recruitment_id aow_person_id height
export delimited using "U:\Born In Bradford - Confidential - Data\BiB\processing\AoW\measures\data\HeightErrors_20250930.csv", replace
restore

// Visualise
scatter hw_height hw_weight, mlabel(aow_recruitment_id)

* Identify weight differences between the datasets
gen weightdiff = abs(hw_weight - weight)
sum weightdiff
edit aow_recruitment_id date_measurement hw_height height hw_weight weight weightdiff if weightdiff>0 & weightdiff<.

/* For weight, the bioimpedance measurements will be correct */ 
replace hw_weight = weight if weightdiff>0 & weightdiff<.

* Drop bioimpedance data
drop height weight heightdiff weightdiff

* Generate and format BMI
gen bmi = hw_weight/hw_height^2 * 10000
replace bmi = round(bmi, 0.1)
lab var bmi "BMI (kg/m2)"

* Rename variables 
rename hw_height height
rename hw_weight weight

* Re-check measurements	
sum height weight bmi, det

scatter height weight, mlabel(aow_recruitment_id)


graph matrix height weight bmi, mlabel(aow_recruitment_id)

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1365550" 
drop if aow_recruitment_id=="aow1365550"

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1313964" 
drop if aow_recruitment_id=="aow1313964"

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1247758" // Outlier but plausible

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1336338" 
drop if aow_recruitment_id=="aow1336338"

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1186105"
drop if aow_recruitment_id=="aow1186105"

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1240910"
drop if aow_recruitment_id=="aow1240910"

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1368497" // Outlier but plausible

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1003284" // Outlier but plausible

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1256239" // Outlier but plausible

edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1247758"
drop if aow_recruitment_id=="aow1247758"


scatter height weight, mlabel(aow_recruitment_id)


graph matrix height weight bmi, mlabel(aow_recruitment_id)

* Order variables
order age_m age_y, after(date_measurement)

* Recheck recruitment id
codebook aow_recruitment_id aow_person_id

* Check the duplicate person IDs
bysort aow_person_id (date_measurement): gen count = _n
bysort aow_person_id: gen total = _N
tab total
edit aow_recruitment_id aow_person_id date_measurement height weight bmi if total==2
// Due to measurements in the last two academic years
drop count total

compress
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_heightweight.dta", replace

*------------------------------------------------------------------------------*
* Blood pressure
*------------------------------------------------------------------------------*

use "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta", clear
* Drop variables not required
keep aow_recruitment_id - date_measurement bp*

* Drop if no bp measurements
drop if bp_sys_1==. & bp_dia_1==.	/* n=416 */
count	/* n=7,383 */

*** Summary stats systolic
sum bp_sys_1 bp_sys_2, det	
scatter bp_sys_1 bp_sys_2

* There are still some huge differences between the two readings here, I will set them to missing if diff>50
gen sbpdiff = abs(bp_sys_1 - bp_sys_2)
sum sbpdiff, det
replace bp_sys_1=. if sbpdiff>50 & sbpdiff<.
replace bp_sys_2=. if sbpdiff>50 & sbpdiff<.
scatter bp_sys_1 bp_sys_2

*** Summary stats diastolic
sum bp_dia_1 bp_dia_2, det	

scatter bp_dia_1 bp_dia_2

* There are some huge differences between the two readings here, I will set them to missing if diff>50
gen dbpdiff = abs(bp_dia_1 - bp_dia_2)
replace bp_dia_1=. if dbpdiff>50 & dbpdiff<.
replace bp_dia_2=. if dbpdiff>50 & dbpdiff<.

* SBP and DBP
scatter bp_sys_1 bp_dia_1
scatter bp_sys_2 bp_dia_2
// implausible diastolic values
replace bp_dia_1=. if bp_dia_1>150
drop *diff

* Check all recruitment and person ids are unique
codebook aow_recruitment_id aow_person_id

* Save
compress
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bp.dta", replace


*------------------------------------------------------------------------------*
* Skin folds
*------------------------------------------------------------------------------*

use "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta", clear

* Drop variables not required
keep aow_recruitment_id - date_measurement sk_tricep sk_subscap

* Drop if no skin fold measurements
drop if sk_tricep==. & sk_subscap==.	/* n=1,224 */

* Relabel
lab var sk_tricep "Triceps skinfold (mm)"
lab var sk_subscap "Subscapular skinfold (mm)"

* Summary stats
sum sk*, det	
scatter sk_tricep sk_subscap
/* all look plausible */

* Check all recruitment and person ids are unique
codebook aow_recruitment_id aow_person_id	// 5 participants with two measurements

* Save
compress
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_sk.dta", replace


erase "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta"





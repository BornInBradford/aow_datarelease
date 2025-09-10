/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW year 9 measurements data

	Date created				: 	13th July 2024
	
	Amended						:	12th December 2024
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all


*------------------------------------------------------------------------------*
* Drop duplicates
*------------------------------------------------------------------------------*

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
duplicates drop

* Drop if no measurement data
drop if hw_height==. & hw_weight==. & bp_sys_1==. & bp_dia_1==. & sk_tricep==. & sk_subscap==.

* Drop records where we cannot identify the recruitment era
*drop if aow_recruitment_id=="aow1002641"
*drop if aow_recruitment_id=="aow1002666"

* Check for duplicate recruitment IDs
duplicates tag aow_recruitment_id, generate(tag)
tab tag

* Drop duplicate recruitment IDs (keeping the earliest measurement). Two duplicates need to be dropped entirely. 
bysort aow_recruitment_id (date_measurement): gen count=_n
keep if count==1
drop count

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

compress
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta", replace


*------------------------------------------------------------------------------*
* Height and weight
*------------------------------------------------------------------------------*

use "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta", clear

* Drop variables not required
keep aow_recruitment_id - hw_weight

* Drop if no height and weight measurements
drop if hw_height==. & hw_weight==.	/* n=15 */

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
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1159037"
replace hw_height=151 if aow_recruitment_id=="aow1159037"
replace hw_weight=40.4 if aow_recruitment_id=="aow1159037"
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1186105"
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1050913"
// Height has been entered same value as weight (40.9); no corresponding bioimpedance data
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1059260"
replace hw_height=146 if aow_recruitment_id=="aow1059260"
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1212273"
replace hw_height=. if aow_recruitment_id=="aow1212273"
// Incorrect height (16)

* Some implausible values here. Check them against bioimpedance data

* Identify height differences between the datasets
gen heightdiff = abs(hw_height - height)
sum heightdiff
edit aow_recruitment_id date_measurement hw_height height hw_weight weight heightdiff if heightdiff>1 & heightdiff<.
/* Either height is feasible. For consistency, I will prioritise the bioimpedance measurements */ 
replace hw_height = height if heightdiff>1 & heightdiff<.
replace hw_weight = weight if heightdiff>1 & heightdiff<.

* Identify weight differences between the datasets
gen weightdiff = abs(hw_weight - weight)
sum weightdiff
edit aow_recruitment_id date_measurement hw_height height hw_weight weight weightdiff if weightdiff>1 & weightdiff<.
/* Prioritise the bioimpedance measurements */ 
replace hw_weight = weight if weightdiff>1 & weightdiff<.

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

scatter height weight
* Remove outlier
replace height=. if height<50

graph matrix height weight bmi, mlabel(aow_recruitment_id)
edit aow_recruitment_id aow_person_id date_measurement height weight bmi if aow_recruitment_id=="aow1186105"
// Outlier but plausible

* Re-check measurements	
sum height weight bmi, det

edit aow_recruitment_id height weight bmi if bmi>50 & bmi<.
// BMI calculated when no height available. Set to missing
replace bmi=. if aow_recruitment_id=="aow1050913" | aow_recruitment_id=="aow1212273"

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
drop if bp_sys_1==. & bp_dia_1==.	/* n=274 */
count	/* n=4,855 */

**** Identify implausible values and export data for checking
*preserve
*keep aow_recruitment_id date_measurement age_y - bp_dia_2

*** Summary stats systolic
sum bp_sys_1 bp_sys_2, det	
scatter bp_sys_1 bp_sys_2

* There are still some huge differences between the two readings here, I will set them to missing if diff>50
gen sbpdiff = abs(bp_sys_1 - bp_sys_2)
replace bp_sys_1=. if sbpdiff>50 & sbpdiff<.
replace bp_sys_2=. if sbpdiff>50 & sbpdiff<.
scatter bp_sys_1 bp_sys_2

*** Summary stats diastolic
sum bp_dia_1 bp_dia_2, det	

scatter bp_dia_1 bp_dia_2

* There are still some huge differences between the two readings here, I will set them to missing if diff>50
gen dbpdiff = abs(bp_dia_1 - bp_dia_2)
replace bp_dia_1=. if dbpdiff>50 & dbpdiff<.
replace bp_dia_2=. if dbpdiff>50 & dbpdiff<.

* SBP and DBP
scatter bp_sys_1 bp_dia_1
scatter bp_sys_2 bp_dia_2
// implausible diastolic values
replace bp_dia_2=. if bp_dia_2<25
drop *diff

* Check all recruitment and person ids are unique
codebook aow_recruitment_id aow_person_id

/* We have 4,850 unique person IDs out of 4,855 */
bysort aow_person_id (date_measurement): gen count = _n
bysort aow_person_id: gen total = _N
tab total
edit aow_recruitment_id aow_person_id date_measurement bp_sys_1 bp_dia_1 bp_sys_2 bp_dia_2 count total if total==2
// Due to readings completed in both academic years
drop total count 

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
drop if sk_tricep==. & sk_subscap==.	/* n=799 */

* Relabel
lab var sk_tricep "Triceps skinfold (mm)"
lab var sk_subscap "Subscapular skinfold (mm)"

* Summary stats
sum sk*, det	
scatter sk_tricep sk_subscap
/* all look plausible */

* Check all recruitment and person ids are unique
codebook aow_recruitment_id aow_person_id

* Save
compress
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_sk.dta", replace


erase "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\meas_denom.dta"




